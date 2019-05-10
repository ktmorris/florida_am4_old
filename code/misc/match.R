match_rolls_to_doc <- function(doc_data, doc_id,
                               roll_data, roll_id){


  library(dplyr)

  big_doc <- doc_data
  big_roll <- roll_data

  doc_id_grab <- enquo(doc_id)
  doc_id <- as.character(substitute(doc_id))

  roll_id_grab <- enquo(roll_id)
  roll_id <- as.character(substitute(roll_id))

  doc_data <- doc_data %>%
    select(!! doc_id_grab, first_name, last_name, middle_name, dob) %>%
    group_by(first_name, middle_name, last_name, dob) %>%
    mutate(count = row_number()) %>%
    ungroup()



  roll_data <- roll_data %>%
    select(!! roll_id_grab, first_name, last_name, middle_name, dob) %>%
    group_by(first_name, middle_name, last_name, dob) %>%
    mutate(count = row_number()) %>%
    ungroup()


  message("START BY DOING FULL FIRST, MIDDLE, LAST, DOB")
  match1 <- inner_join(doc_data, roll_data, by = c("first_name",
                                                   "middle_name",
                                                   "last_name",
                                                   "dob",
                                                   "count")) %>%
    mutate(run = 1)

  doc_rem <- anti_join(doc_data, match1, by = c(doc_id)) %>%
    mutate(middle_in = substr(middle_name, 1, 1),
           group = middle_in == middle_name) %>%
    group_by(first_name, last_name, middle_in, group, dob) %>%
    mutate(count = row_number()) %>%
    ungroup()

  roll_rem <- anti_join(roll_data, match1, by = c(roll_id)) %>%
    mutate(middle_in = substr(middle_name, 1, 1),
           group = middle_in != middle_name) %>%
    rename(middle_name_roll = middle_name) %>%
    group_by(first_name, last_name, middle_in, group, dob) %>%
    mutate(count = row_number()) %>%
    ungroup()

  message("FIRST, MIDDLE IN / MIDDLE, LAST, DOB")
  match2 <- inner_join(doc_rem, roll_rem, by = c("first_name",
                                                 "middle_in",
                                                 "last_name",
                                                 "group",
                                                 "dob",
                                                 "count")) %>%
    mutate(run = 2)

  doc_rem <- anti_join(doc_rem, match2, by = c(doc_id)) %>%
    mutate(group = is.na(middle_in)) %>%
    select(-middle_in) %>%
    group_by(first_name, last_name, group, dob) %>%
    mutate(count = row_number()) %>%
    ungroup()

  roll_rem <- anti_join(roll_rem, match2, by = c(roll_id)) %>%
    mutate(group = !is.na(middle_in)) %>%
    select(-middle_in) %>%
    group_by(first_name, last_name, group, dob) %>%
    mutate(count = row_number()) %>%
    ungroup()

  message("FIRST, MIDDLE IN / MISSING MIDDLE, LAST, DOB")
  match3 <- inner_join(doc_rem, roll_rem, by = c("first_name",
                                                 "last_name",
                                                 "group",
                                                 "dob",
                                                 "count")) %>%
    mutate(run = 3)

  match <- bind_rows(match1, match2, match3) %>%
    mutate(middle_name = ifelse(is.na(middle_name), middle_name_roll, middle_name)) %>%
    select(!! doc_id_grab, !! roll_id_grab, run)

  doc_rem <- anti_join(doc_data, match, by = c(doc_id)) %>%
    select(!! doc_id_grab)
  roll_rem <- anti_join(roll_data, match, by = c(roll_id)) %>%
    select(!! roll_id_grab)

  big_doc <- full_join(big_doc, match, by = c(doc_id)) %>%
    select(-dob) %>%
    rename_at(vars(ends_with("_name")), ~ paste0(., "_doc"))

  merge <- full_join(big_doc, big_roll, by = c(roll_id)) %>%
    rename_at(vars(ends_with("_name")), ~ paste0(., "_roll")) %>%
    mutate(first_name = ifelse(is.na(first_name_roll), first_name_doc, first_name_roll),
           middle_name = ifelse(is.na(middle_name_roll), middle_name_doc, middle_name_roll),
           last_name = ifelse(is.na(last_name_roll), last_name_doc, last_name_roll)) %>%
    select(-ends_with("_name_roll"), -ends_with( "_name_doc"))



  message("I just returned you a list with three items:")
  message("1. Full merge")
  message("2. Unmatched DOC records")
  message("3. Unmatched voter records")
  return(list(merge, doc_rem, roll_rem))
}
