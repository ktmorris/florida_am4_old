library(tidyverse)
library(tidycensus)
library(data.table)
library(readxl)
library(sqldf)
library(lubridate)
library(scales)
library(rgdal)

source("./code/misc/geocode.R")
source("./code/misc/get_basic_census.R")
source("./code/misc/match.R")
source("./code/misc/theme_bc.R")

#### read in doc data
#### old data was bad in online OBDIS, fixed data sent to us directly
old <- read_xlsx("./raw_data/doc/brennan1992_1996.xlsx")
colnames(old) <- c("dcnumber", "prisonreleasedate", "offense", "birthdate", "lastname", "firstname", "middlename")

old <- old %>%
  mutate(prisonreleasedate = as.Date(prisonreleasedate, "%m/%d/%Y"),
         birthdate = as.Date(birthdate, "%m/%d/%Y"))

old2 <- rbindlist(lapply(c("97-00", "01-04", "05-08", "09-12", "13-16"), function(f){
  j <- read_xlsx(paste0("./raw_data/doc/fixed_data/Release ", f, ".xlsx"))
  colnames(j) <- c("dcnumber", "prisonreleasedate", "offense", "birthdate", "lastname", "firstname", "middlename")
  j <- j %>%
    mutate(prisonreleasedate = as.Date(prisonreleasedate, "%m/%d/%Y"),
           birthdate = as.Date(birthdate, "%m/%d/%Y"))
}))


new <- fread("./raw_data/doc/INMATE_RELEASE_ROOT_0319.csv") %>%
  select(dcnumber = DCNumber,
         lastname = LastName,
         firstname = FirstName,
         middlename = MiddleName,
         prisonreleasedate = PrisonReleaseDate,
         birthdate = BirthDate) %>%
  mutate_at(vars(birthdate, prisonreleasedate), funs(as.Date(., "%m/%d/%Y")))

## keep one record per dc number
all_released <- bind_rows(old, old2, new) %>%
  group_by(dcnumber) %>%
  filter(row_number() == 1) %>%
  ungroup()

## clean up names - take out punctuation, spaces
all_released <- all_released %>%
  mutate_at(vars(firstname, middlename, lastname), funs(gsub("[[:punct:]]| ", "", .))) %>%
  rename(first_name = firstname,
         middle_name = middlename,
         last_name = lastname,
         dob = birthdate) %>%
  group_by(first_name, middle_name, last_name, dob) %>%
  mutate(count = row_number()) %>%
  ungroup()


#### read florida data to find matches
db <- dbConnect(SQLite(), "D:/rolls.db")

fl_rolls <- dbGetQuery(db, "select County_Code, Voter_ID, Name_Last, Name_First, Name_Middle,
                       Birth_Date, Registration_Date from fl_roll_0319")

colnames(fl_rolls) <- tolower(colnames(fl_rolls))

fl_rolls <- fl_rolls %>%
  mutate_at(vars(name_last, name_first, name_middle), funs(gsub("[[:punct:]]| ", "", .))) %>%
  mutate(birth_date = as.Date(birth_date, "%m/%d/%Y")) %>%
  rename(first_name = name_first,
         middle_name = name_middle,
         last_name = name_last,
         dob = birth_date) %>%
  group_by(first_name, middle_name, last_name, dob) %>%
  mutate(count = row_number()) %>%
  ungroup()


merge_list <- match_rolls_to_doc(all_released, dcnumber, fl_rolls, voter_id)


save(merge_list, file = "./temp_files/fl_list.RData")

doc_voter <- merge_list[[1]] %>%
  filter(!is.na(voter_id), !is.na(dcnumber)) %>%
  select(voter_id, dcnumber)

saveRDS(doc_voter, "./temp_files/doc_voter.rds")


### new voters
load("./temp_files/fl_list.RData")

regs <- merge_list[[1]] %>%
  filter(!is.na(voter_id), !is.na(dcnumber)) %>%
  group_by(registration_date, county_code) %>%
  tally()

recent_regs <- regs %>%
  ungroup() %>% 
  mutate(registration_date = as.Date(registration_date, "%m/%d/%Y")) %>%
  group_by(ym = make_date(year = year(registration_date), month = month(registration_date), day = 1)) %>%
  summarize(n = sum(n)) %>%
  filter(year(ym) >= 2010)

ggplot(recent_regs, aes(x = ym, y = n)) + geom_line() + theme_bc() +
  labs(y = "Number of Individuals Registered", x = "Month", caption = "Sources: Florida BOE;\nFlorida DOC") +
  scale_y_continuous(labels = comma) +
  ggtitle("Monthly Registrations of Formerly Incarcerated Floridians")

ggsave("./output/regs_2019.png", width = 7.5, height = 5)
ggsave("./output/regs_2019.eps", width = 7.5, height = 5, device = cairo_ps)

fwrite(recent_regs, "./output/monthly_regs.csv")

#### demos of newbies

db <- dbConnect(SQLite(), "D:/rolls.db")

florida <- dbGetQuery(db, "select [Registration_Date], [County_Code], [Residence_Address_Line_1], [Residence_City], [Residence_Zipcode],
                           Race, Gender, [Birth_Date], [Voter_ID]
                           from fl_roll_0319") %>%
  mutate(date = as.Date(as.character(Registration_Date), "%m/%d/%Y"),
         street = Residence_Address_Line_1,
         city = Residence_City,
         zip = Residence_Zipcode,
         state = "FL",
         dob = as.Date(as.character(Birth_Date), "%m/%d/%Y")) %>%
  select(date, street, city, zip, state, race = Race, gender = Gender, dob, voter_id = Voter_ID)

florida$new_ex <- (florida$voter_id %in% doc_voter$voter_id) & (florida$date > "2019-01-01")



florida <- geocode(florida)


bg_shp <- readOGR("./raw_data/tl_2018_12_bg", "tl_2018_12_bg")
pings  <- SpatialPoints(florida[c('longitude','latitude')], proj4string = bg_shp@proj4string)
florida$bg <- over(pings, bg_shp)$GEOID

save(florida, file = "./temp_files/florida_coded.RData")

load("./temp_files/florida_coded.RData")

c <- unique(filter(fips_codes, state == "FL")$county_code)
census <- rbindlist(lapply(c, function(county){
  success <- F
  while(!is.data.frame(success)){
    success <- tryCatch({get_basic_census_stats("block group", state = "FL", year = 2017, county = county)}, error = function(e){"F"})
  }
  return(success)
  }))

saveRDS(census, "./temp_files/fl_census_bgs.rds")

census <- readRDS("./temp_files/fl_census_bgs.rds")
florida <- left_join(florida, census, by = c("bg" = "GEOID"))

demos <- florida %>% 
  group_by(new_ex) %>% 
  mutate(white = race == 5,
         black = race == 3,
         latino = race == 4,
         male = gender == "M",
         age = 2019 - year(dob)) %>% 
  summarize(white = mean(white, na.rm = T),
            black = mean(black, na.rm = T),
            latino = mean(latino, na.rm = T),
            male = mean(male, na.rm = T),
            age = median(age, na.rm = T),
            median_income = mean(median_income, na.rm = T),
            unem = mean(unem, na.rm = T),
            some_college = mean(some_college, na.rm = T))

demos$new_ex <- ifelse(demos$new_ex, "Recently Registered\nPreviously Incarcerated", "Voter Population")
fwrite(demos, "output/demographic_data.csv")

ggplot(demos, aes(x = new_ex, y = black)) + geom_col() + theme_bc() +
  scale_y_continuous(labels = percent) + labs(y = "Percent African-American", x = NULL, caption = "Sources: Florida BOE;\nFlorida DOC") +
  ggtitle("African-American Share of Re-Enfranchised and Registered Floridians\nVersus the Florida Voting Population") +
  geom_text(aes(label = paste0(floor(black * 100), "%")), vjust = -0.25,
            family = "Roboto Black")
ggsave("./output/aa.png", width = 7.5, height = 5)
ggsave("./output/aa.eps", width = 7.5, height = 5, device = cairo_ps)

ggplot(demos, aes(x = new_ex, y = latino)) + geom_col() + theme_bc() +
  scale_y_continuous(labels = percent) + labs(y = "Percent African-American", x = NULL, caption = "Sources: Florida BOE;\nFlorida DOC") +
  ggtitle("Latino Share of Re-Enfranchised and Registered Floridians\nVersus the Florida Voting Population")
ggsave("./output/latino.png", width = 7.5, height = 5)

ggplot(demos, aes(x = new_ex, y = white)) + geom_col() + theme_bc() +
  scale_y_continuous(labels = percent) + labs(y = "Percent White", x = NULL, caption = "Sources: Florida BOE;\nFlorida DOC") +
  ggtitle("White Share of Re-Enfranchised and Registered Floridians\nVersus the Florida Voting Population")
ggsave("./output/white.png", width = 7.5, height = 5)

ggplot(demos, aes(x = new_ex, y = male)) + geom_col() + theme_bc() +
  scale_y_continuous(labels = percent) + labs(y = "Percent Male", x = NULL, caption = "Sources: Florida BOE;\nFlorida DOC") +
  ggtitle("Male Share of Re-Enfranchised and Registered Floridians\nVersus the Florida Voting Population")
ggsave("./output/male.png", width = 7.5, height = 5)

ggplot(demos, aes(x = new_ex, y = age)) + geom_col() + theme_bc() +
  labs(y = "Average Age", x = NULL, caption = "Sources: Florida BOE;\nFlorida DOC") +
  ggtitle("Average Age of Re-Enfranchised and Registered Floridians\nVersus the Florida Voting Population")
ggsave("./output/age.png", width = 7.5, height = 5)


ggplot(demos, aes(x = new_ex, y = median_income)) + geom_col() + theme_bc() +
  scale_y_continuous(labels = dollar) + labs(y = "Average Income", x = NULL, caption = "Sources: Florida BOE;\nFlorida DOC;\nUS Census Bureau") +
  ggtitle("Average Income of Re-Enfranchised and Registered Floridians\nVersus the Florida Voting Population") +
  geom_text(aes(label = dollar(floor(median_income))), vjust = -0.25,
            family = "Roboto Black")
ggsave("./output/income.png", width = 7.5, height = 5)
ggsave("./output/income.eps", width = 7.5, height = 5, device = cairo_ps)

ggplot(demos, aes(x = new_ex, y = unem)) + geom_col() + theme_bc() +
  scale_y_continuous(labels = percent) + labs(y = "Unemployment Rate", x = NULL, caption = "Sources: Florida BOE;\nFlorida DOC\nUS Census Bureau") +
  ggtitle("Unemployment Rate of Re-Enfranchised and Registered Floridians\nVersus the Florida Voting Population")
ggsave("./output/unemployment.png", width = 7.5, height = 5)

ggplot(demos, aes(x = new_ex, y = some_college)) + geom_col() + theme_bc() +
  scale_y_continuous(labels = percent) + labs(y = "Percent With Some College", x = NULL, caption = "Sources: Florida BOE;\nFlorida DOC\nUS Census Bureau") +
  ggtitle("Share with Some College of Re-Enfranchised and Registered Floridians\nVersus the Florida Voting Population")
ggsave("./output/college.png", width = 7.5, height = 5)
