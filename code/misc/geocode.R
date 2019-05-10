
geocode = function(file){
  file$nnn <- c(1:nrow(file))
  file$t <- runif(nrow(file))
  file <- setorder(file, t) %>%
    dplyr::mutate(id = row_number()) %>%
    dplyr::select(-t)

  x <- file %>%
    dplyr::select(id, street, city, state, zip)


  fwrite(x, "c:/users/morrisk/documents/smartylist_windows_latest/geo_file.csv")

  system("cmd.exe",
         input = paste0("c:/users/morrisk/documents/smartylist_windows_latest/smartylist -auth-id=\...\" -auth-token=\"...\"  -input=\"c:/users/morrisk/documents/smartylist_windows_latest/geo_file.csv\""))
  x <- fread("C:/Users/morrisk/Documents/smartylist_windows_latest/geo_file-output.csv")

  files <- list.files("c:/users/morrisk/documents/smartylist_windows_latest/", pattern = "geo_file*", full.names = T)
  lapply(files, function(y){file.remove(y)})

  colnames(x) <- make.unique(make.names(colnames(x)))

  x <- dplyr::select(x, id, latitude = X.latitude., longitude = X.longitude.)

  file <- left_join(file, x, by = "id") %>%
    dplyr::arrange(nnn) %>%
    dplyr::select(-id, -nnn)

  return(file)
}

