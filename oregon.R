
prep_or <- function(roll_dir, history_dir, date){
  dir <- roll_dir
  
  files <- list.files(dir, full.names = T)
  files <- files[!grepl("readme", files)]
  
  data <- rbindlist(lapply(files, function(file){
    d <- fread(file) %>% 
      mutate_at(vars(HOUSE_NUM, PRE_DIRECTION, STREET_NAME, STREET_TYPE, POST_DIRECTION), ~ ifelse(is.na(.), "", .)) %>%
      mutate(street = paste(HOUSE_NUM, PRE_DIRECTION, STREET_NAME, STREET_TYPE, POST_DIRECTION),
             street = gsub("\\s+", " ",  street),
             state = "OR",
             PARTY_CODE = ifelse(PARTY_CODE  == "DEM", "D",
                                 ifelse(PARTY_CODE == "REP", "R",
                                        ifelse(PARTY_CODE == "NAV", "N", "O")))) %>% 
      select(voter_id = VOTER_ID,
             street,
             first_name = FIRST_NAME,
             middle_name = MIDDLE_NAME,
             last_name = LAST_NAME,
             dob = BIRTH_DATE,
             registration_date = EFF_REGN_DATE,
             party = PARTY_CODE,
             city = CITY,
             zip = ZIP_CODE)
  }))
  
  ##############
  
  dir <- history_dir
  
  files <- list.files(dir, full.names = T)
  files <- files[!grepl("readme", files)]
  
  hist <- rbindlist(lapply(files, function(file){
    d <- fread(file)
    
    colnames(d) <- gsub("[.]", "_", make.unique(make.names(colnames(d))))
    
    d <- pivot_longer(select(d, voter_id = VOTER_ID, starts_with("X")),
                      cols = starts_with("X")) %>% 
      filter(value == "YES") %>% 
      mutate(last_voted = as.Date(substring(name, 2), format = "%m_%d_%Y")) %>% 
      group_by(voter_id) %>% 
      arrange(desc(last_voted)) %>% 
      filter(row_number() == 1) %>% 
      select(voter_id, last_voted)
    
  }))
  
  data <- left_join(data, hist, by = "voter_id")
  
  dbWriteTable(db_rolls, name = paste0("OR_", date), value = data, overwrite = T, append = F)
}
