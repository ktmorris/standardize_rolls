## ALASKA
prep_ak <- function(voter_file, date){
  data <- fread(voter_file, fill = T)
  colnames(data) <- gsub("[.]", "_", make.unique(make.names(colnames(data))))
  
  data_roll <- select(data, -starts_with("VH"), VH1)
  
  data_roll <- data %>% 
    mutate(street = gsub("\\s+", " ", trimws(RESIDENCE_ADDRESS)),
           state = "AK",
           party = ifelse(PARTY %in% c("D", "R"), PARTY,
                          ifelse(PARTY %in% c("N", "U"), "N", "O")),
           last_vote = 2000 + as.integer(substring(VH1, 1, 2))) %>% 
    select(voter_id = ASCENSION__,
           first_name = FIRST_NAME,
           middle_name = MIDDLE_NAME,
           last_name = LAST_NAME,
           registration_date = REG_DATE,
           party,
           street,
           city = RESIDENCE_CITY,
           zip = RESIDENCE_ZIP,
           state,
           gender = GENDER,
           last_vote)
  
  data_history <- select(data, voter_id = ASCENSION__, starts_with("VH"))
  
  data_history <- pivot_longer(data_history, cols = starts_with("VH")) %>% 
    filter(value != "")
  
  data_history <- data_history %>% 
    mutate(year = 2000 + as.integer(substring(value, 1, 2)),
           election_type = ifelse(substring(value, 3, 6) == "GENR", "general",
                                  ifelse(substring(value, 3, 6) == "PRIM", "primary", "other")),
           vote_mode = substring(value, 8, 8)) %>% 
    select(-name, -value) %>% 
    mutate(state = "AK")
  
  
  dbWriteTable(db_rolls, name = paste0("AK_", date), value = data_roll, overwrite = T, append = F)
  dbWriteTable(db_history, name = paste0("AK_", date), value = data_history, overwrite = T, append = F)
  
}
