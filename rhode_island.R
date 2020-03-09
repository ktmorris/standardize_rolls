
prep_ri <- function(voter_file, history_file, date){
  data <- fread(voter_file)
  
  colnames(data) <- gsub("[.]", "_", make.unique(make.names(colnames(data))))
  
  data <- data %>% 
    mutate_at(vars(STREET_NUMBER, STREET_NAME, STREET_NAME_2), ~ ifelse(is.na(.), "", .)) %>% 
    mutate(street = paste(STREET_NUMBER, STREET_NAME, STREET_NAME_2),
           street = gsub("\\s+", " ", street),
           state = "RI") %>% 
    select(city = CITY,
           zip = ZIP_CODE,
           registration_date = DATE_ACCEPTED,
           last_name = LAST_NAME,
           first_name = FIRST_NAME,
           middle_name = MIDDLE_NAME,
           gender = SEX,
           dob = YEAR_OF_BIRTH,
           street,
           voter_id = VOTER_ID)
  
  hist <- fread(history_file)
  colnames(hist) <- gsub("[.]", "_", make.unique(make.names(colnames(hist))))
  
  hist <- pivot_longer(select(hist, voter_id = VOTER_ID,
                              party = CURRENT_PARTY,
                              starts_with("DATE_")),
                       cols = starts_with("DATE_")) %>% 
    filter(value != "") %>% 
    group_by(voter_id, party) %>% 
    mutate(last_voted = as.Date(value, format = "%m/%d/%Y")) %>% 
    arrange(desc(last_voted)) %>% 
    filter(row_number() == 1) %>% 
    select(voter_id, party, last_voted)
  
  
  data <- left_join(data, hist, by = "voter_id")
  
  dbWriteTable(db_rolls, name = paste0("RI_", date), value = data, overwrite = T, append = F)
}
