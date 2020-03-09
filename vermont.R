
prep_vt <- function(voter_file, date){
  data <- fread("D:/rolls/vermont/1.25.2019 Statewide Voter File/1.25.2019 Statewide Voter File.txt")
  colnames(data) <- gsub("[.]", "_", make.unique(make.names(colnames(data))))
  
  data <- data %>% 
    select(voter_id = VoterID,
           last_name = Last_Name,
           first_name = First_Name,
           middle_name = Middle_Name,
           street = Legal_Address_Line_1,
           city = Legal_Address_City,
           zip = Legal_Address_Zip,
           dob = Year_of_Birth,
           registration_date = Date_of_Registration,
           last_voted = Date_last_Voted) %>% 
    mutate(state = "VT")
  
  dbWriteTable(db_rolls, name = paste0("VT_", date), value = data, overwrite = T, append = F)
}
