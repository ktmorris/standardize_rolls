prep_ca <- function(voter_file, history_file, date){
  data <- read.csv.sql(voter_file, sep = "\t",
                       sql = "select RegistrantID, AddressNumber, HouseFractionNumber,
                     AddressNumberSuffix, StreetDirPrefix,
                     StreetName, StreetType, StreetDirSuffix,
                     LastName, FirstName, MiddleName, DOB, Gender,
                     PartyCode, RegistrationDate, City, Zip from file limit 10000")
  data <- data %>% 
    mutate_at(vars(AddressNumber, HouseFractionNumber, AddressNumberSuffix, StreetDirPrefix,
                   StreetName, StreetType, StreetDirSuffix), ~ ifelse(is.na(.), "", .)) %>%
    mutate(street = paste(AddressNumber, HouseFractionNumber, AddressNumberSuffix, StreetDirPrefix,
                          StreetName, StreetType, StreetDirSuffix),
           street = gsub("\\s+", " ", trimws(street))) %>% 
    select(voter_id = RegistrantID,
           first_name = FirstName,
           last_name = LastName,
           middle_name = MiddleName,
           dob = DOB,
           gender = Gender,
           party = PartyCode,
           registration_date = RegistrationDate,
           city = City,
           zip = Zip,
           street) %>% 
    mutate(state = "CA")
  
  ######
  
  datah <- read.csv.sql(history_file, sep = "\t",
                        sql = "select RegistrantID, ElectionDate from file") %>%  
    group_by(voter_id = RegistrantID) %>% 
    mutate(ElectionDate = as.Date(ElectionDate)) %>% 
    arrange(desc(ElectionDate)) %>% 
    filter(row_number() == 1) %>% 
    select(voter_id, last_vote = ElectionDate)
  
  data <- left_join(data, datah, by = "voter_id")
  
  #####
  
  dbWriteTable(db_rolls, name = paste0("CA_", date), value = data, overwrite = T, append = F)
  
  rm(data, datah)
}

