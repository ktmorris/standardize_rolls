prep_wa <- function(voter_file, history_file, date){
  data <- fread(voter_file)

data <- data %>% 
  mutate_at(vars(RegStNum, RegStFrac, RegStPreDirection, RegStName, RegStType,
            RegUnitType, RegStPostDirection), ~ ifelse(is.na(.), "", .)) %>% 
  mutate(street = paste(RegStNum, RegStFrac, RegStPreDirection, RegStName, RegStType,
                        RegUnitType, RegStPostDirection),
         street = gsub("\\s+", " ", street),
         state = "WA") %>% 
  select(street,
         state,
         voter_id = StateVoterID,
         first_name = FName,
         middle_name = MName,
         last_name = LName,
         dob = Birthdate,
         gender = Gender,
         zip = RegZipCode,
         registration_date = Registrationdate,
         last_voted = LastVoted)
dbWriteTable(db_rolls, name = paste0("WA_", date), value = data, overwrite = T, append = F)
}