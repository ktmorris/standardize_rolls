prep_ct <- function(voter_file, date){
  names <- read_csv("D:/rolls/connecticut/varnames.csv")
  
  ct <- rbindlist(lapply(c(1:4), function(x){
    j <- fread(paste0("D:/rolls/connecticut/", voter_file, "/VOTELCT", x, "/SSP/ELCT/VOTER/EXT", x, ".csv"))
  }), fill = T)
  
  colnames(ct)[1:nrow(names)] <- names$name
  
  ct <- ct %>% 
    mutate_at(vars(VTR_AD_NUM, NM_STREET), ~ ifelse(is.na(.), "", .)) %>% 
    mutate(street = paste(VTR_AD_NUM, NM_STREET),
           street = gsub("\\s+", " ", street),
           zip = str_pad(ZIP5, side = "left", width = 5, pad = "0")) %>% 
    select(voter_id = VTR_ID_VOTER,
           first_name = VTR_NM_FIRST,
           middle_name = VTR_NM_MID,
           last_name = VTR_NM_LAST,
           street,
           state = ST,
           zip,
           gender = CD_SEX,
           party = CD_PARTY,
           dob = DT_BIRTH,
           last_vote = ELECTION_OCCURS,
           registration_date = DT_ACCEPT)
  
  dbWriteTable(db_rolls, name = paste0("CT_", date), value = ct, overwrite = T, append = F)
}