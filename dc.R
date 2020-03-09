
prep_dc <- function(voter_file, date){
  dc <- read_xlsx(voter_file)
  
  colnames(dc) <- gsub("[.]", "_", make.unique(make.names(colnames(dc))))
  
  dc$id <- c(1:nrow(dc))
  
  dc1 <- dc %>% 
    select(voter_id = id,
           registration_date = REGISTERED,
           last_name = LASTNAME,
           first_name = FIRSTNAME,
           middle_name = MIDDLE,
           party = PARTY,
           RES_HOUSE,
           RES_STREET,
           city = RES_CITY,
           zip = RES_ZIP) %>% 
    mutate_at(vars(RES_HOUSE, RES_STREET), ~ ifelse(is.na(.), "", .)) %>% 
    mutate(street = paste(RES_HOUSE, RES_STREET),
           street = gsub("\\s+", " ", street),
           state = "DC")
  
  h <- pivot_longer(
    select(dc, id, starts_with("X")), cols = starts_with("X"))
  
  h <- h %>% 
    filter(!is.na(value),
           value %in% c("V", "A", "Y")) %>% 
    mutate(m = substring(name, 2, 3),
           d = substring(name, 4, 5),
           y = paste0("20", substring(name, 6, 7)),
           last_voted = make_date(year = y, month = m, day = d)) %>% 
    filter(last_voted <= "2060-01-01") %>% 
    select(id, last_voted) %>% 
    group_by(id) %>% 
    arrange(desc(last_voted)) %>% 
    filter(row_number() == 1)
  
  dc1 <- left_join(dc1, h, by = c("voter_id" = "id"))
  
  dc1 <- dc1 %>% 
    mutate(party = ifelse(party == "DEMOCRATIC", "D",
                          ifelse(party == "REPUBLICAN", "R",
                                 ifelse(party == "NO PARTY", "N", "O"))))
  
  dbWriteTable(db_rolls, name = paste0("DC_", date), value = dc1, overwrite = T, append = F)
  
}
