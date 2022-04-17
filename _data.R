## packages for working with data ####


# spreadsheets data 
# library(xlsx) # to write to xlsx
library(readxl) # for reading xlsx. there is also a writexl...
library(readODS)

# databases
# library(RMariaDB)
library(RSQLite)


# cleaning up data
library(janitor)
library(lubridate)
library(scales)

# knitr tables etc
# might not need these but prefer to ensure they're loaded before tidyverse
library(knitr)
library(kableExtra)



# tidyverse
library(tidytext)
library(tidyverse)


# Create a connection to local db
csdb_conn <- dbConnect(RSQLite::SQLite(), "~/my_docs/work/petitions/cheshire/case_study/tpop_case_study.db")
# updated the db 211230 with the September stuff.


## petitions metadata ####

qs_ches_sqlite <-
  # use sept update instead of full original
  # in fact this isn't just bho but never mind...
  dbGetQuery(csdb_conn, "select * from bho_qs_ches_petitions_metadata")

# this is the previous "level1" without ches
# updated version with responses... should otherwise be the same
qs_level1_sqlite <-
  dbGetQuery(csdb_conn, "select * from bho_qs_petitions_metadata")

# SE petitions instead of now superseded add_petitions. no longer includes somerset but includes middx.
# will usually need to drop Essex, depending on what you're doing
# middx petitions have no petition type ????
qs_level2_sqlite <-
  dbGetQuery(csdb_conn, "select * from qs_se_petitions_metadata")

qs_ches_petitions <-
  qs_ches_sqlite  %>%
  mutate(county="Ches") %>%
  mutate(year_parse = year, year_strict=year) %>%
  mutate(decade = year - (year %% 10) ) %>%
  relocate(decade, .after = year)


# rogue "rate" topic in herts
# no NA year now
# NAs before were all somerset allegedly (71)
qs_level2_petitions <-
  qs_level2_sqlite  %>%
  mutate(decade = year - (year %% 10) ) %>%
  mutate(year_parse = year, year_strict=year) %>%
  rename(petition_gender=pet_gender) %>%
  # afaict description and abstract are the same info but with diff names
  mutate(abstract=case_when(
    is.na(abstract) ~ description,
    TRUE ~ abstract
  )) %>% 
  mutate(topic = case_when(
    topic=="rate" ~ "rates",
    TRUE ~ topic
  )) %>%
  select(-description, -text, -file, -id, -request)
# is reference in middx unique? yes. so id isn't needed, whatever it was for.


# no na documentyear. 44 NA year after proc, as expected. so ok
qs_level1_petitions <-
  qs_level1_sqlite %>%
  rename(petition_gender=pet_gender) %>%
  mutate(year = case_when(
    documentyear=="1620-1621" ~ 1620,
    documentyear=="1620-1628" ~ 1625,
    documentyear=="1634-1639" ~ 1635,
    documentyear=="1636-1640" ~ 1638,
    documentyear=="1639-1640" ~ 1640,
    documentyear %in% c("1649-1659", "1650-1659", "n.d. [1650s]") ~ 1655,
    documentyear=="1680-1689" ~ 1685,
    TRUE ~ parse_number(str_replace(documentyear, "n.d. \\[", ""), na="[1620-1640]")
  ))  %>%
  mutate(decade = year - (year %% 10) ) %>%
  # this will only allow exact years
  mutate(year_strict = case_when(
    str_detect(documentyear, "\\D") ~ NA_real_,
    TRUE ~ parse_number(str_replace(documentyear, "n.d. \\[", ""))
  ))  %>%
  # the opposite extreme: gives *everything* it can a year, using the first year if there's a range, if you want to restrict eg to 1580-1720 but exact dates within the range don't matter
  mutate(year_parse = parse_number(str_replace(documentyear, "n.d. \\[", ""))) %>%
  relocate(year, decade, response_cat, .after = petition_gender) %>%
  relocate(documentdate, documentyear, .after = comments)


# apart from name of object, this should be a simple replacement for all level 2
# levels 1-3 from sqlite - for this one, s/b only stuff that's in all 3 datasets ####
# could make the level col but CBA
# NA gender = collective? all except one multiple
# 191 petitions in level 2 have no petition type. can't remember which county this is! but not Herts.
qs_petitions_combined <-
  bind_rows(
    qs_ches_petitions,
    qs_level1_petitions %>% mutate(transcribed="y"),
    qs_level2_petitions %>% filter(county=="Herts") %>% mutate(transcribed="n")
  ) %>%
  mutate(gender = case_when(
    petition_gender=="f" ~ "female", 
    petition_gender=="fm" ~ "mixed", 
    petition_gender=="m" ~ "male", 
    #pet_gender=="u" ~ "unknown", # only 9 of these apparently
    TRUE ~ "na"))  %>% 
  mutate(petition_type_s = case_when(
    petition_type=="single" & named_petrs==1 ~ "single",
    str_detect(petition_type, "collective") ~ "collective",
    str_detect(petition_type, "multiple") ~ "group"
  )) %>%
  # ok i think you do need to fix spaces in petition_type
  mutate(petition_type = str_trim(str_replace_all(petition_type, "  +", " "))) %>%
  mutate(petition_id = paste(county, reference, doc_no, sep="_")) %>%
  select(reference, doc_no, ll_img, county, petition_id, decade, topic, subtopic, petition_type, petition_type_s, gender, named_petrs, subscribers, response_cat, year, year_strict, year_parse, petitioner, abstract, transcribed) %>%
  rowid_to_column("all_id")



## notes...

# level1_data
# original code in data_code.rmd if you have any problems

# if using original cheshire qjf usually need to drop unphotographed
#dbGetQuery(csdb_conn,"SELECT * FROM ches_qjf_petitions_metadata")
# also rename petr_nice to petitioner. 

# petition type
# pet_type/petition_type should have been fixed in the updated versions. BUT nb middx doesn't have petition type.
# 2 fewer rows for Essex than previous add petitions?!! and Essex includes a NA petition type; maybe single/single on behalf. but possibly a duplicate petition? so simplest thing would be to drop it
# collective counts match previous
#   mutate(petition_type = case_when(
#   str_detect(pet_type, "collective|single|multiple") ~ pet_type,
#   pet_type=="named" & named_petrs==1 ~ "single",
#   pet_type=="named" & named_petrs>1 ~ "multiple",
#   pet_type=="named on behalf" ~ "multiple on behalf"
# )) %>%
# # drop pet_type now you've fixed it, to avoid confusion?
# select(-pet_type)

# # no response cat for level 2???? only did them for tpop counties it seems. perhaps not enough info in abstracts only to classify? but not even herts?

# ches  
# add county
# "petr_location"     "petr_status"        
# "transcribed"      
# 
# level1       
# "pet_gender" rename petition_gender
# "documentyear"      checked it looks same as previous first
# "sub_gender"        ignore?    
# "residences"        "statuses"   
# 
# level2 is messier...
# "year"          looks fine (no NA now)
# "pet_gender"    rename petition_gender 
