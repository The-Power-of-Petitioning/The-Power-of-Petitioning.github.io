## packages for working with data ####

library(RSQLite)

# may need these at some point
# library(readxl) 
# library(readODS)

# knitr tables etc
# if used, load before tidyverse
# library(knitr)
# library(kableExtra)

library(janitor)
library(scales)
library(glue)

library(reactable)
library(htmltools)


# tidyverse
library(tidytext)
library(tidyverse)



## tpop petitions metadata ####

# Create a connection to local db
db_conn <- dbConnect(SQLite(), here::here("_data", "tpop_v1_202208.db"))


qs_level1_sqlite <-
  dbGetQuery(db_conn, "select * from QS_petitions")

qs_level1_petitions <-
  qs_level1_sqlite |>
  mutate(year_orig = year) |>
  mutate(year = case_when(
    year=="1620-1621" ~ 1620,
    year=="1620-1628" ~ 1625,
    year=="1634-1639" ~ 1635,
    year=="1636-1640" ~ 1638,
    year=="1639-1640" ~ 1640,
    year %in% c("1649-1659", "1650-1659", "n.d. [1650s]") ~ 1655,
    year=="1680-1689" ~ 1685,
    TRUE ~ parse_number(str_replace(year, "n.d. \\[", ""), na="[1620-1640]")
  ))  |>
  mutate(decade = year - (year %% 10) )


qs_petitions_combined <-
  # bind_rows(
  #   qs_ches_petitions,
  #   qs_level1_petitions |> mutate(transcribed="y"),
  #   qs_level2_petitions |> filter(county=="Herts") |> mutate(transcribed="n")
  # ) |>
  qs_level1_petitions |>
  mutate(gender = case_when(
    petition_gender=="f" ~ "female", 
    petition_gender=="fm" ~ "mixed", 
    petition_gender=="m" ~ "male", 
    #pet_gender=="u" ~ "unknown", # only 9 of these apparently
    TRUE ~ "na"))  |> 
  mutate(petition_type_s = case_when(
    petition_type=="single" & named_petrs==1 ~ "single",
    str_detect(petition_type, "collective") ~ "collective",
    str_detect(petition_type, "multiple") ~ "group"
  )) |>
  # ok i think you do need to fix spaces in petition_type ????? check if this is still an issue!
  mutate(petition_type = str_trim(str_replace_all(petition_type, "  +", " "))) |>
  
  mutate(petition_id = paste(county, petition_id, sep="_")) |> # don't thikn you actually need this for it to be unique...? unless you add herts.
  #select(reference, doc_no, ll_img, county, petition_id, decade, topic, subtopic, petition_type, petition_type_s, gender, named_petrs, subscribers, response_cat, year, year_strict, year_parse, petitioner, abstract, transcribed) |>
  rowid_to_column("all_id")


qs_petitioners_sqlite <-
  dbGetQuery(db_conn, "select * from QS_petitioners")



hol_sqlite <-
  dbGetQuery(db_conn, "select * from HOL_petitions")


sp_sqlite <-
  dbGetQuery(db_conn, "select * from SP_petitions")


dbDisconnect(db_conn)
