#!/usr/bin/env Rscript

library(dplyr)

# use non-label report CSVs
C2 <- readr::read_csv("UMMAPNACC30_DATA_2018-03-07_1452-C2.csv", trim_ws = TRUE)
D1 <- readr::read_csv("UMMAPNACC30_DATA_2018-03-07_1453-D1.csv", trim_ws = TRUE)

# Eliminate rows that show "Visit 2" in `Event Name` col
C2 %>% 
  filter(redcap_event_name == "visit_1_arm_1") %>% 
  dim()
D1 %>% 
  filter(redcap_event_name == "visit_1_arm_1") %>% 
  dim()

C2 <- C2 %>% 
  filter(redcap_event_name == "visit_1_arm_1")
D1 <- D1 %>% 
  filter(redcap_event_name == "visit_1_arm_1")

## Table 8
## Count number of various diagnoses
D1 %>% 
  mutate(dx = if_else(normcog == 1, "Normal",
              if_else(demented == 1, "Demented",
              if_else(mciamem == 1, "MCI",
              if_else(mciaplus == 1, "MCI",
              if_else(mcinon1 == 1, "MCI",
              if_else(mcinon2 == 1, "MCI", NULL, NULL
              ))))))) %>% 
  select(ptid, redcap_event_name, dx) %>% 
  head(., n = 20)
D1 %>% 
  mutate(dx = if_else(mciamem == 1, "MCI",
              if_else(mciaplus == 1, "MCI", "blah"))) %>% 
  select(ptid, redcap_event_name, dx) %>% 
  head(., n = 20)

D1_cp <- D1
D1_cp$dx <- rep("", nrow(D1_cp))
# Normal
D1_cp <- D1_cp %>% 
  mutate(dx = if_else(normcog == 1, "Normal", dx, missing = dx))
sum(D1_cp$dx == "Normal")
# Impaired, not MCI
D1_cp <- D1_cp %>% 
  mutate(dx = if_else(impnomci == 1, "Impaired, not MCI", dx, missing = dx))
sum(D1_cp$dx == "Normal")
sum(D1_cp$dx == "Impaired, not MCI")

# Dementia
D1_cp <- D1_cp %>% 
  mutate(dx = if_else(demented == 1, "Dementia", dx, missing = dx))
sum(D1_cp$dx == "Normal")
sum(D1_cp$dx == "Impaired, not MCI")
sum(D1_cp$dx == "Dementia")

# MCI
D1_cp <- D1_cp %>% 
  mutate(dx = if_else(mciamem == 1, "MCI", dx, missing = dx))
D1_cp <- D1_cp %>% 
  mutate(dx = if_else(mciaplus == 1, "MCI", dx, missing = dx))
D1_cp <- D1_cp %>% 
  mutate(dx = if_else(mcinon1 == 1, "MCI", dx, missing = dx))
D1_cp <- D1_cp %>% 
  mutate(dx = if_else(mcinon2 == 1, "MCI", dx, missing = dx))
D1_cp <- D1_cp %>% 
  mutate(dx = if_else(impnomci == 1, "Impaired, not MCI", dx, missing = dx))
sum(D1_cp$dx == "Normal")
sum(D1_cp$dx == "Impaired, not MCI")
sum(D1_cp$dx == "Dementia")
sum(D1_cp$dx == "MCI")









