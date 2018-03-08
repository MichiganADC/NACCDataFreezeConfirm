#!/usr/bin/env Rscript

library(dplyr)

# use non-label report CSVs
C2 <- readr::read_csv("C2-UMMAPNACC30_DATA_2018-03-08_0811.csv", trim_ws = TRUE, na = "")
D1 <- readr::read_csv("D1-UMMAPNACC30_DATA_2018-03-08_0812.csv", trim_ws = TRUE, na = "")

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

####
## Table 3
## Count "most recent cognitive status"
D1_cp <- D1
D1_cp$dx <- rep("", nrow(D1_cp))

# Normal
D1_cp <- D1_cp %>% 
  mutate(dx = if_else(normcog == 1, "Normal", dx, missing = dx))
cat("Normal:", sum(D1_cp$dx == "Normal"))

# Impaired, not MCI
D1_cp <- D1_cp %>% 
  mutate(dx = if_else(impnomci == 1, "Impaired, not MCI", dx, missing = dx))
cat("Normal:", sum(D1_cp$dx == "Normal"))
cat("Impaired, not MCI:", sum(D1_cp$dx == "Impaired, not MCI"))

# MCI
D1_cp <- D1_cp %>% 
  mutate(dx = if_else(mciamem == 1, "MCI", dx, missing = dx))
D1_cp <- D1_cp %>% 
  mutate(dx = if_else(mciaplus == 1, "MCI", dx, missing = dx))
D1_cp <- D1_cp %>% 
  mutate(dx = if_else(mcinon1 == 1, "MCI", dx, missing = dx))
D1_cp <- D1_cp %>% 
  mutate(dx = if_else(mcinon2 == 1, "MCI", dx, missing = dx))
cat("Normal:", sum(D1_cp$dx == "Normal"))
cat("Impaired, not MCI:", sum(D1_cp$dx == "Impaired, not MCI"))
cat("MCI:", sum(D1_cp$dx == "MCI"))

# Dementia
D1_cp <- D1_cp %>% 
  mutate(dx = if_else(demented == 1, "Dementia", dx, missing = dx))
cat("Normal:", sum(D1_cp$dx == "Normal"))
cat("Impaired, not MCI:", sum(D1_cp$dx == "Impaired, not MCI"))
cat("MCI:", sum(D1_cp$dx == "MCI"))
cat("Dementia:", sum(D1_cp$dx == "Dementia"))

####
## Table 8
## Count "most recent primary etiologic diagnosis by dementia syndrom"
D1_cp$pr_etio_dx <- rep("", nrow(D1_cp))

class(D1_cp$alzdis)
D1_cp$alzdis <- as.integer(D1_cp$alzdis)
class(D1_cp$lbdis)
D1_cp$lbdis <- as.integer(D1_cp$lbdis)
class(D1_cp$msa)
D1_cp$msa <- as.integer(D1_cp$msa)
class(D1_cp$psp)
D1_cp$psp <- as.integer(D1_cp$psp)
class(D1_cp$cort)
D1_cp$cort <- as.integer(D1_cp$cort)
class(D1_cp$ftldmo)
D1_cp$ftldmo <- as.integer(D1_cp$ftldmo)
class(D1_cp$ftldnos)
D1_cp$ftldnos <- as.integer(D1_cp$ftldnos)
class(D1_cp$cvd)
D1_cp$cvd <- as.integer(D1_cp$cvd)


D1_cp %>% 
  filter(demented == 1) %>% 
  select(ptid, redcap_event_name, alzdis, lbdis, msa, psp, cort, ftldmo, ftldnos, cvd) %>% 
  replace(is.na(.), 0) %>% 
  mutate(pr_etio_dx_sum = alzdis + cort + msa + psp + cort + ftldmo + ftldnos + cvd) %>%
  select(ptid, pr_etio_dx_sum) %>% 
  print(., n = nrow(.)) %>%
  group_by(pr_etio_dx_sum) %>% 
  summarize(n = n())


# Alzheimer's disease
D1_cp <- D1_cp %>% 
  mutate(pr_etio_dx = if_else(alzdis == 1 & alzdisif == 1, "Alzheimer's disease", pr_etio_dx, missing = pr_etio_dx))
cat("Alzheimer's disease:", sum(D1_cp$pr_etio_dx == "Alzheimer's disease"))
# Lewy body disease
D1_cp <- D1_cp %>% 
  mutate(pr_etio_dx = if_else(lbdis == 1 & lbdif == 1, "Lewy body disease", pr_etio_dx, missing = pr_etio_dx))
cat("Alzheimer's disease:", sum(D1_cp$pr_etio_dx == "Alzheimer's disease"))
cat("Lewy body disease:", sum(D1_cp$pr_etio_dx == "Lewy body disease"))
# Multiple system atrophy
D1_cp <- D1_cp %>%
  mutate(pr_etio_dx = if_else(msa == 1 & msaif == 1, "Multiple system atrophy", pr_etio_dx, missing = pr_etio_dx))
cat("Alzheimer's disease:", sum(D1_cp$pr_etio_dx == "Alzheimer's disease"))
cat("Lewy body disease:", sum(D1_cp$pr_etio_dx == "Lewy body disease"))
cat("Multiple system atrophy", sum(D1_cp$pr_etio_dx == "Multiple system atrophy"))
# PSP/CBD/FTLD w/ motor neuron disease
D1_cp <- D1_cp %>% 
  mutate(pr_etio_dx = if_else(psp == 1 & pspif == 1, "PSP/CBD/FTLD w/ motor neuron disease", pr_etio_dx, missing = pr_etio_dx))
D1_cp <- D1_cp %>% 
  mutate(pr_etio_dx = if_else(cort == 1 & cortif == 1, "PSP/CBD/FTLD w/ motor neuron disease", pr_etio_dx, missing = pr_etio_dx))
D1_cp <- D1_cp %>% 
  mutate(pr_etio_dx = if_else(ftldmo == 1 & ftldmo == 1, "PSP/CBD/FTLD w/ motor neuron disease", pr_etio_dx, missing = pr_etio_dx))
cat("Alzheimer's disease:", sum(D1_cp$pr_etio_dx == "Alzheimer's disease"))
cat("Lewy body disease:", sum(D1_cp$pr_etio_dx == "Lewy body disease"))
cat("Multiple system atrophy", sum(D1_cp$pr_etio_dx == "Multiple system atrophy"))
cat("PSP/CBD/FTLD w/ motor neuron disease", sum(D1_cp$pr_etio_dx == "PSP/CBD/FTLD w/ motor neuron disease"))
# FTLD, other
D1_cp <- D1_cp %>% 
  mutate(pr_etio_dx = if_else(ftldnos == 1 & ftldnos == 1, "FTLD, other", pr_etio_dx, missing = pr_etio_dx))
cat("Alzheimer's disease:", sum(D1_cp$pr_etio_dx == "Alzheimer's disease"))
cat("Lewy body disease:", sum(D1_cp$pr_etio_dx == "Lewy body disease"))
cat("Multiple system atrophy", sum(D1_cp$pr_etio_dx == "Multiple system atrophy"))
cat("PSP/CBD/FTLD w/ motor neuron disease", sum(D1_cp$pr_etio_dx == "PSP/CBD/FTLD w/ motor neuron disease"))
cat("FTLD, other", sum(D1_cp$pr_etio_dx == "FTLD, other"))
# Vascular brain injury
D1_cp <- D1_cp %>% 
  mutate(pr_etio_dx = if_else(cvd == 1 & cvdif == 1, "Vascular brain injury", pr_etio_dx, missing = pr_etio_dx))
cat("Alzheimer's disease:", sum(D1_cp$pr_etio_dx == "Alzheimer's disease"))
cat("Lewy body disease:", sum(D1_cp$pr_etio_dx == "Lewy body disease"))
cat("Multiple system atrophy", sum(D1_cp$pr_etio_dx == "Multiple system atrophy"))
cat("PSP/CBD/FTLD w/ motor neuron disease", sum(D1_cp$pr_etio_dx == "PSP/CBD/FTLD w/ motor neuron disease"))
cat("FTLD, other", sum(D1_cp$pr_etio_dx == "FTLD, other"))
cat("Vascular brain injury", sum(D1_cp$pr_etio_dx == "Vascular brain injury"))

D1_cp %>% 
  filter(demented == 1) %>% 
  select(ptid, redcap_event_name, alzdis, lbdis, msa, psp, cort, ftldmo, ftldnos, cvd, pr_etio_dx) %>% 
  replace(is.na(.), 0) %>% 
  mutate(pr_etio_dx_sum = alzdis + cort + msa + psp + cort + ftldmo + ftldnos + cvd) %>%
  select(ptid, pr_etio_dx, pr_etio_dx_sum) %>% 
  print(., n = nrow(.)) %>%
  group_by(pr_etio_dx) %>% 
  summarize(n = n())

pr_etio_dx_df <- 
  data.frame(pr_etio_dx = (c("Alzheimer's disease", "Lewy body disease", 
                                 "Multiple system atrophy", "PSP/CBD/FTLD w/ motor neuron disease",
                                           "FTLD, other", "Vascular brain injury", "Other")))

# Amnestic multidomain dementia syndrom
D1_cp %>% 
  filter(demented == 1) %>% 
  filter(amndem == 1) %>% 
  select(ptid, redcap_event_name, amndem, pr_etio_dx) %>%
  group_by(pr_etio_dx) %>% 
  summarize(amndem_n = n()) %>% 
  right_join(pr_etio_dx_df)

# Posterior cortical atrophy syndrome
D1_cp %>% 
  filter(demented == 1) %>% 
  filter(pca == 1) %>% 
  select(ptid, redcap_event_name, pca, pr_etio_dx) %>% 
  group_by(pr_etio_dx) %>% 
  summarize(pca_n = n()) %>% 
  right_join(pr_etio_dx_df)

# Primary progressive aphasia syndrome
D1_cp %>% 
  filter(demented == 1) %>% 
  filter(ppasyn == 1) %>% 
  select(ptid, redcap_event_name, ppasyn, pr_etio_dx) %>% 
  group_by(pr_etio_dx) %>% 
  summarize(ppa_n = n()) %>% 
  right_join(pr_etio_dx_df)

# Behavioral variant FTD syndrome
D1_cp %>% 
  filter(demented == 1) %>% 
  filter(ftdsyn == 1) %>% 
  select(ptid, redcap_event_name, ftdsyn, pr_etio_dx) %>% 
  group_by(pr_etio_dx) %>% 
  summarize(ftd_n = n()) %>% 
  right_join(pr_etio_dx_df)

# Lewy body demention syndrome
D1_cp %>% 
  filter(demented == 1) %>% 
  filter(lbdsyn == 1) %>% 
  select(ptid, redcap_event_name, lbdsyn, pr_etio_dx) %>% 
  group_by(pr_etio_dx) %>% 
  summarize(lbd_n = n()) %>% 
  right_join(pr_etio_dx_df)

# Non-amnestic multidomain dementia syndrome
D1_cp %>% 
  filter(demented == 1) %>% 
  filter(namndem == 1) %>% 
  select(ptid, redcap_event_name, namndem, pr_etio_dx) %>% 
  group_by(pr_etio_dx) %>% 
  summarize(namndem_n = n()) %>% 
  right_join(pr_etio_dx_df)












###########################
###########################
####### EXTRA SPACE #######
###########################
###########################

