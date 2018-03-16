#!/usr/bin/env Rscript

library(dplyr)

# use non-label report CSVs
C2 <- readr::read_csv("C2-UMMAPNACC30_DATA_2018-03-08_0811.csv", 
                      trim_ws = TRUE, na = "")
D1 <- readr::read_csv("D1-UMMAPNACC30_DATA_2018-03-08_0812.csv", 
                      trim_ws = TRUE, na = "")

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

# Participants have how many diagnoses?
D1_cp %>% 
  filter(demented == 1) %>% 
  select(ptid, redcap_event_name, alzdis, lbdis, msa, psp, cort, ftldmo, 
         ftldnos, cvd) %>% 
  replace(is.na(.), 0) %>% 
  mutate(pr_etio_dx_sum = 
           alzdis + cort + msa + psp + cort + ftldmo + ftldnos + cvd) %>%
  select(ptid, pr_etio_dx_sum) %>% 
  print(., n = nrow(.)) %>%
  group_by(pr_etio_dx_sum) %>% 
  summarize(n = n())

# Alzheimer's disease
D1_cp <- D1_cp %>% 
  mutate(pr_etio_dx = if_else(alzdis == 1 & alzdisif == 1, "Alzheimer's disease", 
                              pr_etio_dx, missing = pr_etio_dx))
cat("Alzheimer's disease:", 
    sum(D1_cp$pr_etio_dx == "Alzheimer's disease"))
# Lewy body disease
D1_cp <- D1_cp %>% 
  mutate(pr_etio_dx = if_else(lbdis == 1 & lbdif == 1, "Lewy body disease", 
                              pr_etio_dx, missing = pr_etio_dx))
cat("Alzheimer's disease:", 
    sum(D1_cp$pr_etio_dx == "Alzheimer's disease"))
cat("Lewy body disease:", 
    sum(D1_cp$pr_etio_dx == "Lewy body disease"))
# Multiple system atrophy
D1_cp <- D1_cp %>%
  mutate(pr_etio_dx = 
           if_else(msa == 1 & msaif == 1, "Multiple system atrophy", 
                   pr_etio_dx, missing = pr_etio_dx))
cat("Alzheimer's disease:", 
    sum(D1_cp$pr_etio_dx == "Alzheimer's disease"))
cat("Lewy body disease:", 
    sum(D1_cp$pr_etio_dx == "Lewy body disease"))
cat("Multiple system atrophy", 
    sum(D1_cp$pr_etio_dx == "Multiple system atrophy"))
# PSP/CBD/FTLD w/ motor neuron disease
D1_cp <- D1_cp %>% 
  mutate(pr_etio_dx = if_else(psp == 1 & pspif == 1, 
                              "PSP/CBD/FTLD w/ motor neuron disease", 
                              pr_etio_dx, missing = pr_etio_dx))
D1_cp <- D1_cp %>% 
  mutate(pr_etio_dx = if_else(cort == 1 & cortif == 1, 
                              "PSP/CBD/FTLD w/ motor neuron disease", 
                              pr_etio_dx, missing = pr_etio_dx))
D1_cp <- D1_cp %>% 
  mutate(pr_etio_dx = if_else(ftldmo == 1 & ftldmo == 1, 
                              "PSP/CBD/FTLD w/ motor neuron disease", 
                              pr_etio_dx, missing = pr_etio_dx))
cat("Alzheimer's disease:", 
    sum(D1_cp$pr_etio_dx == "Alzheimer's disease"))
cat("Lewy body disease:", 
    sum(D1_cp$pr_etio_dx == "Lewy body disease"))
cat("Multiple system atrophy:", 
    sum(D1_cp$pr_etio_dx == "Multiple system atrophy"))
cat("PSP/CBD/FTLD w/ motor neuron disease:", 
    sum(D1_cp$pr_etio_dx == "PSP/CBD/FTLD w/ motor neuron disease"))
# FTLD, other
D1_cp <- D1_cp %>% 
  mutate(pr_etio_dx = 
           if_else(ftldnos == 1 & ftldnos == 1, "FTLD, other", 
                   pr_etio_dx, missing = pr_etio_dx))
cat("Alzheimer's disease:", 
    sum(D1_cp$pr_etio_dx == "Alzheimer's disease"))
cat("Lewy body disease:", 
    sum(D1_cp$pr_etio_dx == "Lewy body disease"))
cat("Multiple system atrophy", 
    sum(D1_cp$pr_etio_dx == "Multiple system atrophy"))
cat("PSP/CBD/FTLD w/ motor neuron disease", 
    sum(D1_cp$pr_etio_dx == "PSP/CBD/FTLD w/ motor neuron disease"))
cat("FTLD, other", 
    sum(D1_cp$pr_etio_dx == "FTLD, other"))
# Vascular brain injury
D1_cp <- D1_cp %>% 
  mutate(pr_etio_dx = 
           if_else(cvd == 1 & cvdif == 1, "Vascular brain injury", 
                   pr_etio_dx, missing = pr_etio_dx))
cat("Alzheimer's disease:", sum(D1_cp$pr_etio_dx == "Alzheimer's disease"))
cat("Lewy body disease:", 
    sum(D1_cp$pr_etio_dx == "Lewy body disease"))
cat("Multiple system atrophy:", 
    sum(D1_cp$pr_etio_dx == "Multiple system atrophy"))
cat("PSP/CBD/FTLD w/ motor neuron disease:", 
    sum(D1_cp$pr_etio_dx == "PSP/CBD/FTLD w/ motor neuron disease"))
cat("FTLD, other:", 
    sum(D1_cp$pr_etio_dx == "FTLD, other"))
cat("Vascular brain injury:", 
    sum(D1_cp$pr_etio_dx == "Vascular brain injury"))

D1_cp %>% 
  filter(demented == 1) %>% 
  select(ptid, redcap_event_name, alzdis, lbdis, msa, psp, cort, ftldmo, ftldnos, 
         cvd, pr_etio_dx) %>% 
  replace(is.na(.), 0) %>% 
  mutate(pr_etio_dx_sum = alzdis + cort + msa + psp + cort + ftldmo + ftldnos + 
           cvd) %>%
  select(ptid, pr_etio_dx, pr_etio_dx_sum) %>% 
  print(., n = nrow(.)) %>%
  group_by(pr_etio_dx) %>% 
  summarize(n = n())

pr_etio_dx_df <- 
  data.frame(pr_etio_dx = c("Alzheimer's disease", "Lewy body disease", 
                            "Multiple system atrophy", 
                            "PSP/CBD/FTLD w/ motor neuron disease",
                            "FTLD, other", "Vascular brain injury", "Other"))

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


####
## Table 10
## C2 neuropsychological tests at most recent visit

# Join D1 and C2 by UDS ID
D1_C2 <- right_join(D1_cp, C2, by = names(D1_cp)[names(D1_cp) %in% names(C2)])

D1_C2$mocatots
class(D1_C2$mocatots)
mean(D1_C2$mocatots)

# Clean out non-test value codes (88, 98, etc.)
# D1_C2$mocatots[D1_C2$mocatots < 0] 
# D1_C2$mocatots[D1_C2$mocatots > 30]
D1_C2$mocatots[D1_C2$mocatots < 0 | D1_C2$mocatots > 30]
D1_C2$mocatots[D1_C2$mocatots < 0 | D1_C2$mocatots > 30] <- NA
D1_C2$minttots[D1_C2$minttots < 0 | D1_C2$minttots > 32] <- NA
D1_C2$craftvrs[D1_C2$craftvrs < 0 | D1_C2$craftvrs > 44] <- NA
D1_C2$craftvrs[D1_C2$crafturs < 0 | D1_C2$crafturs > 25] <- NA
D1_C2$craftdvr[D1_C2$craftdvr < 0 | D1_C2$craftdvr > 44] <- NA
D1_C2$craftdre[D1_C2$craftdre < 0 | D1_C2$craftdre > 25] <- NA
D1_C2$udsbentc[D1_C2$udsbentc < 0 | D1_C2$udsbentc > 17] <- NA
D1_C2$udsbentd[D1_C2$udsbentd < 0 | D1_C2$udsbentd > 17] <- NA
D1_C2$digforct[D1_C2$digforct < 0 | D1_C2$digforct > 14] <- NA
D1_C2$digbacct[D1_C2$digbacct < 0 | D1_C2$digbacct > 14] <- NA
D1_C2$animals_c2[D1_C2$animals_c2 < 0 | D1_C2$animals_c2 > 77] <- NA
D1_C2$veg_c2[D1_C2$veg_c2 < 0 | D1_C2$veg_c2 > 77] <- NA
D1_C2$traila_c2[D1_C2$traila_c2 < 0 | D1_C2$traila_c2 > 150] <- NA
D1_C2$trailb_c2[D1_C2$trailb_c2 < 0 | D1_C2$trailb_c2 > 300] <- NA
D1_C2$udsverfc[D1_C2$udsverfc < 0 | D1_C2$udsverfc > 40] <- NA
D1_C2$udsverlc[D1_C2$udsverlc < 0 | D1_C2$udsverlc > 40] <- NA

# Get table of mean and SD values
tbl_10_mn_sd <- D1_C2 %>% 
  select(dx, mocatots, minttots, craftvrs, crafturs, craftdvr, craftdre, udsbentc, 
         udsbentd, digforct, digbacct, animals_c2, veg_c2, traila_c2, trailb_c2, 
         udsverfc, udsverlc) %>% 
  group_by(dx) %>% 
  summarize_all(funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE)))
tbl_10_mn_sd[, 2:17] <- round(tbl_10_mn_sd[, 2:17])
tbl_10_mn_sd[, 18:33] <- round(tbl_10_mn_sd[, 18:33], 1)

# Get table of n values
# D1_C2 %>% 
#   select(dx, mocatots, minttots, craftvrs, crafturs, craftdvr, craftdre, udsbentc, 
#          udsbentd, digforct, digbacct, animals_c2, veg_c2, traila_c2, trailb_c2, 
#          udsverfc, udsverlc) %>% 
#   group_by(dx) %>% 
#   summarize_at(vars(matches("mocatots")), funs(n()))
D1_C2 %>% 
  select(ptid, dx, mocatots) %>% 
  group_by(dx) %>% 
  summarize(n = n())
D1_C2 %>% 
  select(ptid, dx, minttots) %>% 
  group_by(dx) %>% 
  summarize(n = n())
table(D1_C2$dx, D1_C2[["mocatots"]]) %>% 
  rowSums(.)
tests <- list("mocatots", "minttots", "craftvrs", "crafturs", "craftdvr",
              "craftdre", "udsbentc", "udsbentd", "digforct", "digbacct",
              "animals_c2", "veg_c2", "traila_c2", "trailb_c2", "udsverfc",
              "udsverlc")
# lapply(X = tests, FUN = function(x) { 
#   table(D1_C2$dx, D1_C2[[x]]) 
# })
sapply(X = tests, FUN = function(x) { 
  table(D1_C2$dx, D1_C2[[x]]) %>% rowSums(.) 
  })


table(D1_C2$normcog, D1_C2$mocaabst)[1, ]
sum(table(D1_C2$normcog, D1_C2$mocatots)[1, ])
sum(table(D1_C2$normcog, D1_C2$mocatots)[2, ])
sum(table(D1_C2$demented, D1_C2$mocatots)[1, ])
sum(table(D1_C2$demented, D1_C2$mocatots)[2, ])




###########################
###########################
####### EXTRA SPACE #######
###########################
###########################

