#### example import ####
# RW Morris
# Import and preprocessing 
# Nov 26th 2022
# 
#### Setup ####
library(tidyverse)
library(haven)
home <- "~/path/to/hilda/data/"

# Helper functions
source('gather_hilda.R')

extract_numeric <- function (x) {
  as.numeric(gsub("[^0-9.-]+", "", as.character(x)))
}

`%notin%` <- Negate(`%in%`)


#### Import ####
paths_to_hilda <- list.files(
  path = home,
  pattern = '^Combined.*.dta$',
  full.names = TRUE
)

hilda <- list()
for (pathtofile in paths_to_hilda) {
  df <- read_dta(pathtofile)
  hilda <- append(hilda, list(df))
  cat('.')
}



#### Gather variables ####


#### Regions ####
state_key = c(`1` = "NSW", `2` = "VIC", `3` = "QLD", `4` = "SA", `5` = "WA", 
              `6` = "TAS", `7` = "NT", `8` = "ACT")


# For remoteness index classifications see:
# https://www.qgso.qld.gov.au/about-statistics/statistical-standards-
# classifications/accessibility-remoteness-index-australia
region_key = c(`0` = "Major city",
               `1` = "Inner regional",
               `2` = "Outer regional",
               `3` = "Remote",
               `4` = "Very remote")

regions <- gather_hilda(hilda, c(
  "hhstate", # household State of residence (NSW, VIC, QLD, SA, WA, TAS, NT, ACT)
  "hhsra" # household remote area ASGS 2011 
)) %>%
  spread(code, val) %>%
  mutate_if(is.double, ~ ifelse(. < 0, NA_real_, .)) %>%
  mutate(hhsra = replace_na(hhsra, 3),
         hhsra = recode(hhsra, !!!region_key),
         hhstate = recode(hhstate, !!!state_key)
  ) 

#### Demographics ####
edu_key = c(`1` = "Postgraduate", # "Masters or doctorate",
            `2` = "Graduate diploma",
            `3` = "Bachelors degree",
            `4` = "Year 12", # "Diploma",
            `5` = "Year 12", # "Certificate III or IV",
            `8` = "Year 12",
            `9` = "Year 11 or below"
            )

demographics <- gather_hilda(hilda, c(
  "hhsad10", # SEIFA index 2011
  "hgage",   # age
  "hgsex",  
  "mrcurr",  # current marital status (married/de facto ≤ 2)
  "hh0_4",   # children under 4 in household
  "edhigh1"  # highest education achieved
  )) %>%
  spread(code, val) %>% 
  transmute(
    xwaveid,
    wave,
    Female = hgsex == 2,
    age = as.integer(hgage),
    coupled = if_else(mrcurr <= 2, TRUE, FALSE, missing = FALSE),
    new_parent = hh0_4 > 0,
    SEIFA = as.integer(extract_numeric(hhsad10)),
    edu = recode_factor(edhigh1, !!!edu_key, .default = NA_character_, .ordered = T)
  )


#### Income ####
CPI = data.frame(
  wave = letters[1:20],
  deflator = c(1.555778894,
               1.510735198,
               1.470550982,
               1.436881188,
               1.399216632,
               1.351178353,
               1.320443560,
               1.265395095,
               1.243373494,
               1.208116545,
               1.169478721,
               1.149220490,
               1.121739130,
               1.094508602,
               1.078244718,
               1.064649243,
               1.044299528,
               1.024713151,
               1.008469055,
               1)) #%>% mutate(inflator = rev(deflator))


income <- gather_hilda(hilda, c(
  "hifdip",  # household disposable regular income (positive values)
  "hifdin",  # household disposable regular income (negative values)
  "hhpers",  # no. persons in household
  "hh0_4",
  "hh5_9",
  "hh10_14",
  "hhadult")  
) %>%
  spread(code, val) %>%
  left_join(CPI, by = "wave") %>%
  mutate_if(is.double, ~ ifelse(. < 0, NA_real_, .)) %>%
  mutate(
    hh_disp_inc = (hifdip - hifdin) * deflator, # convert to base year 2019
    hifdip_rl2019 = hifdip * deflator,
    hhchild = hh0_4 + hh5_9 + hh10_14,
    OECD_mod = 1 + ((hhadult-1)*(0.5)) + (hhchild*(0.3))
  ) %>%
  transmute(
    xwaveid, 
    wave,
    hh_disp_inc_eq = hh_disp_inc/OECD_mod # convert to base and adjust by size
  ) 

# to do:
# 1. Add topcoded thresholds from HILDA for income?


#### Wellbeing ####
wellbeing <- gather_hilda(hilda, c("ghmh", "losat")) %>%
  spread(code, val) %>%
  mutate_if(is.double, ~ ifelse(. < 0, NA_real_, .))




#### Final join ####
preprocessed <- wellbeing %>%
  left_join(regions) %>%
  left_join(demographics) %>%
  left_join(income) 


write_rds(preprocessed, "preprocessed.RDS")