#### Happy {{insert profession here}} ####
# RW Morris
# Import and preprocessing 
# July 21st 2022
# 
#### Setup ####
library(tidyverse)
library(haven)
home <- "~/Work/HILDA (restricted release)/"

#### Import data ####
path_to_hilda <- list.files(
  path = paste0(home, "data"),
  pattern = '^Combined.*.sav$',
  full.names = TRUE
)

hilda <- list()
for (pathtofile in path_to_hilda) {
  df <- read_sav(pathtofile)
  hilda <- append(hilda, list(df))
  cat('.')
}

# Helper functions
source('~/Dropbox (Sydney Uni)/HILDA/src/gather_hilda.R')
source("~/Documents/R/helpers.R")



#### Preprocessing ####

#### All professions ####
# 
# Dictionary keys for occupation codes
# "jbmocc"  # (4-digit ASCO 1997)
# "jbmo06"  # (4-digit ANZSCO 2006)
# "jbm688" # (ISC 4-digit ANZSCO 2006)
# 
# extract_occupations <- function(.df, pattern) {
#   
#   select(.df, ends_with(pattern)) %>%
#     rename(hcode = 1) %>%
#     as_factor() %>%
#     transmute(code = str_extract(hcode, "-?\\d+"),
#               code = extract_numeric(code),
#               idx = str_locate(hcode, "]")[, 1],
#               description = str_sub(hcode, start = idx + 2)) %>%
#     select(-idx) %>%
#     distinct()
# }
# 
# jbm688_key <- map(hilda, extract_occupations, "jbm688") %>%
#   bind_rows() %>%
#   distinct() %>%
#   arrange(code)
# 
# jbmo06_key <- map(hilda, extract_occupations, "jbmo06") %>%
#   bind_rows() %>%
#   distinct() %>%
#   arrange(code)
# 
# jbmocc_key <- map(hilda[1:6], extract_occupations, "jbmocc") %>%
#   bind_rows() %>%
#   distinct() %>%
#   arrange(code)
# 
# write_csv(jbm688_key, paste0(home, "data/jbm688_key.csv"))
# write_csv(jbmo06_key, paste0(home, "data/jbmo06_key.csv"))



# Gather occupation codes
occupations <- gather_hilda(hilda, c("jbm688")) %>%
  group_by(wave) %>%
  mutate(year = which(letters == wave[1]) + 2000) %>%
  ungroup() %>%
  select(-code) %>%
  arrange(xwaveid, year) %>%
  filter(val > 0) %>%
  rename(occupation = val)



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
#
# https://www.rba.gov.au/calculator/annualDecimal.html
CPI = data.frame(
  wave = letters[1:22],
  deflator = c(1.705862647,
               1.600335008,
               1.555778894,
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
               1)) # base year 2001


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
    hifdip_adj = hifdip_rl2019/sqrt(hhpers), # for backward compatibility
    hh_disp_inc_eq = hh_disp_inc/OECD_mod # convert to base and adjust by size
  ) 

# to do:
# 1. Add topcoded thresholds from HILDA for income?


#### Wellbeing ####
# To do: add "losateo"?
#
wellbeing <- gather_hilda(hilda, c("ghmh", "losat")) %>%
  spread(code, val) %>%
  mutate_if(is.double, ~ ifelse(. < 0, NA_real_, .))


#### Psychosocial work characteristics ####
#
# To do: construct a job control and job stress variable: see Leach job strain 
# questions (for no control/high demand jobs)


job_security <- c(
  "jomsf", # I have a secure future in my job [all waves]
  "jomwf", # I worry about the future of my job [all waves]
  "jompf", # I get paid fairly for the things I do in my job
  "jomcsb" # Company I work for will still be in business in 5 years
)

# Job control
job_control <- c(
  "jomfd", # I have a lot of freedom to decide how I DO my job [all waves]
  "jomfw", # I have a lot of freedom to decide WHEN I do my work [all waves]
  "jomflex", # My working times can be flexible 
  "jomls", # I have a lot of say about what happens in my job
  "jomdw", # I have a lot of choice in deciding what I do at work
  "jomflex", # My working times can be flexible
  "jombrk", # I can decide when to take a break
  "jomrpt", # My job requires me to do the same things over and over again
  "jomvar" # My job provides me with a variety of interesting things to do
)

# Job demands (Waves 5:20) & Job complexity (Waves 1:20)
job_demands <- c(
  "jomini", # My job requires me to take initiative
  "jomfast", # I have to work fast in my job
  "jomwi",  # I have to work very intensely in my job
  "jomtime" # I don’t have enough time to do everthing in my job
)

job_complexity <- c(
  "jomms",	# My job is more stressful than I had ever imagined	
  "jompi",  # I fear that the amount of stress in my job will make me phys. ill
  "jomcd",  # My job is complex and difficult 
  "jomns",  # My job often required me to learn new skills
  "jomus"   # I use my skills in current job*
)

job_other <- c(
  "lsemp", # Combined hrs/mins per week - Paid employment [waves 2:16, all pop]
  "jbhruc", # Hours per week usually worked in all jobs
  "jbmhruc", # Hours per week usually worked in main job
  "jbhrqf", # Data Quality Flag: hours of work main job vs all jobs
  "jbmploj", # Percent chance of losing job in next 12 months [0:100, all waves]
  "jbmpgj",	# Percent chance will find a job at least as good as current job
  "jbempt", # Tenure with current employer
  "jbcmocc" # Occupation changed since last interview
  )

combine_scores <- function(.df, .varnames) {
  
  # All items are 7-point Likert scale (Agreement)
  reversed_items <- c('jomwf', 'jomrpt', 'jomus')
  
  gather_hilda(.df, .varnames) %>%
    spread(code, val) %>%
    mutate_if(is.double, ~ ifelse(. < 0, NA_real_, .)) %>%
    # reverse score
    mutate(across(any_of(reversed_items), ~ 8 - .)) %>%
    # component score (impute by mean)
    gather(code, val, -xwaveid, -wave) %>%
    group_by(xwaveid, wave) %>%
    transmute(combined = mean(val, na.rm=T)) %>% # NaN if missing for all values
    ungroup() %>%
    distinct() %>%
    # remove NaN
    na.omit() 
  
}

security <- combine_scores(hilda, job_security) %>%
  rename(job_security = combined)

control <- combine_scores(hilda, job_control) %>%
  rename(job_control = combined)

# Waves 5:20
demand5 <- combine_scores(hilda, job_demands) %>% 
  rename(job_demand5 = combined)

complexity <- combine_scores(hilda, job_complexity) %>%
  rename(job_complexity = combined)

# Waves 1:20
demand <- combine_scores(hilda, c(job_complexity, job_demands)) %>%
  rename(job_demand = combined)


psychosocial_components <- full_join(demand, control) %>%
  full_join(security) %>%
  full_join(complexity) %>%
  full_join(demand5)

# Check the join
# psychosocial_components %>%
#   group_by(wave) %>%
#   summarize(
#     demand_missing = sum(is.na(job_demand)),
#     control_missing = sum(is.na(job_control)),
#     security_missing = sum(is.na(job_security)),
#     complexity_missing = sum(is.na(job_complexity)),
#     demand5_missing = sum(is.na(job_demand5))
#   )


#### Other employment variables ####

employment_items <- gather_hilda(hilda, job_other) %>%
  spread(code, val) %>% 
  mutate_if(is.double, ~ ifelse(. < 0, NA_real_, .)) %>%
  mutate(across(c(jbmploj, jbmpgj), ~replace(., . %in% c(997, 998, 999), NA)))





#### Final join ####
preprocessed <- occupations %>%
  left_join(regions) %>%
  left_join(demographics) %>%
  left_join(income) %>%
  left_join(wellbeing) %>%
  left_join(psychosocial_components) %>%
  left_join(employment_items)



write_rds(
  preprocessed, 
  "~/Dropbox (Sydney Uni)/modern-teachers/data/occupations.RDS"
)
