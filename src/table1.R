#### Table 1 ####
library(tidyverse)
library(gtsummary)

home <- "~/Work/Happy teachers/"
`%notin%` <- Negate(`%in%`)

occupation_key = c(
  `2310` = "College",
  `2320` = "Secondary",
  `2331` = "Primary",
  `2332` = "Pre-primary",
  `2340` = "Special ed",
  .default = NA_character_
)


teachers <- read_rds(paste0(home, "data/preprocessed.rds")) %>%
  mutate(
    remoteness = fct_collapse(hhsra, 
                              `Outer/Remote` = c("Outer regional", 
                                                 "Remote", 
                                                 "Very remote")),
    remoteness = fct_relevel(remoteness, "Major city"),
    profession = recode_factor(occupation, !!!occupation_key)
  ) %>% 
  filter(!is.na(profession))

colnames(teachers)
[1] "xwaveid"           "occupation"        "wave"              "year"              "hhsra"             "hhstate"          
[7] "Female"            "age"               "coupled"           "new_parent"        "SEIFA"             "edu"              
[13] "hifdip_adj"        "hh_disp_inc_eq"    "Mental health"     "Life satisfaction" "jbhrqf"            "jbhruc"           
[19] "jbmhruc"           "jbmpgj"            "jbmploj"           "jomcd"             "jomfd"             "jomflex"          
[25] "jomfw"             "jomms"             "jompi"             "jomsf"             "jomwf"             "lsemp"            
[31] "remoteness"        "profession"       


t1 <- teachers %>% 
  filter(year == 2001) %>%
  mutate(income = hh_disp_inc_eq/1000) %>%
  mutate(Total = 1) %>%
  select(xwaveid, profession, 
         Total,
         Female, 
         Age = age, 
         Coupled = coupled, 
         `New parent` = new_parent, # child under 4
         Edu = edu, 
         Region = remoteness,
         `Real household income ($000s)` = income # OECD adjusted
  ) %>%
  distinct() %>%
  select(-xwaveid) %>%
  tbl_summary(
    by = profession,
    statistic = Total ~ "N = {N}"
  ) %>%
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  add_p(test.args = all_tests("fisher.test") ~ list(simulate.p.value=T),
        include = -Total)

t2 <- teachers %>% 
  filter(year == 2020) %>%
  mutate(income = hh_disp_inc_eq/1000) %>%
  mutate(Total = 1) %>%
  select(xwaveid, profession, 
         Total,
         Female, 
         Age = age, 
         Coupled = coupled, 
         `New parent` = new_parent, # child under 4
         Edu = edu, 
         Region = remoteness,
         `Real household income ($000s)` = income # OECD adjusted
  ) %>%
  distinct() %>%
  select(-xwaveid) %>%
  tbl_summary(
    by = profession,
    statistic = Total ~ "N = {N}"
  ) %>%
  modify_header(all_stat_cols() ~ "**{level}**") %>%
  add_p(test.args = all_tests("fisher.test") ~ list(simulate.p.value=T),
        include = -Total)

tbl_stack(list(t1, t2),
          group_header = c("2001", "2020"))

