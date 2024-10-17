# This is a run file for generating the outputs for:
# 
# The growing effect of job demands on teacher mental health: results from a 
# longitudinal national household panel survey

# the current directory of this file
home <- dirname(rstudioapi::getSourceEditorContext()$path)

# JOEM submission ####
# 
#### Render main docx
rmarkdown::render(
  paste0(home, "/src/Word JOEM.Rmd"),
  output_file = "MORRIS.docx",
  output_dir = paste0(home, "/JOEM/")
)

#### Render figures.pdf
rmarkdown::render(
  paste0(home, "/src/Figures JOEM.Rmd"),
  output_file = "Figures.pdf",
  output_dir = paste0(home, "/JOEM/")
)

#### Render appendix.pdf
rmarkdown::render(
  paste0(home, "/src/Supplemental JOEM.Rmd"),
  output_file = "Supplemental MORRIS.pdf",
  output_dir = paste0(home, "/JOEM/")
)

# LCC working paper series ####
# 
#### Render main pdf for LCC Working paper series
rmarkdown::render(
  paste0(home, "/src/Word JOEM.Rmd"),
  output_format = rmarkdown::pdf_document(latex_engine = "xelatex"),
  output_file = "Modern teachers.pdf",
  output_dir = paste0(home, "/LCC working paper/")
)


# Github-pages ####
teachers_key = c(
  `2310` = "College",
  `2320` = "Secondary",
  `2331` = "Primary",
  `2332` = "Pre-primary",
  `2340` = "Special ed",
  `2359` = "Other teaching n.e.c.",
  .default = NA_character_
)

engineers_key = c(
  `2142` = "Engineer",     #"Civil",
  `2143` = "Engineer",     #"Electrical",
  `2144` = "Engineer",     #"Electronics and telecommunications",
  `2147` = "Engineer",     #"Mining, metallurgists and related",
  `2149` = "Engineer",     #"Architects, engineers and related",
  `3110` = "Technicians",  #"Physical and engineering science technicians",
  `3114` = "Technicians",  #"Electronics and telecommunications technicians",
  `3119` = "Technicians",  #"Technicians n.e.c.",
  .default = NA_character_
)

occupation_key = c(
  `2320` = "Teachers",
  `2331` = "Teachers",
  `2230` = "Nurses",
  `2411` = "Accountants",
  # `2000` = "Professionals",
  .default = NA_character_
)


rmarkdown::render(
  paste0(home, "/src/Report.Rmd"),
  params = list(key = occupation_key),
  output_file = "index.html",
  output_dir = paste0(home, "/docs"))


# Count teachers ####
library(tidyverse)

occupation_key = c(
  `2310` = "College",
  `2320` = "Secondary",
  `2331` = "Primary",
  `2332` = "Pre-primary",
  `2340` = "Special ed",
  `2359` = "Other teaching n.e.c.",
  .default = NA_character_
)


teachers <- read_rds("data/occupations.rds") %>%
  mutate(
    remoteness = fct_collapse(hhsra, 
                              `Outer/Remote` = "Outer regional", 
                              `Outer/Remote` = "Remote", 
                              `Outer/Remote` =  "Very remote"),
    remoteness = fct_relevel(remoteness, "Major city"),
    profession = recode_factor(occupation, !!!occupation_key)
  ) %>% 
  filter(!is.na(profession)) %>%
  rename(`Mental health` = ghmh, `Life satisfaction` = losat)

count(teachers, year, profession) %>%
  spread(profession, n)

# A tibble: 20 × 7
#     year College Secondary Primary `Pre-primary` `Special ed` `Other teaching n.e.c.`
#    <dbl>   <int>     <int>   <int>         <int>        <int>                   <int>
#  1  2001      50       187     158            13           17                      27
#  2  2002      57       181     165            20            9                      33
#  3  2003      46       168     159            23           21                      28
#  4  2004      47       174     161            21           18                      31
#  5  2005      60       191     158            23           14                      30
#  6  2006      60       178     158            18           21                      39
#  7  2007      54       171     179            23           15                      36
#  8  2008      48       169     163            23           21                      29
#  9  2009      52       187     167            24           16                      24
# 10  2010      49       183     167            26           18                      40
# 11  2011      61       227     207            29           25                      53
# 12  2012      60       210     193            32           24                      55
# 13  2013      60       235     188            37           24                      55
# 14  2014      68       208     185            29           28                      53
# 15  2015      73       210     193            24           35                      51
# 16  2016      78       207     182            26           39                      61
# 17  2017      75       234     194            21           33                      61
# 18  2018      72       227     199            27           37                      43
# 19  2019      76       243     176            29           28                      50
# 20  2020      69       236     170            26           22                      44
