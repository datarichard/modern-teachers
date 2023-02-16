#### Setup ####
library(tidyverse)
# home <- "~/Work/Happy teachers/"
`%notin%` <- Negate(`%in%`)

occupation_key = c(
  # `2310` = "College",
  `2320` = "Secondary",
  `2331` = "Primary",
  # `2332` = "Pre-primary",
  # `2340` = "Special ed",
  # `2359` = "Other teaching n.e.c."
  .default = NA_character_
)


teachers <- read_rds("data/preprocessed.rds") %>%
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

#### Wellbeing ####
p1 <- teachers %>%
  select(xwaveid, year, profession, remoteness, `Mental health`, `Life satisfaction`) %>%
  mutate(`Life satisfaction` = `Life satisfaction` * 10) %>%
  gather("outcome", "val", `Mental health`, `Life satisfaction`) %>%
  ggplot(aes(x = year, color = outcome)) +
  geom_smooth(aes(y = val), method = "loess", span = 1.5) +
    facet_grid(remoteness~profession) +
    labs(x = "", y = "") +
    theme(legend.position = "bottom",
          legend.title = element_blank())


p2 <- teachers %>%
  select(xwaveid, year, remoteness, profession, `Mental health`) %>% 
  mutate(
    profession = fct_collapse(profession,
                              other = c("Special ed", "Other teaching n.e.c."))
    ) %>%
  ggplot(aes(x = year, color = remoteness)) +
  geom_smooth(aes(y = `Mental health`), method = "loess", se =F, span = 1.5) +
    facet_wrap(~profession) +
    labs(x = "", y = "") +
    theme(legend.position = "bottom",
          legend.title = element_blank())


p3 <- teachers %>%
  select(xwaveid, year, remoteness, `Mental health`, Female) %>%
  mutate(Female = if_else(Female, "Female", "Male")) %>%
  ggplot(aes(x = year, color = Female)) +
    geom_smooth(aes(y = `Mental health`), method = "loess", span = 1.5) +
    facet_wrap(~remoteness, nrow = 1) +
    labs(x = "", y = "") +
    theme(legend.position = "bottom")


library(ggthemes)

p1 + 
  labs(title = "Mental health declines in city and remote regions",
       subtitle = "\n (but life satisfaction generally improves) \n") +
  scale_color_fivethirtyeight() +
  theme_economist() + 
  theme(legend.position = "bottom",
        legend.title = element_blank())

p2 + 
  scale_color_wsj() +
  labs(title = "Mental health declines across college, secondary, primary and special ed teachers
in city and remote regions\n",
       ) +
  theme_economist() + 
  theme(legend.position = "bottom",
        legend.title = element_blank())

p3 + scale_color_fivethirtyeight() + 
  labs(title = "Mental health declines in female teachers\nmore than male teachers\n") +
  theme_wsj() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

p3 + theme_fivethirtyeight()


#### Psychosocial components ####

psychosocial = c("job_control", "job_demand", "job_security")

teachers %>%
  select(xwaveid, year, remoteness, profession, psychosocial) %>%
  gather("outcome", "val", !!!psychosocial) %>%
  filter(profession %in% c("Secondary", "Primary")) %>%
  ggplot(aes(x = year, color = remoteness)) +
    geom_smooth(aes(y = val), method = "loess", se=F, span=1.5) +
    facet_grid(outcome~profession, scales = "free_y") +
    labs(title = "Increasing job strain across teaching sectors", 
         subtitle = "Job strain increases with high demand & low control\n",
         x = "", y = "") +
    theme_economist() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          plot.title = element_text(vjust = 2),
          strip.text = element_text(vjust = 1.2))

#### Appendix ####
#
# Job demands
job_security <- c(
  "jomsf", # I have a secure future in my job [1:7, all waves]
  "jomwf", # I worry about the future of my job [1:7, all waves]
  "jbmploj", # Percent chance of losing job in the next 12 months [0:100, all waves]
  "jbmpgj"	# Percent chance will find and accept job at least as good as current job
)

# Job control
job_control <- c(
  "jomfd", # I have a lot of freedom to decide how I DO my job 
  "jomfw", # I have a lot of freedom to decide WHEN I do my work (not needed)
  "jomflex" # My working times can be flexible
)

# Job stress/demand
job_stress <- c(
  "jomcd", # My job is complex and difficult 
  "jomms",	# My job is more stressful than I had ever imagined	
  "jompi", #	I fear that the amount of stress in my job will make me physically ill
  "lsemp", # Combined hrs/mins per week - Paid employment [waves 2:16, all pop]
  "jbhruc", # Hours per week usually worked in all jobs
  "jbmhruc", # Hours per week usually worked in main job
  "jbhrqf" # Data Quality Flag: hours of work main job vs all jobs
)


teachers %>%
  mutate(exclusion = hhstate %in% "VIC" & remoteness %in% "Major city") %>%
  filter(!exclusion) %>%
  select(xwaveid, year, remoteness, profession, any_of(job_control)) %>%
  gather("outcome", "val", !!!job_control) %>%
  filter(profession %in% c("College", "Secondary", "Primary")) %>%
  ggplot(aes(x = year, color = remoteness)) +
  geom_smooth(aes(y = val), method = "loess", se=F, span=1.5) +
  facet_grid(outcome~profession, scales = "free_y") +
  labs(title = "Job control sans Melbourne\n", x = "", y = "") +
  theme_economist() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

#### Questions of interest ####
# long term trends in teachers
# compare remoteness
# job demands drivers?
