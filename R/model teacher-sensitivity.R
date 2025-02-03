# Preprocessing ####
#
# Requires occupations

ctrl_t <- 3.25 # with(read_rds("../data/occupations.rds"), summary(job_control))
dmnd_t <- 4.556 # with(read_rds("../data/occupations.rds"), summary(job_demand))

# create model frame
model.df <- occupations %>%
  filter(profession == "Teachers") %>%
  transmute(
    xwaveid,
    year = year - 2000,
    ghmh,
    job_control,
    job_demand,
    rjob_demand = 8 - job_demand,
    zjob_control = c(scale(job_control)),
    zrjob_demand = c(scale(rjob_demand)),
    PSC = case_when(
      job_control < ctrl_t & job_demand > dmnd_t ~ "job_strain",
      job_control < ctrl_t ~ "low_control",
      job_demand > dmnd_t ~ "high_demand",
      TRUE ~ "none"),
    PSC = fct_relevel(PSC, "none", "low_control"),
    sex,
    cage = c(scale(age, scale=F)),
    cjbempt = c(scale(jbempt, scale=F)))

# Sensitivity model ####
# 
# Using reversed job demands so partial effects represent estimates at 0 (lack
# of stressor)

# fit smooths
fit_year_psc_int <- gam(ghmh ~ 1 + job_control + rjob_demand +  
                          s(year) + s(year, by = rjob_demand) + 
                          s(year, by = job_control),
                        data = model.df)

job_smooths <- bind_rows(
  `Job demand` = smooth_data(fit_year_psc_int, 2),
  `Job control` = smooth_data(fit_year_psc_int, 3),
  .id = "term"
) 

# fit points
lm_fits <- model.df %>%
  group_by(year) %>%
  nest() %>%
  mutate(
    lin.fit = map(data, ~lm(
      formula = ghmh ~ 1 + job_control + rjob_demand, 
      data = .))
    )

lm_betas <- lm_fits %>%
  transmute(
    betas = map(lin.fit, tidy)) %>%
  unnest(betas)

job_betas <- lm_betas %>%
  filter(term %notin% c("(Intercept)", "age", "jbempt")) %>%
  mutate(
    # fix the coefficient names
    term = str_replace(term, "_", " "),
    term = str_remove(term, "^r"),
    term = str_to_sentence(term),
    # center the OLS estimates
    # estimate = if_else(term == "Job demand", estimate - 1.4, estimate - 0.6),
    std.error = std.error * 1.96
  ) %>%
  select(term, x = year, se = std.error, fit = estimate) %>%
  ungroup()



ggplot(job_smooths, aes(x = x + 2000, y = fit)) +
  geom_pointrange(data = job_betas, aes(ymin = fit - se, ymax = fit + se),
                  color = "grey30") +
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se),
              alpha = 0.2) +
  geom_line() +
  facet_wrap(~term) +
  labs(x = "
       Survey year", y = "Absolute MHI-5 units (±95%CI)") 



# Mediation models ####
fit_year_bothpsc <- gam(ghmh ~ 1 + job_control + rjob_demand +  
                          s(year) + s(year, by = rjob_demand) + 
                          s(year, by = job_control),
                        data = model.df)

fit_year_cntl <- gam(ghmh ~ 1 + job_control +  
                          s(year) + s(year, by = job_control),
                        data = model.df)

plot(fit_year_bothpsc, shade=T, pages=1, scale=0, all.terms=T)

plot(fit_year_bothpsc, shade=T, pages=0, select=3, ylim = c(-1,3))
plot(fit_year_cntl, shade=T, pages=0, select=2, ylim = c(-1,3))



# Interaction models ####
interaction_fits <- model.df %>%
  group_by(year) %>%
  nest() %>%
  mutate(
    int.fit = map(data, ~lm(
      formula = ghmh ~ 1 + zjob_control*zrjob_demand, 
      data = .)),
    nl.fit = map(data, ~lm(
      formula = ghmh ~ 1 + PSC, 
      data = .))
  )

int_betas <- interaction_fits %>%
  transmute(
    betas = map(int.fit, tidy)) %>%
  unnest(betas) %>%
  arrange(year) %>%
  mutate(sig = case_when(p.value < .001 ~ "***", p.value < .01 ~ "**", 
                         p.value < .05 ~ "*", TRUE ~ ""))

int_betas %>%
  filter(term %notin% "(Intercept)") %>%
  mutate(term = str_remove_all(term, "zr"),
         term = str_remove_all(term, "z"),
         term = fct_relevel(term, "job_control", "job_demand")) %>%
  ggplot(aes(x = year, y = estimate)) +
  geom_pointrange(aes(ymin = estimate - std.error, 
                      ymax = estimate + std.error)) +
  facet_wrap(~term)

nl_betas <- interaction_fits %>%
  transmute(
    betas = map(nl.fit, tidy)) %>%
  unnest(betas) %>%
  arrange(year) %>%
  mutate(sig = case_when(p.value < .001 ~ "***", p.value < .01 ~ "**", 
                         p.value < .05 ~ "*", TRUE ~ ""))

nl_betas %>%
  filter(term %notin% "(Intercept)") %>%
  mutate(term = str_remove(term, "PSC"),
         term = str_replace(term, "_", " "),
         term = str_to_sentence(term),
         term = fct_relevel(term, "Low control")) %>%
  ggplot(aes(x = year, y = estimate)) +
  geom_pointrange(aes(ymin = estimate - std.error, 
                      ymax = estimate + std.error)) +
  facet_wrap(~term)