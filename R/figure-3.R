#### Smooth fits ####
model.df <- occupations %>%
  filter(profession == "Teachers") %>%
  transmute(
    xwaveid,
    year = year - 2000,
    ghmh,
    job_control,
    rjob_demand = 8 - job_demand,
    sex,
    cage = c(scale(age, scale=F)),
    cjbempt = c(scale(jbempt, scale=F)))
  
fit_year_psc_int <- gam(ghmh ~ 1 + #cage + sex + cjbempt + cage:year + cjbempt:year +
                          job_control + rjob_demand +  
                          s(year) + s(year, by = rjob_demand) + 
                          s(year, by = job_control),
                        data = model.df)

job_smooths <- bind_rows(
  `Job demand` = smooth_data(fit_year_psc_int, 2),
  `Job control` = smooth_data(fit_year_psc_int, 3),
  .id = "term"
) 

ggplot(job_smooths, aes(x = x + 2000, y = fit)) +
  # geom_hline(aes(yintercept = 0), color = "red", linetype = 2) +
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se),
              alpha = 0.2) +
  # geom_pointrange(data = fig3_betas, 
  #                 aes(ymin = fit - se, ymax = fit + se)) +
  geom_line() +
  # scale_x_continuous(breaks = c(1, 5, 10, 15, 20)) +
  facet_wrap(~term) +
  labs(x = "Survey year", y = "")




#### Linear fits ####
lm_fits <- df %>%
  group_by(year) %>%
  nest() %>%
  mutate(
    ghmh.fit = map(data, ~lm(
      formula = ghmh ~ 1 + job_control + rjob_demand, 
      data = .)
    ))

lm_betas <- lm_fits %>%
  transmute(
    betas = map(ghmh.fit, tidy)) %>%
  unnest(betas)

job_betas <- lm_betas %>%
  filter(term %notin% c("(Intercept)", "age", "jbempt")) %>%
  mutate(term = str_replace(term, "_", " "),
         term = str_remove(term, "^r"),
         term = str_to_sentence(term)) %>%
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
       Survey year", y = "Absolute change in MHI-5 units") 