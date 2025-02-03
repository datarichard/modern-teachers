colnames(teachers)

# jbempt: tenure with current employer (years)
hist(teachers$jbempt)

summary(teachers$jbempt)
summary(teachers$age)
# Note this data includes n = 8 people reported as teachers who are under 19, &
# have not completed any post-secondary education

lm(ghmh ~ 1 + job_control*jbempt, data = teachers) %>% summary()
lm(ghmh ~ 1 + job_demand*jbempt, data = teachers) %>% summary()


# tenure in teaching
teachers %>%
  group_by(year) %>%
  summarise(
    min = min(jbempt, na.rm=T),
    Q1 = quantile(jbempt, 0.25, na.rm=T),
    med = quantile(jbempt, 0.5, na.rm=T),
    Q3 = quantile(jbempt, 0.75, na.rm=T),
    max = max(jbempt, na.rm=T)
  )

#  year    min    Q1   med    Q3   max
#  <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#  2001 0.0192  3       10  19      50
#  2002 0.0192  2.25     9  17      41
#  2003 0.0769  3       10  19.5    52
#  2004 0.0192  3       10  18      53
#  2005 0.0769  3        9  18      56
#  2006 0.0192  3       10  20      55
#  2007 0.0385  3        8  19      45
#  2008 0.0192  3        8  20      45
#  2009 0.0192  3        8  20      46
#  2010 0.0192  3        8  19      47
#  2011 0.0192  3        8  19      48
#  2012 0.0192  3        8  18      48
#  2013 0.0192  3        8  18      50
#  2014 0.0192  3        7  17      47
#  2015 0.0385  3        8  16      45
#  2016 0.0385  3        8  17      50
#  2017 0.0192  2        7  15      51
#  2018 0.0385  2.25     7  15      48
#  2019 0.0192  3        7  15      49
#  2020 0.0385  3        7  15.8    48
#  
# To discretize tenure, try jnr < 3, mid < 10, sen < 20, vet > 20


teachers %>%
  mutate(ghmh = c(scale(ghmh)),
         tenure = case_when(
           jbempt < 4 ~ "JNR",
           jbempt < 11 ~ "MID",
           jbempt < 21 ~ "SEN",
           TRUE ~ "VET"
           )
         ) %>% 
  group_by(year) %>%
  nest() %>%
  mutate(
    control_fit = map(data, ~lm(ghmh ~ 0 + job_control*tenure, data = .)),
    demand_fit = map(data, ~lm(ghmh ~ 0 + job_demand*tenure, data = .))
    # security_fit = map(data, ~lm(ghmh ~ 1 + job_security*tenure, data = .))
    ) -> fits

fits %>%
  transmute(
    control.betas = map(control_fit, tidy),
    demand.betas = map(demand_fit, tidy)
    # security.betas = map(security_fit, tidy)
  ) -> df

# Plot betas and r-squared
scrap <- c("_control", "_demand", "_security", "tenure")

bind_rows(
  `job control` = select(df, year, control.betas) %>%
    unnest(control.betas),
  `job demand` = select(df, year, demand.betas) %>%
    unnest(demand.betas),
  # `job security` = select(df, year, security.betas) %>%
  #   unnest(security.betas),
  .id = "model"
) %>%
  filter(term %notin% "(Intercept)") %>%
  mutate(term = str_remove_all(term, paste(scrap, collapse = "|"))) %>%
  ggplot(aes(x = year, y = estimate, color = term)) +
  geom_hline(aes(yintercept = 0), color = "white", size = 1.25) +
  geom_line() +
  geom_pointrange(aes(ymin = estimate - std.error, 
                      ymax = estimate + std.error)) +
  facet_grid(term~model, scales = "free_y") +
  labs(title = "Effect on mental health in each year\n", 
       x = "", y = "") +
  ggthemes::theme_economist() +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(size = 0)) 



