# Different models of job characteristics on ghmh over years
# 
# Model I: Person-level changes in ghmh
teachers %>%
  select(xwaveid, year, swb = ghmh, job_control, job_demand, job_security) %>%
  group_by(xwaveid) %>%
  mutate(swb = c(scale(swb))) %>%
  ungroup() %>%
  filter(year > 2004) %>%
  mutate(year = as.character(year),
         swb = replace_na(swb, 0)) -> df

# Model II: All changes in ghmh
teachers %>%
  select(xwaveid, year, swb = ghmh, job_control, job_demand, job_security) %>%
  mutate(swb = c(scale(swb))) %>%
  filter(year > 2004) %>%
  mutate(year = as.character(year)) -> df
  
# Fit the total effect of each job characteristic in a well-specified model
control_fit <- lm(swb ~ 1 + job_control*year, data = df)
demand_fit <- lm(swb ~ 1 + job_demand*year, data = df)
security_fit <- lm(swb ~ 1 + job_security*year, data = df)

bind_rows(
  
  `job demand` = tidy(demand_fit) %>%
    filter(str_detect(term, ":")) %>%
    mutate(year = extract_numeric(term)),
  
  `job control` = tidy(control_fit) %>%
    filter(str_detect(term, ":")) %>%
    mutate(year = extract_numeric(term)),
  
  `job security` = tidy(security_fit) %>%
    filter(str_detect(term, ":")) %>%
    mutate(year = extract_numeric(term)),
  
  .id = "model"
  ) %>%
  ggplot(aes(x = year, y = estimate, color = model)) +
    geom_hline(aes(yintercept = 0), color = "white", size = 1.25) +
    geom_line() +
    geom_pointrange(aes(ymin = estimate - std.error, 
                        ymax = estimate + std.error)) +
    facet_wrap(~model) +
    labs(title = "Effects (betas) of job characteristics on mental health 
  in each year\n", 
         x = "", y = "") +
    theme_economist() +
    scale_color_wsj() +
    theme(legend.position = "none",
          panel.grid.major.y = element_line(size = 0)) 


# Fit the conditional effect of each job characteristic in an interaction model
fit <- lm(swb ~ 1 + year + job_control:year + job_demand:year + job_security:year, 
          data = df)

tidy(fit) %>%
  filter(str_detect(term, ":")) %>%
  separate(term, into = c("year", "model"), sep = ":") %>%
  mutate(year = extract_numeric(year)) %>% #count(model)
  ggplot(aes(x = year, y = estimate, color = model)) +
    geom_hline(aes(yintercept = 0), color = "white", size = 1.25) +
    geom_line() +
    geom_pointrange(aes(ymin = estimate - std.error, 
                        ymax = estimate + std.error)) +
    facet_wrap(~model) +
    labs(title = "Effects (betas) of job characteristics on mental health 
in each year\n", 
         x = "", y = "") +
    theme_economist() +
    scale_color_wsj() +
    theme(legend.position = "none",
          panel.grid.major.y = element_line(size = 0)) 