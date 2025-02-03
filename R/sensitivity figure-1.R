# Requires occupations

# Figure 1 ####
# 
# Set thresholds
#  e.g., with(read_rds("../data/occupations.rds"), summary(job_control))

with(read_rds("../data/occupations.rds"), summary(job_resources))

ctrl_t <- 3.25  # job_control 1st Qu.
dmnd_t <- 4.667 # job_demand6 3rd Qu.
rsrc_t <- 3.667 # job_resources 1st Qu.


# binomial mean and ci
percent_ci <- function(.x) {
  # must pass a numeric vector!
  
  .x <- stats::na.omit(.x)
  x = sum(.x)
  n = sum(!is.na(.x))
  
  prop.test(x = x, n = n) %>%
    broom::tidy() %>%
    select(y = estimate, ymin = conf.low, ymax = conf.high) %>%
    mutate(across(everything(), ~. *100)) %>%
    mutate(n = n)
}

fig1 <- bind_rows(
  `MHI-5 < 54 (%)` = occupations %>%
    mutate(low_mhi5 = ghmh < 54) %>%
    group_by(profession, year) %>%
    summarise(
      result = percent_ci(low_mhi5),
      .groups = "drop"
    ) %>%
    unnest(result),
  
  `K10 score` = occupations %>%
    group_by(profession, year) %>%
    summarise(
      result = mean_se(pdk10s, mult = 1.96),
      .groups = "drop"
    ) %>%
    unnest(result),
  
  `low control (%)` = occupations %>%
    mutate(low_control = job_control < ctrl_t) %>%
    group_by(profession, year) %>%
    summarise(
      result = percent_ci(low_control),
      .groups = "drop"
    ) %>%
    unnest(result),
  
  `high demand (%)` = occupations %>%
    mutate(high_demand = job_demand6 > dmnd_t) %>%
    group_by(profession, year) %>%
    summarise(
      result = percent_ci(high_demand),
      .groups = "drop"
    ) %>%
    unnest(result),
  
  `low resources (%)` = occupations %>%
    mutate(low_resources = job_resources < rsrc_t) %>%
    group_by(profession, year) %>%
    summarise(
      result = percent_ci(low_resources),
      .groups = "drop"
    ) %>%
    unnest(result),
  
  .id = "outcome"
) %>%
  mutate(
    outcome = fct_relevel(outcome, 
                          "MHI-5 < 54 (%)", "K10 score", "low control (%)")) 


ggplot(fig1, aes(x = year, y = y)) +
  geom_pointrange(aes(ymin = ymin, ymax = ymax)) +
  geom_smooth(se=F, linetype = "dashed", linewidth = .5, colour = "black") +
  facet_grid(outcome ~ profession, scales = "free_y") +
  labs(y = "", x = "
       Survey year") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1))