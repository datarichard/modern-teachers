# Requires occupations

ctrl_t <- 3.25 # with(read_rds("../data/occupations.rds"), summary(job_control))
dmnd_t <- 4.556 # with(read_rds("../data/occupations.rds"), summary(job_demand))

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

percent_ci(occupations$job_demand6)



bind_rows(
  `MHI-5 score` = occupations %>%
    group_by(profession, year) %>%
    summarise(
      result = mean_se(ghmh, mult = 1.96),
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
    mutate(high_demand = job_demand > dmnd_t) %>%
    group_by(profession, year) %>%
    summarise(
      result = percent_ci(high_demand),
      .groups = "drop"
    ) %>%
    unnest(result),
  
  .id = "outcome"
) %>%
  mutate(outcome = fct_relevel(outcome, 
          "MHI-5 score", "low control (%)")) -> fig1


ggplot(fig1, aes(x = year, y = y)) +
  geom_pointrange(aes(ymin = ymin, ymax = ymax)) +
  geom_smooth(se=F, linetype = "dashed", linewidth = .5, colour = "black") +
  facet_grid(outcome ~ profession, scales = "free_y") +
  labs(y = "", x = "
       Survey year") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1))


# test the linear trend in prevalence
fig1 %>%
  filter(outcome %in% c("low control (%)", "high demand (%)")) %>%
  group_by(profession, outcome) %>%
  nest() %>%
  transmute(
    fit = map(data, ~lm(y ~ year, weights = n, data = .x) %>% 
                broom::tidy() %>%
                filter(term == "year"))
  ) %>%
  unnest(fit)

# profession outcome         term  estimate std.error statistic     p.value
# Teachers   low control (%) year     1.13      0.145      7.80 0.000000767
# Nurses     low control (%) year     0.532     0.165      3.23 0.00521    
# Teachers   high demand (%) year     0.452     0.152      2.98 0.00879    
# Nurses     high demand (%) year     0.972     0.132      7.36 0.00000160 


## Not run ####
# test the linear trend in component scores
test_lme_trend <- function(.df, .y, .prof) {
  
  require(nlme)
  
  df <- select(.df, xwaveid, year, profession, y = one_of(.y)) %>%
    filter(profession == .prof) %>%
    mutate(yt = as.numeric(y < ctrl_t)) %>%
    # mutate(yt = as.numeric(y > dmnd_t)) %>%
    na.omit()
  
  nlme::lme(fixed = yt ~ year,
      random = ~1|xwaveid,
      correlation = corAR1(form = ~year|xwaveid),
      data = df) %>%
  broom.mixed::tidy(effects = "fixed")
    
}

test_lme_trend(occupations, "job_demand", "Teachers") 
test_lme_trend(occupations, "job_control", "Teachers") 

test_logit_trend <- function(.df, .outcome, .prof) {
  
  df <- filter(.df, outcome == .outcome, profession == .prof) %>%
    mutate(y = y/100)
  
  glm(y ~ year,
      family = binomial(link = "logit"),
      weights = n,
      data = df) %>%
    broom::tidy(exponentiate=T)
  
}

test_logit_trend(fig1, "low control (%)", "Teachers")
test_logit_trend(fig1, "low control (%)", "Nurses")

test_logit_trend(fig1, "high demand (%)", "Teachers")
test_logit_trend(fig1, "high demand (%)", "Nurses")


test_lm_trend <- function(.df, .outcome, .prof) {
  
  df <- filter(.df, outcome == .outcome, profession == .prof)
  
  lm(y ~ year,
      weights = n,
      data = df) %>%
    broom::tidy()
  
}

test_lm_trend(fig1, "low control (%)", "Teachers")
test_lm_trend(fig1, "low control (%)", "Nurses")

test_lm_trend(fig1, "high demand (%)", "Teachers")
test_lm_trend(fig1, "high demand (%)", "Nurses")



# test the trend in proportions
test_trends <- function(.df, .outcome, .prof) {
  filter(.df, outcome == .outcome, profession == .prof) %>%
    pull(n) -> low_control_n
  
  filter(.df, outcome == .outcome, profession == .prof) %>%
    pull(y) -> low_control_y
  
  low_control_p = low_control_y / 100
  low_control_tally = low_control_p * low_control_n
  
  prop.trend.test(x = low_control_tally, n = low_control_n)
}

test_trends(fig1, "low control (%)", "Teachers")
test_trends(fig1, "high demand (%)", "Teachers")
test_trends(fig1, "low control (%)", "Nurses")
test_trends(fig1, "high demand (%)", "Nurses")

ans <- test_trends(df, "high demand (%)", "Nurses")
ans$method

