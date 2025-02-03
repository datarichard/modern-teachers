model.df <- occupations %>%
  mutate(rjob_control = 7 - job_control,
         ajob_demand = job_demand - 1,
         cjob_control = c(scale(job_control, scale = F)),
         cjob_demand = c(scale(job_demand, scale = F)),
         cage = c(scale(age, scale=F)),
         cjbempt = c(scale(jbempt, scale=F)))

count(model.df, job_control, rjob_control)
summary(model.df$rjob_control)
summary(model.df$cjbempt)
summary(model.df$ajob_demand)

# Model definitions ####
# Using a continuous by variable allows the smooth for year to interact with the
# parametric job term. Such models are sometimes called varying coefficient 
# models. The idea is that the linear regression coefficient for year is varying
# smoothly with job (or vice versa).
# 
# (Must include jbempt, uncentered so partial effects work below)
unadj_f <- formula(ghmh ~ s(year))
adjst_f <- formula(ghmh ~ s(year) + 
                     cage + cage:year + sex + sex:year +
                     jbempt + jbempt:year)
cntrl_f <- formula(ghmh ~ s(year) + 
                     cage + cage:year + sex + sex:year +
                     jbempt + jbempt:year +
                     s(year, by = rjob_control) + rjob_control)
demnd_f <- formula(ghmh ~ s(year) + cage + cage:year + sex + sex:year +
                     jbempt + jbempt:year +
                     # s(year, by = rjob_control) + rjob_control +
                     s(year, by = ajob_demand) + ajob_demand)



# demnd_f <- formula(ghmh ~ s(year) + year + s(year, by = cjob_demand) + 
#                      cjob_demand + cage + cage:year + jbempt + jbempt:year)

fit_t_unadj <- fit_ghmh(model.df, .prof = "Teachers", .f = unadj_f)
fit_t_adjst <- fit_ghmh(model.df, .prof = "Teachers", .f = adjst_f)
fit_t_cntrl <- fit_ghmh(model.df, .prof = "Teachers", .f = cntrl_f)
fit_t_demnd <- fit_ghmh(model.df, .prof = "Teachers", .f = demnd_f)
fit_n_unadj <- fit_ghmh(model.df, .prof = "Nurses",   .f = unadj_f)
fit_n_adjst <- fit_ghmh(model.df, .prof = "Nurses",   .f = adjst_f)
fit_n_cntrl <- fit_ghmh(model.df, .prof = "Nurses",   .f = cntrl_f)
fit_n_demnd <- fit_ghmh(model.df, .prof = "Nurses",   .f = demnd_f)

# Partial effect plots ####
# Plot the zero-centred, partial effect of year (i.e., the partial-effect sets
# all other terms to zero)
# 
# https://ecogambler.netlify.app/blog/interpreting-gams/
# 
# individual plots
# plot(fit_t_nc, select = 1, shade = T, main = "unadjusted")
# plot(fit_t_wc, select = 1, shade = T, main = "age + gender")
# plot(fit_t_control, select = 1, shade = T, main = "age + gender + control")
# plot(fit_t_demand, select = 1, shade = T, main = "age + gender + demand")

# main plot
df_t <- bind_rows(
  `Model I` = smooth_data(fit_t_unadj),
  `Model II` = smooth_data(fit_t_adjst),
  `Model III` = smooth_data(fit_t_cntrl),
  `Model IV` = smooth_data(fit_t_demnd),
  .id = "model"
) %>%
  mutate(profession = "Teachers")

df_n <- bind_rows(
  `Model I` = smooth_data(fit_n_unadj),
  `Model II` = smooth_data(fit_n_adjst),
  `Model III` = smooth_data(fit_n_cntrl),
  `Model IV` = smooth_data(fit_n_demnd),
  .id = "model"
) %>%
  mutate(profession = "Nurses")
  
df <- bind_rows(df_t, df_n) %>%
  mutate(
    # model = fct_relevel(model, "unadjusted", "age + sex + tenure"),
    profession = fct_relevel(profession, "Teachers"))

ggplot(df, aes(x = x, y = fit)) +
  geom_hline(aes(yintercept = 0), color = "grey50", linetype = "dashed") +
  geom_ribbon(aes(ymin = fit - se, ymax = fit + se),
              alpha = 0.2) +
  geom_line(aes()) +
  facet_grid(model ~ profession) +
  labs(y = "Estimated MHI-5 trend", x = "
       Survey year", y = "") +
  theme(axis.title = element_text(size = 14))

# Results #####

# Non-linear changes
summary(fit_t_unadj)
summary(fit_n_unadj)
summary(fit_t_adjst)
summary(fit_n_adjst)
summary(fit_t_cntrl)
summary(fit_t_demnd)
summary(fit_n_cntrl)
summary(fit_n_demnd)

# Changes in sensitivity
plot(fit_t_cntrl, pages = 1, shade = T)
plot(fit_t_demnd, pages = 1, shade = T)

# Marginal effect plots ####
# Plot the predicted effect of year, setting all other terms to the average 
# value (I don't think this is what we want)
library(ggeffects)

ggpredict(fit_t_nc, terms = "year") %>% plot()
ggpredict(fit_t_wc, terms = "year") %>% plot()
ggpredict(fit_t_control, terms = "year", conditions = c(job_control = 7)) %>% plot()
ggpredict(fit_t_demand, terms = "year", condition = c(job_demand = 1)) %>% plot()

## Not run ####
ctrl_t <- 3.25 # with(read_rds("../data/occupations.rds"), summary(job_control))
dmnd_t <- 4.556 # with(read_rds("../data/occupations.rds"), summary(job_demand))

fig2 <- occupations %>%
  # select(xwaveid, year, ghmh, job_control, job_demand, job_security) %>%
  mutate(
    job_strain = case_when(
      job_control < ctrl_t & job_demand > dmnd_t ~ "Job strain",
      job_control < ctrl_t ~ "Low control",
      job_demand > dmnd_t ~ "High demand",
      TRUE ~ "Low strain"),
    job_strain = fct_relevel(job_strain, "Job strain")
  )

prop.ci <-  function(n, p, a=0.05) {
  ## Level (1-a) confidence interval for proportion (equivalent to prop.test)
  z <- qnorm(1-a/2,lower.tail=FALSE)
  l <- 1/(1+1/n*z^2)*(p + 1/2/n*z^2 + 
                        z*sqrt(1/n*p*(1-p) + 1/4/n^2*z^2))
  u <- 1/(1+1/n*z^2)*(p + 1/2/n*z^2 -
                        z*sqrt(1/n*p*(1-p) + 1/4/n^2*z^2))
  c(lower=l, upper=u)
}

binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "quasibinomial"), ...)
}

fig2 %>%
  percent(profession, year, job_strain) %>%
  mutate(N = sum(n)) %>%
  filter(job_strain != "Low strain") %>%
  rowwise() %>%
  mutate(lo = prop.ci(N, proportion)[1],
         hi = prop.ci(N, proportion)[2]) %>% 
  ggplot(aes(x = year, y = proportion, group = job_strain)) +
  geom_pointrange(aes(ymin = lo, ymax = hi)) +
  binomial_smooth(formula = 'y ~ x', size = .5, color = "black",
                  # adjust level for prediction intervals
                  level = .999999999999999) + 
  scale_y_continuous(labels = scales::percent) +
  facet_grid(job_strain~profession, switch = "y") +
  labs(x = "Survey year", y = "") +
  theme(strip.placement = "outside")
