demand_items = c("jomms", "jompi", "jomcd", "jomns", "jomus", "jomini", 
                  "jomfast", "jomwi", "jomtime")

control_items = c("jomfd", "jomls", "jomfw", "jomdw", "jomflex", "jombrk", 
                  "jomrpt", "jomvar")

# total effects of each item
teachers %>% 
  select(xwaveid, year, ghmh, all_of(demand_items)) %>%
  gather(key = "key", value = "val", jomms:jomtime) %>%
  filter(year > 2004) %>%
  group_by(year, key) %>%
  nest() %>%
  mutate(fit = map(data, ~lm(formula = ghmh ~ 1 + val, data = .))) %>%
  transmute(betas = map(fit, tidy)) %>%
  unnest(betas) %>%
  filter(term %notin% "(Intercept)") %>%
  ggplot(aes(x = year, y = estimate, color = key)) +
    geom_hline(aes(yintercept = 0), color = "white", size = 1.25) +
    geom_line() +
    geom_pointrange(aes(ymin = estimate - std.error, 
                        ymax = estimate + std.error)) +
    coord_cartesian(ylim = c(-4, 4)) +
    facet_wrap(~key, ncol=1) +
    theme_economist() +
    labs(title = "Job demands\n", x = "", y = "", caption = "total effects") +
    theme(legend.position = "none",
          panel.grid.major.y = element_line(size = 0),
          axis.text.y = element_blank(),
          plot.title = element_text(hjust=0.5),
          strip.text = element_text(hjust=0)) -> p1

teachers %>% 
  select(xwaveid, year, ghmh, all_of(control_items)) %>%
  gather(key = "key", value = "val", jomfd:jomvar) %>%
  filter(year > 2004) %>%
  group_by(year, key) %>%
  nest() %>%
  mutate(fit = map(data, ~lm(formula = ghmh ~ 1 + val, data = .))) %>%
  transmute(betas = map(fit, tidy)) %>%
  unnest(betas) %>%
  filter(term %notin% "(Intercept)") %>%
  ggplot(aes(x = year, y = estimate, color = key)) +
    geom_hline(aes(yintercept = 0), color = "white", size = 1.25) +
    geom_line() +
    geom_pointrange(aes(ymin = estimate - std.error, 
                        ymax = estimate + std.error)) +
    coord_cartesian(ylim = c(-4, 4)) +
    facet_wrap(~key, ncol=1) +
    theme_economist() +
    labs(title = "Job control\n", x = "", y = "", caption = "total effects") +
    theme(legend.position = "none",
          panel.grid.major.y = element_line(size = 0),
          axis.text.y = element_blank(),
          plot.title = element_text(hjust=0.5),
          strip.text = element_text(hjust=0)) -> p2


p1 + p2
ggsave(file = "job demands & control by item.png",
       path = "../figures/",
       device = "png",
       dpi = 100,
       width = 600,
       height = 900,
       units = "px")

# Conditional/unique effects of each item
teachers %>%
  group_by(xwaveid) %>%
  mutate(ghmh = c(scale(ghmh))) %>%
  ungroup() %>%
  filter(year > 2004) %>%
  arrange(year) %>%
  group_by(year) %>%
  nest() %>%
  mutate(
    demand.fit = map(data, ~lm(
      formula = reformulate(demand_items, response= "ghmh"), 
      data = .)
    ),
    control.fit = map(data, ~lm(
      formula = reformulate(control_items, response= "ghmh"), 
      data = .)
    ),
    demand.betas = map(demand.fit, tidy),
    control.betas = map(control.fit, tidy)
  ) %>%
  select(year, demand.betas, control.betas) -> df

select(df, year, demand.betas) %>%
  unnest(demand.betas) %>%
  filter(term %notin% "(Intercept)") %>%
  ggplot(aes(x = year, y = estimate, color = term)) +
    geom_hline(aes(yintercept = -0.1), color = "#d5e4eb", size = 0) +
    geom_hline(aes(yintercept = 0), color = "white", size = 1.25) +
    geom_hline(aes(yintercept = 0.1), color = "#d5e4eb", size = 0) +
    geom_line() +
    geom_pointrange(aes(ymin = estimate - std.error, 
                        ymax = estimate + std.error)) +
    labs(title = "Job demands\n", x = "", y = "", caption = "conditional") +
    facet_wrap(~term, ncol = 1) +
    theme_economist() +
    theme(legend.position = "none",
          panel.grid.major.y = element_line(size = 0),
          axis.text.y = element_blank(),
          plot.title = element_text(hjust=0.5),
          strip.text = element_text(hjust=0)) -> p3

select(df, year, control.betas) %>%
  unnest(control.betas) %>%
  filter(term %notin% "(Intercept)") %>%
  ggplot(aes(x = year, y = estimate, color = term)) +
    geom_hline(aes(yintercept = -0.1), color = "#d5e4eb", size = 0) +
    geom_hline(aes(yintercept = 0), color = "white", size = 1.25) +
    geom_hline(aes(yintercept = 0.1), color = "#d5e4eb", size = 0) +
    geom_line() +
    geom_pointrange(aes(ymin = estimate - std.error, 
                        ymax = estimate + std.error)) +
    facet_wrap(~term, ncol = 1) +
    labs(title = "Job control\n", x = "", y = "", caption = "conditional") +
    theme_economist() +
    theme(legend.position = "none",
          panel.grid.major.y = element_line(size = 0),
          axis.text.y = element_blank(),
          plot.title = element_text(hjust=0.5),
          strip.text = element_text(hjust=0)) -> p4

p3 + p4

ggsave(file = "job demands & control by item-conditional.png",
       path = "../figures/",
       device = "png",
       dpi = 100,
       width = 600,
       height = 900,
       units = "px")