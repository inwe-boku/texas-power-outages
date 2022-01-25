library(tidyverse)
library(units)

events.load.pooled <- "../../data/cache/events-load-cached.rds" %>%
  read_rds() %>%
  filter(duration > set_units(6, h)) %>%
  transmute(
    year,
    loss = drop_units(loss)
  )


# events.load.pooled <- data.frame(
#   year = c(1951, 1962, 1963, 1979, 1982, 1984, 1985, 1990, 2021), 
#   loss = c(566.2, 758, 201.4, 247, 228.4, 603.9, 155.2, 640.4, 981.4)
# )

p <- ggplot(events.load.pooled, aes(year, loss)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = "y~x", fullrange = TRUE, 
              col = "black", size = 0.5) + 
  labs(y = "Loss of load in GW h", x = "Year") + 
  #theme_gray(base_size = 8)
  theme_bw(base_size=8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
plot(p)
ggsave("../../data/figures/extremes-trend_LR24temptrend_Hook-8_+1.5GW.pdf", plot = p, 
       width = 122, height = 122 * 4/ 6, units = "mm")


events.load.pooled %>% 
  write_csv('../../data/figures_raw_data/JS_events_load_pooled.csv')

# "../../data/cache/events-load-cached.rds" %>%
#   read_rds()  %>%
#   writexl::write_xlsx(path = "../../data/cache/events-load-cached.xlsx")
