library(tidyverse)
library(lubridate)
library(cowplot)
library(scales)
library(units)

source("functions_tg.R")

# Zeitreihen der Rohdaten Temperatur und Load für den ganzen Zeitraum
texas <- read_rds("../../data/cache/data-cached.rds") %>%
  mutate(
    across(where(~inherits(.x, what = "units")), drop_units),
    delta = temp - thresh.temp,
    deficit = (load_est - thresh) / 1000
  ) %>%
  select(time, temp, deficit)

events.temp.pooled <- "../../data/cache/events-temp-cached.rds" %>%
  read_rds() %>%
  filter(duration > set_units(2, d), min.temp < set_units(-8.5, "°C"))  %>%
  mutate(
    across(where(~inherits(.x, what = "units")), drop_units),
    event = row_number()
  )

events.load.pooled <- "../../data/cache/events-load-cached.rds" %>%
  read_rds() %>%
  filter(duration > set_units(6, h)) %>%
  mutate(
    event = row_number(),
    across(where(~inherits(.x, what = "units")), drop_units),
  )

# Spaltennamen angleichen für Load und Temp Events
events <- tribble(
  ~variable, ~data,
  "load", events.load.pooled %>% 
    rename(area = loss, extreme.time = time.max, extreme.value = max.deficit) %>%
    mutate(
      data = map(data, transmute, event.orig, time, value = drop_units(delta) / 1000)
    ),
  "temp", events.temp.pooled %>% 
    rename(extreme.time = time.min, extreme.value = min.temp) %>%
    mutate(
      data = map(data, transmute, event.orig, time, value = drop_units(temp))
    )
) %>%
  unnest(data) 

# Grenzen der x-Achse für jedes Event finden, sodass alle events 
# gleich breit sind und das Zentrum des Events in der Mitte ist
xlimits <- events %>%
  group_by(event) %>%
  summarise(
    onset = min(onset), 
    termination = max(termination),
    center = round_date(mean(c(onset, termination)), "hour"), 
    duration = termination - onset
  ) %>%
  transmute(
    event,
    xstart = floor_date(center + max(duration) * -0.5 * 1.08, "hour"),
    xend = floor_date(center + max(duration) * 0.5 * 1.08, "hour")
  ) %>%
  transmute(
    event,
    time = map2(xstart, xend, seq, by = "1 hour")
  )


# y-Achsen gleich halten über alle Panele
ylimits <- xlimits %>%
  unnest(time) %>%
  left_join(texas, by = "time") 


# Farbige Flächen der Events
polygons <- events %>%
  select(event, variable, data) %>%
  unnest(data) 

# Erstellen der Plots ---- 
plots <- list()
for(i in unique(events$event)) {
  
  # Seitenränder (padding) abhänig von der Spalte des Plots
  # 3 spaltig und wir wollen Platz sparen
  pad <- if(i %in% c(2, 5, 8)) 0 else 2
  
  
  # Temperatur plot
  xlim <- xlimits %>% filter(event == !!i) %>% pull(time) %>% map(range)
  ylim <- range(ylimits$temp, na.rm = TRUE)
  
  label <- events %>% 
    filter(event == !!i, variable == "temp") %>%
    mutate(
      label = glue::glue("{round(extreme.value, 1)} °C"),
      label.panel = glue::glue("Event {event} ({year})")
    )
  
  poly <- polygons %>% filter(event == !!i, variable == "temp")
  
  p.temp <- ggplot(mapping = aes(x = time)) + 
    geom_ribbon(data = poly, aes(ymin = 0, ymax = value, group = event.orig), 
                fill = "lightblue", alpha = 0.75) + 
    
    geom_hline(yintercept = 0, size = 0.1, col = "darkgrey") + 
    geom_point(data = label, aes(x = extreme.time, y = extreme.value),
               size = 1.5, shape = 21, fill = "lightgrey", stroke = 0.35, col = "black") + 
    geom_line(data = texas, aes(y = temp), size = 0.35) + 
    
    geom_text(data = label, aes(x = extreme.time, y = extreme.value, label = label), 
              hjust = -0.1, vjust = 1.1, size = 2) + 
    labs(subtitle = label$label.panel, 
         y = "Tempera-\nture °C") + 
    geom_text(data = label, 
              aes(label = glue::glue("Duration: {duration} h")),  
              x = mean(c(label$onset, label$termination)), y = Inf, 
              hjust = 0.5, vjust = 1.5, size = 2) + 
    
    
    scale_x_datetime(date_breaks = "2 day", minor_breaks = six_hours,
                     date_labels = "%b\n%d", expand = expansion()) + 
    scale_y_continuous(expand = expansion(mult = c(0.15, 0.04))) + 
    coord_cartesian(ylim = ylim, xlim = xlim[[1]]) + 
    theme_grey(base_size = 8) + 
    theme(axis.title.x = element_blank(),
          axis.ticks.x = element_blank(), 
          axis.text.x = element_blank(),
          plot.margin = unit(c(2, pad, 0, pad), units = "pt"), 
          axis.title.y = element_text(size = rel(0.8)),
          plot.subtitle = element_text(size = rel(0.8)), 
          panel.grid = element_blank(),
          panel.background = element_rect(fill = NA, colour = "black"))
  
  
  # load plot
  ylim <- range(ylimits$deficit, na.rm = TRUE)
  
  label <- events %>% 
    filter(event == !!i, variable == "load") %>%
    mutate(
      label = glue::glue("{round(extreme.value, 1)} GW")
    )
  poly <- polygons %>% filter(event == !!i, variable == "load")
  
  p.load <- ggplot(mapping = aes(x = time)) + 
    geom_ribbon(data = poly, aes(ymin = 0, ymax = value, group = event.orig), 
                fill = "#F8766D", alpha = 0.75) + 
    
    geom_hline(yintercept = 0, size = 0.1, col = "darkgrey") + 
    geom_point(data = label, aes(x = extreme.time, y = extreme.value),
               size = 1.5, shape = 21, fill = "lightgrey", stroke = 0.35, col = "black") + 
    geom_line(data = texas, aes(y = deficit), size = 0.35) + 
    
    geom_text(data = label, aes(x = extreme.time, y = extreme.value, label = label), 
              hjust = -0.1, vjust = -0.1, size = 2) + 
    labs(y = "Capacity\ndeficit GW") + 
    geom_text(data = label, 
              aes(label = glue::glue("Duration: {duration} h")),  
              x = mean(c(label$onset, label$termination)), y = -Inf, 
              hjust = 0.5, vjust = -0.5, size = 2) + 
    
    
    scale_x_datetime(date_breaks = "2 day", minor_breaks = six_hours,
                     date_labels = "%b\n%d", expand = expansion()) + 
    scale_y_continuous(expand = expansion(mult = c(0.04, 0.15))) + 
    coord_cartesian(ylim = ylim, xlim = xlim[[1]]) + 
    theme_grey(base_size = 8)  + 
    theme(axis.title.x = element_blank(), 
          plot.subtitle = element_blank(),
          plot.margin = unit(c(0, pad, 6, pad), units = "pt"), 
          axis.title.y = element_text(size = rel(0.8)), 
          panel.grid = element_blank(),
          panel.background = element_rect(fill = NA, colour = "black"))
  
  
  if ((i %% 3) != 1) {
    p.temp <- p.temp + theme(axis.title.y = element_blank(),
                             axis.text.y = element_blank())
    p.load <- p.load + theme(axis.title.y = element_blank(),
                             axis.text.y = element_blank())
    
  }
  
  plots[[as.character(i)]] <- plot_grid(p.temp, p.load, ncol = 1, align = "v")
  
}

p <- plot_grid(plotlist = plots, align = "h", rel_widths = c(1.29, 1, 1.04))
plot(p)
ggsave("../../data/figures/deficit_events_LR24temptrend_Hook-8_+1.5GW.pdf", plot = p, width = 122, height = 122, units = "mm")
