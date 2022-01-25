library(tidyverse)
library(lubridate)
library(units)
library(rlang)

source("functions_tg.R")

# Rohdaten importieren und cachen -----
raw <- read_csv("../../data/interim/load/load_est70_LR24_temptrend_Hook-8.csv") %>%
  mutate(
    time = force_tz(time, tzone = "Etc/GMT-6")
  )


thres_total <- read_csv("../../data/interim/thresholds/thresh_totalPP63.5GW.csv",
                        col_names = c("time", "value")) %>%
  transmute(
    time = force_tz(time, tzone = "Etc/GMT-6"),
    thresh = value*1e3
  )

texas <- inner_join(raw, thres_total, by = "time") %>%
  mutate(thresh.temp = 0)
write_rds(texas, file = "../../data/cache/data-cached.rds", compress = "gz")


# Daten einlesen -----
texas <- read_rds("../../data/cache/data-cached.rds") 


# Temperatur Events ableiten und poolen ------
events.temp <- texas %>%
  summarise_event(
    values_from = "temp",             # Name der Datenspalte
    threshold_from = "thresh.temp",   # Name der Thresholdspalte
    min.temp = min(delta),     # zusätzliche Variablen die berechnet werden sollen, 
    time.min = time[which.min(delta)],
    
    default_unit = "°C",               # Einheit von Threshold und Datenspalte, falls noch nicht existent
    reverse_sign = FALSE
  ) 

# das letzte Event hat mitunter kein Ende, wenn die Zeitreihe nicht 
# wieder über den Threshold steigt 
events.temp <- events.temp %>% 
  filter(!(is.na(duration) & row_number() == n()))

# die Zeitreihe ist etwas grauslich, weil sie nur Winter enthält. 
# Für den Sommer leider keine NAs, wir erkennen nicht, wie lange der letzte Wert gilt
# events die am letzten Tag des Winters noch nicht beendet sind entfernen
events.temp <- events.temp %>%
  filter(month(termination) != 3)

events.temp.pooled <- events.temp %>%
  pool_events(
    events = "below",                  # wann tritt ein event ein? Wenn value < threshold oder umgekehrt?
    min.duration = set_units(39, h),   # Poolingdauer unterhalb der Events zusammengefasst werden
    time.min = time.min[which.min(min.temp)], # overrulen der default Funktion sum()
    min.temp = min(min.temp),
    #duration = termination - onset,
  ) %>%
  mutate(
    duration = set_units(as.double(duration, units = "hours"), "h"),
    year = year(onset)
  ) 

events.temp.pooled %>%
  write_rds(
    file = "../../data/cache/events-temp-cached.rds", 
    compress = "gz"
  )


# Load Events ableiten und poolen ------
events.load <- texas %>%
  summarise_event(
    values_from = "load_est",    # Name der Datenspalte
    threshold_from = "thresh",   # Name der Thresholdspalte
    max.deficit = max(delta),     # zusätzliche Variablen die berechnet werden sollen, 
    time.max = time[which.max(delta)],
    default_unit = "MW"          # Einheit von Threshold und Datenspalte, falls noch nicht existent
  ) %>%
  mutate(
    area = set_units(area, h * GW),
    max.deficit = set_units(max.deficit, GW)
  )

# das letzte Event hat mitunter kein Ende, wenn die Zeitreihe nicht 
# wieder über den Threshold steigt 
events.load <- events.load %>% 
  filter(!(is.na(duration) & row_number() == n()))

# Poolen der Events 
events.load.pooled <- events.load %>%
  pool_events(
    events = "above",  # wann tritt ein event ein? Wenn value < threshold oder umgekehrt?
    #min.duration = set_units(11, d),   # Poolingdauer, 
    min.duration = set_units(4, d),   # Poolingdauer, 
    time.max = time.max[which.max(max.deficit)],
    max.deficit = max(max.deficit)
  ) %>%
  rename(loss = area) %>%
  mutate(
    #duration = set_units(as.double(duration, units = "hours"), "h"),
    # year = year(rollback(onset, roll_to_first = TRUE) + months(2))
    year = year(onset)
  ) 


events.load.pooled %>%
  write_rds(
    file = "../../data/cache/events-load-cached.rds", 
    compress = "gz"
  )


# Extremwertreihe beider Variablen ---- 
series <- bind_rows(
  
  events.load.pooled  %>%
    # Load events <= 6h sollen zero events werden
    filter(duration > set_units(6, h)) %>%
    select(duration, loss, max.deficit, year) %>%
    mutate(
      variable = "load",
      across(where(~inherits(.x, what = "units")), drop_units)
    ) %>%
    pivot_longer(-c(year, variable)),
  
  events.temp.pooled %>%
    group_by(year) %>%
    summarise(duration=max(duration), area=-min(area), min.temp=min(min.temp))
  %>%
    select(duration, area, min.temp, year) %>%
    mutate(variable = "temp",
           across(where(~inherits(.x, what = "units")), drop_units)
    ) %>%
    pivot_longer(-c(year, variable))
) 

s <- series %>%
  group_by(variable, name) %>%
  nest() %>%
  ungroup() %>%
  mutate(
    label = paste(variable, name),
    label = factor(label, 
                   levels = c("temp duration", "temp area", "temp min.temp",
                              "load duration", "load loss", "load max.deficit"), 
                   labels = c("frost duration in h",
                              "frost sum in °C h",
                              "min temperature in °C", 
                              "capacity deficit duration in h", 
                              "total loss of load in GW h",
                              "max capacity deficit in GW"))
  ) %>%
  arrange(label)

s %>%
  write_rds(
    file = "../../data/cache/ev-series-cached.rds", 
    compress = "gz"
  )

s %>% 
  unnest(cols=c(data)) %>% 
  write_csv('/data/projects/texas-power-outages/data/figures_raw_data/TG_s.csv')

events.load.pooled %>% 
  mutate(duration1=duration) %>% 
  dplyr::select(-duration) %>% 
  unnest(cols=c(data)) %>%
  write_csv('/data/projects/texas-power-outages/data/figures_raw_data/TG_events_load_pooled.csv')

events.temp.pooled %>% 
  mutate(duration1=duration) %>% 
  dplyr::select(-duration) %>% 
  mutate(area1=area) %>% 
  dplyr::select(-area) %>% 
  unnest(cols=c(data)) %>% 
  write_csv('/data/projects/texas-power-outages/data/figures_raw_data/TG_events_temp_pooled.csv')

texas %>% 
  write_csv('/data/projects/texas-power-outages/data/figures_raw_data/TG_texas.csv')
