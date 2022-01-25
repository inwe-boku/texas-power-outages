library(ggrepel)
library(cowplot)
library(tidyverse)
library(scales)
library(units)
library(lfstat)

source("functions_tg.R")


series <- read_rds("../../data/cache/ev-series-cached.rds") 

# lfstat kann keine Extremwertverteilung für negative Werte anpassen... 
series$data[[3]]$value <- -series$data[[3]]$value

# Gregor will nur ein Event pro Jahr, aufs annual maximum filtern
am <- function(x) 
{
  x %>%
    group_by(year) %>%
    summarise(value = max(value))
}

ss <- series %>%
  mutate(
    data = map(data, am),
    observations = map(data, obs2empirical), 
    fit = map(observations, ~fit_maximum(.x$value)),
    fun = map(fit, quant),
    row = ((row_number() - 1) %/% 3) + 1, 
    
    point.labs = map(observations, slice_max, value, n = 5),
    value2021 = map_dbl(observations, ~pull(filter(.x, year == 2021), "value")),
    prob2021 = map_dbl(observations, ~pull(filter(.x, year == 2021), "prob")),
    rp.theo = map2_dbl(value2021, fit, ev_return_period),
    prob.theo = 1 - 1/rp.theo,
    #value.theo = map2_dbl(fit, rp.theo, ~unlist(evquantile(.x, .y)$T_Years_Event))
  ) %>%
  rowwise() %>%
  mutate(
    end.guide = max(prob.theo, prob2021),
    guides = list(tibble(prob = c(0, end.guide, NA, prob.theo, prob.theo),
                         value = c(value2021, value2021, NA, Inf, -Inf)))
  )   

ss$label <- sub("(.)", "\\U\\1",ss$label,perl=TRUE)

plots <- list()
for(i in seq_len(nrow(ss))) {
  
  p <- ggplot(mapping = aes(x = prob, y = value)) + 
    geom_text_repel(data = ss$point.labs[[i]], aes(label = year), 
                    size = 2, force_pull = 0.1, # direction = "x", # label.padding = unit(50, "pt"),
                    min.segment.length = unit(0.1, "pt"),
                    segment.colour = "darkgrey") +
    geom_point(data = ss$observations[[i]], alpha = 0.2) + 
    geom_line(data = ss$fun[[i]], size = 0.5) + 
    geom_path(data = ss$guides[[i]], col = "blue", 
              linetype = "dashed", size = 0.2) + 
    
    labs(x = "Probability", 
         y = sub(" in", "\n", ss$label[i])) + 
    scale_x_continuous(trans = rv_trans, sec.axis = sec_axis_rp, 
                       expand = expansion(), 
                       labels = function(x) gsub("(\\.[^0]+)0+", "\\1", x)) + 
    theme_grey(base_size = 8) + 
    theme(plot.margin = unit(c(1, 6, 0, 6), units = "pt"),
          plot.title = element_text(hjust = 0.5), 
          panel.background = element_rect(fill = NA, colour="black"))
  
  if(ss$label[i] == "Min temperature in °C") {
    p <- p + scale_y_reverse(label = function(x)if_else(x == "0", "0", paste0("-", x)))
  }
  
  if(i == 1) p <- p + labs(title = "Temperature")
  if(i == 4) p <- p + labs(title = "Loss of load")
  
  if(i %in% c(1, 2, 4, 5)) {
    p <- p + theme(axis.title.x.bottom = element_blank(),
                   axis.ticks.x.bottom = element_blank(),
                   axis.text.x.bottom = element_blank())
  } 
  if(i %in% c(2, 3, 5, 6)) {
    p <- p + theme(axis.title.x.top = element_blank(),
                   axis.ticks.x.top = element_blank(),
                   axis.text.x.top = element_blank())
  }
  
  plots[[ss$label[i]]] <- p
}

pp <- plot_grid(plotlist = plots, align = "v", nrow = 3, byrow = F,
                rel_heights = c(1.32, 1, 1))

plot(pp)
ggsave("../../data/figures/quantile-plot_LR24temptrend_Hook-8_+1.5GW.pdf", plot = pp, width = 122, height = 110, units = "mm")




# ev.rawdata <- ss %>%
#   select(variable, name, label, observations) %>%
#   unnest(observations)
# 
# ev.rawdata %>%
#   write_rds(
#     file = "../../data/cache/ev-series-rawdata-cached.rds", 
#     compress = "gz"
#   )


# return periods
ss %>% 
  ungroup() %>%
  transmute(
    label, 
    rp.theo = round(rp.theo, 1), 
    rp.emp = round(1 / (1 - prob2021), 1)
  )  






