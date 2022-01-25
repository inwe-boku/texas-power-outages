summarise_event <- function(.data, ...,  default_unit = 1, 
                            values_from = "value", times_from = "time", 
                            threshold_from = "threshold", reverse_sign = FALSE)
{
  # setting default units, if not present
  if (!is.na(default_unit)) {
    .data <- .data %>%
      mutate(
        across(
          .cols = where(~(!inherits(.x, what = "units"))) & any_of(c(values_from, threshold_from)),
          .fns = ~set_units(.x, !!default_unit)
        )
      )
  }
  
  # compute duration for every observation before grouping
  # this approach is slow, but resilient against irregular time series
  .data %>%
    mutate( 
      duration = as.double(lead(time) - time, unit = "hours"),
      duration = set_units(duration, h),
      delta = .data[[values_from]] - .data[[threshold_from]],
      delta = delta * if_else(reverse_sign, -1, 1),
      state = if_else(sign(delta) >= 0, "above", "below"),
      event = cumsum(state != dplyr::lag(state, default = state[1])) + 1,
      area = delta * duration,
    ) %>%
    group_by(event, state) %>%
    summarise(data = list(mutate(cur_data(), event.orig = event)),
              duration = sum(duration),
              area = sum(area),
              onset = first(time), 
              termination = last(time),
              ..., 
              .groups = "drop") %>%
    relocate(data, .after = last_col())
}


pool_events <- function(.data, events = c("above", "below"), 
                        min.duration = set_units(2, d), ...) 
{
  events <-  match.arg(events)
  
  tbl <- .data %>%
    mutate(
      toPool = duration < min.duration & state != events,
      event.pooled = state == events | toPool | dplyr::lag(toPool, default = FALSE) | dplyr::lead(toPool, default = FALSE), 
      event.pooled = cumsum(event.pooled != dplyr::lag(event.pooled, default = event.pooled[1])) + 1
    ) 
  
  standardColnames <- c("event", "state", "toPool", "event.pooled")
  
  extra_funs <- enexprs(...)
  
  defaults <- exprs(
    duration = sum(duration), 
    area = sum(area), 
    onset = min(onset), termination = max(termination),
    data = list(bind_rows(data))
  ) 
  
  defaults <- defaults[!names(defaults) %in% names(extra_funs)]
  
  handeled <- c(names(extra_funs), names(defaults), standardColnames)
  default_sum <- setdiff(colnames(tbl), handeled)
  if(length(default_sum)) {
    default_sum <- parse_exprs(set_names(x = paste0("sum(", default_sum, ")"), nm = default_sum))
    defaults <- c(defaults, default_sum)
  }
  
  if (length(defaults)) {
    txt <- c("Using the following default aggregation functions for pooling:", 
             glue::glue("\t{names(defaults)} = {defaults},"))
    message(paste0(txt, "\n"))
  }
  agg_funs <- c(defaults, extra_funs)
  
  tbl %>% 
    filter(state == events) %>%
    group_by(event = event.pooled) %>%
    summarise(!!!agg_funs, .groups = "drop") %>%
    mutate(
      event = row_number()
    ) %>%
    relocate(data, .after = last_col())
}


# noch anpassen auf boot::boot() und boot::boot.ci() für
# bias corrected ci 
bootstrap <- function(loss, n = 30, record.length =  2021 - 1950 + 1, 
                      bootstraps = 100000)
{
  nevents <- length(loss)
  noevents <- record.length - nevents
  
  # mit Nullen auffüllen für Jahre ohne Event
  series <- c(loss, rep(0 * loss[1], noevents))
  
  m <- sample(series, size = bootstraps, replace = TRUE)
  
  sum30 <- function() sum(sample(series, size = n, replace = TRUE))
  sum30 <- function() sum(sample(series, size = n, replace = TRUE))
  s <- replicate(bootstraps, sum30())
  s <- set_units(s, units(loss), mode = "standard")
  
  
  tibble(
    mean = mean(m), 
    mean.lwr = quantile(m, probs = c(0.025)), 
    mean.upr = quantile(m, probs = c(0.975)),
    cum = mean(s), 
    cum.lwr = quantile(s, probs = c(0.025)),
    cum.upr = quantile(s, probs = c(0.975)),
    prob.noevent = (noevents / record.length)^n
  )
}



# n <- 60
# x <- tibble(
#   time = seq(as.POSIXct("2010-01-01"), length.out = n, by = "1 hour"), 
#   value = set_units(sin(seq_len(n) / n * 4 * pi), "W")
# ) 
# 
# 
# x %>%
#   mutate(
#     threshold = set_units(0.8, W)
#   ) %>%
#   summarise_event() %>%
#   pool_events()






fit_metric <- function(tbl, metric, highlight = 2021, nyears = 2021 - 1950 + 1, ...) 
{
  label <- paste(metric, "in", deparse_unit(tbl[[metric]]))
  
  # Serie extrahieren und mit Nullen auffüllen
  x <- drop_units(tbl[[metric]])
  series <- c(x, rep(0, nyears - length(x)))
  
  x.evfit <- evfit(series, distribution = c("gev"), extreme = "maximum")
  plot(x.evfit, ylab = label, ...)
  
  
  highlight <- tbl %>%
    filter(year == !!highlight)
  value <- drop_units(highlight[[metric]])
  
  # freq <- cdfgev(x = value, para = x.evfit$parameters$gev)
  rp <- ev_return_period(x = value, fit = x.evfit)
  rpline(x.evfit, return.period = rp)
  
  tibble(
    metric = metric, 
    value = value, 
    freq = 1 - 1/rp, 
    return.period = rp
  )
}

# damit map() auch für difftime und POSIX Objekte funktioniert
map_other <- function(.x, .f, ...)
{
  reduce(map(.x, .f, ...), c)
}


ppure2pmixed <- function(p, freq.zeros) (1 - freq.zeros) * p + freq.zeros
pmixed2ppure <- function(p.adj, freq.zeros) (p.adj - freq.zeros) / (1- freq.zeros)

prob_empirical <- function(x, freq.zeros = 0, plotting.position = gringorten, 
                           lower.tail = TRUE)
{
  p <- plotting.position(x)
  if (!lower.tail) p <- 1 - p
  p.adj <- ppure2pmixed(p, freq.zeros = freq.zeros)
  #(1 - freq.zeros) * p + freq.zeros
  
  return(p.adj) 
}



rp_trans <- scales::trans_new(
  name = "rp",
  transform = function(prob) 1/(1 - prob),
  inverse = function(rp) 1 - 1/rp, 
  # The breaks function is applied to the raw data.
  breaks = function(x) 1- 1/c(1, 2, 5, 10, 20, 50, 100, 200),
  domain = c(0, 1)
)


rv_trans <- scales::trans_new(
  name = "reducedVariate",
  transform = function(x) -log(-log(x)), 
  inverse = function(x) exp(-exp(-x)), 
  # breaks = function(x) c(0.01, 0.1, 0.2, 0.4, 0.6, 0.7, 0.8, 0.9, 0.95, 0.98, 0.99),
  breaks = function(x) c(0.01, 0.1, 0.5, 0.8, 0.9, 0.95, 0.99), 
  domain = c(0, 1)
)

sec_axis_rp <- sec_axis(trans =  ~(1 / (1 - .)),
                        name = "Return period in years",
                        breaks = c(1.5, 2, 5, 10, 20, 50, 100, 200), 
                        labels = function(x) gsub("(\\.[^0]+)0+", "\\1", x))




obs2empirical <- function(x, year = 1950:2021)
{
  
  x %>%
    full_join(tibble(year = year), by = "year") %>%
    replace_na(list(value = 0)) %>%
    mutate(
      prob = gringorten(value)
    ) %>%
    arrange(desc(prob))
  
  
}

fit_maximum <- function(x) {
  suppressWarnings(evfit(x, distribution = c("gev"), extreme = "maximum"))
}


quant <- function(fit, prob = exp(-exp(-seq(-2.2, 5.2, 0.01))), 
                  rp = 1 /(1 - prob)) {
  
  e <- evquantile(fit, return.period = rp)$T_Years_Event
  
  tibble(
    rp = as.numeric(rownames(e)),
    prob = 1 - 1 / rp,
    value = e[, "gev"]
  ) %>%
    filter(value >= 0)
}

six_hours <- function(x, label = c(0, 12)) 
{
  s <- seq(x[1], x[2], by = "1 hour")
  s[hour(s) %in% label]
}

