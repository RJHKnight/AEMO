source("Utils.R")
library(lubridate)
library(tidyverse)

update <- function(input)
{
  this_year <- year(today())
  this_month <- month(today())
  years  <- seq(START_DATE, 2020, by =1)
  months <- 1:12
  
  perms <- data.frame(expand.grid(state = states, year = years, month = months, stringsAsFactors = FALSE), stringsAsFactors = FALSE)
  perms <- filter(perms, year > this_year-3) %>% 
    filter(!(year == this_year & month > this_month)) %>% 
    mutate(month = get_month_format(month))
  
  # Find missing data from previous months
  input <- mutate(input, key = paste0(year(date), "-", get_month_format(month(date))))
  match <- data.frame(region = perms$state, key = paste0(perms$year, "-", perms$month))
  missing <- anti_join(match, input, by = c("region", "key"))
  input <- select(input, -key)
  
  if (nrow(missing) > 0)
  {
    hist_results <- pmap_dfr(missing, get_one_file)
    hist_results <- standardise(hist_results)
    input <- rbind(input, hist_results)
  }
  
  # Rerun this month
  perms <- data.frame(expand.grid(state = states, year = this_year, month = get_month_format(this_month), stringsAsFactors = FALSE), stringsAsFactors = FALSE)
  this_month_data <- pmap_dfr(perms, get_one_file)
  this_month_data <- standardise(this_month_data)
  
  # And merge
  input <- filter(input, !(year(date) == this_year & month(date) == this_month))
  
  input <- rbind(input, this_month_data)

  return (input)
}