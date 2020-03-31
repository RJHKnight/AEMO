library(tidyverse)
library(lubridate)

source("Utils.R")

years  <- seq(START_DATE, 2020, by =1)
months <- get_month_format(1:12)

perms <- expand.grid(state = states, year = years, month = months, stringsAsFactors = FALSE)

results <- pmap_dfr(perms, get_one_file)

results <- standardise(results)