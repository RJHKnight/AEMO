URL_STEM <- "https://aemo.com.au/aemo/data/nem/priceanddemand/PRICE_AND_DEMAND"
URL_SEP <- "_"
URL_SUFFIX <- "1.csv"

START_DATE <- 1998

states <- c("NSW", "QLD", "VIC", "SA", "TAS")

get_one_file <- function(state, year, month)
{
  cat(paste("Running for", state, year, month, "\n"))
  url <- paste0(URL_STEM, URL_SEP, year, month, URL_SEP, state, URL_SUFFIX)
  
  try(
    this_file <- read_csv(file = url,
                          col_types = cols(PERIODTYPE = col_character(), 
                                           REGION = col_character(), RRP = col_double(), 
                                           SETTLEMENTDATE = col_character(), 
                                           TOTALDEMAND = col_double()))
  )
  
  if (!exists("this_file"))
    return (NULL)
  
  return (this_file)
}

standardise <- function(results)
{
  FULL_DATE <- "\\d{4}/\\d{2}/\\d{2} \\d{2}:\\d{2}:\\d{2}"
  
  # Standardise date/time format
  results %>% 
    mutate(SETTLEMENTDATE = if_else(str_detect(SETTLEMENTDATE, FULL_DATE),
                                    SETTLEMENTDATE,
                                    paste0(SETTLEMENTDATE, ":00"))) %>% 
    mutate(SETTLEMENTDATE = ymd_hms(SETTLEMENTDATE)) %>% 
    rename(
      region = REGION,
      date = SETTLEMENTDATE,
      demand = TOTALDEMAND,
      price = RRP,
      type = PERIODTYPE
    ) %>% 
    mutate(region = str_remove(region, "1"))
}


get_month_format <- function(input)
{
  formatC(input, width = 2, format = "d", flag = "0")
}