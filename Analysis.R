# Time series plot
library(tsibble)
library(fable)
library(tidyverse)
library(lubridate)
library(feasts)



# Create tsibble ----------------------------------------------------------

res_daily <- results %>% 
  mutate(date = as.Date(date)) %>% 
  filter(date < today()) %>% 
  group_by(date, region) %>% 
  summarise(demand = sum(demand))

res_daily <- as_tsibble(res_daily, index = date, key = region) 

res_daily %>% 
  autoplot(demand)


# Simple model ------------------------------------------------------------

fit <- res_daily %>% 
  fill_gaps() %>% 
  model(
    snaive = SNAIVE(demand ~ lag("year")),
    ets = ETS(demand),
    arima = ARIMA(demand)
  )

fc <- fit %>%
  forecast(h = 12)

fc %>%
  autoplot(res_daily, level = NULL) 


 
# Decomposition -----------------------------------------------------------

stl_decomp <- res_daily %>% 
  model(STL(demand)) %>%
  components() 

stl_decomp_recent <- filter_index(stl_decomp, "2019.01.01" ~ .)
                
stl_decomp_recent %>% 
  gather(component, demand, -region, -.model, -date) %>% 
  ggplot(., aes(date, demand, colour = region)) + 
  geom_line() + 
  facet_wrap(~ component, ncol = 1, scales = "free_y")

