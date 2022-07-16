# 'scratch'
library(tidyverse)
library(rms)
library(plotly)
library(fable)
library(tsibble)
library(tsibbledata)
library(feasts)
library(urca)
delirium <- read.csv("./NHFD_Data/delirium_postop.csv")
names(delirium) <- c("my", "delirious")
delirium$my <- lubridate::my(delirium$my)
ts_delirium <- delirium %>% mutate(my = tsibble::yearmonth(my)) %>% tsibble()

p <- delirium |> ggplot() + geom_line(aes(x=my, y=delirious)) +
  labs(x="Month-Year of discharge",
       title="Percentage of patients delirium free postoperatively",
       subtitle = "Data courtesy of the NHFD", y="Delirium-free (4AT) (%)")
p


fable_5 <- ts_delirium%>% model(
  ets = ETS(delirious ~ error("M") + trend("M") + season("M")),
  arima = ARIMA(delirious ~ 0 + pdq(1, 1, 1) + PDQ(1, 1, 1)),
  prophet = prophet(delirious ~ growth("linear") +
                      season("year",
                             type = "multiplicative"))
)
forecast_5 <- forecast(fable_5, h = "3 years")
autoplot(forecast_5, data=delirium, level=c(80,95)) +
  labs(x="Month-Year of discharge",
       title="ARIMA/ETS/Prophet models predicting \n percentage of pts delirium free postop",
       subtitle = "Data courtesy of the NHFD", y="Delirium-free (4AT) (%)")


arima_means <- forecast_5 %>% filter(.model=='arima')
arima_mean <- mean(arima_means$.mean)
ets_means <- forecast_5 %>% filter(.model=='ets')
ets_mean <- mean(ets_means$.mean)
prophet_means <- forecast_5 %>%
  filter(.model=='prophet')
prophet_mean <- mean(prophet_means$.mean)
print_mean_5 <- mean(c(ets_mean,
                       arima_mean,
                       prophet_mean)) |> round(2)
print(print_mean_5)


#--------------------------------------------------------------
# Create a population of patients from which to sample
# 80% of them score 0 on the ordinal rating scale
# The other 20% are distributed as a half-normal with a ceiling
## really need to investigate this and see how severity is in fact distributed
##



