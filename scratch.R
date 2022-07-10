# 'scratch'
library(tidyverse)
library(rms)
library(plotly)
library(fable)
library(tsibble)
library(tsibbledata)
library(feasts)
library(urca)
surg <- read.csv("./NHFD_Data/surgery_apr_22.csv")
names(surg) <-c("my", "nail", "nail_annual",
                "sliding_hs", "sliding_hs_annual",
                "cemented_arthro",  "cemented_arthro_annual",
                "THR", "THR_annual",
                "sliding_a1a2", "sliding_a1a2_annual")
surg <- surg %>% select(my, nail, cemented_arthro, sliding_hs, THR)
surg$my <- lubridate::my(surg$my)
ts_surg <- surg %>% mutate(my = tsibble::yearmonth(my)) %>% tsibble()

surg_long <- pivot_longer(surg, cols=c(2:5))
names(surg_long) <- c("my", "procedure", "number")
p <- surg_long |> ggplot(aes(x=my, y=number, fill=procedure)) +
  geom_bar(position="stack", stat="identity")
p
  labs(x="Month-Year of discharge",
       title="Operative procedure of patients presenting to Wythenshawe Hospital with #NOF by month",
       subtitle = "Data courtesy of the National Hip Fracture Database", y="Length of stay")
p

fable_2 <- ts_acute_los %>% model(
  ets = ETS(acutelos),
  arima = ARIMA(acutelos),
  snaive = SNAIVE(acutelos)
)
forecast_2 <- forecast(fable_2, h = "3 years")
autoplot(forecast_2, data=acute_los, level=c(80,95)) +
  labs(x="Month-Year of discharge",
       title="ARIMA/ETS/SNAIVE models predicting LOS of patients presenting to
              Wythenshawe Hospital with #NOF",
       subtitle = "Data courtesy of the National Hip Fracture Database",
       y="Number of patients")

confint_f1 <- tapply(forecast_2 , FUN=hilo())

hilo(forecast_2[[3]][[1]], 80)

arima_means <- forecast_2 %>% filter(.model=='arima')
arima_mean <- mean(arima_means$.mean)
ets_means <- forecast_2 %>% filter(.model=='ets')
ets_mean <- mean(ets_means$.mean)
snaive_means <- forecast_2 %>% filter(.model=='snaive')
snaive_mean <- mean(snaive_means$.mean)
print_mean_2 <- mean(c(snaive_mean, ets_mean, arima_mean)) |> round(2)
