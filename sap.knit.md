---
title: "AnTIDOTe RCT \n statistical analysis plan"
subtitle: "Draft v0.1"
author: "Ross Dunne"
format: 
  html:
    code-fold: true
    code-summary: "Show the code"
    toc: true
    toc-depth: 3
    toc-title: "Contents"
    toc-location: left
    page-layout: article
editor: source
reference-location: margin
citation-location: margin
body-footer: "Copyright (c) 2022"
---

## Part I: Sample size calculations for a randomised controlled trial in delirium {.unnumbered .unlisted}

## Project Title: ANakinra for the Treatment of Inflammation and Delirium after Orthopaedic Trauma and rEpair (ANTIDOTE) -- Randomised Controlled Trial. {.unnumbered .unlisted}

## PICOT

**(P)** -- Patients presenting to Emergency Departments in South Manchester with radiologically confirmed hip fracture who are aged 65 and over and have none of the following exclusion criteria:

1) Lack of a study partner or caregiver 

2) Active infection 

3) History of hypersensitivity to or unacceptable side effects of Anakinra 

4) Current substance misuse 

5) Active malignancy 

6) Acute stroke or TIA 

7) Currently participating in clinical trial of an investigational medicinal product (CTIMP) 

8) Women of child-bearing potential 

9) Neutropenia of any cause.

**(I)** -- Anakinra given subcutaneously at time 0 \[presentation to ED\] then bd for 48 hours during the perioperative period.

**(C)** -- Normal saline given subcutaneously at time 0 \[presentation to ED\] then bd for 48 hours during the perioperative period.

**(O)** -- 1°) Severity of delirium postoperatively as measured by the XXXXXXXX 2) (2°) Time to first weight bearing postoperatively 3) (3°) Time until medically fit for discharge postoperatively 4) (3°) Safety of Anakinra during the perioperative period and in wound healing 5) (3°) Tolerability of Anakinra versus placebo in terms of common side effects 6) (3°) Effect of Anakinra on 30-day mortality

**(T)** -- Recruitment period: 24 months; Treatment/assessment period: to 48hrs postop, measured from surgical wound closure.

## Descriptive statistics
### Number of patients 

::: {.cell .column-screen-inset-shaded layout-nrow="1"}

```{.r .cell-code}
# Descriptive statistics and forecasting
# for the Hip fracture population
library(tidyverse)
library(rms)
library(fable)
library(tsibble)
library(feasts)
library(urca)
library(downloadthis)
library(fpp3)
los <- read.csv("./NHFD_Data/los_apr_22.csv")
names(los) <-c("my", "ptnum", 
               "acutelos", 
               "acutelosannual", 
               "trustlos", 
               "trustlosannual" )
vol <- los |> select(my, ptnum)
vol$my <- lubridate::my(vol$my) 
ts_vol <- vol %>% 
  mutate(my = tsibble::yearmonth(my)) %>% 
  tsibble()

p <- vol |> ggplot() + 
  geom_line(aes(x=my, y=ptnum)) +
  scale_x_date(date_labels = "%m-%Y") +
  labs(x="Month-Year of discharge", 
  y="Number of patients",
  title="Patients presenting to Wythenshawe with #NOF", 
  subtitle ="Data courtesy of the NFHD")
p
```

::: {.cell-output-display}
![](sap_files/figure-html/unnamed-chunk-1-1.png){width=672}
:::

```{.r .cell-code}
fable_1 <- ts_vol %>% 
  model(  
    ets = ETS(ptnum ~ error("M") + trend("M") + season("M")),
    arima = ARIMA(ptnum ~ 0 + pdq(1, 1, 1) + PDQ(1, 1, 1)),
    snaive = SNAIVE(ptnum)
  )
forecast_1 <- forecast(fable_1, 
                       h = "3 years") 
autoplot(forecast_1, 
         data=vol, 
         level = c(80, 95)) +
  labs(x="Month-Year of discharge",
  title="ARIMA/ETS/SNAIVE models predicting pts presenting to  
  Wythenshawe with #NOF",
  subtitle = "Data courtesy of the NFHD",
  y="Number of patients")
```

::: {.cell-output-display}
![](sap_files/figure-html/unnamed-chunk-1-2.png){width=672}
:::

```{.r .cell-code}
arima_means <- forecast_1 %>%
  filter(.model=='arima')
arima_mean <- mean(arima_means$.mean)
ets_means <- forecast_1 %>% 
  filter(.model=='ets')
ets_mean <- mean(ets_means$.mean)
snaive_means <- forecast_1 %>% 
  filter(.model=='snaive')
snaive_mean <- mean(snaive_means$.mean)
print_mean <- mean(c(snaive_mean, 
                     ets_mean, 
                     arima_mean)) |> 
  round(2)
```
:::

::: {.cell}
::: {.cell-output-display}
```{=html}
<a onclick="fetch(&#39;data:text/csv;base64,VHJ1c3QuRGlzY2hhcmdlLlllYXIuLi5Nb250aDtQYXRpZW50cy4ubnVtYmVyLnBlci5tb250aC47QWN1dGUuaG9zcGl0YWwubGVuZ3RoLm9mLnN0YXkuLmRheXMuO0FjdXRlLmhvc3BpdGFsLmxlbmd0aC5vZi5zdGF5Li5kYXlzLi5hbm51YWwuO092ZXJhbGwuVHJ1c3QubGVuZ3RoLm9mLnN0YXkuLmRheXMuO092ZXJhbGwuVHJ1c3QubGVuZ3RoLm9mLnN0YXkuLmRheXMuLmFubnVhbC4KQXByIDIwMTE7OTs5LDU3O05BOzksMzc7TkEKTWF5IDIwMTE7MjU7MTMsMTQ7TkE7MjAsMDg7TkEKSnVuIDIwMTE7MjE7MTYsMDY7TkE7MjMsNjI7TkEKSnVsIDIwMTE7Mjk7MTcsMjM7TkE7MjgsNTE7TkEKQXVnIDIwMTE7MjI7MTcsNTA7TkE7MjMsNTM7TkEKU2VwIDIwMTE7MjQ7MjAsNTI7TkE7MzAsMzA7TkEKT2N0IDIwMTE7MjE7MjIsNjA7TkE7MjksMjU7TkEKTm92IDIwMTE7MTc7MTgsNzk7TkE7MjYsMDM7TkEKRGVjIDIwMTE7MzU7MjUsMjU7TkE7MjYsNTY7TkEKSmFuIDIwMTI7MjA7MTcsOTU7TkE7MjMsNjk7TkEKRmViIDIwMTI7MjQ7MTksMjI7TkE7MjYsNzc7TkEKTWFyIDIwMTI7MTM7MjksMTg7MTksMjU7MzMsNDA7MjUsNzMKQXByIDIwMTI7MTM7MzEsNTA7MjAsMDc7MzEsMTA7MjYsNTYKTWF5IDIwMTI7MjU7MTYsMTc7MjAsMzE7MjQsMTc7MjYsOTQKSnVuIDIwMTI7Mjg7MjIsMDA7MjAsODA7MjksMjc7MjcsNDQKSnVsIDIwMTI7MjE7MTksNDI7MjEsMDg7MjYsNDc7MjcsMjUKQXVnIDIwMTI7MTc7MTgsMjA7MjEsMjE7MjcsMDU7MjcsNTUKU2VwIDIwMTI7MjM7MjAsNTM7MjEsMjE7MzEsODM7MjcsNjgKT2N0IDIwMTI7MjI7MTYsNTM7MjAsNzA7MjYsMzY7MjcsNDQKTm92IDIwMTI7MjY7MTYsMjk7MjAsMzc7MjIsMDY7MjcsMDAKRGVjIDIwMTI7Mjc7MjEsNjI7MTksOTI7MjQsMjA7MjYsNzcKSmFuIDIwMTM7MzM7MjMsOTA7MjAsNTU7MjksODk7MjcsMzgKRmViIDIwMTM7MzE7MjQsMzE7MjEsMTE7MzYsMTI7MjgsNDAKTWFyIDIwMTM7MjA7MjYsNjU7MjEsMjA7MzgsNDM7MjgsODcKQXByIDIwMTM7Mjc7MTksODg7MjAsNzA7MzYsMDQ7MjksNDIKTWF5IDIwMTM7MjA7MTMsNjA7MjAsNTY7MTgsMzM7MjksMTIKSnVuIDIwMTM7MjI7MTQsODE7MTksOTU7MjAsNDI7MjgsNDQKSnVsIDIwMTM7MjQ7MTgsNjc7MTksODc7MjUsNzQ7MjgsMzYKQXVnIDIwMTM7MTg7MjgsMDA7MjAsNTA7MzIsMTc7MjgsNjcKU2VwIDIwMTM7Mjg7MjMsMjM7MjAsNzY7MjgsOTI7MjgsNDUKT2N0IDIwMTM7MjE7MjIsNTA7MjEsMTc7MzAsNzQ7MjgsNzYKTm92IDIwMTM7MjQ7MTcsNTk7MjEsMzE7MjAsMDI7MjgsNjQKRGVjIDIwMTM7Mjk7MTksMDQ7MjEsMDg7MjIsNDM7MjgsNDQKSmFuIDIwMTQ7MzM7MjQsMjk7MjEsMTQ7MzIsNDg7MjgsNzMKRmViIDIwMTQ7MjI7MjYsOTU7MjEsMjU7MzgsMjY7MjgsNjYKTWFyIDIwMTQ7MzI7MjIsNTI7MjEsMDE7MzgsMTI7MjksMDIKQXByIDIwMTQ7MjQ7MTgsMzU7MjAsODk7MjUsMjI7MjgsMDcKTWF5IDIwMTQ7MjM7MTUsNDM7MjAsOTY7MjIsMDc7MjgsMjYKSnVuIDIwMTQ7Mjc7MjcsNTY7MjIsMDI7MzMsOTQ7MjksMzMKSnVsIDIwMTQ7Mjg7MjAsNzU7MjIsMTc7MjcsODc7MjksNDgKQXVnIDIwMTQ7MjQ7MjAsMjA7MjEsNjg7MjUsNzg7MjksMDQKU2VwIDIwMTQ7Mjk7MjQsNjU7MjEsODE7MjYsMzA7MjgsODAKT2N0IDIwMTQ7MzE7MjYsMzc7MjIsMjE7MzQsNDQ7MjksMjEKTm92IDIwMTQ7MjU7MjAsOTI7MjIsNDM7MjQsNzY7MjksNTUKRGVjIDIwMTQ7NDA7MjgsMTc7MjMsNDA7MzMsNzI7MzAsNjUKSmFuIDIwMTU7MzM7MTgsODA7MjIsODk7MjQsNjE7MjksODgKRmViIDIwMTU7MzE7MjMsNzY7MjIsNzE7MjcsMjY7MjksMTIKTWFyIDIwMTU7MzE7MTcsMTk7MjIsMjE7MjQsNzQ7MjcsODkKQXByIDIwMTU7MzE7MTcsOTc7MjIsMTA7MjcsMDE7MjgsMDAKTWF5IDIwMTU7MjY7MTgsMDA7MjIsMjU7MjEsODY7MjcsOTMKSnVuIDIwMTU7MzY7MTcsODI7MjEsNDA7MjgsODY7MjcsNTgKSnVsIDIwMTU7MzE7MjMsMTc7MjEsNjA7MjgsMTc7MjcsNjEKQXVnIDIwMTU7MjI7MjEsMTQ7MjEsNjU7MjQsMTg7MjcsNTIKU2VwIDIwMTU7Mjk7MjIsMDg7MjEsNDY7MjEsOTk7MjcsMTgKT2N0IDIwMTU7Mjc7MTksMzg7MjAsODg7MTksNjg7MjYsMDAKTm92IDIwMTU7MjA7MTQsNzg7MjAsNTY7MTYsNjg7MjUsNTYKRGVjIDIwMTU7MzY7MjYsMzk7MjAsMjI7MjYsMDA7MjQsNjgKSmFuIDIwMTY7MzA7MjIsMTc7MjAsNTI7MjIsNjQ7MjQsNTEKRmViIDIwMTY7MTc7MjQsMjU7MjAsNDI7MjMsNzM7MjQsMjIKTWFyIDIwMTY7MzI7MTgsMjU7MjAsNTE7MjAsMTY7MjMsNzkKQXByIDIwMTY7MTY7MTYsOTQ7MjAsNTc7MjIsNjI7MjMsNDIKTWF5IDIwMTY7MzQ7MjUsMDA7MjEsMjU7MjcsNTU7MjMsOTYKSnVuIDIwMTY7NDA7MjMsNzE7MjEsOTE7MjUsNDA7MjMsNjAKSnVsIDIwMTY7MjY7MjQsMzI7MjEsOTg7MjMsNjI7MjMsMTcKQXVnIDIwMTY7MzE7MjcsMzA7MjIsNTM7MjcsMzA7MjMsNDcKU2VwIDIwMTY7MjA7MjgsMDU7MjIsOTA7MjcsMjU7MjMsODMKT2N0IDIwMTY7MjI7MjUsMjM7MjMsMzY7MjUsNzc7MjQsMzEKTm92IDIwMTY7Mzc7MjgsMDY7MjQsMzQ7MjcsNzA7MjUsMTMKRGVjIDIwMTY7MjQ7MjcsNjc7MjQsMzc7MjksOTI7MjUsMzkKSmFuIDIwMTc7MzE7MjMsOTA7MjQsNTM7MjcsOTc7MjUsODkKRmViIDIwMTc7MjY7MjIsNjA7MjQsNDA7MjIsMTI7MjUsNzAKTWFyIDIwMTc7Mzc7MjksMzc7MjUsNTE7MzAsNjU7MjYsNzYKQXByIDIwMTc7Mjc7MjcsMjM7MjYsMDM7MjksNTY7MjcsMTYKTWF5IDIwMTc7MzE7MjksMjg7MjYsNDE7MzIsNjU7MjcsNjEKSnVuIDIwMTc7Mjg7MjcsNDM7MjYsODE7MjcsNjQ7MjcsODcKSnVsIDIwMTc7MjQ7MjQsNjI7MjYsODQ7MzAsMjU7MjgsMzcKQXVnIDIwMTc7MzU7MjUsODg7MjYsNzA7MzIsMTQ7MjgsODUKU2VwIDIwMTc7MjM7MjYsOTU7MjYsNjQ7MzMsODc7MjksMjgKT2N0IDIwMTc7MzQ7MjIsOTc7MjYsMzg7MjYsNzk7MjksMjUKTm92IDIwMTc7MzQ7MjEsMTU7MjUsNjk7MjQsNTA7MjgsOTYKRGVjIDIwMTc7NDM7MjAsMTA7MjQsOTE7MjIsNzA7MjgsMTgKSmFuIDIwMTg7MzM7MTMsOTQ7MjQsMDU7MTksMTU7MjcsNDAKRmViIDIwMTg7Mjc7MjQsMzc7MjQsMTc7MjQsMzc7MjcsNTUKTWFyIDIwMTg7MzM7MTcsMjg7MjMsMDY7MjQsNzM7MjYsOTkKQXByIDIwMTg7MzA7MjEsNTQ7MjIsNjQ7MjMsNzc7MjYsNTUKTWF5IDIwMTg7MzQ7MjksNTk7MjIsNzY7MzQsMzg7MjYsNzUKSnVuIDIwMTg7MzA7MjIsNzA7MjIsNDA7MjIsNzA7MjYsMzcKSnVsIDIwMTg7Mjg7MjIsMzk7MjIsMjU7MjMsMDQ7MjUsODgKQXVnIDIwMTg7Mzc7MTksMzg7MjEsNjY7MjAsNDY7MjQsNzkKU2VwIDIwMTg7Mjk7MTgsODM7MjEsMTU7MjAsMDA7MjMsOTEKT2N0IDIwMTg7Mjk7MjEsMTQ7MjAsOTk7MjEsMTQ7MjMsNDQKTm92IDIwMTg7MzQ7MjQsMjY7MjEsMjc7MjQsMjY7MjMsNDIKRGVjIDIwMTg7MjA7MTQsNjU7MjEsMDQ7MTUsNzU7MjMsMDkKSmFuIDIwMTk7MzQ7MTgsMzU7MjEsNDA7MjAsNTY7MjMsMjEKRmViIDIwMTk7MjI7MTUsODI7MjAsODM7MjAsMjc7MjIsOTQKTWFyIDIwMTk7MjE7MTksMTA7MjEsMDU7MzAsOTA7MjMsMjUKQXByIDIwMTk7MzQ7MTcsMzU7MjAsNjY7MjIsNDQ7MjMsMTMKTWF5IDIwMTk7MzY7MjIsMDA7MTksOTQ7MjYsNTg7MjIsNDAKSnVuIDIwMTk7MzI7MTcsMDM7MTksNDQ7MjYsMTI7MjIsNzEKSnVsIDIwMTk7MzQ7MTUsMjQ7MTgsODI7MjMsMDk7MjIsNzIKQXVnIDIwMTk7Mjg7MTYsMzI7MTgsNTY7MjUsMTE7MjMsMTUKU2VwIDIwMTk7Mjg7MTYsOTY7MTgsNDE7MjgsMzM7MjMsODEKT2N0IDIwMTk7MzI7MjIsNDQ7MTgsNTU7MjksMjI7MjQsNTEKTm92IDIwMTk7MzU7MTgsNjM7MTgsMDE7MjYsNjA7MjQsNzQKRGVjIDIwMTk7MzU7MTcsNDE7MTgsMTQ7MjksNzk7MjUsNzAKSmFuIDIwMjA7Mzc7MTcsNzA7MTgsMDg7MzUsODQ7MjcsMTcKRmViIDIwMjA7MjU7MTksNTI7MTgsMzE7MzgsMzI7MjgsMzIKTWFyIDIwMjA7NDE7MTksOTM7MTgsNDM7MzksNzE7MjksMzcKQXByIDIwMjA7MzQ7MTYsNjY7MTgsMzg7MjksMDY7MjksOTQKTWF5IDIwMjA7MjY7MTUsNjE7MTcsODc7MjUsOTY7MjksOTgKSnVuIDIwMjA7MjY7MTMsNDg7MTcsNjU7MjEsODE7MjksNzUKSnVsIDIwMjA7MjA7OSwzNzsxNyw0NDsxNSwwNTsyOSw1NgpBdWcgMjAyMDsyNjsxNCwxMjsxNywyOTsyNyw4MTsyOSw3OApTZXAgMjAyMDsxNDsxMywzODsxNywxNzsyNiw4NjsyOSw3NwpPY3QgMjAyMDsyODsxNiw4NTsxNiw2NTsyNCw5MzsyOSw0MwpOb3YgMjAyMDszNDsxMSwzNTsxNSw5MDsyNCwyNjsyOSwyMQpEZWMgMjAyMDs0MjsxMSwxMjsxNSwxNzsyMywxNzsyOCw0NApKYW4gMjAyMTs1MjsxMSw2NTsxNCw0MjsyNiw5MjsyNyw0OApGZWIgMjAyMTsyOTsxMCw0MjsxMyw3NzsyNyw2OTsyNiw3NwpNYXIgMjAyMTs0NDs5LDYzOzEyLDU3OzIxLDc1OzI0LDc2CkFwciAyMDIxOzM4OzE1LDQyOzEyLDUwOzMyLDUwOzI1LDE1Ck1heSAyMDIxOzM0OzEwLDUzOzEyLDEzOzMxLDkxOzI1LDY5Ckp1biAyMDIxOzMxOzExLDk3OzEyLDAzOzI4LDgxOzI2LDIwCkp1bCAyMDIxOzQ5OzEyLDAyOzEyLDE1OzMwLDE2OzI3LDE5CkF1ZyAyMDIxOzMyOzEwLDk0OzExLDk0OzMwLDk3OzI3LDQzClNlcCAyMDIxOzMyOzExLDEzOzExLDg0OzI0LDUwOzI3LDI0Ck9jdCAyMDIxOzMyOzksMjg7MTEsMzQ7MjEsODc7MjcsMDAKTm92IDIwMjE7NTI7OSw2NzsxMSwxNjszMiwzMTsyNyw3OQpEZWMgMjAyMTs0NzsxMCw0OTsxMSwwOTsyNSwzNjsyNyw5NgpKYW4gMjAyMjs0ODsxMSw1NjsxMSwwODsyOSwyMTsyOCwyMQpGZWIgMjAyMjs0MDsxMSw4MjsxMSwxNzszMiwxNTsyOCw1NwpNYXIgMjAyMjszMjsxMSwyODsxMSwzMzsyNyw0NzsyOSwxMwpBcHIgMjAyMjs0MjsxMCw5ODsxMCw5NjsyOCw2MjsyOCw4MgpNYXkgMjAyMjszOTsxMyw2MjsxMSwyMDsyOSw4NzsyOCw2OAo=&#39;).then(res =&gt; res.blob()).then(blob =&gt; {&#10;      const downloadURL = window.URL.createObjectURL(blob);&#10;      const a = document.createElement(&#39;a&#39;);&#10;      document.body.appendChild(a);&#10;      a.href = downloadURL;&#10;      a.download = &#39;presenting_population.csv&#39;; a.click();&#10;      window.URL.revokeObjectURL(downloadURL);&#10;      document.body.removeChild(a);&#10;    });">
<button class="btn btn-warning" has_icon="TRUE"><i class="fa fa-save"></i> Download data</button>
</a>
```
:::
:::


We conducted a time series forecast (ARIMA, ETS and Naive (SNAIVE)) based on discharges from Wythenshawe over the period 2011 - 2022. This suggests an expected mean number of presentations per month (simple average of 3 model estimates) of 41.28. At this rate, we would expect ~41.28 x 24 = 990.72 patients to have surgical repair of fractured neck of femur over the 24 month recruitment course of the trial.  

### Length of stay
::: {.cell .column-screen-inset-shaded layout-nrow="1"}

```{.r .cell-code}
names(los) <-c("my", "ptnum", 
               "acutelos", "acutelosannual", 
               "trustlos", "trustlosannual" )
acute_los <- los %>% select(my, acutelos)
acute_los$my <- lubridate::my(acute_los$my) 
ts_acute_los <- acute_los %>% mutate(my = tsibble::yearmonth(my)) %>% tsibble()

p <- acute_los |> ggplot() + geom_line(aes(x=my, y=acutelos)) +
  labs(x="Month-Year of discharge", 
       title="Length-of-stay (LOS) of pts presenting to Wythenshawe with #NOF by month", 
       subtitle = "Data courtesy of the NHFD", y="Length of stay")
p
```

::: {.cell-output-display}
![](sap_files/figure-html/unnamed-chunk-3-1.png){width=672}
:::

```{.r .cell-code}
fable_2 <- ts_acute_los %>% model(  
  ets = ETS(acutelos ~  error("M") + trend("M") + season("M")),
  arima = ARIMA(acutelos),
  snaive = SNAIVE(acutelos)
)
forecast_2 <- forecast(fable_2, h = "3 years")
autoplot(forecast_2, data=acute_los, level=c(80,95)) +
  labs(x="Month-Year of discharge",
       title="ARIMA/ETS/SNAIVE models predicting LOS of pts presenting to  
              Wythenshawe  with #NOF",
       subtitle = "Data courtesy of the NHFD",
       y="Number of patients")
```

::: {.cell-output-display}
![](sap_files/figure-html/unnamed-chunk-3-2.png){width=672}
:::

```{.r .cell-code}
arima_means <- forecast_2 %>% filter(.model=='arima')
arima_mean <- mean(arima_means$.mean)
ets_means <- forecast_2 %>% filter(.model=='ets')
ets_mean <- mean(ets_means$.mean)
snaive_means <- forecast_2 %>% filter(.model=='snaive')
snaive_mean <- mean(snaive_means$.mean)
print_mean_2 <- mean(c(snaive_mean, ets_mean, arima_mean)) |> round(2)
```
:::
We conducted a further time series forecast (ARIMA, ETS and Naive (SNAIVE)) based on discharges from Wythenshawe over the period 2011 - 2022. This suggests an expected mean length of stay per patient (simple average of 3 model estimates of LOS in days) of 10.88. This means that while it is unlikely for patients to be in hospital longer than 2 weeks, we will have adequate time to give Anakinra for up to 72 hours post treatment, even if surgery is delayed.

### General Anaesthetic
::: {.cell .column-screen-inset-shaded layout-nrow="1"}

```{.r .cell-code}
ana <- read.csv("./NHFD_Data/ana_apr_22.csv")
names(ana)
names(ana) <-c("my", 
               "ga", 
               "ga_ann", 
               "nblock", 
               "nblockann", 
               "spinal",
               "spinalann",
               "blockpre",
               "allNFHDnerveblock")
ana <- ana %>% select(my, ga)
ana$my <- lubridate::my(ana$my) 
ts_ana <- ana %>% mutate(my = tsibble::yearmonth(my)) %>% tsibble()

p <- ana |> ggplot() + geom_line(aes(x=my, y=ga)) +
  labs(x="Month-Year of discharge", 
       title="#NOF pts discharged from Wythenshawe after GA", 
       subtitle = "Data courtesy of the NHFD", y="General Anaesthetic (%)")
p
```

::: {.cell-output-display}
![](sap_files/figure-html/unnamed-chunk-4-1.png){width=672}
:::

```{.r .cell-code}
fable_3 <- ts_ana %>% model(  
  ets = ETS(ga ~ error("M") + trend("M") + season("M")),
  arima = ARIMA(ga),
  snaive = SNAIVE(ga)
)
forecast_3 <- forecast(fable_3, h = "3 years")
autoplot(forecast_3, data=ana, level=c(80,95)) +
  labs(x="Month-Year of discharge",
       title="ARIMA/ETS/SNAIVE models predicting percentage pts discharged from
              Wythenshawe after GA",
       subtitle = "Data courtesy of the NHFD",
       y="Number of patients")
```

::: {.cell-output-display}
![](sap_files/figure-html/unnamed-chunk-4-2.png){width=672}
:::

```{.r .cell-code}
arima_means <- forecast_3 %>% filter(.model=='arima')
arima_mean <- mean(arima_means$.mean)
ets_means <- forecast_3 %>% filter(.model=='ets')
ets_mean <- mean(ets_means$.mean)
snaive_means <- forecast_3 %>% filter(.model=='snaive')
snaive_mean <- mean(snaive_means$.mean)
print_mean_3 <- mean(c(snaive_mean, ets_mean, arima_mean)) |> round(2)
```
:::
::: {.cell}
::: {.cell-output-display}
```{=html}
<a onclick="fetch(&#39;data:text/csv;base64,QWRtaXNzaW9uLlllYXIuLi5Nb250aDtHZW5lcmFsLmFuYWVzdGhldGljLi47R2VuZXJhbC5hbmFlc3RoZXRpYy4uLi5hbm51YWwuO1guLm9mLkdlbmVyYWxzLmdpdmVuLm5lcnZlLmJsb2NrO1guLm9mLkdlbmVyYWxzLmdpdmVuLm5lcnZlLmJsb2NrLi5hbm51YWwuO1NwaW5hbC5hbmFlc3RoZXRpYy4uO1NwaW5hbC5hbmFlc3RoZXRpYy4uLi5hbm51YWwuO1guLm9mLlNwaW5hbHMuZ2l2ZW4ubmVydmUuYmxvY2s7WC4ub2YuU3BpbmFscy5naXZlbi5uZXJ2ZS5ibG9jay4uYW5udWFsLjtYLi5HaXZlbi5uZXJ2ZS5ibG9jay5wcmUudGhlYXRyZTtYLi5BTEwuTkhGRC5naXZlbi5uZXJ2ZS5ibG9jay5wcmUudGhlYXRyZQpBcHIgMjAxMTszNiwwMDtOQTsyMiwyMjtOQTs1MiwwMDtOQTs3LDc7TkE7TkE7TkEKTWF5IDIwMTE7NTcsNjk7TkE7MTMsMzM7TkE7MjYsOTI7TkE7MCwwO05BO05BO05BCkp1biAyMDExOzU3LDE0O05BOzM3LDUwO05BOzM1LDcxO05BOzEwLDA7TkE7TkE7TkEKSnVsIDIwMTE7NDAsMDA7TkE7NTAsMDA7TkE7NTAsMDA7TkE7MCwwO05BO05BO05BCkF1ZyAyMDExOzI2LDA5O05BOzY2LDY3O05BOzY5LDU3O05BOzEyLDU7TkE7TkE7TkEKU2VwIDIwMTE7NDAsMDA7TkE7MTIsNTA7TkE7NTUsMDA7TkE7MTgsMjtOQTtOQTtOQQpPY3QgMjAxMTs0MSwxODtOQTs1NywxNDtOQTs0MSwxODtOQTswLDA7TkE7TkE7TkEKTm92IDIwMTE7MzcsNTA7TkE7MTEsMTE7TkE7NjIsNTA7TkE7MTMsMztOQTtOQTtOQQpEZWMgMjAxMTs0MCwwMDtOQTswLDAwO05BOzU2LDAwO05BOzcsMTtOQTtOQTtOQQpKYW4gMjAxMjs1Miw2MztOQTsxMCwwMDtOQTs0NywzNztOQTswLDA7TkE7TkE7TkEKRmViIDIwMTI7MzEsMjU7TkE7MjAsMDA7TkE7NjgsNzU7TkE7MzYsNDtOQTtOQTtOQQpNYXIgMjAxMjsyMSw0Mzs0MSwyNTswLDAwOzI0LDUzOzc4LDU3OzUyLDE0OzAsMDs5LDc7TkE7TkEKQXByIDIwMTI7NTcsMTQ7NDMsMDg7MCwwMDsyMiwwMjs0Miw4Njs1MSwzODsxMSwxOzEwLDA7TkE7TkEKTWF5IDIwMTI7NjUsNjI7NDQsNDA7MjMsODE7MjMsNDg7MzQsMzg7NTEsNzQ7MjcsMzsxMSw5O05BO05BCkp1biAyMDEyOzYzLDY0OzQ0LDY2OzAsMDA7MTgsNTg7MzEsODI7NTEsNzg7NTcsMTsxNCw1O05BO05BCkp1bCAyMDEyOzU2LDI1OzQ1LDc4OzIyLDIyOzE2LDY3OzMxLDI1OzUwLDYwOzAsMDsxNSwxO05BO05BCkF1ZyAyMDEyOzYwLDAwOzQ4LDU1OzExLDExOzEzLDY4OzQwLDAwOzQ4LDEzOzE2LDc7MTUsNTtOQTtOQQpTZXAgMjAxMjs1OCwzMzs1MCwyMDsxNCwyOTsxMyw4MjszNyw1MDs0Niw1MzszMywzOzE2LDc7TkE7TkEKT2N0IDIwMTI7NjIsOTY7NTIsMTY7MTcsNjU7MTIsMDM7MzMsMzM7NDUsNDk7MTEsMTsxNywyO05BO05BCk5vdiAyMDEyOzU4LDMzOzU0LDEyOzIxLDQzOzEzLDA0OzQxLDY3OzQzLDUzOzMwLDA7MTgsOTtOQTtOQQpEZWMgMjAxMjs2NCw3MTs1Niw4Mjs0NSw0NTsxOCw2NzszMiwzNTs0MCw5MTsxOCwyOzIwLDQ7TkE7TkEKSmFuIDIwMTM7NzEsODc7NTgsODQ7NDMsNDg7MjIsNzA7MTgsNzU7MzcsOTE7MzMsMzsyMiw5O05BO05BCkZlYiAyMDEzOzc1LDAwOzYxLDM3OzQxLDY3OzI0LDEyOzI1LDAwOzM1LDM4OzI1LDA7MjEsNDtOQTtOQQpNYXIgMjAxMzs2MSwxMTs2MywzNTsyNywyNzsyNCw3MjszMywzMzszMywxMDswLDA7MjIsNjtOQTtOQQpBcHIgMjAxMzszMywzMzs2MSwyNzsyNSwwMDsyNiw0NDs2Miw1MDszNCw4NjsyNiw3OzI0LDI7TkE7TkEKTWF5IDIwMTM7NTAsMDA7NTksOTM7NzAsMDA7MjksNDU7NDUsMDA7MzUsNjY7MTEsMTsyMiw3O05BO05BCkp1biAyMDEzOzUwLDAwOzU4LDYzOzcxLDQzOzM1LDU4OzQyLDg2OzM2LDY5OzMzLDM7MjEsNjtOQTtOQQpKdWwgMjAxMzszOCwxMDs1NywyNDs1MCwwMDszNywwNDs0Nyw2MjszNyw4MTsxMCwwOzIxLDU7TkE7TkEKQXVnIDIwMTM7NjIsNTA7NTcsNTM7MjYsNjc7MzcsNTA7MzcsNTA7MzcsNjc7MjIsMjsyMSw4O05BO05BClNlcCAyMDEzOzg4LDg5OzU5LDQ0OzUwLDAwOzQwLDU5OzExLDExOzM2LDAxOzUwLDA7MjEsNDtOQTtOQQpPY3QgMjAxMzs2Niw2Nzs1OSw1NzsxNiw2Nzs0MSwyMTsyMiwyMjszNSwzODsyNSwwOzIyLDQ7TkE7TkEKTm92IDIwMTM7NDgsNDg7NTgsMzk7MzcsNTA7NDIsNTE7NDIsNDI7MzUsNjY7MjgsNjsyMiw1O05BO05BCkRlYyAyMDEzOzY3LDU3OzU4LDgyOzQwLDAwOzQxLDc2OzI0LDMyOzM0LDYwOzMzLDM7MjQsMDtOQTtOQQpKYW4gMjAxNDs1MywzMzs1Niw3OTs0Myw3NTs0MSw3MjszNiw2NzszNiw1OTs1NCw1OzI2LDc7TkE7TkEKRmViIDIwMTQ7ODgsMjQ7NTcsNjQ7NDAsMDA7NDEsNTc7MTEsNzY7MzUsNzY7MCwwOzI2LDI7TkE7TkEKTWFyIDIwMTQ7NDAsOTE7NTYsMTY7NTUsNTY7NDMsMjk7NTksMDk7MzcsNjc7MCwwOzI0LDU7TkE7TkEKQXByIDIwMTQ7NTAsMDA7NTcsMzg7NTMsMzM7NDUsMDM7NDAsMDA7MzUsOTE7MzMsMzsyNSwyO05BO05BCk1heSAyMDE0OzcwLDAwOzU4LDcyOzI4LDU3OzQyLDI5OzMwLDAwOzM0LDkwOzMzLDM7MjYsOTtOQTtOQQpKdW4gMjAxNDs2Niw2Nzs2MCwyNzs1NSw1Njs0MSwzNDsyNSw5MzszMywzMzsxNCwzOzI1LDM7TkE7TkEKSnVsIDIwMTQ7NzUsODY7NjMsMjg7NTksMDk7NDMsMDE7MjAsNjk7MzEsMTU7NTAsMDsyOCw0O05BO05BCkF1ZyAyMDE0Ozc0LDA3OzY0LDI5OzMwLDAwOzQyLDkzOzIyLDIyOzI5LDg3OzAsMDsyNywyO05BO05BClNlcCAyMDE0OzcwLDgzOzYzLDM4OzIzLDUzOzQwLDcwOzI1LDAwOzMwLDU3OzE2LDc7MjYsMDtOQTtOQQpPY3QgMjAxNDs2Miw4Njs2MywxNDs0NSw0NTs0Miw1ODszNCwyOTszMSw0MjsxNiw3OzI1LDA7TkE7TkEKTm92IDIwMTQ7ODUsNzE7NjYsMTQ7MzgsODk7NDIsNjU7OSw1MjsyOCw4NDswLDA7MjMsOTtOQTtOQQpEZWMgMjAxNDs3MSwxMTs2Niw2Nzs1OSwzODs0NSw0MTsyOCw4OTsyOSwzNjs3LDc7MjAsODtOQTtOQQpKYW4gMjAxNTs2NSwzODs2Nyw4MDs1Miw5NDs0NiwxMjsyNiw5MjsyOCw0ODsxNCwzOzE2LDM7TkE7TkEKRmViIDIwMTU7OTAsMDA7NjgsNzU7NDAsNzQ7NDUsODk7MTAsMDA7MjcsNjg7MzMsMzsxNywyO05BO05BCk1hciAyMDE1Ozg2LDY3OzcyLDA5OzIzLDA4OzQzLDE1OzEzLDMzOzI0LDQyOzI1LDA7MjAsMjtOQTtOQQpBcHIgMjAxNTs2NCwwMDs3Myw0NTs0Myw3NTs0Miw1NzsyOCwwMDsyMywzMDs0Miw5OzIwLDM7TkE7TkEKTWF5IDIwMTU7ODAsMDA7NzQsMjk7NDIsODY7NDMsMzU7MTEsNDM7MjEsNzU7MjUsMDsxOSw1O05BO05BCkp1biAyMDE1OzgwLDAwOzc1LDM1OzQxLDY3OzQyLDM4OzEzLDMzOzIwLDczOzI1LDA7MjAsMztOQTtOQQpKdWwgMjAxNTs2NSwwMDs3NCw3MTs4NCw2Mjs0MywwODsyMCwwMDsyMCw2OTsyNSwwOzE4LDE7TkE7TkEKQXVnIDIwMTU7ODgsODk7NzUsNTI7NTAsMDA7NDQsNTM7MTEsMTE7MjAsMDY7MCwwOzE5LDE7TkE7TkEKU2VwIDIwMTU7NzAsNTk7NzUsMzY7NTgsMzM7NDcsMTU7MjksNDE7MjAsNjM7MzAsMDsyMCw4O05BO05BCk9jdCAyMDE1OzgwLDk1Ozc3LDAxOzU4LDgyOzQ4LDA2OzE0LDI5OzE4LDgxOzAsMDsyMCw2O05BO05BCk5vdiAyMDE1Ozg3LDUwOzc3LDIyOzcxLDQzOzUwLDU3OzEyLDUwOzE4LDkzOzAsMDsyMCwzO05BO05BCkRlYyAyMDE1OzYxLDc2Ozc2LDQ1OzQ3LDYyOzQ5LDIwOzM1LDI5OzE5LDI3OzMzLDM7MjUsNDtOQTtOQQpKYW4gMjAxNjs3MCw4Mzs3Niw5Mjs3MCw1OTs1MCw0MDsyOSwxNzsxOSwzODsyOCw2OzI3LDA7TkE7TkEKRmViIDIwMTY7NjksMjM7NzUsMDg7NjEsMTE7NTIsMjg7MzAsNzc7MjEsMTg7MzcsNTsyNyw5O05BO05BCk1hciAyMDE2OzY5LDU3OzczLDU3OzM3LDUwOzU0LDU1OzIxLDc0OzIxLDk3OzYwLDA7MzAsNDtOQTtOQQpBcHIgMjAxNjs3OCwyNjs3NCw2ODs2MSwxMTs1NSw3OTsxNywzOTsyMSwxNTsyNSwwOzI4LDg7TkE7TkEKTWF5IDIwMTY7ODMsMzM7NzUsMDg7NjAsMDA7NTcsODc7MTYsNjc7MjEsNzM7NTAsMDszMCw5O05BO05BCkp1biAyMDE2Ozg3LDUwOzc1LDg3OzMyLDE0OzU2LDQ5OzEyLDUwOzIxLDU5OzI1LDA7MzAsOTtOQTtOQQpKdWwgMjAxNjs2MCw4Nzs3NSw0Nzs1MCwwMDs1NCw1ODszNCw3ODsyMiw2NDswLDA7MjcsODtOQTtOQQpBdWcgMjAxNjs1NCw4NDs3Miw4MTs0MSwxODs1Myw5NDszMiwyNjsyNCwxNzsyMCwwOzI3LDU7TkE7TkEKU2VwIDIwMTY7NzIsNzM7NzMsMDQ7NTYsMjU7NTMsNjU7MjcsMjc7MjMsODI7MTYsNzsyNiwzO05BO05BCk9jdCAyMDE2OzY1LDM4OzcxLDkxOzQ3LDA2OzUyLDc5OzMwLDc3OzI1LDAwOzEyLDU7MjUsOTtOQTtOQQpOb3YgMjAxNjs4NSw3MTs3MSw5NTs0NSw4Mzs1MCw0MjsxMCw3MTsyNCw3MDswLDA7MjUsOTtOQTtOQQpEZWMgMjAxNjs3MiwwMDs3MywwNDs1NSw1Njs1MSwwNzsyNCwwMDsyMyw1MTs2Niw3OzI4LDA7TkE7TkEKSmFuIDIwMTc7NjcsNzQ7NzIsNzA7OTAsNDg7NTMsMTY7MjUsODE7MjMsMzE7MTAwLDA7MzUsNTs1NCw4OzM2LDAKRmViIDIwMTc7NzEsNDM7NzIsODE7OTMsMzM7NTcsNDM7MjMsODE7MjIsODE7ODAsMDs0MSwwOzY5LDA7MzYsMwpNYXIgMjAxNzs3OSwzMTs3Myw1Njs5NSw2NTs2MiwxMTsxNywyNDsyMiw0MTs4MCwwOzQyLDM7NzAsMDs0MCw5CkFwciAyMDE3OzYwLDg3OzcyLDQxOzcxLDQzOzYyLDcwOzM5LDEzOzIzLDg1OzIyLDI7NDEsMDs2OSw2OzQxLDIKTWF5IDIwMTc7NjcsNzQ7NzAsODU7MTAwLDAwOzY2LDI2OzI5LDAzOzI1LDA3OzY2LDc7NDMsMDs3NCwyOzQxLDgKSnVuIDIwMTc7NjAsODc7NjgsNTY7MTAwLDAwOzcyLDQ5OzM0LDc4OzI2LDk1Ozc1LDA7NDYsNzs0OCwwOzQzLDEKSnVsIDIwMTc7NzYsNjc7NjksNzk7NTIsMTc7NzEsODU7MjMsMzM7MjYsMTA7NTcsMTs1MSw3OzcwLDA7NDMsMQpBdWcgMjAxNzs4MiwxNDs3MiwxOTs5MSwzMDs3NSw4MjsxMCw3MTsyNCwyNjsxMDAsMDs1NywzOzU1LDI7NDQsMQpTZXAgMjAxNzs3OSwxNzs3Miw2NTsxMDAsMDA7NzgsOTU7MjAsODM7MjMsODI7NDAsMDs1OSwzOzYwLDA7NDUsNQpPY3QgMjAxNzs3NSw4Njs3Myw0Nzs4NiwzNjs4MSw3NTsxMyw3OTsyMiw0NTsxMDAsMDs2NiwyOzY3LDc7NDUsMApOb3YgMjAxNzs3NSwwMDs3Miw2ODs5Niw2Nzs4Niw4MjsxNyw1MDsyMiw4Mjs4NSw3OzcwLDQ7NjMsNDs0Nyw5CkRlYyAyMDE3OzU3LDE0OzcxLDIzOzEwMCwwMDs5MCwwMDszMSw0MzsyMyw1Njs2Myw2OzY5LDg7NjEsMTs0NywzCkphbiAyMDE4OzY1LDYyOzcxLDA0OzY2LDY3Ozg4LDA4OzI4LDEyOzIzLDc3OzQ0LDQ7NjQsNDs4MSwyOzQ4LDEKRmViIDIwMTg7NzUsMDA7NzEsMzU7NTgsMzM7ODQsNjU7MjUsMDA7MjMsODg7MzcsNTs2MCwwOzU4LDg7NDcsNgpNYXIgMjAxODs1OCwzMzs2OSw0Mjs1NywxNDs4MSwzNTszOCw4OTsyNSw5MDswLDA7NTAsMDs4MCw2OzQ3LDMKQXByIDIwMTg7NjcsODY7NjksODQ7NTIsNjM7NzksNzc7MjgsNTc7MjUsMjc7MjUsMDs1MCw1OzY3LDk7NDcsOQpNYXkgMjAxODs1Nyw2OTs2OSwxNTs1MywzMzs3Niw0OTs0MiwzMTsyNiwxNzszNiw0OzQ3LDQ7ODUsMjs0OSwwCkp1biAyMDE4OzY4LDAwOzY5LDU5OzQxLDE4OzcyLDgzOzMyLDAwOzI2LDAzOzM3LDU7NDQsMjs2OSwyOzUwLDcKSnVsIDIwMTg7NjEsNzY7NjgsMjk7NDIsODY7NzIsMjI7MjYsNDc7MjYsMjk7MzMsMzs0MiwzOzYxLDg7NDksNApBdWcgMjAxODs4NCw4NTs2OCw3Mjs3NSwwMDs3MCw4MjsxMiwxMjsyNiwyMDsyNSwwOzM5LDg7ODcsOTs0OSw1ClNlcCAyMDE4OzY4LDk3OzY4LDA3OzU1LDAwOzY3LDQ0OzI0LDE0OzI2LDM5OzAsMDszNywwOzY5LDA7NTEsNQpPY3QgMjAxODs4Miw4Njs2OCw4Mzs4Miw3Njs2Nyw1NTsxNywxNDsyNiw0OTsxNiw3OzMzLDM7NjIsOTs1MSw0Ck5vdiAyMDE4OzczLDY4OzY4LDQxOzc4LDU3OzY0LDY2OzI2LDMyOzI3LDQ3OzIwLDA7MjksMDs5MCwwOzU0LDIKRGVjIDIwMTg7NzMsMzM7NjksOTI7NTksMDk7NjEsMzU7MjYsNjc7MjcsMDI7NzUsMDsyOCw5OzgzLDM7NTMsMgpKYW4gMjAxOTs3MSw0Mzs3MCw0NDs0NCwwMDs1OSwyMjsyNSw3MTsyNiw4MDszMywzOzI3LDg7NzQsMzs1NCw1CkZlYiAyMDE5OzY1LDIyOzY5LDY5OzQ2LDY3OzU4LDU0OzM0LDc4OzI3LDQ4OzYyLDU7MjksOTs3OCwzOzU0LDUKTWFyIDIwMTk7ODQsNjI7NzIsMDE7NTksMDk7NTgsNzA7MTEsNTQ7MjUsMDc7MzMsMzszNCw5OzY2LDc7NTUsNgpBcHIgMjAxOTs0OCw3Mjs2OSw3Nzs0NywzNzs1OCwzMDs1MSwyODsyNyw2ODsyNSwwOzMzLDc7NzMsODs1MywyCk1heSAyMDE5OzgzLDMzOzcxLDc5OzQwLDAwOzU2LDgxOzE2LDY3OzI1LDcwOzIwLDA7MzIsNjs4NywxOzU2LDMKSnVuIDIwMTk7NTYsMjU7NzAsNjg7MjIsMjI7NTUsNDM7NDAsNjM7MjYsNTg7NDYsMjszNCwwOzc4LDE7NTYsNgpKdWwgMjAxOTs2NCw4Njs3MCw5MjsyOSwxNzs1NCwwMjsyOSw3MzsyNiw5MDszNiw0OzM0LDM7NjcsNjs1NCwzCkF1ZyAyMDE5Ozc1LDAwOzY5LDk3OzY2LDY3OzUyLDc2OzI1LDAwOzI4LDEwOzAsMDszMiw0OzcxLDQ7NTUsNQpTZXAgMjAxOTs3Nyw3ODs3MCw4MTszNSw3MTs1MCw3NjsyMiwyMjsyNyw4NDs4Nyw1OzM4LDg7ODMsMzs1NSw1Ck9jdCAyMDE5OzY2LDY3OzY5LDI5OzE4LDE4OzQ0LDMxOzI0LDI0OzI4LDUzOzI1LDA7MzksMDs5Myw5OzU2LDgKTm92IDIwMTk7NTcsMTQ7NjcsNzc7MzMsMzM7NDEsNTE7NDAsNDg7MjksOTI7MTcsNjszNiw4Ozk1LDI7NTYsNgpEZWMgMjAxOTs3Miw1MDs2Nyw4MzszNyw5MzszOSw3MTsyNyw1MDsyOSw5Mzs0NSw1OzM1LDA7ODUsMDs1Niw2CkphbiAyMDIwOzc3LDc4OzY4LDE5OzIzLDgxOzM4LDA2OzIyLDIyOzI5LDc3OzAsMDszMywzOzc0LDE7NTcsNQpGZWIgMjAyMDs4NCwwMDs2OSwzNzszMywzMzszNywyMzs4LDAwOzI4LDEwOzUwLDA7MzEsNTs4NCwwOzU3LDcKTWFyIDIwMjA7NzgsNTc7NjksMDI7MTgsMTg7MzMsOTQ7MjEsNDM7MjgsNzI7MTYsNzszMCw3OzgyLDg7NTgsMwpBcHIgMjAyMDs0NSwxNjs2OSwxNTs1NywxNDszNCwyMDs0NSwxNjsyNyw3NjsyOCw2OzMxLDU7ODcsMTs1OCwyCk1heSAyMDIwOzQ1LDQ1OzY2LDY3OzAsMDA7MzIsMjg7NTAsMDA7MjksOTI7MTgsMjszMCw3Ozk1LDU7NTksNwpKdW4gMjAyMDs0Myw0ODs2NiwxMzs4MCwwMDszNCw5Njs1MiwxNzszMCwzODsyNSwwOzI4LDM7ODcsNTs1OSwzCkp1bCAyMDIwOzU1LDU2OzY1LDQ3OzQ2LDY3OzM2LDI5OzMzLDMzOzMwLDY2OzIyLDI7MjcsMDs4NSwyOzYwLDYKQXVnIDIwMjA7NzAsNTk7NjQsOTY7MjUsMDA7MzIsODk7MjMsNTM7MzAsNzc7MCwwOzI3LDg7ODgsOTs2MCw5ClNlcCAyMDIwOzY2LDY3OzYzLDc3OzUwLDAwOzM0LDA5OzMwLDAwOzMxLDU5OzU1LDY7MjUsNzs5MCwzOzYxLDMKT2N0IDIwMjA7MzgsODk7NjIsMTI7NzEsNDM7MzcsMDc7NjEsMTE7MzMsOTQ7MjcsMzsyNSw5Ozg1LDA7NjAsNApOb3YgMjAyMDs1MiwzODs2MSw1Mjs0MCw5MTszNyw5Mzs0Miw4NjszNCwyNDszMywzOzI4LDM7OTAsNTs2MCw5CkRlYyAyMDIwOzQ5LDA2OzU4LDMxOzQyLDMxOzM4LDUwOzQ3LDE3OzM3LDAzOzI4LDA7MjYsODs5MCw5OzYxLDcKSmFuIDIwMjE7NDEsOTQ7NTUsMzM7NzYsOTI7NDIsNzE7NTQsODQ7MzksNzc7MzUsMzsyOSwwOzk2LDg7NjEsNwpGZWIgMjAyMTs1MSwwNjs1Miw4NTs1NCwxNzs0NSwxMzs0NCw2ODs0Miw1NTs0Miw5OzMwLDY7OTcsOTs2NSwyCk1hciAyMDIxOzQyLDg2OzUwLDAwOzY2LDY3OzUwLDAwOzUxLDQzOzQ0LDk1OzMzLDM7MzEsNDs5NywxOzY0LDAKQXByIDIwMjE7NDIsMTE7NDksNjE7NTAsMDA7NDksNDc7NTUsMjY7NDUsOTU7MjMsODszMCw3Ozk3LDQ7NjYsNApNYXkgMjAyMTs0NiwxNTs0OSw1MDs1NSw1Njs1Miw1Mzs0Myw1OTs0NSw1MDsyMyw1OzMwLDg7ODQsNjs2MywyCkp1biAyMDIxOzU3LDE0OzUwLDQ5OzU1LDAwOzUxLDQ0OzQwLDAwOzQ0LDY2OzQyLDk7MzIsMTs5MSw0OzYzLDcKSnVsIDIwMjE7MzMsMzM7NDgsNjk7NjYsNjc7NTIsNjg7NjMsODk7NDcsMDM7OCw3OzI5LDg7OTQsNDs2NCw5CkF1ZyAyMDIxOzQ3LDA2OzQ3LDcyOzUwLDAwOzU0LDA3OzUyLDk0OzQ4LDQwOzIyLDI7MjksNzs5NywxOzY0LDgKU2VwIDIwMjE7MzQsMzg7NDUsNDU7MTgsMTg7NTIsNTA7NjIsNTA7NTAsNjg7NTUsMDszMCw5Ozg0LDg7NjQsNgpPY3QgMjAyMTs0Niw1MTs0NSw4MTsyNSwwMDs0OSwzMDs1Myw0OTs1MCw1NDsxMywwOzI5LDQ7OTMsMDs2NCw2Ck5vdiAyMDIxOzQ4LDg5OzQ1LDUxOzMxLDgyOzQ4LDM2OzQ4LDg5OzUxLDA3OzksMTsyNywyOzg3LDA7NjMsOQpEZWMgMjAyMTs0OCwyODs0NSw0NTszNSw3MTs0Nyw0NDs0OCwyODs1MSwxNjsyMSw0OzI2LDQ7OTMsMTs2NCwzCkphbiAyMDIyOzUzLDY2OzQ2LDM4OzIyLDczOzQzLDMwOzQxLDQ2OzUwLDEwOzIzLDU7MjUsNjs5MCwyOzY0LDkKRmViIDIwMjI7NTMsMzM7NDYsMzU7MzEsMjU7NDEsMjA7NDYsNjc7NTAsNDM7MjEsNDsyMyw4OzkwLDM7NjUsMgpNYXIgMjAyMjs1MSwyODs0NywwMjsyMCwwMDszNyw1Njs0MSwwMzs0OSw1NzsxMiw1OzIyLDM7OTAsMjs2NCwyCkFwciAyMDIyOzYxLDIyOzQ4LDg2OzQ2LDY3OzM3LDg3OzM4LDc4OzQ4LDAyOzQyLDE7MjMsODs4NSw3OzY0LDkKTWF5IDIwMjI7NjAsMDA7NDksOTA7MTksMDU7MzQsODc7NDAsMDA7NDcsODA7MCwwOzIyLDQ7OTcsMjs2Niw1Cg==&#39;).then(res =&gt; res.blob()).then(blob =&gt; {&#10;      const downloadURL = window.URL.createObjectURL(blob);&#10;      const a = document.createElement(&#39;a&#39;);&#10;      document.body.appendChild(a);&#10;      a.href = downloadURL;&#10;      a.download = &#39;anaesthetic_data.csv&#39;; a.click();&#10;      window.URL.revokeObjectURL(downloadURL);&#10;      document.body.removeChild(a);&#10;    });">
<button class="btn btn-warning" has_icon="TRUE"><i class="fa fa-save"></i> Download data</button>
</a>
```
:::
:::
A forecast based on the percentage of patients discharged from Wythenshawe who had a General Anaesthetic over the period 2011 - 2022. This suggests an expected mean percentage GA recipients (simple average of 3 model estimates of %GA) of 51.66%. 

### Spinal Anaesthetic

::: {.cell .column-screen-inset-shaded layout-nrow="1"}

```{.r .cell-code}
ana <- read.csv("./NHFD_Data/ana_apr_22.csv")
names(ana)
names(ana) <-c("my", 
               "ga", 
               "ga_ann", 
               "nblock", 
               "nblockann", 
               "spinal",
               "spinalann",
               "blockpre",
               "allNFHDnerveblock")
spinal <- ana %>% select(my, spinal)
spinal$my <- lubridate::my(spinal$my) 
ts_spinal <- spinal %>% mutate(my = tsibble::yearmonth(my)) %>% tsibble()

p <- spinal |> ggplot() + geom_line(aes(x=my, y=spinal)) +
  labs(x="Month-Year of discharge", 
       title="#NOF pts discharged from Wythenshawe after spinal", 
       subtitle = "Data courtesy of the NHFD", y="Spinal Anaesthetic (%)")
p
```

::: {.cell-output-display}
![](sap_files/figure-html/unnamed-chunk-6-1.png){width=672}
:::

```{.r .cell-code}
fable_4 <- ts_spinal%>% model(  
  ets = ETS(spinal ~ error("M") + trend("M") + season("M")),
  arima = ARIMA(spinal),
  snaive = SNAIVE(spinal)
)
forecast_4 <- forecast(fable_4, h = "3 years")
autoplot(forecast_4, data=spinal, level=c(80,95)) +
  labs(x="Month-Year of discharge",
       title="ARIMA/ETS/SNAIVE models predicting percentage pts discharged from
              Wythenshawe after spinal",
       subtitle = "Data courtesy of the NHFD",
       y="Spinal Anaesthetic (%)")
```

::: {.cell-output-display}
![](sap_files/figure-html/unnamed-chunk-6-2.png){width=672}
:::

```{.r .cell-code}
arima_means <- forecast_4 %>% filter(.model=='arima')
arima_mean <- mean(arima_means$.mean)
ets_means <- forecast_4 %>% filter(.model=='ets')
ets_mean <- mean(ets_means$.mean)
snaive_means <- forecast_4 %>% filter(.model=='snaive')
snaive_mean <- mean(snaive_means$.mean)
print_mean_4 <- mean(c(snaive_mean, ets_mean, arima_mean)) |> round(2)
```
:::

A forecast based on the percentage of patients discharged from Wythenshawe who had a Spinal Anaesthetic over the period 2011 - 2022. This suggests an expected mean percentage Spinal recipients (simple average of 3 model estimates of %Spinal) of 46%. 







## Outcome measures
### OSLA scale

![OSLA](Delirium-assessment-tool-OSLA.png){fig-align="center"} The OSLA scale is a 4-item scale with a minimum score of 0 and a maximum score of 15. Items respectively have a range of :

| Item        | Min | Max |
|-------------|-----|-----|
| Eye opening | 0   | 5   |
| Eye contact | 0   | 3   |
| Posture     | 0   | 3   |
| Movement    | 0   | 4   |

: OSLA Items

::: {.cell}

```{.r .cell-code}
# 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
#| column: screen-inset-shaded
#| layout-nrow: 1
OSLA <- function(severity="None", return_format = "df"){
  # generate an OSLA score vector depending on severity
  # return as dataframe if return_format = df
  # return as vector if return_format = vector
  # return as integer sum if return_format = sum

  if (severity == "Severe"){
    probability <- 0.85
  }
  else if (severity == "Moderate"){
    probability <- 0.5
  }
  else if (severity == "Mild"){
    probability <- 0.3
  }
  else if (severity == "None"){
    probability <- 0.05
  }
  EO <- rbinom(1, 5, prob=probability) # eye opening
  EC <- rbinom(1, 3, prob=probability) # eye contact
  P <- rbinom(1, 3, prob=probability) # posture
  M <- rbinom(1, 4, prob=probability) # movement
  if(return_format == "df"){
    returned_df <- data.frame(cbind(EO, EC, P, M))
    return(returned_df)
  }
  else if(return_format == "vector"){
    returned_vec <- c(EO, EC, P, M)
    return(returned_vec)
  }
  else if(return_format == "sum"){
    returned_sum <- sum(EO, EC, P, M)
    return(returned_sum)
  }
  else {
    print("Unknown value for returned_format")
    return(NULL)
  }
  }

# make a **population** of mixed severity N= 100,000
# at baseline from which to sample

severe_pop <- replicate(10000, OSLA("Severe", "sum"))
moderate_pop <- replicate(15000, OSLA("Moderate", "sum"))
mild_pop <- replicate(15000, OSLA("Mild", "sum"))
none_pop <- replicate(60000, OSLA("None", "sum"))
total_pop_1 <- c(severe_pop, moderate_pop, mild_pop, none_pop)
total_pop_1 |>hist(main="Histogram I of OSLA scores (zero inflated)", xlab="OSLA score")
```

::: {.cell-output-display}
![](sap_files/figure-html/unnamed-chunk-7-1.png){width=672}
:::

```{.r .cell-code}
severe_pop <- replicate(60000, OSLA("Severe", "sum"))
moderate_pop <- replicate(15000, OSLA("Moderate", "sum"))
mild_pop <- replicate(15000, OSLA("Mild", "sum"))
none_pop <- replicate(10000, OSLA("None", "sum"))
total_pop_2 <- c(severe_pop, moderate_pop, mild_pop, none_pop)
total_pop_2|>hist(main="Histogram II of OSLA scores (severe)", xlab="OSLA score")
```

::: {.cell-output-display}
![](sap_files/figure-html/unnamed-chunk-7-2.png){width=672}
:::

```{.r .cell-code}
severe_pop <- replicate(30000, OSLA("Severe", "sum"))
moderate_pop <- replicate(25000, OSLA("Moderate", "sum"))
mild_pop <- replicate(30000, OSLA("Mild", "sum"))
none_pop <- replicate(15000, OSLA("None", "sum"))
total_pop_3 <- c(severe_pop, moderate_pop, mild_pop, none_pop)
total_pop_3 |>hist(main="Histogram III of OSLA scores (mild/severe)", xlab="OSLA score")
```

::: {.cell-output-display}
![](sap_files/figure-html/unnamed-chunk-7-3.png){width=672}
:::

```{.r .cell-code}
severe_pop <- replicate(10000, OSLA("Severe", "sum"))
moderate_pop <- replicate(60000, OSLA("Moderate", "sum"))
mild_pop <- replicate(20000, OSLA("Mild", "sum"))
none_pop <- replicate(10000, OSLA("None", "sum"))
total_pop_4 <- c(severe_pop, moderate_pop, mild_pop, none_pop)
total_pop_4 |>hist(main="Histogram IV of OSLA scores (moderate)", xlab="OSLA score")
```

::: {.cell-output-display}
![](sap_files/figure-html/unnamed-chunk-7-4.png){width=672}
:::
:::
## Frequentist simulation

**Assumption:**: Delirium rating scales are ordinal measures, not interval measures (a person scoring 4 is not half as delirious as someone scoring 8)

**Solution**: Ordinal logistic regression does not assume interval scales, nor normal distributions of the scores. It does make a proportional-odds assumption testable using the parallel test. The coefficients can be interpreted as the log-odds of a one point increase in the scale score, $y$, with a one point (or one unit) increase in the predictor variable $x$.

**Assumption**: While delirium is by its nature a fluctuating condition, we assume there will be correlation within participants in delirium scale scores over time.

**Solution**: Mixed effects models will allow for an autoregressive correlation structure and regularisation of the model. Frequentist ordinal logistic mixed models are facilitated by R packages 'ordinal' and 'rms' and Bayesian mixed effects ordinal regression models are facilitated by packages 'brms' (with cumulative log-odds link function) and 'rmsb'. These have been used in simulation below.

**Assumption**: There may be significant anaesthetic methods (spinal/GA) in those presenting to the ED with hip fractures. The descriptive statistics for Wythenshawe hospital can be found below.

**Solution** Because the nature of the anaesthetic (spinal versus general) may relate to the nature of the participants' comorbidities and the presence or absence of preoperative delirium, we will use a minimisation strategy to ensure equal numbers of spinal and general anaesthetic procedures in each trial arm. The numbers of participants having GA will vary month-month.
