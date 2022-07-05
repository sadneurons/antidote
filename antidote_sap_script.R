# Dr. Ross A. Dunne 01/07/2022

# Statistical analysis plan
# Directed Acyclic graphs
# Simulations
# Sample size calculation
# Planned analyses
# Analysis code
library(tidyverse)
library(lme4)
library(ordinal)
library(broom.mixed)
library(lmerTest)
# Proposal to use the OSLA
# OSLA details and properties
# Score 0-15 with 4 items EO, EC, P and M
# items likely to be correlated
# i.e. scoring 3 on 1 means more likely to score 3 on another

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

# make a **population** of mixed severity
# at baseline from which to sample

severe_pop <- replicate(1500, OSLA("Severe", "sum"))
moderate_pop <- replicate(5000, OSLA("Moderate", "sum"))
mild_pop <- replicate(5000, OSLA("Mild", "sum"))
none_pop <- replicate(10000, OSLA("Mild", "sum"))
total_pop <- c(severe_pop, moderate_pop, mild_pop, none_pop)


randomise_and_split <- function(total_pop, N){
  # sample N from total_pop to generate a trial sample
  rct_sample <- sample(total_pop, N)
  # randomise to treatment or placebo (already "shuffled")
  rx_baseline <- rct_sample[1:(N/2)]
  ctrl_baseline <- rct_sample[((N/2)+1):N]
  return(list(rx_baseline, ctrl_baseline))
}

# ctrl people - some get worse, some get better
# if you were delirious at baseline, you are more likely
# to be delirious postoperatively,
# the more so the worse your score
make_24 <- function(baseline_results,
                    max=15,
                    error=5,
                    max_increase=4){
results_24 <- c()
for(i in 1:length(baseline_results)){
  results_24[i] <- baseline_results[i] +
    rbinom(1, max_increase, prob=baseline_results[i]/max) +
    rnorm(1, 0, error)
  results_24[i] <- ifelse(results_24[i]>=0, results_24[i], 0)
  results_24[i] <- ifelse(results_24[i]<=max, results_24[i], max)
}
return(round(results_24))
}

# better on treatment

make_48 <- function(results_24,
                    max=15,
                    error=5,
                    max_increase=4){
  results_48 <- c()
  for(i in 1:length(results_24)){
    results_48[i] <- results_24[i] +
      rbinom(1, max_increase, prob=results_24[i]/max) +
      rnorm(1, 0, error)
    results_48[i] <- ifelse(results_48[i]>=0, results_48[i], 0)
    results_48[i] <- ifelse(results_48[i]<=max, results_48[i], max)
  }
  return(round(results_48))
}

setup <- function(N, random_error){
  baseline <- randomise_and_split(total_pop, N)
  ctrl_baseline <- baseline[[1]]
  rx_baseline <- baseline[[2]]
  ctrl_24 <- make_24(ctrl_baseline, max=15, error=random_error, max_increase=4)
  rx_24 <- make_24(rx_baseline, max=15, error=random_error, max_increase=1)
  ctrl_48 <- make_48(ctrl_24, max=15, error=random_error, max_increase=2)
  rx_48 <- make_48(rx_24, max=15, error=random_error, max_increase=0)
  study_id <- as.character(1:N)
  treatment <- rep("rx", n=(N/2))
  rx_wide <- tibble(study_id[1:(N/2)], treatment, rx_baseline, rx_24, rx_48)
  names(rx_wide)<-c("id", 'treatment', 'baseline', 'time_24', 'time_48')
  treatment <- rep('ctrl', n=(N/2))
  ctrl_wide <- tibble(study_id[(1+(N/2)):N], treatment, ctrl_baseline, ctrl_24, ctrl_48)
  names(ctrl_wide)<-c("id", 'treatment', 'baseline', 'time_24', 'time_48')
  rx_long <- pivot_longer(rx_wide, cols=c(3:5))
  ctrl_long <- pivot_longer(ctrl_wide , cols=c(3:5))
  longdata <- rbind(rx_long, ctrl_long)
  longdata$name <- as.factor(longdata$name)
  levels(longdata$name)<- c("0", "24", "48")
  longdata$time <- as.numeric(as.character(longdata$name))
  return(longdata)
}

simulate_continuous <- function(longdata){
  mod_c <- lmer(value ~ treatment * time +
                  (1|id),
                data=longdata)
  return(mod_c)
}

simulate_ordinal <- function(longdata){
mod_ord <- ordinal::clmm(as.factor(value) ~ treatment * time +
                           (1|id),
                         data=longdata)
  return(mod_ord)
}

plot_antidote <- function(longdata){
  longdata |> ggplot() + geom_smooth(aes(x=time,
                                       y=value,
                                       col=treatment), lwd=3)
}


simultiord <- function(min, max, rep, random_error = 4){
  ordlist <- vector("list", length = max-min)
  replist <- vector("list", length = rep)
  for(j in min:max){
    for(i in 1:rep){
      longdata <- setup(j, random_error = random_error)
      cat("\r", paste0(i), " inner loop(s) of ", paste0(j-min), " outer loop(s) of ", paste0(max-min), sep="")
      replist[[i]] <- simulate_ordinal(longdata)
    }
    ordlist[[j]] <- replist
  }
  return(ordlist)
}

simulticont <- function(min, max, rep, random_error = 4){
  contlist <- vector("list", length = max-min)
  replist <- vector("list", length = rep)
  for(j in min:max){
    for(i in 1:rep){
      longdata <- setup(j, random_error = random_error)
      cat("\r", paste0(i*j), " of ", paste0(rep*max), paste0("[",(100*rep*max)/(i*j), "]"))
      replist[[i]] <- simulate_continuous(longdata)
    }
    contlist[[j]] <- replist
  }
  return(contlist)
}

getp <- function(model){
  anovdata <- lmerTest::as_lmerModLmerTest(model) |> anova()
  anovdata$`Pr(>F)`[3]
}

get_n <- function(model){
  n <- length(levels(model@flist$id))
  return(n)
}

cont <- simulticont(min = 50, max=121, rep=100, random_error = 3)
size <- sapply(unlist(cont), get_n)
p_values <- sapply(unlist(cont), getp)
boxplot(p_values~size, xlab="Total recruited (n)",
        ylab="p-value for treatment x time",
        main = "Power versus sample size, error=3")
typeII <- p_values<0.05
l <- prop.table(table(typeII, size), margin = 2)
data.frame(l) |> filter(typeII==TRUE) |>
  select(size, Freq) |>
  plot(type="l")

ord <- simultiord(min = 100, max=121, rep=5, random_error = 3)

get_ordp <- function(model){
  summary_1 <- summary(model)
  ordps <- summary_1$coef |>
  data.frame() |>
  select(starts_with("P"))
ordp <- ordps[18,]
return(ordp)
}



for(i in 100:length(ord)){
  for(j in 1: length(ord[[i]])){
    get_ordp(ord[i][[j]])
  }
  }

