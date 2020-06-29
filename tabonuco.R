



#--------------------------------------------
# Leaf litter decomposition analysis
# 30 Apr 2020
#PEGF
#--------------------------------------------
#

library(tidyr) 
library(rlang)
library(dplyr)
library(plyr)
library(tidyverse) 
library(lme4)
library(broom)

# Controlling by manipulation ----------------------------------------------

manipulation <- function(data,
                         InitDryW,
                         FinalDryW,
                         Treatment,
                         Difference) {
  control <- data %>% 
    filter(Treatment == "Control") %>%
    select_(InitDryW,FinalDryW) %>%
    mutate_(Difference = lazyeval::interp (~a/b,  a=as.name(FinalDryW),b=as.name(InitDryW)))

  meanControl <- mean(control$Difference, na.rm = TRUE)
  return (meanControl)
}

manipulation()




# Percentage of AFDM Remaining --------------------------------------------

# IDWc= Initial drya mass corrected by manipulations
# AFDMFraction = Amount of AFDM in a subsample

AFDM <- function(data,
                    InitDryW,
                    FinalDryW,
                    FractIntW,
                    FractFinW,
                    Treatment,
                    Day,
                    Replicate,
                    Difference,
                    control) {
  
# Calculate the control by manupulation
  control <- data %>% 
    filter(Treatment == "Control") %>%
    select_(InitDryW,FinalDryW) %>%
    mutate_(Difference = lazyeval::interp (~a/b,  a=as.name(FinalDryW),b=as.name(InitDryW)))
  
  meanControl <- mean(control$Difference, na.rm = TRUE)

# Corrects the initial dry mass by manipulation
  . <- data %>%
    filter(Treatment != "Control") %>%
    mutate_(IDWc = lazyeval::interp (~a*b, a=as.name(InitDryW), b=as.name("meanControl"))) %>% # Corrects dry mass (laboratory) for mass lost from handling

# Calculate the AFDM in the subsample
    mutate_(AFDMFraction = lazyeval::interp (~(a-b)/a, a= as.name (FractIntW), b= as.name (FractFinW))) %>%   # AFDM in the subsample

# Calculate the AFDM in the corrected initial mass and in the final mass   
      mutate_(AFDM_Initial = lazyeval::interp (~a*b, a= as.name("IDWc"), b= as.name("AFDMFraction"))) %>% # AFDM in the initial sample
      mutate_(AFDM_Final = lazyeval::interp (~a*b, a= as.name(FinalDryW), b= as.name("AFDMFraction"))) %>%   #AFDM in the Final sample         

# Calculate the percentage of remaining mass
      mutate_(AFDMRemaining = lazyeval::interp (~a/b*100, a= as.name("AFDM_Final"), b= as.name("AFDM_Initial")))    # % AFDM Remaining

    AFDM1 <- select_(.,Day, Replicate, Treatment, "AFDMRemaining")
    AFDM2 <- arrange(AFDM1, Treatment, Replicate)

# Calculate LN 
    
    Remaing<- AFDM2 %>%
      group_by(grp = cumsum(Day == 2)) %>% 
      complete(Day =  c(0, unique(Day)), fill = list(AFDMRemaining = meanControl * 100))%>%
      fill(Replicate, Treatment , .direction = 'updown')%>%
      mutate_(Ln_AFDM = lazyeval::interp(~log(a), a= as.name("AFDMRemaining")))
    
    Remaing <- as.data.frame(Remaing)
    
    return(Remaing)
}

AFDM()



# Slope -------------------------------------------------------------------

slope <- function(data,
                  Treatment, 
                  Replicate,
                  Day,
                  Ln_AFDM){
  fitted_models <- data  %>% group_by(Treatment, Replicate) %>% 
  do(model = lm(Ln_AFDM ~ Day, data = .)) 

  fitted_models$model 
  slope <- fitted_models %>% tidy(model) %>% print(n = Inf) # Calculate the slope and estimate
  r_squared <- fitted_models %>% glance(model) %>% print(n = Inf) # Calculate the r-squared and p-value
 # fitted_models %>% augment(model) %>% print(n = Inf) 
  
  return(slope)
  return(r_squared)
}

  
# Plots -------------------------------------------------------------------

by_trearment <- function(data)
  {
  ggplot(data, aes(x = Day, y = Ln_AFDM)) +
    geom_point() +
    geom_smooth(aes(colour=Treatment ), method = "lm", se = FALSE)
    }
  
  
by_replicate <- function(data)
  { 
  ggplot(data, (aes(x = Day, y = Ln_AFDM, colour = Replicate))) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ Treatment + Replicate)
  }
  

Replicate <- function(data)
  {
  ggplot(data,(aes(x = Day, y = Ln_AFDM, colour = Treatment))) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    # Splitting the single figure into multiple depending on treatment
    facet_wrap(~ Replicate)
  }
  

Treatment <- function(data)
  {
  ggplot(data,(aes(x = Day, y = Ln_AFDM, colour = factor(Replicate)))) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    # Splitting the single figure into multiple depending on treatment
    facet_wrap(~ Treatment)
  }
  
  