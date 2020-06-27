



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

leafdecay <- read.csv("LeafLitterDecomp.csv")
leafdecay


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


# Example Clasen-Rodriguez et al. 2019 -----------------------------------

leafdecay
control <- manipulation(data= leafdecay,
                     InitDryW = "Initial_Dry_Weight",
                     FinalDryW = "Final_Dry_Weight",
                     Treatment = "Treatment")
control

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
    
    return(AFDM2)
}

AFDM()



# Example AFDM Classen-R 2019 ---------------------------------------------

leafdecay
remaing <- AFDM (data=leafdecay,
              InitDryW= "Initial_Dry_Weight",
              FinalDryW = "Final_Dry_Weight",
              FractIntW ="Fraction_Initial_Weight",
              FractFinW = "Fraction_Final_Weight",
              Treatment ="Treatment",
              Day= "Day",
              Replicate="Replicate")
remaing


# Adding Replicate 0 to dataframe -----------------------------------------

test <- remaing %>%
  group_by(grp = cumsum(Day == 2)) %>% 
  complete(Day =  c(0, unique(Day)), fill = list(AFDMRemaining = 0.98)) %>%
  fill(Replicate, Treatment , .direction = 'updown')
test



# Slope -------------------------------------------------------------------

  library(broom)
  
test

  fitted_models = test  %>% group_by(Treatment, Replicate) %>% do(model = lm(log(AFDMRemaining) ~ Day, data = .))
  fitted_models$model 
  fitted_models %>% tidy(model)
  fitted_models %>% glance(model)
  fitted_models %>% augment(model)

  




