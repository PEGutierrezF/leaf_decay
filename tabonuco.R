
library(rlang)
library(dplyr)
library(plyr)
library(tidyverse) 
library(lme4)

setwd("D:/Curriculum/02_ Articulos/00 In progress/221 tabonuco")
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



AFDM <- function (){
  wControl <- example %>% 
        filter(Treatment != "Control") %>%
        mutate(InitialDryMass = Initial_Dry_Weight*ControlManipulation) %>% # Corrects dry mass (laboratory) for mass lost from handling
        mutate(AFDMFraction =  (Fraction_Initial_Weight - Fraction_Final_Weight)/Fraction_Initial_Weight)%>%   # AFDM in the subsample
        mutate(AFDM_Initial = InitialDryMass*AFDMFraction) %>%   # AFDM in the initial sample
        mutate(AFDM_Final = Final_Dry_Weight*AFDMFraction) %>%   #AFDM in the Final sample
        mutate(AFDMRemaining = (AFDM_Final/AFDM_Initial*100))    # % AFDM Remaining
   AFDM1 <- select(wControl, Day, Replicate, Treatment,AFDMRemaining)
   AFDM2 <- arrange(AFDM1, Treatment, Replicate)
  
  return (AFDM2)
  }

AFDM()


# Slope -------------------------------------------------------------------

fits <- lmList(log(AFDMRemaining) ~ Day | Treatment , data=AFDM2)
summary(fits)


AFDM2 %>% add_row(Rank = 0, Country = "Nepal",   
                                      Population.2019 = 28608710,   
                                      Population.2018 = 28095714,    
                                      Growth.Rate = "1.83%") 





