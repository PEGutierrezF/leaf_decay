



#--------------------------------------------
# Leaf litter decomposition analysis
# 30 Apr 2020
#PEGF
#--------------------------------------------
#
library(leafdecay)
library(tidyr) 
library(rlang)
library(dplyr)
library(plyr)
library(tidyverse) 
library(lme4)
library(broom)

# Controlling by manipulation ----------------------------------------------

manipulation <- function(data,InitialWt,FinalWt,Treatment) {
  control <- data %>% 
    dplyr::filter(Treatment == "Control") %>%
    dplyr::select({{InitialWt}},{{FinalWt}}) %>%
    dplyr::mutate(Difference = {{FinalWt}}/{{InitialWt}})
  
   . <- mean(control$Difference, na.rm = TRUE)
   meanControl <- .*100
  return (meanControl)
}


# Percentage of AFDM Remaining --------------------------------------------

# IDWc= Initial drya mass corrected by manipulations
# AFDMFraction = Amount of AFDM in a subsample

AFDM <- function(data,
                 InitialWt,
                 FinalWt,
                 Frac.InitialWt,
                 Frac.FinalWt,
                 Treatment,
                 Day,
                 Replicate) {
# Calculate the control by manipulation
  control <- data %>% 
    dplyr::filter(Treatment == "Control") %>%
    dplyr::select({{InitialWt}},{{FinalWt}}) %>%
    dplyr::mutate(Difference = {{FinalWt}}/{{InitialWt}})
  meanControl <- mean(control$Difference, na.rm = TRUE)
  
# Corrects the initial dry mass by manipulation
  . <- data %>%
    dplyr::filter(Treatment != "Control") %>%
    dplyr::mutate(IDWc = {{InitialWt}} * meanControl) %>%  # Corrects dry mass (laboratory) for mass lost from handling
    
# Calculate the AFDM in the subsample
    dplyr::mutate(AFDMFraction = ({{Frac.InitialWt}} - {{Frac.FinalWt}}) / {{Frac.InitialWt}}) %>%   # AFDM in the subsample
    
# Calculate the AFDM in the corrected initial mass and in the final mass   
    dplyr::mutate(AFDM_Initial = IDWc * AFDMFraction) %>% # AFDM in the initial sample
    dplyr::mutate(AFDM_Final = {{FinalWt}} * AFDMFraction) %>%  #AFDM in the Final sample 
    
# Calculate the percentage of remaining mass
    dplyr::mutate(AFDMrem = (AFDM_Final/AFDM_Initial) * 100)     # % AFDM Remaining
  
  AFDM1 <- dplyr::select(., {{Day}}, {{Replicate}}, {{Treatment}}, AFDMrem)
  AFDM2 <- dplyr::arrange(AFDM1, {{Treatment}}, {{Replicate}})
  
# Calculate LN 
  Remaining <- AFDM2 %>%
    group_by(grp = cumsum(Day == 2)) %>% 
    complete(Day =  c(0, unique(Day)), fill = list(AFDMrem = meanControl * 100))%>%
    fill(Replicate, Treatment , .direction = 'updown')%>%
    dplyr::mutate(Ln.AFDMrem = log(AFDMrem))
  
  Remaining <- as.data.frame(Remaining)
  
  return(Remaining)
}


# Slope -------------------------------------------------------------------

slope <- function(data,
                  Treatment, 
                  Replicate,
                  Day,
                  Ln.AFDMrem){
  fitted_models <- data  %>% group_by(Treatment, Replicate) %>% 
  do(model = lm(Ln.AFDMrem ~ Day, data = .)) 

  broom::tidy(fitted_models,model) %>% print(n = Inf) # Calculate the slope and estimate

}


# rSquared ----------------------------------------------------------------


rSquared <- function(data,
                  Treatment, 
                  Replicate,
                  Day,
                  Ln.AFDMrem){
  fitted_models <- data  %>% group_by(Treatment, Replicate) %>% 
    do(model = lm(Ln.AFDMrem ~ Day, data = .)) 

  broom::glance(fitted_models,model) %>% print(n = Inf) # Calculate the r-squared and p-value

}
 
 
# Plots -------------------------------------------------------------------


plot.A <- function(data)
{
  data%>% # the names of the new data frame and the data frame to be summarised
    group_by(Treatment, Day) %>%   # the grouping variable
    dplyr::summarise(mean = mean(AFDMrem),  # calculates the mean of each group
                     sd = sd(AFDMrem), # calculates the standard deviation of each group
                     n = n(),  # calculates the sample size per group
                     SE = sd(AFDMrem)/sqrt(n()) # calculates the standard error of each group
    ) %>% 
    ggplot(aes(x = Day , y= mean, group = Treatment,color=Treatment))+
    geom_line(size=1)+ geom_point(size=2)  +
    #    geom_smooth(method = "lm", se=FALSE, color="blue", formula = y ~ x)+
    geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), width=0.5) + 
    xlab('Day') + ylab('AFDM remaining') +
    facet_wrap(~ Treatment)
}
plot.A(remaining)

plot.B <- function(data)
{
  ggplot(data,(aes(x = Day, y = AFDMrem, colour = factor(Replicate)))) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    # Splitting the single figure into multiple depending on treatment
    facet_wrap(~ Treatment)
}


plot.C <- function(data)
  {
  ggplot(data, aes(x = Day, y = AFDMrem)) +
    geom_point() +
    geom_smooth(aes(colour=Treatment ), method = "lm", se = FALSE)
    }
plot.C(remaining)
 


plot.D <- function(data)
{
  ggplot(data,(aes(x = Day, y = AFDMrem, colour = Treatment))) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    # Splitting the single figure into multiple depending on treatment
    facet_wrap(~ Replicate)
}



plot.E <- function(data)
  { 
  ggplot(data, (aes(x = Day, y = AFDMrem, colour = Replicate))) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ Treatment + Replicate)
  }
  



  







  
  by_errorBar <- function(data)
  {
    data%>% # the names of the new data frame and the data frame to be summarised
      group_by(Treatment) %>%   # the grouping variable
      dplyr::summarise(mean = mean(AFDMrem),  # calculates the mean of each group
                       sd = sd(AFDMrem), # calculates the standard deviation of each group
                       n = n(),  # calculates the sample size per group
                       SE = sd(AFDMrem)/sqrt(n()) # calculates the standard error of each group
      ) %>% 
      ggplot(aes(x = Treatment , y= mean))+
      geom_bar(stat="identity",  position=position_dodge()) +
      #    geom_smooth(method = "lm", se=FALSE, color="blue", formula = y ~ x)+
      geom_errorbar(aes(ymax=mean+SE, ymin=mean-SE), width=0.5, position=position_dodge(.9)) + 
      xlab('Treatment') + ylab('AFDM remaining') 
  }


