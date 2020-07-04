



# Leaf decay packages -----------------------------
# 28 Mar 2020
#PEGF
#--------------------------------------------
#



# Alternative functions ---------------------------------------------------


# Control by manipulation -------------------------------------------------


control_manipulation <- function(data,
                                 InitDryW,
                                 FinalDryW,
                                 Treatment,
                                 Difference) {
  # Select the samples identified as Control
  control <- data %>% 
    group_by(Treatment) %>%
    filter(Treatment == "Control") %>%
    ungroup() 
  control <- as.data.frame(control)
  InitDryW   <- as.numeric(control[, InitDryW])
  FinalDryW  <- as.numeric(control[, FinalDryW])
  Difference <- (FinalDryW/InitDryW)
  control    <- cbind(control,Difference)
  
  mean_Control <- mean(control$Difference, na.rm = TRUE)
  
  return (mean_Control)      
}

control_manipulation()




# Adding Replicate 0 to dataframe -----------------------------------------

test <- remaing %>%
  group_by(grp = cumsum(Day == 2)) %>% 
  complete(Day =  c(0, unique(Day)), fill = list(AFDMRemaining = 98))%>%
  fill(Replicate, Treatment , .direction = 'updown')%>%
  mutate_(Ln_AFDM = lazyeval::interp(~log(a), a= as.name("AFDMRemaining")))

Remaing <- as.data.frame(test)
Remaing



# Old functions  ----------------------------------------------------------

manipulation <- function(data,
                         InitDryW,
                         FinalDryW,
                         Treatment,
                         Difference) {
  control <- data %>% 
    filter(Treatment == "Control") %>%
    select(InitDryW,FinalDryW) %>%
    mutate_(Difference = lazyeval::interp (~a/b,  a=as.name(FinalDryW),b=as.name(InitDryW)))
  
  meanControl <- mean(control$Difference, na.rm = TRUE)
  return (meanControl)
}
manipulation()

#####################################################

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
  
  Remaining<- AFDM2 %>%
    group_by(grp = cumsum(Day == 2)) %>% 
    complete(Day =  c(0, unique(Day)), fill = list(AFDMRemaining = meanControl * 100))%>%
    fill(Replicate, Treatment , .direction = 'updown')%>%
    mutate_(Ln_AFDM = lazyeval::interp(~log(a), a= as.name("AFDMRemaining")))
  
  Remaining <- as.data.frame(Remaining)
  
  return(Remaining)
}

AFDM()


##################################################################

#  fitted_models$model 
#  Slope <- fitted_models %>% tidy(model) %>% print(n = Inf) # Calculate the slope and estimate
# r_squared <- fitted_models %>% glance(model) %>% print(n = Inf) # Calculate the r-squared and p-value
# fitted_models %>% augment(model) %>% print(n = Inf) 

