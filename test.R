


head(leafdecay)

AFDM1 <- function(data,
                 InitDryW,
                 FinalDryW,
                 FractIntW,
                 FractFinW,
                 Treatment,
                 Day,
                 Replicate,
                 Difference,
                 control) {

    control <- data %>% 
      filter(Treatment == "Control") %>%
      select_(InitDryW,FinalDryW) %>%
      mutate_(Difference = lazyeval::interp (~a/b,  a=as.name(FinalDryW),b=as.name(InitDryW)))
    
    meanControl <- mean(control$Difference, na.rm = TRUE)

  
  . <- data %>%
  filter(Treatment != "Control") %>%
  mutate_(IDWc = lazyeval::interp (~a/b, a=as.name(InitDryW), b=as.name("meanControl")))%>% # Corrects dry mass (laboratory) for mass lost from handling
  mutate_(AFDMFraction = lazyeval::interp (~(a-b)/a, a= as.name (FractIntW), b= as.name (FractFinW))) %>%
    mutate_(AFDM_Initial = lazyeval::interp (~a*b, a= as.name("IDWc"), b= as.name("AFDMFraction"))) %>% # AFDM in the initial sample
    mutate_(AFDM_Final = lazyeval::interp (~a*b, a= as.name(FinalDryW), b= as.name("AFDMFraction")))%>%
    mutate_(AFDMRemaining = lazyeval::interp (~(a/b)*100, a= as.name("AFDM_Final"), b= as.name("AFDM_Initial")))
  
  AFDM1 <- select_(.,Day, Replicate, Treatment, "AFDMRemaining")
  AFDM2 <- arrange(AFDM1, Treatment, Replicate)
  
  return(AFDM2)
}


remaing <- AFDM1 (data=leafdecay,
                 InitDryW= "Initial_Dry_Weight",
                 FinalDryW = "Final_Dry_Weight",
                 FractIntW ="Fraction_Initial_Weight",
                 FractFinW = "Fraction_Final_Weight",
                 Treatment ="Treatment",
                 Day= "Day",
                 Replicate="Replicate")

remaing




