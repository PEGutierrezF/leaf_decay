


head(leafdecay)

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
    dplyr::filter(Treatment == "Control") %>%
    dplyr::select({{InitDryW}},{{FinalDryW}}) %>%
    dplyr::mutate(Difference = {{FinalDryW}}/{{InitDryW}})
  meanControl <- mean(control$Difference, na.rm = TRUE)

# Corrects the initial dry mass by manipulation
  . <- data %>%
    dplyr::filter(Treatment != "Control") %>%
    dplyr::mutate(IDWc = {{InitDryW}} * meanControl) %>%  # Corrects dry mass (laboratory) for mass lost from handling
  
# Calculate the AFDM in the subsample
    dplyr::mutate(AFDMFraction = ({{FractIntW}} - {{FractFinW}}) / {{FractIntW}}) %>%   # AFDM in the subsample
  
# Calculate the AFDM in the corrected initial mass and in the final mass   
    dplyr::mutate(AFDM_Initial = IDWc * AFDMFraction) %>% # AFDM in the initial sample
    dplyr::mutate(AFDM_Final = {{FinalDryW}} * AFDMFraction) %>%  #AFDM in the Final sample 

# Calculate the percentage of remaining mass
  dplyr::mutate(AFDMRemaining = (AFDM_Final/AFDM_Initial) * 100)     # % AFDM Remaining
  
  AFDM1 <- dplyr::select(., {{Day}}, {{Replicate}}, {{Treatment}}, AFDMRemaining)
  AFDM2 <- dplyr::arrange(AFDM1, {{Treatment}}, {{Replicate}})
  
# Calculate LN 
  
  Remaining <- AFDM2 %>%
    group_by(grp = cumsum(Day == 2)) %>% 
    complete(Day =  c(0, unique(Day)), fill = list(AFDMRemaining = meanControl * 100))%>%
    fill(Replicate, Treatment , .direction = 'updown')%>%
    dplyr::mutate(Ln_AFDM = log(AFDMRemaining))
  
  Remaining <- as.data.frame(Remaining)
  
  return(Remaining)
}


    
remaing <- AFDM(data= leafdecay,
                InitDryW= Initial_Dry_Weight,
                FinalDryW = Final_Dry_Weight,
                FractIntW = Fraction_Initial_Weight,
                FractFinW = Fraction_Final_Weight,
                Treatment = Treatment,
                Day = Day,
                Replicate= Replicate)

remaing



  

    control <- data %>% 
      filter(Treatment == "Control") %>%
      select_(InitDryW,FinalDryW) %>%
      mutate_(Difference = lazyeval::interp (~a/b,  a=as.name(FinalDryW),b=as.name(InitDryW)))
    
    meanControl <- mean(control$Difference, na.rm = TRUE)

  
  . <- data %>%
  filter(Treatment != "Control") %>%
  mutate_(IDWc = lazyeval::interp (~a*b, a=as.name(InitDryW), b=as.name("meanControl")))%>% # Corrects dry mass (laboratory) for mass lost from handling
  mutate_(AFDMFraction = lazyeval::interp (~(a-b)/a, a= as.name (FractIntW), b= as.name (FractFinW))) %>%
    mutate_(AFDM_Initial = lazyeval::interp (~a*b, a= as.name("IDWc"), b= as.name("AFDMFraction"))) %>% # AFDM in the initial sample
    mutate_(AFDM_Final = lazyeval::interp (~a*b, a= as.name(FinalDryW), b= as.name("AFDMFraction")))%>%
    mutate_(AFDMRemaining = lazyeval::interp (~(a/b)*100, a= as.name("AFDM_Final"), b= as.name("AFDM_Initial")))
  return(.)
  AFDM1 <- select_(.,Day, Replicate, Treatment, "AFDMRemaining")
  AFDM2 <- arrange(AFDM1, Treatment, Replicate)
  
  return(AFDM)
}






remaing1 <- AFDM(data=leafdecay,
                InitDryW=  Initial_Dry_Weight,
                FinalDryW = Final_Dry_Weight,
                FractIntW =Fraction_Initial_Weight,
                FractFinW =Fraction_Final_Weight,
                Treatment = Treatment,
                Day= Day,
                Replicate= Replicate)

remaing1



df <- data.frame(a=c(1,1,1,2,2), b=1:5)
df

df %>%
  group_by(a) %>%
  do(bind_rows(data.frame(a=.$a[1], b=0), ., data.frame(a=.$a[1], b=10)))

