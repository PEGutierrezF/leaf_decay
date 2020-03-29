



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


