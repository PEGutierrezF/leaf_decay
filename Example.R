



#--------------------------------------------
# Leaf litter decomposition example
# 28 Jun 2020
#PEGF
#--------------------------------------------
#

# Example Clasen-Rodriguez et al. 2019 

devtools::install_github("PEGutierrezF/leafdecay")
library(leafdecay)
library(tidyr) 
library(rlang)
library(dplyr)
library(plyr)
library(tidyverse) 
library(lme4)
library(broom)


leafdecay <- read.csv("LeafLitterDecomp.csv")
leafdecay
head(leafdecay)

# Control by manipulation -------------------------------------------------

control <- manipulation(data= leafdecay,
                        InitialWt= Initial_Dry_Weight,
                        FinalWt= Final_Dry_Weight,
                        Treatment= Control)
control


# Percentage of AFDM Remaining ---------------------------------------------

leafdecay
remaining <- AFDM(data      =  leafdecay,
                InitialWt  =  Initial_Dry_Weight,
                FinalWt =  Final_Dry_Weight,
                Frac.InitialWt =  Fraction_Initial_Weight,
                Frac.FinalWt =  Fraction_Final_Weight,
                Treatment =  Treatment,
                Day       =  Day,
                Replicate =  Replicate)

remaining


# Slope and r_squared  --------------------------------------------------

slope.k(data= remaining,
               Treatment  = Treatment, 
               Replicate  = Replicate,
               Day        = Day,
               Ln.AFDMrem = Ln.AFDMrem)


rsquared.k(data  = remaining,
         Treatment  = Treatment, 
         Replicate  = Replicate,
         Day        = Day,
         Ln.AFDMrem = Ln.AFDMrem)
    
 # Plots -------------------------------------------------------------------

plot.A(remaining)
plot.B(remaining)
plot.C(remaining)
plot.D(remaining)
plot.E(remaining)

