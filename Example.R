



#--------------------------------------------
# Leaf litter decomposition example
# 28 Jun 2020
#PEGF
#--------------------------------------------
#

# Example Clasen-Rodriguez et al. 2019 

leafdecay <- read.csv("LeafLitterDecomp.csv")
leafdecay
head(leafdecay)

# Control by manipulation -------------------------------------------------

control <- manipulation(data      = leafdecay,
                        InitialWt  = Initial_Dry_Weight,
                        FinalWt = Final_Dry_Weight,
                        Treatment = Control)
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

 slope(data  = remaining,
               Treatment  = Treatment, 
               Replicate  = Replicate,
               Day        = Day,
               Ln.AFDMrem = Ln.AFDMrem)


   rSquared(data  = remaining,
         Treatment  = Treatment, 
         Replicate  = Replicate,
         Day        = Day,
         Ln.AFDMrem = Ln.AFDMrem)
    
 # Plots -------------------------------------------------------------------

  by_treatment(remaing)
  by_replicate(remaing)  
  Replicate(remaing)
  Treatment(remaing)
  by_error(remaing)
  by_errorBar(remaing)

