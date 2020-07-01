



#--------------------------------------------
# Leaf litter decomposition example
# 28 Jun 2020
#PEGF
#--------------------------------------------
#

# Example Clasen-Rodriguez et al. 2019 

leafdecay <- read.csv("LeafLitterDecomp.csv")
leafdecay

# Control by manipulation -------------------------------------------------

leafdecay
control <- manipulation(data= leafdecay,
                        InitDryW = "Initial_Dry_Weight",
                        FinalDryW = "Final_Dry_Weight",
                        Treatment = "Treatment")
control


# Percentage of AFDM Remaining ---------------------------------------------

leafdecay
RioPiedras <- AFDM(data=leafdecay,
                InitDryW= "Initial_Dry_Weight",
                FinalDryW = "Final_Dry_Weight",
                FractIntW ="Fraction_Initial_Weight",
                FractFinW = "Fraction_Final_Weight",
                Treatment ="Treatment",
                Day= "Day",
                Replicate="Replicate")
RioPiedras



# # Slope and r_squared  --------------------------------------------------

Slope <- slope(data=RioPiedras,
                 Treatment=Treatment, 
                 Replicate=Replicate,
                 Day=Day,
                 Ln_AFDM=Ln_AFDM)
Slope

 # Plots -------------------------------------------------------------------

  by_treatment(RioPiedras)
  by_replicate(RioPiedras)  
  Replicate(RioPiedras)
  Treatment(RioPiedras)
  by_error(RioPiedras)
  by_errorBar(RioPiedras)

