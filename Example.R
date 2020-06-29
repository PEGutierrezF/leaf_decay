



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
remaing <- AFDM(data=leafdecay,
                InitDryW= "Initial_Dry_Weight",
                FinalDryW = "Final_Dry_Weight",
                FractIntW ="Fraction_Initial_Weight",
                FractFinW = "Fraction_Final_Weight",
                Treatment ="Treatment",
                Day= "Day",
                Replicate="Replicate")
remaing



# # Slope and r_squared  --------------------------------------------------

Slope <- slope (data=remaing,
                 Treatment=Treatment, 
                 Replicate=Replicate,
                 Day=Day,
                 Ln_AFDM=Ln_AFDM)


# Plots -------------------------------------------------------------------

  by_trearment(data=remaing)
  by_replicate(data=remaing, Day=Day, Ln_AFDM=Ln_AFDM, Replicate = Replicate)


