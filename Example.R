



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

control <- manipulation(data= leafdecay,
                        InitDryW = Initial_Dry_Weight,
                        FinalDryW = Final_Dry_Weight,
                        Treatment = Control)
control


# Percentage of AFDM Remaining ---------------------------------------------

leafdecay
remaing <- AFDM(data= leafdecay,
                InitDryW= Initial_Dry_Weight,
                FinalDryW = Final_Dry_Weight,
                FractIntW = Fraction_Initial_Weight,
                FractFinW = Fraction_Final_Weight,
                Treatment = Treatment,
                Day = Day,
                Replicate= Replicate)

remaing


# # Slope and r_squared  --------------------------------------------------

Slope <- slope(data=remaing,
                 Treatment=Treatment, 
                 Replicate=Replicate,
                 Day=Day,
                 Ln_AFDM=Ln_AFDM)


 # Plots -------------------------------------------------------------------

  by_treatment(remaing)
  by_replicate(remaing)  
  Replicate(remaing)
  Treatment(remaing)
  by_error(remaing)
  by_errorBar(remaing)

