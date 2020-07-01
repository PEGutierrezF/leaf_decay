
control <- data.frame(Day=c(0,0,0,0,0,0),
                  Replica=c(1,1,1,1,1,1),
                  Initial_Dry_Weight=c(5.010,5.010,5.010,5.010,5.010,5.000),
                  Final_Dry_Weight=c(4.990,4.940,4.840,4.820,4.960,4.970),
                  InitiaFraction=c(1.1071,1.1964,1.0647,1.0005,1.0453,1.1212),
                  FinalFraction=c(0.3858,0.3504,0.4248,0.3333,0.3417,0.3467),
                  Treatment=c("Control","Control","Control","Control","Control","Control"))
control

sites <-data.frame(Day=c(2,4,8,16,32,44),
                   Replica=c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3),
                   Initial_Dry_Weight=c(5.000,5.000,5.000,5.000,5.01,5.000,5.000,5.000,
                                        5.000,5.000,5.000,5.01,5.01,5.01,5.000,5.000,5.000,5.000),
                   Final_Dry_Weight=c(4.65,4.63,4.67,4.64,4.37,4.37,4.17,3.72,4.12,4,3.99,3.64,
                                      4.26,3.3,3.47,3.7,3.75,3.3),
                InitiaFraction=c(1.0081,1.0972,1.1307,1.0898,1.075,1.0295,1.0956,1.042,1.0876,
                 1.006,1.1052,1.0922,1.0472,1.0843,1.0177,1.0143,1.1112,1.0061),
                FinalFraction=c(0.3229,0.3605,0.3304,0.3489,0.3181,0.2948,0.4098,0.3762,0.3787,
                0.3345,0.3595,0.3511,0.3921,0.3908,0.3385,0.347,0.3366,0.3318),
                Treatment=c("CC","CC","CC","CC","CC","CC","CC","CC","CC","CC","CC","CC","CC",
                    "CC","CC","CC","CC","CC"))

sites

total <- dplyr::bind_rows(control,sites)
total


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


control <- manipulation(data= total,
                        InitDryW = "Initial_Dry_Weight",
                        FinalDryW = "Final_Dry_Weight",
                        Treatment = "Treatment")
control




example<- data.frame(Day=c(2,4,8,16,32,44,2,4,8,16,32,44,2,4,8,16,32,44),
          Replicate=c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,
                      1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,
                      1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3),
          Treatment=c("CC","CC","CC","CC","CC","CC","CC","CC","CC","CC","CC","CC","CC","CC","CC","CC","CC","CC",
                     "HP","HP","HP","HP","HP","HP","HP","HP","HP","HP","HP","HP","HP","HP","HP","HP","HP","HP",
                     "LL","LL","LL","LL","LL","LL","LL","LL","LL","LL","LL","LL","LL","LL","LL","LL","LL","LL"),
          AFDM=c(94.669342,94.465752,84.897023,81.435993,86.556221,75.328294,94.262162,88.791240,75.735474,81.232403,
                 67.050593,76.346244,95.076522,88.968823,83.879073,73.958836,70.645724,67.184695,99.763156,92.022673,
                 92.245362,74.513934,50.083136,36.979418,94.872932,86.353037,81.843173,67.795465,46.622106,18.323099,
                 95.089932,93.244212,81.679814,65.352385,18.286525,7.517794,99.559972,86.759404,84.693433,79.196504,
                 67.456961,54.765706,94.074014,87.543693,82.492548,72.333367,51.304676,51.304676,98.340870,86.322153,
                 87.950873,84.693433,63.316485,63.723665))
example

