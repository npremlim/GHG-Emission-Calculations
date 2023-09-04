library(writexl)
library(openxlsx)
library(readxl)
args = commandArgs(trailingOnly=TRUE)
callibration_df <-read_excel(args[2])
callibration_df
CH4model <- lm(formula =CH4_PPM ~ CH4_Peak, data=callibration_df)
N2Omodel <- lm(formula =N2O_PPM ~ N2O_Peak, data=callibration_df)
CH4_c <- CH4model$coefficients[1]
CH4_m <- CH4model$coefficients[2]
N2O_c <- N2Omodel$coefficients[1]
N2O_m <- N2Omodel$coefficients[2]
CH4_Output <-CH4_m*callibration_df["CH4_Input"]+CH4_c
N2O_Output <-N2O_m*callibration_df["N2O_Input"]+N2O_c
df_out <- cbind(callibration_df, CH4_Output,N2O_Output)
colnames(df_out)[7] <- "CH4_Output"
colnames(df_out)[8] <- "N2O_Output"
write_xlsx(df_out,file.path(getwd(), "Callibration_Output.xlsx"))
plot(x = callibration_df$CH4_Peak,                          # True values on x-axis
     y = callibration_df$CH4_PPM,
     type = "o",
     xlab = "CH4 Peak",
     ylab = "CH4 PPM",
     main = "CH4 Callibration Curve")
abline(b = 1, a = 0) 
plot(x = callibration_df$N2O_Peak,                          # True values on x-axis
     y = callibration_df$N2O_PPM,
     type = "o",
     xlab = "N2O Peak",
     ylab = "N2O PPM",
     main = "N2O Callibration Curve")
abline(b = 1, a = 0)
df <- read_excel(args[1])
rad <- df["Chamber_Radius"]
baseArea <- pi*rad**2
ChamberVolCm3 <- baseArea*df$Chamber_Height
ChamberVolL <- ChamberVolCm3/1000
df2 <- cbind(df, ChamberVolCm3,ChamberVolL,baseArea)
colnames(df2)<- c("Plot","Lid_Length","Headspace","Extension_Length","Net_Chamber_Height","Chamber_Radius","Total_Volume_cm3","Total_Volume_L","Base_Area")
write_xlsx(df2,file.path(getwd(), "VolumeExport.xlsx"))



