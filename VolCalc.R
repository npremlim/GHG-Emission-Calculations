library(writexl)
library(openxlsx)
library(readxl)
args = commandArgs(trailingOnly=TRUE)




#Volume calculations 
df <- read_excel(args[1])
headspace_mean <- rowMeans(df[,c("Headspace1_cm", "Headspace2_cm","Headspace3_cm","Headspace4_cm")], na.rm=TRUE)
extension_cm <- df$Extension_ft*30.48
chamber_Height<- extension_cm+7.62+headspace_mean 
rad <- df["Chamber_Radius"]
baseArea <- pi*rad**2
ChamberVolCm3 <- baseArea*chamber_Height
ChamberVolL <- ChamberVolCm3/1000
BaseAreaM3 <- baseArea/10000
VolAreaRatio <- ChamberVolL /BaseAreaM3 


df2 <- cbind(df,headspace_mean,extension_cm,chamber_Height, ChamberVolCm3,ChamberVolL,baseArea,BaseAreaM3,VolAreaRatio)

colnames(df2)<- c("Std_CH4_Peak","Std_N2O_Peak","Std_CH4_PPM","Std_N2O_PPM","CH4_Sample_Peak","N2O_Sample_Peak","Sample_Type","GC_Run","Date","Plot","Time","Chamber_Temp_C","Headspace1_cm","Headspace2_cm","Headspace3_cm","Headspace4_cm","Extension_ft","Chamber_Radius","HeadspaceMean_cm","Extension_cm","Chamber_Height","Total_Volume_cm3","Total_Volume_L","Base_Area","Base_Area_m3","VolumeRatio")


write_xlsx(df2,file.path(getwd(), "VolumeExport.xlsx"))



#callibration_df <-read_excel(args[2])
#callibration_df
#generating linear relation
CH4model <- lm(formula =Std_CH4_PPM ~ Std_CH4_Peak, data=df)
N2Omodel <- lm(formula =Std_N2O_PPM ~ Std_N2O_Peak, data=df)
CH4_c <- CH4model$coefficients[1]
CH4_m <- CH4model$coefficients[2]
CH4_r2 <- summary(CH4model)$r.squared
N2O_r2 <- summary(N2Omodel)$r.squared
#CH4_r2
#N2O_r2
# print alert is r2 value is below 0.95
N2O_c <- N2Omodel$coefficients[1]
N2O_m <- N2Omodel$coefficients[2]

#getting output values from linear relation 
CH4_Output <-CH4_m*df["CH4_Sample_Peak"]+CH4_c
N2O_Output <-N2O_m*df["N2O_Sample_Peak"]+N2O_c



#generating both curves 
plot(x = df$Std_CH4_Peak,                          
     y = df$Std_CH4_PPM,
     type = "o",
     xlab = "CH4 Peak",
     ylab = "CH4 PPM",
     main = "CH4 Callibration Curve")
abline(b = 1, a = 0) 
plot(x = df$Std_N2O_Peak, # True values on x-axis
     y = df$Std_N2O_PPM,
     type = "o",
     xlab = "N2O Peak",
     ylab = "N2O PPM",
     main = "N2O Callibration Curve")
abline(b = 1, a = 0)

#volume and concentration calculations 

N2O_Vol <-(((760*22.4)*(273+N2O_Output))/(760*273))
N2O_Conc <- ((N2O_Output/N2O_Vol*(0.044014)))

CH4_Vol <-(((760*22.4)*(273+CH4_Output))/(760*273))
CH4_Conc <- ((CH4_Output/CH4_Vol*(0.016043)))


              
df_out <- cbind(df2, CH4_Output,N2O_Output,N2O_Vol,N2O_Conc,CH4_Vol,CH4_Conc)

colnames(df_out)[27] <- "CH4_Output"
colnames(df_out)[28] <- "N2O_Output"
colnames(df_out)[29] <- "N2O_Volume"
colnames(df_out)[30] <- "N2O_Concentration"
colnames(df_out)[31] <- "CH4_Volume"
colnames(df_out)[32] <- "CH4_Concentration"
write_xlsx(df_out,file.path(getwd(), "MainExport.xlsx"))

CH4Conc <- df_out["CH4_Concentration"]
N20Conc <- df_out["N2O_Concentration"]
Plot <- df_out["Plot"]
Date <- df_out["Date"]
VolumeRatio <- df_out["VolumeRatio"]


fluxna <- data.frame(N20Conc, CH4Conc,Plot,Date,VolumeRatio)
flux <- fluxna[!is.na(fluxna$Plot),]
colnames(flux)[1] <- "N20Conc"
colnames(flux)[2] <- "CH4Conc"


counter = 0

results = data.frame(matrix(ncol = 12, nrow = 0))
colnames(results) = c("Plot","Date","N20Detection","CH4Detection","N20_Rsq","CH4_Rsq","N20_Linearity","CH4_Linearity","N20_Slope","CH4_Slope","N20_Flux","CH4_Flux")
leftboundN = 0 
leftboundC = 0 
Nres = "PASS"
Cres = "PASS"
NLin= "MISS"
CLin ="MISS"
vectorN <- c()
vectorC <- c()

time <- c(0,21,42,63)

for (row in 1:nrow(flux)) {
  vectorN <- c(vectorN, flux[row, "N20Conc"])
  vectorC <- c(vectorC, flux[row, "CH4Conc"])
  
  if ( counter ==0 ){
    leftboundN = flux[row, "N20Conc"]
    leftboundC = flux[row, "CH4Conc"]

  }
  
  if ( counter ==3 ){
    finalN = abs( leftboundN - flux[row, "N20Conc"])
    finalC = abs( leftboundC - flux[row, "CH4Conc"])
    if(finalN <0.000183){
     Nres ="FAIL"
     Ngrad=0
    }
    if(finalC <0.000183){
      Cres ="FAIL"
      Cgrad=0
    }
    
     
    
    
    counter =0
    
    Nrsq<-  summary(lm(vectorN~time))$r.squared
    Crsq<-  summary(lm(vectorC~time))$r.squared
    
    Ngrad <-coef(lm(vectorN~time))[2]
    Cgrad <-coef(lm(vectorC~time))[2]
    
    if(Nrsq >0.845){
      NLin ="LIN"
    }else{
      NLin ="MISS"
      Ngrad=0
    }
    if(Crsq >0.845){
      CLin ="LIN"
    }else{
      CLin ="MISS"
      Cgrad=0
    }
    
    plotno <- flux[row, "Plot"]
    dateno <- format(flux[row, "Date"])
   
    volumeratio <-flux[row,"VolumeRatio"]
    
    
  
   
  
    Cflux <- Cgrad*volumeratio*240*60
    Nflux <- Ngrad*volumeratio*240*60
    
    

    results[nrow(results) + 1,] = c(plotno,dateno,Nres,Cres,Nrsq,Crsq,NLin,CLin,Ngrad,Cgrad,Nflux,Cflux)

    
    
  
    
    
    
 

    vectorN <- c()
    vectorC <- c()
    
  }else{
    counter = counter + 1
    
  }
  


  
  
  
  
}







write_xlsx(results,file.path(getwd(), "Results.xlsx"))












