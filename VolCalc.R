library(writexl)
library(openxlsx)
library(readxl)
args = commandArgs(trailingOnly=TRUE)
filename=args[1]
df <- read_excel(args[1])
rad <- df["Chamber_Radius"]
baseArea <- pi*rad**2
ChamberVolCm3 <- baseArea*df$Chamber_Height
ChamberVolL <- ChamberVolCm3/1000
df2 <- cbind(df, ChamberVolCm3,ChamberVolL,baseArea)
write_xlsx(df2,file.path(getwd(), "TestExport.xlsx"))

  



