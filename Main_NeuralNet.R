# Main NeuralNet File


# Libraries ---------------------------------------------------------------
require(quantmod)


# # Setting WD --------------------------------------------------------------
# this.dir <- dirname(parent.frame(2)$ofile)
# setwd(this.dir)


# Import Stock Data -------------------------------------------------------
for (i in 1:length(list.files("./Stock_Data/", pattern = ".rds$"))){
  inputFileString <- paste("./Stock_Data/", list.files("./Stock_Data/", pattern = ".rds$")[i], sep = "")
  assign(unlist(strsplit(list.files("./Stock_Data/", pattern = ".rds$")[i], "\\."))[1], readRDS(inputFileString))
}


# Setting Up Data With Technical Indicators -------------------------------
addTechnicalIndicators <- function(stockInfo){
  stockInfo$rsi <- RSI(stockInfo[,4])
  stockInfo$pctB <- BBands(stockInfo[,4])
  stockInfo$signal <- MACD(stockInfo[,4])
  stockInfo$daily.returns <- dailyReturn(stockInfo[,4])
  
  # Classify daily return -
  # 0 = x < -0.02
  # 1 = -0.02 <= x <= 0.02
  # 2 = x > 0.02
  stockInfo$daily.returns.classified <- 1.0
  stockInfo$daily.returns.classified[stockInfo$daily.returns < -0.02, ] <- 0.0
  stockInfo$daily.returns.classified[stockInfo$daily.returns > 0.02, ] <- 2.0
  return(stockInfo)
}

VTI <- addTechnicalIndicators(VTI)

