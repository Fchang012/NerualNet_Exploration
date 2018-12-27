# Main NeuralNet File


# Libraries ---------------------------------------------------------------
require(quantmod)
require(data.table)
require(neuralnet)
require(nnet)

# Setting WD --------------------------------------------------------------
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
  
  # Remove Adjusted
  stockInfo <- stockInfo[,-6]
  return(stockInfo)
}

VTI <- addTechnicalIndicators(VTI)

# Remove 1st 34 rows due to NA
VTI <- VTI[34:length(VTI$VTI.Close),]

# Split Technical Indicators and DailyReturns
TechnicalIndicators <- as.data.frame(VTI[,1:12])
TheDailyReturns <- as.data.frame(VTI[,13:14])

# Shifting Daily Returns
TheDailyReturns$daily.returns.classified.shifted <- shift(TheDailyReturns$daily.returns.classified, type = 'lead')

# Encode as a one hot vector multilabel data
TheDailyReturns <- cbind(TheDailyReturns, 
             class.ind(as.factor(TheDailyReturns$daily.returns.classified.shifted))
             )

# Set labels name
names(TheDailyReturns) <- c(names(TheDailyReturns)[1:3],"Loss_0.02","Mid","Gain_0.02")

# Max-Min Normalization ---------------------------------------------------
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

maxminDF <- as.data.frame(lapply(TechnicalIndicators, normalize))

# Re-combine
maxminDF$'Loss_0.02' <- TheDailyReturns$`Loss_0.02`
maxminDF$'Mid' <- TheDailyReturns$Mid
maxminDF$'Gain_0.02' <- TheDailyReturns$`Gain_0.02`

# Training and Test Data ------------------------------------------------
# Test Train split on 80%
TrainTestDataSplit <- function(TheDF){
  trainLength <- round(length(TheDF[,1])*0.8)
  
  TrainDF <- TheDF[1:trainLength,]
  TestDF <- TheDF[(trainLength+1):length(TheDF[,1]),]
  
  return(list(TrainDF,TestDF))
}

trainset <- TrainTestDataSplit(maxminDF)[[1]]
testset <- TrainTestDataSplit(maxminDF)[[2]]


# Neural Network ----------------------------------------------------------
theFormula <- paste(paste(colnames(trainset)[13:15], collapse = " + "), " ~ ", paste(colnames(trainset)[1:12], collapse = " + "))
nn <- neuralnet(theFormula, data = trainset, hidden = c(3,1), linear.output = FALSE, lifesign = 'minimal', threshold = 0.01)
plot(nn)

# Compute predictions
pr.nn <- compute(nn, trainset[,1:12])

# Extract results
pr.nn.results <- pr.nn$net.result

# Accuracy (training set)
original_values <- max.col(trainset[, 13:15])
pr.nn.results_2 <- max.col(pr.nn.results)
mean(pr.nn.results_2 == original_values)
