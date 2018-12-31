# Main NeuralNet File


# Libraries ---------------------------------------------------------------
require(quantmod)
require(data.table)
require(neuralnet)
require(nnet)
require(foreach)
require(parallel)
require(doParallel)


# Set Seed ----------------------------------------------------------------
set.seed(123)


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
  # 0 = x < 0.0
  # 1 = x = 0
  # 2 = x > 0.0
  stockInfo$daily.returns.classified <- 0.0
  stockInfo$daily.returns.classified[stockInfo$daily.returns >= 0.0, ] <- 1.0
  
  # Remove Adjusted, High, Low
  stockInfo <- stockInfo[,-c(2,3,6)]
  return(stockInfo)
}

VTI <- addTechnicalIndicators(VTI)

# Remove 1st 34 rows due to NA
VTI <- VTI[34:length(VTI$VTI.Close),]

# Split Technical Indicators and DailyReturns
TechnicalIndicators <- as.data.frame(VTI[,1:10])
TheDailyReturns <- as.data.frame(VTI[,11:12])

# Shifting Daily Returns
TheDailyReturns$daily.returns.classified.shifted <- shift(TheDailyReturns$daily.returns.classified, type = 'lead')

# Encode as a one hot vector multilabel data
TheDailyReturns <- cbind(TheDailyReturns, 
             class.ind(as.factor(TheDailyReturns$daily.returns.classified.shifted))
             )

# Set labels name
names(TheDailyReturns) <- c(names(TheDailyReturns)[1:3],"Loss","Gain")

# Max-Min Normalization ---------------------------------------------------
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

maxminDF <- as.data.frame(lapply(TechnicalIndicators, normalize))

# Re-combine
maxminDF$'Loss' <- TheDailyReturns$`Loss`
maxminDF$'Gain' <- TheDailyReturns$`Gain`

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
# softplus function
softplus <- function(x) {log(1+exp(x))}

theFormula <- paste(paste(colnames(trainset)[11:12], collapse = " + "), " ~ ", paste(colnames(trainset)[1:10], collapse = " + "))
nn <- neuralnet(theFormula, data = trainset, hidden = c(3,2), linear.output = FALSE, lifesign = 'full', rep = 2, threshold = 0.01, learningrate = 0.0001, act.fct = softplus, err.fct = 'sse', stepmax = 250000)
plot(nn)

# Compute predictions and accuracy function
Prediction_And_Accuracy <- function(nn, dataset){
  # Compute predictions
  pr.nn <- compute(nn, dataset[,1:10])
  
  # Extract results
  pr.nn.results <- pr.nn$net.result
  
  # Accuracy (training set)
  original_values <- max.col(dataset[, 11:12])
  pr.nn.results_2 <- max.col(pr.nn.results)
  accuracy <- mean(pr.nn.results_2 == original_values)
  ConfusionMatrix <- table(original_values, pr.nn.results_2)
  
  return(list(pr.nn.results,
              original_values,
              pr.nn.results_2,
              accuracy,
              ConfusionMatrix))
}

trainset.results <- Prediction_And_Accuracy(nn, trainset)
testset.results <- Prediction_And_Accuracy(nn, testset)

trainset.results[[4]]
testset.results[[4]]
