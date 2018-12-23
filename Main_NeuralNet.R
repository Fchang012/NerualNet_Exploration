# Main NeuralNet File


# Libraries ---------------------------------------------------------------
require(quantmod)


# Setting WD --------------------------------------------------------------
# this.dir <- dirname(parent.frame(2)$ofile)
# setwd(this.dir)


# Import Stock Data -------------------------------------------------------
for (i in 1:length(list.files("./Stock_Data/", pattern = ".rds$"))){
  inputFileString <- paste("./Stock_Data/", list.files("./Stock_Data/", pattern = ".rds$")[i], sep = "")
  assign(unlist(strsplit(list.files("./Stock_Data/", pattern = ".rds$")[i], "\\."))[1], readRDS(inputFileString))
}
