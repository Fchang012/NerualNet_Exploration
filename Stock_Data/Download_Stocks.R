# R Script to get financial data from yahoo

# Libraries ---------------------------------------------------------------
require(quantmod)


# Setting WD --------------------------------------------------------------
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)


# Get Stock Info And Write ------------------------------------------------
wantedStocks <- c("VTI")

for (i in 1:length(wantedStocks)){
  getSymbols(wantedStocks[i], from=as.Date("08-01-01", format="%y-%m-%d"))
  saveRDS(
    get(wantedStocks[i]),
    file = paste(wantedStocks[i], ".rds", sep="")
    )
}
