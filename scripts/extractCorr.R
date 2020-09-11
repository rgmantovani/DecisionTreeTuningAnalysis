#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

options(warn=-1)
source("R/config.R")
source("R/getCorrData.R")

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

extractCorr = function(algo) {

  checkmate::assertChoice(x=algo, choices=AVAILABLE.ALGOS, .var.name="algo") 

  cor.dir = paste("data", algo, "corr", sep="/")
  if(!dir.exists(path = cor.dir)) {
    dir.create(path = cor.dir)
  }
  
  all.dirs = list.files(paste("data", algo, "results", sep="/"))
  aux.data = lapply(all.dirs, function(dataset) {
    print(dataset)
    ret = getCorrData(algo = algo, dataset = dataset, cor.dir = cor.dir) 
  })
  cat("\n Done ... \n")
}

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

options(echo = TRUE) 
args = commandArgs(trailingOnly = TRUE)

parseArgs = function(x) strsplit(sub("^--", "", x), "=")
argsDF = as.data.frame(do.call("rbind", parseArgs(args)))
argsL = as.list(as.character(argsDF$V2))
algo =  argsL[[1]]

extractCorr(algo = algo)

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------