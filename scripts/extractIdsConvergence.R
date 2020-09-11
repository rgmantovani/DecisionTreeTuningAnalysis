#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

options(warn=-1)
source("../R/config.R")
source("../R/getIdsData.R")

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

extractIds = function(algo) {

  checkmate::assertChoice(x = algo, choices = AVAILABLE.ALGOS, .var.name = "algo")

  ids.dir = paste("data", algo, "ids", sep="/")
  if(!dir.exists(path = ids.dir)) {
    dir.create(path = ids.dir)
  }

  all.dirs = list.files(paste("data", algo, "results", sep="/"))
  aux.data = lapply(all.dirs, function(dataset) {
    print("============================")
    print(dataset)
    ret = getIdsData(algo = algo, dataset = dataset, ids.dir = ids.dir)
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

extractIds(algo = algo)

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------
