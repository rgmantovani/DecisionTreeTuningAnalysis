#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

options(warn=-1)
source("../R/config.R")
source("../R/extractTrees.R")
library("mlr")

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

.getDatasetResults = function(algo, dataset, algo.dir) {

  local.file = paste0(algo.dir, "/", dataset, ".RData")
  if(file.exists(file = local.file)) {
    cat(" - models already extracted .\n")
    load(local.file) #df.full
  } else {

    tun.paths = gsub(x = algo.dir, pattern = "models_stats", replacement = "")
    tun.paths = paste0(tun.paths, "/results/", dataset, "/", algo)
    
    tun.techniques = list.files(path = tun.paths)
    aux.tun = lapply(tun.techniques, function(tun) {
      print(tun)
      return(.getTuningResults(algo = algo, dataset = dataset, tun = tun, tun.paths = tun.paths))
    })

    df.full = do.call("rbind", aux.tun)
    save(df.full, file = local.file)
  }
  return(df.full)
}

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

.getTuningResults = function(algo, dataset, tun, tun.paths) {

  reps.paths = paste0(tun.paths, "/", tun)
  reps = list.files(path = reps.paths)

  aux.rep = lapply(reps, function(rep) {
    cat(rep, "\n")
    return(.getRepResults(algo = algo, dataset = dataset, tun = tun, rep = rep, reps.paths = reps.paths))
  })
  cat("\n")

  df.tun = data.frame(do.call("rbind", aux.rep))
  df.tun$tun = tun
  return(df.tun)
}

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

.getRepResults = function(algo, dataset, tun, rep, reps.paths) {


  job = paste0(reps.paths, "/", rep, "/opt_params_", dataset, ".RData")

  if(!file.exists(job) & tun != "defaults") {
    print(job2)
   return(NULL)
  }

  data.df = RWeka::read.arff(file = paste0("../data/datasets/", dataset, ".arff"))
  task = mlr::makeClassifTask(id = dataset, data = data.df, target = "Class")
  lrn  = mlr::makeLearner(cl = algo)

  seed = as.integer(gsub(x=rep, pattern="rep", replacement=""))
  set.seed(seed = seed)
  options(mlr.debug.seed = seed)
        
  if(algo == "classif.J48") {
    df.models = extractJ48Trees(  job = job, task = task, lrn = lrn, tun = tun, seed = seed)
  } else if(algo == "classif.rpart") {
    df.models = extractRpartTrees(job = job, task = task, lrn = lrn, tun = tun, seed = seed)
  } else if(algo == "classif.ctree") {
    df.models = extractCTreeTrees(job = job, task = task, lrn = lrn, tun = tun, seed = seed)
  }
  else {
    stop("- Invalid classifier.\n")
  }
  return(df.models) 
}

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

extractModels = function(algo) {

  checkmate::assertChoice(x=algo, choices=AVAILABLE.ALGOS, .var.name="algo") 

  algo.dir = paste("../data/hptuning_full_space", algo, "models_stats", sep="/")
  if(!dir.exists(path = algo.dir)) {
    dir.create(path = algo.dir)
  }
  
  results = list.files(path = paste("../data/hptuning_full_space", algo, "results", sep="/"))
  aux.data = lapply(results, function(dataset) {
    print(dataset)
    ret = .getDatasetResults(algo = algo, dataset = dataset, algo.dir = algo.dir) 
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

extractModels(algo = algo)

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------