#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

mainAnalysis = function(algo) {

  cat("*******************************************\n")
  cat("* Automated Hyper-parameter tuning Analysis\n")
  cat("*******************************************\n")
 
  # load all files
  devtools::load_all(pkg = ".")

  # check Algorithm and if its dir with results is empty
  AVAILABLE.ALGOS = list.files(path="data/")
  checkmate::assertChoice(x=algo, choices=AVAILABLE.ALGOS, .var.name="algo") 

  n.files = list.files(path = paste0("data/", algo), recursive=TRUE)
  if(length(n.files) == 0) {
    stop(paste0("There is no result for algo: ", algo, "\n"))
  } 

  cat("* Checking required info for plots: \n")
  
  # generates Ids
  if(!checkSubdir(algo = algo, subdir="ids")) {
    stop("You did not generate info tuning. Plase, run the \'extractIds.R\' script first.\n")
  }
  cat("  - ids: \t\tok\n")
  

  # generates models interpretability measures
  if(!checkSubdir(algo = algo, subdir="models")) {
    stop("You did not generate info from models. Plase, run the \'extractModels.R\' script first.\n")
  }
  cat("  - models: \t\tok\n")
  
  # generates correlation
  if(!checkSubdir(algo = algo, subdir = "corr")) {
    stop("You did not generate correlation info from HPs. Plase, run the \'extractCorr.R\' script first.\n")
  }
  cat("  - correlation: \tok\n")
  

  # fAnova is external
  if(!checkSubdir(algo = algo, subdir = "fanova")) {
    stop("You did not generate fAnova info from HPs. Plase, generate them first.\n")
  }
  cat("  - fanova: \t\tok\n")
  
  # Calling Analysis
  runAnalysis(
    algo        = algo, 
    performance = TRUE, 
    models      = TRUE, 
    runtime     = TRUE, 
    ids         = TRUE, 
    convergence = TRUE
  )

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

# parse params and call main
# options(echo = TRUE) 
# args = commandArgs(trailingOnly = TRUE)

# Parse arguments (we expect the form --arg=value)
# parseArgs = function(x) strsplit(sub("^--", "", x), "=")
# argsDF = as.data.frame(do.call("rbind", parseArgs(args)))
# argsL = as.list(as.character(argsDF$V2))
# algo =  argsL[[1]]
# algo = "classif.J48"

mainAnalysis(algo = algo)

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
