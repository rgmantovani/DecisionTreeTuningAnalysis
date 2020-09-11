#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

mainAnalysis = function(parsed.obj) {

  cat("*******************************************\n")
  cat("* Automated Hyperparameter tuning Analysis\n")
  cat("*******************************************\n")

  devtools::load_all(pkg = ".")

  algo  = parsed.obj$algo
  space = parsed.obj$space

  #----------------------
  #----------------------

  checkmate::assertChoice(x = algo, choices = AVAILABLE.ALGOS,
    .var.name = "algo")

  n.files = list.files(path = paste0("data/", algo), recursive = TRUE)
  if(length(n.files) == 0) {
    stop(paste0("There is no result for algo: ", algo, "\n"))
  }

  cat("*******************************************\n")
  cat(paste0("* Doing Analysis for algo: ", algo,"\n"))
  cat("*******************************************\n")

  #----------------------
  #----------------------

  cat("* Checking required info for analyse: \n")


  # generates convergence ids (iterations with best solutions)
  if(!checkSubdir(algo = algo, subdir="ids")) {
    stop("You did not generate info tuning. Plase, run the \'extractRepResults.R\' script.\n")
  }
  cat("  - performances overall repetitions: \t\tok\n")
  #
  # # generates convergence ids (iterations with best solutions)
  # if(!checkSubdir(algo = algo, subdir="ids")) {
  #   stop("You did not generate info tuning. Plase, run the \'extractIds.R\' script first.\n")
  # }
  # cat("  - ids: \t\tok\n")
  #
  # # generates model interpretability measures
  # if(!checkSubdir(algo = algo, subdir="models")) {
  #   stop("You did not generate info from models. Plase, run the \'extractModels.R\' script first.\n")
  # }
  # cat("  - models: \t\tok\n")
  #
  # # generates correlation bewteen hyperparameters
  # if(!checkSubdir(algo = algo, subdir = "corr")) {
  #   stop("You did not generate correlation info from HPs. Plase, run the \'extractCorr.R\' script first.\n")
  # }
  # cat("  - correlation: \tok\n")
  #
  # # fAnova is external
  # if(!checkSubdir(algo = algo, subdir = "fanova")) {
  #   stop("You did not generate fAnova info from HPs. Plase, generate them first.\n")
  # }
  # cat("  - fanova: \t\tok\n")
  # cat("*******************************************\n")


  #----------------------
  #----------------------

  # Calling Analysis
  runAnalysis(algo = algo)

  cat("* Done\n")
  cat("*******************************************\n")
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

options(echo = TRUE)
args = commandArgs(trailingOnly = TRUE)

option_list = list(
  optparse::make_option(c("--algo"),  type="character", action="store",
    help = "ML algorithm"),
  optparse::make_option(c("--space"), type="character", action="store",
    help = "HP space (full or reduced)")
)

parsed.obj = optparse::parse_args(optparse::OptionParser(option_list=option_list), args = args)
# print(parsed.obj)

# Calling execution
mainAnalysis(parsed.obj = parsed.obj)

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
