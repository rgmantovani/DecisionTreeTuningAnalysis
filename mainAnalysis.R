#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

mainAnalysis = function(parsed.obj) {

  cat("*******************************************\n")
  cat("* Automated Hyperparameter tuning Analysis\n")
  cat("*******************************************\n")

  devtools::load_all(path = ".")

  algo  = parsed.obj$algo
  space = parsed.obj$space

  #----------------------
  #----------------------

  checkmate::assertChoice(x = algo, choices = AVAILABLE.ALGOS, .var.name = "algo")

  n.files = list.files(path = paste0("data/hptuning_",space,"_space/", algo, "/results/"), recursive = TRUE)
  if(length(n.files) == 0) {
    stop(paste0("There is no result for algo: ", algo, "\n"))
  } else {
    cat(" * results found ... beginning the analysis ...\n")
  }

  cat("*******************************************\n")
  cat(paste0("* Doing Analysis for algo: ", algo,"\n"))
  cat("*******************************************\n")

  #----------------------
  #----------------------

  cat("* Checking required data for analyzes: \n")

  # ----------------------------
  # ----------------------------
  # Raw results in: data/hptuning_full_space/{algo} /results
  # For an {algo}, we must extract from raw data:
  # ----------------------------
  # ----------------------------
  

  # ----------------------------
  # * 01: extract all performances 
  # ----------------------------
  
  if(!checkSubdir(algo = algo, space = space, subdir="extracted_results")) {
    stop("Plase, run the script: \'01_extractRepResults.R\'\n")
  } else {
    cat("  - all performances overall repetitions already extracted: \t\tok\n")
  }
  
  # ----------------------------
  # ----------------------------
  
  # * 02: extract all optimization paths
  if(!checkSubdir(algo = algo, space = space, subdir="opt_paths")) {
    stop("Plase, run the script: \'02_extractOptPaths.R\'\n")
  } else {
    cat("  - all optimization paths were already extracted: \t\tok\n")
  }

  # ----------------------------
  # ----------------------------

  # * 03: extract models' statistics
  if(!checkSubdir(algo = algo, space = space, subdir="models_stats")) {
    stop("Plase, run the script: \'03_extractModelsStats.R\'\n")
  } else {
    cat("  - all optimization paths were already extracted: \t\tok\n")
  }

  # ----------------------------
  # ----------------------------

  # * 04: extract fanova files (data/algorithm/fullspace/fanova_input)
  # fAnova is external
  if(!checkSubdir(algo = algo, subdir = "fanova_output")) {
    stop("You did not generate fAnova info from HPs. Plase, generate them first.\n")
  } else {
    cat("  - all fanova results were already extracted: \t\tok\n")
  }

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
