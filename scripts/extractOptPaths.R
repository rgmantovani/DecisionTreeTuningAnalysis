#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

options(warn=-1)
source("../R/config.R")
source("../R/getOptPath.R")

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

extractOptPaths = function(parsed.obj) {

  algo  = parsed.obj$algo
  space = parsed.obj$space

  cat("#Algo: ", algo, "\n")
  cat("#Space: ", space, "\n")

  checkmate::assertChoice(x = algo, choices = AVAILABLE.ALGOS, .var.name = "algo")
  algo.name = gsub(x = algo, pattern="classif.", replacement = "")

  # perf.dir = where to output
  opt.path.dir = paste0("../data/", space, "_space/", algo, "/opt_paths")
  dir.create(path = opt.path.dir, recursive = TRUE, showWarnings = FALSE)

  # input.dir = where to check results
  input.dir = paste0("../data/", space, "_space/", algo, "/results")
  if(!dir.exists(path = input.dir)) {
    stop("Sorry, but there is no file with results to extract. Please, add them first.\n")
  }

  # getting results from all repetitions
  cat("- Loading results: \n")
  all.dirs    = list.files(path = input.dir, full.names = TRUE)
  all.results = getOptPath(algo = algo, all.dirs = all.dirs)

  cat("\n Done ... \n")
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

# Calling execution
extractOptPaths(parsed.obj = parsed.obj)

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
