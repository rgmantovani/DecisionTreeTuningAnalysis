#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

options(warn=-1)
source("../R/config.R")

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

generateFanovaData = function(algo, dataset) {

  tech.dir = paste0("../data/hptuning_full_space/", algo, "/results/", dataset, "/", algo, "/irace")
  reps = list.files(path = tech.dir, full.names = TRUE)[1:3]

  inner.aux = lapply(reps, function(rep) {
    
    job = paste0(rep, "/opt_params_", dataset, ".RData")
    load(job)

    traces = lapply(ret.params.list[[1]][[1]], function(trace) {
      df = as.data.frame(trace$opt.path)
      df$ber.test.mean = 1 - df$ber.test.mean
      return(df)
    })

    df.trace = do.call("rbind", traces)
    return(df.trace)
  })

  comp.trace = do.call("rbind", inner.aux)
  comp.trace$dob = comp.trace$eol = comp.trace$error.message = comp.trace$exec.time = NULL

  if(algo == "classif.J48") {
    comp.trace = comp.trace[, c("C", "M", "N", "O", "R", "B", "S", "A", "J", "ber.test.mean")]
  }

  # True/False = 1/0
  col.ids = which(sapply(comp.trace,class) == "logical")
  for(n in names(col.ids)) {
    comp.trace[, n] = as.numeric(comp.trace[,n])
  }

  comp.trace[is.na(comp.trace)] = 0
  colnames(comp.trace) = NULL
  return(comp.trace)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

createFanovaInputs = function(algo) {

  checkmate::assertChoice(x=algo, choices=AVAILABLE.ALGOS, .var.name="algo") 
  algo.name = gsub(x = algo, pattern="classif.", replacement = "")

  input.dir = paste("../data/hptuning_full_space", algo, "fanova_input", sep="/")
  if(!dir.exists(path = input.dir)) {
    dir.create(path = input.dir)
  }

  files = list.files(path = paste("../data/hptuning_full_space", algo, "results", sep="/"))
  aux = lapply(files, function(dataset) {
    print(dataset)
    comp.trace = generateFanovaData(algo = algo, dataset = dataset)
    output.file = paste0(input.dir, "/", dataset, ".csv")
    if(!file.exists(output.file)) {
      write.table(x = comp.trace, file = output.file, sep=",", row.names = FALSE)
    } else {
      cat("file already exists!\n")
    }
  })

  cat("\n Done ... \n")
}


#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

options(echo = TRUE) 
args = commandArgs(trailingOnly = TRUE)

parseArgs = function(x) strsplit(sub("^--", "", x), "=")
argsDF = as.data.frame(do.call("rbind", parseArgs(args)))
argsL = as.list(as.character(argsDF$V2))
algo =  argsL[[1]]

createFanovaInputs(algo = algo)

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
