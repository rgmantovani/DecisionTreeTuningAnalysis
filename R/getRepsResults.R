#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

extractPerformancesDf = function(data.dir, algo, dataset) {

  tech.aux = lapply(INNER.NAMES, function(tech) {
    tech.dir = paste0(data.dir, "/", algo, "/", tech)
    if(!dir.exists(path = tech.dir)) {
      return (rep(NA, 30))
    }

    rep.dirs = list.files(path = tech.dir, full.names = TRUE)
    rep.aux = lapply(rep.dirs, function(rep) {
      file = paste0(rep, "/perf_", dataset, ".RData")
      read = try(suppressWarnings(load(file, verbose = FALSE)), silent = TRUE)

      if (inherits(read, "try-error")) {return(NA)}
      perfs = 1 - ret.perf$ber
      return(perfs)
    })

    ret = do.call("rbind", rep.aux)
    length(ret) = 30
    return(ret)
  }) # tech.aux

  df = data.frame(do.call("cbind", tech.aux))
  df$dataset = dataset

  colnames(df)[1:7] = TECHNIQUES
  return(df)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getRepsResults = function(algo, all.dirs) {

  aux = lapply(all.dirs, function(data.dir) {

    pattern = paste0("../data/full_space/|../data/reduced_space/|",algo,"|/results/")
    dataset = gsub(x = data.dir, pattern = pattern, replacement = "")
    cat("Dataset: ", dataset, "\n")

    # ----------------
    # export file - thus, if a new dataset is inserted, we dont need to extract again
    exported.file = gsub(x = data.dir, pattern = "results", replacement = "extracted_results")
    exported.file = paste0(exported.file, ".RData")

    if(!file.exists(exported.file)) {
      cat("\t - exported for the first time.\n")
      df = extractPerformancesDf(data.dir = data.dir, algo = algo, dataset = dataset)
      save(df, file = exported.file)
    } else {
      cat("\t - loading existing file.\n")
      load(exported.file)
    }
    # ----------------

    return(df)
  }) #aux
  return(aux)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
