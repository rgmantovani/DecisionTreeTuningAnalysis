#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

runAnalysis = function(algo = algo, performance = TRUE, models = TRUE, runtime = TRUE, 
  ids = TRUE, correlation = TRUE, curves = TRUE) {

  if(!dir.exists(path="output")) {
    dir.create(path="output")
  }

  #----------------------
  #----------------------

  cat(" * Loading required data \n")

  algo.path    = paste("data", algo, "results", sep="/")
  all.dirs     = list.files(path = algo.path, full.names = TRUE)

  # all results from 30 repetitions
  all.results  = getAllRepetitions(algo = algo, all.dirs = all.dirs)

  # statistical results
  df.stats     = getStatsDf(all.results = all.results)

  # average results
  av.results   = data.frame(do.call("rbind", lapply(all.results, colMeans)))

  # list with datasets names
  datasets     = gsub(x = all.dirs, pattern = paste0("data/|", algo, "/results/"), replacement = "")
  rownames(av.results) = datasets

  # save table to run Friedman stat tests

  write.table(x = av.results, file = paste0("output/avg_", algo, ".txt"),  
    sep = "\t", col.names = FALSE, row.names = FALSE)

  #----------------------
  #----------------------

  ids.order = NULL

  if(performance == TRUE) {
    cat(" * Average performance plot ... ")
    obj = averagePerformancePlot(av.results = av.results, df.stats = df.stats, put.annotations = TRUE)
    ids.order = obj$ids.order

    ggsave(obj$g, file = paste0("output/", algo,"_AvgPerf.pdf"), dpi = 500, 
      width = 7.5, height = 2.6, units = "in")
    cat("ok\n")
  }

  if(models == TRUE) {
    cat(" * Average tree size plot ... ")
    df.info = getModelsInfo(algo = algo)
    g = getTreeSizePlot(df.info = df.info, ids.order  = ids.order, measure = "median")

    ggsave(g, file = paste0("output/", algo,"_TreeSize.pdf"), dpi = 500, 
      width = 7.5, height = 1.6, units = "in")
    cat("ok\n")
  }

  # TODO: generate again when Irace be finished (for J48)
  # TODO: generate again when all be finished (for CART)
  if(runtime == TRUE) {
    cat(" * Runtime plot ... ")
    df.training = getTrainingTime(algo = algo, all.dirs = all.dirs)
    df.tuning   = getTuningTimes(algo = algo, all.dirs = all.dirs)
    df.time     = rbind(df.training, df.tuning)

    g = getRuntimePlot(df.time = df.time)
    ggsave(g, file = paste0("output/", algo,"_runtime.pdf"), dpi = 500, 
      width = 8, height = 4.6, units = "in")
    cat("ok\n")
  }

  # if(ids == TRUE) {

  # }

  # TODO: generate again when Irace be finished (for J48)
  # TODO: generate again when all be finished (for CART)
  # if(correlation == TRUE) {
  #   # Correlation plot
  #   df.cor = getCorrelations(all.dirs = all.dirs)
  #   cat(" - Spearman correlation plot\n")
  #   getCorPlots(df.cor = df.cor)
  # }

  # Just for some specific dataset ids
  if(curves == TRUE) {

  }



}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
