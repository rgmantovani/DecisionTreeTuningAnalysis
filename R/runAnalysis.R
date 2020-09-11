#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

runAnalysis = function(algo) {

  if(!dir.exists(path="output")) {
    dir.create(path="output")
  }

  #----------------------
  #----------------------

  cat(" * Loading required data \n")

  algo.name = gsub(x = algo, pattern="classif.", replacement = "")
  algo.path = paste("data", algo, "results", sep="/")
  all.dirs  = list.files(path = algo.path, full.names = TRUE)

  # all results from 30 repetitions
  all.results = getRepsResults(algo = algo, all.dirs = all.dirs)

  # statistical results
  df.stats = getStatsDf(all.results = all.results)

  # average results
  av.results = data.frame(do.call("rbind", lapply(all.results, colMeans, TRUE)))

  # list with datasets names
  datasets = gsub(x = all.dirs, pattern = paste0("data/|", algo, "/results/"), replacement = "")
  rownames(av.results) = datasets

  # save table to run Friedman stat tests
  write.table(x = av.results, file = paste0("output/avg_", algo.name, ".txt"),  
    sep = "\t", col.names = FALSE, row.names = FALSE)

  #----------------------
  # Loss time curves 
  #----------------------

  # loss curve with avg rank

  algo.path.list = getOptPath(algo = algo, all.dirs = all.dirs)
  algo.path.list = Filter(Negate(is.null), algo.path.list)

  algo.paths = lapply(algo.path.list, avgRankPath)
  df.algo.paths = Reduce("+", algo.paths)/length(algo.paths)

  # call [plot]
  g = lossCurvePlot(df = df.algo.paths, measure = "rank") 

  ggsave(g, file = paste0("output/LossCurve_avgRK_", algo, ".pdf"), 
    width = 6.56, height = 2.89)

  # loss curve with avg loss error

  algo.paths.2 = lapply(algo.path.list, avgLossPath)
  df.algo.paths.2 = Reduce("+", algo.paths.2)/length(algo.paths.2)

  g2 = lossCurvePlot(df = df.algo.paths.2, measure = "loss") 

  ggsave(g2, file = paste0("output/LossCurve_avgLoss_", algo, ".pdf"), 
    width = 6.56, height = 2.89)


  #----------------------
  # Average performance plot
  #----------------------

  ids.order = NULL

  cat(" * Average performance plot ... ")
  obj = getAvgPerfPlot(av.results = av.results, df.stats = df.stats, put.annotations = TRUE)
  ids.order = obj$ids.order

  ggsave(obj$g, file = paste0("output/", algo.name,"_AvgPerf.pdf"), dpi = 500, 
    width = 7.5, height = 2.6, units = "in")
  cat("ok\n")
  
  #----------------------
  #  Tree size plot
  #----------------------
  
  cat(" * Average tree size plot ... ")
  df.info = getModelsInfo(algo = algo)
  g = getTreeSizePlot(df.info = df.info, ids.order  = ids.order, measure = "median")

  ggsave(plot = g, filename = paste0("output/", algo.name, "_TreeSize.pdf"), dpi = 500, 
    width = 7.5, height = 1.6, units = "in")
  cat("ok\n")
  
  #----------------------
  #  Runtime plot
  #----------------------

  cat(" * Runtime plot ... ")
 
  df.training = getTrainingTime(algo = algo, all.dirs = all.dirs)
  df.tuning   = getTuningTimes(algo = algo, all.dirs = all.dirs)
  df.time     = rbind(df.training, df.tuning)

  g = getRuntimePlot(df.time = df.time, ids.order = ids.order)
  ggsave(plot = g, filename = paste0("output/", algo.name, "_runtime.pdf"), dpi = 500, 
    width = 8, height = 4.6, units = "in")
  cat("ok\n")  
  
  #----------------------
  # Params distributions
  #----------------------

  cat(" * Hyperparameters' distributions (one per hyperparam) ")
  getParamsDistributionPlots(algo = algo) 

  #----------------------
  #  Iterations' plot
  #----------------------

  cat(" * Iterations Plot ... ")
 
  ids.dir = paste("data", algo, "ids", sep="/")
  all.dirs = list.files(paste("data", algo, "results", sep="/"))
  fullIds = lapply(all.dirs, function(dataset) {
    ret = getIdsData(algo = algo, dataset = dataset, ids.dir = ids.dir) 
  })

  g1 = getIdsBoxPlot(fullIds = fullIds)
  ggsave(plot = g1, filename = paste0("output/", algo.name, "_iterations_boxplot.pdf"), dpi = 500, 
    width = 5, height = 2.4, units = "in")

  g2 = getIdsHistoPlot(fullIds = fullIds)
    ggsave(plot = g2, filename = paste0("output/", algo.name, "_iterations_histo.pdf"), dpi = 500,
      width = 3.05, height = 6.65, units = "in")
 
  cat("ok\n")
 
  #----------------------
  #  Correlation plots
  #----------------------

  cor.dir = paste("data", algo, "corr", sep="/")
  aux.dirs = list.files(paste("data", algo, "results", sep="/"))
  aux.cor = lapply(aux.dirs, function(dataset) {
    ret = getCorrData(algo = algo, dataset = dataset, cor.dir = cor.dir)
    return(ret)
  })

  df.cor = do.call("rbind", aux.cor)
  cat(" - Spearman correlation plot ... ")
  obj = getCorPlots(df.cor = df.cor, algo.name = algo.name)

  if(algo == "classif.J48") {
    ggsave(plot = obj$g1, filename = paste0("output/", algo.name, "_CorrHPs.pdf"), 
      dpi = 500, width = 7, height = 5, units = "in")
    ggsave(plot = obj$g2, filename = paste0("output/", algo.name, "_CorrAcc.pdf"), 
      dpi = 500, width = 7, height = 2, units = "in")
  } else {
    ggsave(plot = obj$g1, filename = paste0("output/", algo.name, "_CorrHPs.pdf"), 
      dpi = 500, width = 8, height = 3.5, units = "in")
    ggsave(plot = obj$g2, filename = paste0("output/", algo.name, "_CorrAcc.pdf"), 
      dpi = 500, width = 7, height = 2, units = "in")
  }
  cat("ok\n")
 
  #----------------------
  # fAnova plots
  #----------------------
 
  cat(" - Fanova plot ...")
  df = getFanovaData(algo = algo)

  #  Filtering HPs that do not contribute at least with 0.5% of the performance
  g = getFanovaPlot(df = df, algo.name = algo.name, threshold = 0.005)
  ggsave(g, filename = paste0("output/", algo.name, "_FAnova.pdf"), 
    dpi = 500, width = 7, height = 2, units = "in")
  cat("ok\n")

  g = getFanovaHpBoxplot(df = df)
  ggsave(g, filename = paste0("output/", algo.name, "_FAnovaParams.pdf"),
    dpi = 500, width = 5.28, height = 3.6)

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
