#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

runAnalysis = function(algo) {

  if(!dir.exists(path = "plots")) {
    dir.create(path = "plots")
  }

  #----------------------
  #----------------------

  cat(" * Loading required data \n")

  algo.name = gsub(x = algo, pattern="classif.", replacement = "")
  algo.path = paste("data/hptuning_full_space", algo, "results", sep="/")
  all.dirs  = list.files(path = algo.path, full.names = TRUE)

  # all results from 30 repetitions
  all.results = getRepsResults(algo = algo, all.dirs = all.dirs)

  # statistical results
  suppressWarnings(df.stats <- getStatsDF(all.results = all.results))

  # average results
  aux.avg = lapply(all.results, function(res) {
    tmp = colMeans(res[, -ncol(res)])
    return(tmp)
  })
  av.results = data.frame(do.call("rbind", aux.avg))

  # list with datasets names
  datasets = gsub(x = all.dirs, pattern = paste0("data/hptuning_full_space/|", algo, "/results/"), 
    replacement = "")
  rownames(av.results) = datasets

  #----------------------
  # save results to run Friedman statistical tests
  #----------------------
  write.table(x = av.results, file = paste0("plots/", algo.name, "_averagePerformance.txt"),
    sep = "\t", col.names = FALSE, row.names = FALSE)
  write.csv(x = av.results, file = paste0("plots/", algo.name, "_averagePerformance.csv"))

  #----------------------
  #----------------------
  # Average performance plot
  #----------------------
  #----------------------

  ids.order = NULL

  cat("@ Plotting: Average performance plot \n")

  obj = getAvgPerfPlot(av.results = av.results, df.stats = df.stats, put.annotations = TRUE)
  ids.order = obj$ids.order

  ggsave(obj$g, file = paste0("plots/", algo.name,"_AvgPerf.pdf"), dpi = 500,
    width = 7.5, height = 2.6, units = "in")
  ggsave(obj$g, file = paste0("plots/", algo.name,"_AvgPerf.jpeg"), dpi = 500,
    width = 7.5, height = 2.6, units = "in")
  ggsave(obj$g, file = paste0("plots/", algo.name,"_AvgPerf.eps"), dpi = 500,
    width = 7.5, height = 2.6, units = "in")
  
  cat("ok\n")

  #----------------------
  #----------------------
  # Scatter plot: default x tuned performances
  #----------------------
  #----------------------

  cat("@ Plotting: Scatter plot (default vs optimized) \n")
  
  df.perf = melt(av.results, id.vars = 1)
  g = getAvgPerfScatterPlot(df.perf = df.perf)
  ggsave(g, file = paste0("plots/", algo.name, "_scatterPlot.pdf"),  width = 4.2, height = 3.01, dpi = 500)
  ggsave(g, file = paste0("plots/", algo.name, "_scatterPlot.jpeg"), width = 4.2, height = 3.01, dpi = 500)
  ggsave(g, file = paste0("plots/", algo.name, "_scatterPlot.eps"),  width = 4.2, height = 3.01, dpi = 500)


  #----------------------
  #----------------------
  # Loss time curves
  #----------------------
  #----------------------

  cat("@ Plotting: Loss curves [Avg Rank | Avg Loss] \n")

  # loss curve with avg rank
  algo.path.list = getOptPath(algo = algo, all.dirs = all.dirs)
  algo.path.list = Filter(Negate(is.null), algo.path.list)

  algo.paths = lapply(algo.path.list, avgRankPath)
  df.algo.paths = Reduce("+", algo.paths)/length(algo.paths)

  # call [plot]
  g = lossCurvePlot(df = df.algo.paths, measure = "rank")

  ggsave(g, file = paste0("plots/", algo.name, "_LossCurve_avgRK.pdf"),  width = 6.56, height = 2.89, dpi = 500)
  ggsave(g, file = paste0("plots/", algo.name, "_LossCurve_avgRK.jpeg"), width = 6.56, height = 2.89, dpi = 500)
  ggsave(g, file = paste0("plots/", algo.name, "_LossCurve_avgRK.eps"),  width = 6.56, height = 2.89, dpi = 500)

  # loss curve with avg loss error
  algo.paths.2 = lapply(algo.path.list, avgLossPath)
  df.algo.paths.2 = Reduce("+", algo.paths.2)/length(algo.paths.2)

  # call [plot]
  g2 = lossCurvePlot(df = df.algo.paths.2, measure = "loss")

  ggsave(g2, file = paste0("plots/", algo.name, "_LossCurve_avgLoss.pdf"),  width = 6.56, height = 2.89, dpi = 500)
  ggsave(g2, file = paste0("plots/", algo.name, "_LossCurve_avgLoss.jpeg"), width = 6.56, height = 2.89, dpi = 500)
  ggsave(g2, file = paste0("plots/", algo.name, "_LossCurve_avgLoss.eps"),  width = 6.56, height = 2.89, dpi = 500)

  #----------------------
  #----------------------
  #  Tree size plot
  #----------------------
  #----------------------

  cat("@ Plotting: Tree models' size \n")

  df.info = getModelsInfo(algo = algo)
  g = getTreeSizePlot(df.info = df.info, ids.order  = ids.order, measure = "median")

  ggsave(g, filename = paste0("plots/", algo.name, "_TreeSize.pdf"),  dpi = 500, width = 7.5, height = 1.6, units = "in")
  ggsave(g, filename = paste0("plots/", algo.name, "_TreeSize.jpeg"), dpi = 500, width = 7.5, height = 1.6, units = "in")
  ggsave(g, filename = paste0("plots/", algo.name, "_TreeSize.eps"),  dpi = 500, width = 7.5, height = 1.6, units = "in")


  #----------------------
  #----------------------
  # Params distributions
  #----------------------
  #----------------------

  cat("@ Plotting: Hyperparameters distributions (one per hyperparameter) \n")
  getParamsDistributionPlots(algo = algo)

  #----------------------
  #----------------------
  # fAnova plots
  #----------------------
  #----------------------

  cat("@ Plotting: FANOVA plot \n")

  df.fanova = getFanovaData(algo = algo)

  #  Filtering HPs that do not contribute at least with 0.5% of the performance
  g = getFanovaPlot(df = df.fanova, algo.name = algo.name, threshold = 0.005)

  ggsave(g, filename = paste0("plots/", algo.name, "_FAnovaHeatmap.pdf"),  dpi = 500, width = 7, height = 2, units = "in")
  ggsave(g, filename = paste0("plots/", algo.name, "_FAnovaHeatmap.jpeg"), dpi = 500, width = 7, height = 2, units = "in")
  ggsave(g, filename = paste0("plots/", algo.name, "_FAnovaHeatmap.eps"),  dpi = 500, width = 7, height = 2, units = "in")
  

  g = getFanovaHpBoxplot(df = df.fanova)
  ggsave(g, filename = paste0("plots/", algo.name, "_FAnovaParams.pdf"),
    dpi = 500, width = 5.28, height = 3.6)

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
