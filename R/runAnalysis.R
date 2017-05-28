#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

runAnalysis = function(algo = algo, performance = TRUE, models = TRUE, runtime = TRUE, 
  ids = TRUE, correlation = TRUE, favnova = TRUE) {

  if(!dir.exists(path="output")) {
    dir.create(path="output")
  }

  #----------------------
  #----------------------

  cat(" * Loading required data \n")

  algo.name    = gsub(x = algo, pattern="classif.", replacement = "")
  algo.path    = paste("data", algo, "results", sep="/")
  all.dirs     = list.files(path = algo.path, full.names = TRUE)

  # all results from 30 repetitions
  all.results  = getRepsResults(algo = algo, all.dirs = all.dirs)

  # statistical results
  df.stats     = getStatsDf(all.results = all.results)

  # average results
  av.results   = data.frame(do.call("rbind", lapply(all.results, colMeans, TRUE)))

  # list with datasets names
  datasets     = gsub(x = all.dirs, pattern = paste0("data/|", algo, "/results/"), replacement = "")
  rownames(av.results) = datasets

  # save table to run Friedman stat tests
  write.table(x = av.results, file = paste0("output/avg_", algo, ".txt"),  
    sep = "\t", col.names = FALSE, row.names = FALSE)

  #----------------------
  #----------------------

  # TODO: Update plots (code works)

  ids.order = NULL

  if(performance == TRUE) {
    cat(" * Average performance plot ... ")
    obj = getAvgPerfPlot(av.results = av.results, df.stats = df.stats, put.annotations = TRUE)
    ids.order = obj$ids.order

    ggsave(obj$g, file = paste0("output/", algo.name,"_AvgPerf.pdf"), dpi = 500, 
      width = 7.5, height = 2.6, units = "in")
    cat("ok\n")
  }


  #----------------------
  #----------------------

  # Getting data from Irace
  # Requires Tree data from rpart

  if(models == TRUE) {
    cat(" * Average tree size plot ... ")
    df.info = getModelsInfo(algo = algo)
    g = getTreeSizePlot(df.info = df.info, ids.order  = ids.order, measure = "median")

    ggsave(plot = g, filename = paste0("output/", algo.name, "_TreeSize.pdf"), dpi = 500, 
      width = 7.5, height = 1.6, units = "in")
    cat("ok\n")
  }

  #----------------------
  #----------------------

  # TODO: Update plots (code is working)
  if(runtime == TRUE) {
    cat(" * Runtime plot ... ")
    df.training = getTrainingTime(algo = algo, all.dirs = all.dirs)
    df.tuning   = getTuningTimes(algo = algo, all.dirs = all.dirs)
    df.time     = rbind(df.training, df.tuning)

    g = getRuntimePlot(df.time = df.time, ids.order = ids.order)
    ggsave(plot = g, filename = paste0("output/", algo.name, "_runtime.pdf"), dpi = 500, 
      width = 8, height = 4.6, units = "in")
    cat("ok\n")
  
  }

  #----------------------
  #----------------------

  # TODO: Update plots (code is working)
  if(ids == TRUE) {
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
  }

  #----------------------
  #----------------------

  # TODO: Update plots (code is working)
  if(correlation == TRUE) {

    cor.dir = paste("data", algo, "corr", sep="/")
    aux.dirs = list.files(paste("data", algo, "results", sep="/"))
    aux.cor = lapply(aux.dirs, function(dataset) {
      print(dataset)
      ret = getCorrData(algo = algo, dataset = dataset, cor.dir = cor.dir)
      return(ret)
    })

    df.cor = do.call("rbind", aux.cor)
    cat(" - Spearman correlation plot\n")
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

  }

  #----------------------
  #----------------------

  if(fanova == TRUE) {
    cat(" - Fanova plot\n")
    df = getFanovaData(algo = algo)

    g = getFanovaPlot(df = df, algo.name = algo.name)
    ggsave(g, filename = paste0("output/", algo.name, "_FAnova.pdf"), 
     dpi = 500, width = 7, height = 5, units = "in")
    cat("ok\n")
  }

  #----------------------
  #----------------------

  # TODO: 
  # if(curves == TRUE) {
  #   cat(" - curves\n")
  #   cat("ok\n")
  # }

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
