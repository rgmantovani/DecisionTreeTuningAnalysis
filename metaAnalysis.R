#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

library("setwidth")
source("R/config.R")
source("R/getRepsResults.R")
source("R/metaLearning.R")
source("R/stats.R")

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

rfImportance = function(task) {

  aux = lapply(1:10, function(i) {
    modelRF = mlr::train(learner = getMetaLearners()[[3]], task = task)
    model1  = mlr::getLearnerModel(modelRF, more.unwrap=TRUE)
    return(t(model1$importance))
  })

  df.1 = data.frame(do.call("rbind", aux))
  i1 = colMeans(df.1)
  ret = data.frame(i1[order(i1, decreasing=TRUE)])
  colnames(ret) = c("gini")
  return(t(ret))
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getTrees = function(task) {

  modelJ48 = mlr::train(learner = getMetaLearners()[[1]], task = task)
  model.1 = getLearnerModel(modelJ48, more.unwrap=TRUE)

  modelCART = mlr::train(learner = getMetaLearners()[[2]], task = task)
  model.2 = getLearnerModel(modelCART, more.unwrap=TRUE)

  obj = list(tree.J48 = model.1, tree.rpart = model.2)
  return(obj)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

metaAnalysis = function() {

  cat("*******************************************\n")
  cat("* Automated meta-learning Analysis\n")
  cat("*******************************************\n")

  task1 = getMetaBase(algo = "classif.J48")
  cat("* Task 1 ok ... \n")
  
  task2 = getMetaBase(algo = "classif.rpart")
  cat("* Task 2 ok ... \n")
  
  task3 = getMetaBase(algo = "classif.ctree")
  cat("* Task 3 ok ... \n")
  
  ret = benchmark(tasks = list(task1, task2, task3), 
    learners = getMetaLearners(predict.type = "prob"), 
    resamplings = makeResampleDesc(method = "LOO"), 
    measures = list(acc, ber), show.info = FALSE, 
    models = TRUE, keep.pred = TRUE)

  res = getBMRAggrPerformances(ret, as.df = TRUE)

  pred.list = mlr::getBMRPredictions(bmr = ret)

  auc.values = lapply(pred.list, function(elem) {
    inner = lapply(elem, function(lrn) {
       meas.auc = mlr::measureAUC(
        probabilities = mlr::getPredictionProbabilities(lrn), 
        truth = lrn$data$truth, 
        negative = "tuning", 
        positive = "defaults")
      return(meas.auc)
    })
    return(data.frame(inner))
  })

  auc.aux = do.call("rbind", auc.values)
  res$auc.test.mean = 0
  res$auc.test.mean[1:3] = auc.aux[1,]
  res$auc.test.mean[4:6] = auc.aux[2,]

  cat("*******************************************\n")
  cat("* BMR results ok ... \n")

  print(res)

  save(ret, file = "output/mtl_raw.RData")
  save(res, file = "output/mtl_aggr.RData")

  cat("*******************************************\n")
  cat("* Extracting models ... \n")

  t1 = getTrees(task = task1)
  t2 = getTrees(task = task2)
  t3 = getTrees(task = task3)

  # meta-features most important
  df1 = rfImportance(task = task1)
  df2 = rfImportance(task = task2)
  df3 = rfImportance(task = task3)

  models.J48   = list(rf.gini = df1, trees = t1)
  save(models.J48, file = "output/J48_meta_models.RData")

  cat("* models task1 ok ... \n")
  
  models.rpart = list(rf.gini = df2, trees = t2)
  save(models.rpart, file = "output/rpart_meta_models.RData")
  
  cat("* models task2 ok ... \n")

  models.ctree = list(rf.gini = df3, trees = t3)
  save(models.ctree, file = "output/ctree_meta_models.RData")
  
  cat("* models task3 ok ... \n")

  cat("*******************************************\n")
  cat("* Done\n")
  cat("*******************************************\n")

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

metaAnalysis()

#--------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------