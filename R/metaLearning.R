#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getMetaLearners = function(predict.type = "prob") {

  j48.lrn  = mlr::makeLearner("classif.J48", predict.type = predict.type)
  cart.lrn = mlr::makeLearner("classif.rpart", predict.type = predict.type)
  rf.lrn   = mlr::makeLearner("classif.randomForest", predict.type = predict.type)

  return(list(j48.lrn, cart.lrn, rf.lrn))
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getMetaFeatures = function() {
  load("data/meta_features.RData")
  return(mfeat)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getMetaBase = function(algo) {

  mfeat = getMetaFeatures()

  algo.name = gsub(x = algo, pattern="classif.", replacement = "")
  algo.path = paste("data", algo, "results", sep="/")
  all.dirs  = list.files(path = algo.path, full.names = TRUE)

  all.results = getRepsResults(algo = algo, all.dirs = all.dirs)
  df.stats    = getStatsDf(all.results = all.results)

  base = cbind(mfeat, df.stats)
  colnames(base)[ncol(base)] = "Class"
  
  ids.class = grep("DF", x = base$Class)
  base$Class = as.character(base$Class)
  base$Class[ids.class]  = "defaults"
  base$Class[-ids.class] = "tuning" 
  base$Class = as.factor(base$Class)
  
  task = mlr::makeClassifTask(id = paste0("meta_base_",algo), data = base[2:ncol(base)], 
    target = "Class")
  return(task)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
