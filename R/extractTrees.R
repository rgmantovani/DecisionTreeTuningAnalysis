#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

extractJ48Trees = function(job, task, lrn, tun, seed) {

   if(tun == "defaults") {

    t = system.time(model <- mlr::train(learner = lrn, task = task))
    real.model = getLearnerModel(model, more.unwrap = TRUE)

    num.leaves = real.model$classifier$measureNumLeaves()
    tree.size  = real.model$classifier$measureTreeSize()
    num.rules  = real.model$classifier$measureNumRules()
    train.time = t[3]
    feat = c(tree.size, num.leaves, num.rules, train.time)
    aux.models = list(data.frame(t(feat)))
      
  } else {
          
    load(job) 
    params = lapply(ret.params.list[[1]][[1]], function(res) {
      return(res$x)
    })

    aux.models = lapply(params, function(setting) {
          
      new.lrn = setHyperPars(learner = lrn, par.vals = setting)
      t = system.time(model <- mlr::train(learner = new.lrn, task = task))
    
      real.model = getLearnerModel(model, more.unwrap = TRUE)
      num.leaves = real.model$classifier$measureNumLeaves()
      tree.size  = real.model$classifier$measureTreeSize()
      num.rules  = real.model$classifier$measureNumRules()
      train.time = t[3]
      feat = c(tree.size, num.leaves, num.rules, train.time)

      return(feat)
    })
  }

  df.models = data.frame(do.call("rbind", aux.models))
  colnames(df.models) = c("tree.size", "num.leaves", "num.rules", "train.time")
  df.models$rep = seed
  df.models = colMeans(df.models)

  return(df.models)
}

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

extractRpartTrees = function(job, task, lrn, tun, seed) {

  if(tun == "defaults") {

    t = system.time(model <- mlr::train(learner = lrn, task = task))
    real.model = getLearnerModel(model, more.unwrap = TRUE)

    obj = partykit::as.party(real.model) 
    tree.size = length(obj)
    train.time = t[3]

    feat = c(tree.size, train.time)
    aux.models = list(data.frame(t(feat)))

  } else { 

    load(job) 
    params = lapply(ret.params.list[[1]][[1]], function(res) {
      return(res$x)
    })

    aux.models = lapply(params, function(setting) {
          
      new.lrn = setHyperPars(learner = lrn, par.vals = setting)
      t = system.time(model <- mlr::train(learner = new.lrn, task = task))
      real.model = getLearnerModel(model, more.unwrap = TRUE)
      

      obj = partykit::as.party(real.model) 
      tree.size = length(obj)
      train.time = t[3]
     
      feat = c(tree.size, train.time)
      return(feat)
    })
  }

  df.models = data.frame(do.call("rbind", aux.models))
  colnames(df.models) = c("tree.size", "train.time")
  df.models$rep = seed
  df.models = colMeans(df.models)
  
  return(df.models)  
}

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------

visitTree = function(node) {

  dat <- data.frame (x=node$nodeID)
  inner.df <<- rbind(inner.df, dat)

  if(node$terminal) {
    return (NULL)
  } else {
    visitTree(node = node$left)
    visitTree(node = node$right)
  }
}

extractCTreeTrees = function(job, task, lrn, tun, seed) {
 
  if(tun == "defaults") {

    t = system.time(model <- mlr::train(learner = lrn, task = task))
    real.model = getLearnerModel(model, more.unwrap = TRUE)

    inner.df <<- data.frame()
    ok = visitTree(node = real.model@tree)

    tree.size = max(inner.df)
    train.time = t[3]

    feat = c(tree.size, train.time)
    aux.models = list(data.frame(t(feat)))

  } else { 

    load(job) 
    params = lapply(ret.params.list[[1]][[1]], function(res) {
      return(res$x)
    })

    aux.models = lapply(params, function(setting) {
          
      new.lrn = setHyperPars(learner = lrn, par.vals = setting)
      t = system.time(model <- mlr::train(learner = new.lrn, task = task))
      real.model = getLearnerModel(model, more.unwrap = TRUE)
      
      # visit tree and return
      inner.df <<- data.frame()
      ok = visitTree(node = real.model@tree)

      tree.size = max(inner.df)
      train.time = t[3]
      feat = c(tree.size, train.time)
      return(feat)
    })
  }

  df.models = data.frame(do.call("rbind", aux.models))
  colnames(df.models) = c("tree.size", "train.time")
  df.models$rep = seed
  df.models = colMeans(df.models)

  return(df.models)
}

#----------------------------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------------------------
# 