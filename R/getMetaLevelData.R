#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getMetaLevelData = function(all.files) {

  aux = lapply(all.files, function(file){
    load(file, verbose = FALSE) # load an object called all
  	return(all)
  })

  df = do.call("rbind", aux)

  # featsel == none ,tuning  == none, balance == none,norm == no_norm, resamp  == 10-CV
  sub = dplyr::filter(df, featsel == "none" & tuning == "none" & balance == "none" 
  	& norm == "no_norm" & resamp == "10-CV")

  select.task = c("classif.J48_95_165d_all_original_dist", "classif.rpart_95_165d_all_original_dist")
  sub.task = dplyr::filter(sub, task %in% select.task)

  # selecting just the simple results
  # [task, algo, auc, rep]
  select.columns = c("task", "algo", "auc", "rep")
  ret =  sub.task[,which(colnames(sub) %in% select.columns)]

  return(ret)
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

aggregateMetaLevelData = function(meta.data) {

	algos = unique(meta.data$algo)
	tasks = unique(meta.data$task)

	combinations = expand.grid(algos, tasks)

	aux = lapply(1:nrow(combinations), function(k) {
		sel = dplyr::filter(meta.data, task == combinations[k,2] & algo == combinations[k,1])
		sel = sel[1:30, ]

		ret = sel[1,1:2]
		ret$avgAuc = mean(sel$auc)
		ret$sdAuc  = sd(sel$auc)
		return(ret)
	})

	avg.df = do.call("rbind", aux)
	avg.df$task = gsub(x = avg.df$task, pattern = "classif.|_95_165d_all_original_dist", replacement = "")

	# head(avg.df)
	#   task algo    avgAuc      sdAuc
	# 1  J48   GP 0.7071465 0.01607670
	# 2  J48  KNN 0.6587283 0.02655235
	# 3  J48   LR 0.6256960 0.02685239
	# 4  J48   NB 0.6827455 0.02230745
	# 5  J48   RF 0.7199424 0.01814593
	# 6  J48 CART 0.6096682 0.03001245
	return(avg.df)
}


#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------