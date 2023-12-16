#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

getTop15RFImportancePlot = function(metadataset) {

    mlrTask = mlr::makeClassifTask(metadataset[,-1], id = "test", target = "Class")
    lrn     = mlr::makeLearner("classif.ranger", importance = "permutation") 

    # Simulating 30 seeds
    aux = lapply(1:30, function(seed) {
    
        set.seed(seed)
        options(mlr.debug.seed = seed)

        model     = mlr::train(task = mlrTask, learner = lrn)
        trueModel = model$learner.model
    
        importance = as.data.frame(trueModel$variable.importance)
        rf.df = cbind(rownames(importance), importance)
        rownames(rf.df) = NULL
        colnames(rf.df) = c("Feature", "Importance")
        rf.df = rf.df[order(rf.df$Feature),]
        return(rf.df)
    })

    # averaging importance values
    values = lapply(aux, function(elem) {return(elem[,2])})
    avg.values = Reduce("+", values)/length(values)

    # final dataset
    rf.df = data.frame(cbind(aux[[1]]$Feature, avg.values))
    rf.df$avg.values = as.numeric(rf.df$avg.values)
    colnames(rf.df) = c("Feature", "Importance")

    # ordering them, according to their importance
    rf.df  = rf.df[order(rf.df$Importance, decreasing=TRUE),]
    # selecting top 15 features
    top.rf = rf.df[1:15,] 

    g = ggplot(top.rf, aes(x = reorder(Feature, Importance), y = Importance))
    g = g  + geom_col(width = 0.8, fill="lightblue", col="darkblue")
    g = g  + labs(y="Importance", x = "Feature") + coord_flip() + theme_bw() 
    return(g)

}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------


metaAnalysis = function() {

    # ------------------
    # creating output dir (plots)
    # ------------------
  
    dir.create("plots/", recursive = TRUE, showWarnings = FALSE)


    # ------------------
    # Required scripts
    # ------------------
    
    devtools::load_all()
    
    # ------------------
    # Reading meta-datasets
    # ------------------
  
    J48.metadataset   = farff::readARFF(path = "data/metadatasets/classif.J48_95_165d_all_original_dist.arff")
    RPART.metadataset = farff::readARFF(path = "data/metadatasets/classif.rpart_95_165d_all_original_dist.arff")
  
    # --------------------------
    ## Random Forest (top15)
    # --------------------------

    cat(" @ Plot: Random Forest (importance) \n")

    g.j48 = getTop15RFImportancePlot(metadataset = J48.metadataset)
    ggsave(g.j48, file="plots/J48_RFimportanceTop15.pdf",  units = "in", width = 9, height = 6, dpi = 500, pointsize = 20)
    ggsave(g.j48, file="plots/J48_RFimportanceTop15.jpeg", units = "in", width = 9, height = 6, dpi = 500, pointsize = 20)
    ggsave(g.j48, file="plots/J48_RFimportanceTop15.eps",  units = "in", width = 9, height = 6, dpi = 500, pointsize = 20)

    g.rpart = getTop15RFImportancePlot(metadataset = RPART.metadataset)
    ggsave(g.rpart, file="plots/rpart_RFimportanceTop15.pdf",  units = "in", width = 9, height = 6, dpi = 500, pointsize = 20)
    ggsave(g.rpart, file="plots/rpart_RFimportanceTop15.jpeg", units = "in", width = 9, height = 6, dpi = 500, pointsize = 20)
    ggsave(g.rpart, file="plots/rpart_RFimportanceTop15.eps",  units = "in", width = 9, height = 6, dpi = 500, pointsize = 20)

    # --------------------------
    # Meta-level results from MtLSuite project
    # --------------------------

    cat(" @ Plot: Random Forest (importance) \n")

    meta.files    = list.files(path = "data/metaResults",full.names = TRUE, recursive = TRUE)
    meta.data     = getMetaLevelData(all.files = meta.files)
    agg.meta.data = aggregateMetaLevelData(meta.data = meta.data)
    df.stats      = getMetaLevelStatisticalDifferences(meta.data = meta.data, agg.meta.data = agg.meta.data)

    g.meta = getMetaLevelAvgPerformancePlot(data = agg.meta.data, df.stats = df.stats)

    ggsave(g.meta, file = "plots/metalevel_averagePerformance.pdf",  width = 5.89, height = 2.65, dpi = 500)
    ggsave(g.meta, file = "plots/metalevel_averagePerformance.jpeg", width = 5.89, height = 2.65, dpi = 500)
    ggsave(g.meta, file = "plots/metalevel_averagePerformance.eps",  width = 5.89, height = 2.65, dpi = 500)

    cat(" *** Done !!! \n")
}

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

metaAnalysis()

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
