#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

devtools::load_all(path = ".")

if(!dir.exists(path = "plots")) {
    dir.create(path = "plots")
}

#----------------------
# Loading all experimental data (94 datasets)
#----------------------

cat(" * Loading required data \n")

algo = "classif.rpart"

algo.name = gsub(x = algo, pattern="classif.", replacement = "")
algo.path = paste("data/hptuning_full_space", algo, "results", sep="/")
all.dirs  = list.files(path = algo.path, full.names = TRUE)

# all results from 30 repetitions
all.results = getRepsResults(algo = algo, all.dirs = all.dirs)

aux.avg = lapply(all.results, function(res) {
    tmp = colMeans(res[, -ncol(res)])
    return(tmp)
})
av.results = data.frame(do.call("rbind", aux.avg))
datasets = gsub(x = all.dirs, pattern = paste0("data/hptuning_full_space/|", algo, "/results/"), 
    replacement = "")
rownames(av.results) = datasets

#----------------------
#  Loading defaults-skl
#----------------------

df.files = list.files("data/hptuning_full_space/classif.rpart/defaults-skl", full.names = TRUE)

df.datasets = lapply(df.files, function(df.file){
    return(strsplit(x = df.file, split = "/")[[1]][5])
})
df.datasets = unlist(df.datasets)

# selecting datasets used in the base level experiments
df.ids = which(df.datasets %in% rownames(av.results))
sel.files = df.files[df.ids]

#loading raw results
aux.df.skl = lapply(sel.files, function(file) {
    print(file)
    all.files = list.files(file, full.names = TRUE, recursive=TRUE)
    all.files = all.files[grepl(x = all.files, pattern = "perf_")]
    inner.aux = lapply(all.files, function(rep.file) {
        load(rep.file, verbose = FALSE)
        return(mean(1-ret.perf$ber))
    })
    inner.perf = unlist(inner.aux)
    return(inner.perf)
}) 

#average skl results
av.df.skl    = unlist(lapply(aux.df.skl, mean))
skl.datasets = gsub(x = sel.files, pattern = "data/hptuning_full_space/classif.rpart/defaults-skl/", replacement = "") 

av.df.skl = data.frame(av.df.skl)
rownames(av.df.skl) = skl.datasets

df.merge = cbind(av.results, av.df.skl)
colnames(df.merge)[ncol(df.merge)] = "defaultsSkl"

#----------------------
# Scatter plot defaults skl x defaults rpart
#----------------------

g = ggplot(df.merge , aes(x = defaultsSkl, y = defaults))
g = g + geom_point() + theme_bw()
g = g + geom_abline (slope=1, linetype = "dashed", color="red")
g = g + labs(x = "Default SKL performance", y = "Defaults rpart performance")

ggsave(g, file = "plots/rpart_vs_sklearn_scatterPlot.pdf",  width = 4.2, height = 3.01, dpi = 500)
ggsave(g, file = "plots/rpart_vs_sklearn_scatterPlot.eps",  width = 4.2, height = 3.01, dpi = 500)
ggsave(g, file = "plots/rpart_vs_sklearn_scatterPlot.jpeg", width = 4.2, height = 3.01, dpi = 500)

#----------------------
# Scatter plot defaults skl x tuning techniques
#----------------------

df.temp = df.merge[,c(8, 2:7)]
df.perf = melt(df.temp, id.vars = 1)
colnames(df.perf)[1] = "defaults"

g2 = getAvgPerfScatterPlot(df.perf = df.perf)
ggsave(g2, file = "plots/rpart_defaultsSkl_scatterPlot.pdf",  width = 4.2, height = 3.01, dpi = 500)
ggsave(g2, file = "plots/rpart_defaultsSkl_scatterPlot.eps",  width = 4.2, height = 3.01, dpi = 500)
ggsave(g2, file = "plots/rpart_defaultsSkl_scatterPlot.jpeg", width = 4.2, height = 3.01, dpi = 500)

#----------------------
# table - bar plot (defaults skl x defaults rpart)
#----------------------

sub.df = df.merge[, c("defaults", "defaultsSkl")]
aux.max = lapply(1:nrow(sub.df), function(i) {
    return(which.max(sub.df[i, ]))
})

tab = data.frame(table(unlist(aux.max)))
tab$Var1 = c("defaults", "defaultsSkl")

g3 = ggplot(tab, aes(x = Var1, y = Freq, colour = Var1, fill = Var1))
g3 = g3 + geom_bar(stat='identity')
g3 = g3 + labs(y = "Frequence", x = "") + guides(fill = "none", colour = "none")
g3 = g3 + geom_text(aes(label = Freq), colour = "black")

ggsave(g3, file = "plots/defaults_comparison.pdf",  width = 3.84, height = 4.50, dpi = 500)
ggsave(g3, file = "plots/defaults_comparison.jpeg", width = 3.84, height = 4.50, dpi = 500)
ggsave(g3, file = "plots/defaults_comparison.eps",  width = 3.84, height = 4.50, dpi = 500)

#---------------------------
# histogram - performance differences
#---------------------------

df.hist = sub.df[1] -sub.df[2]
h = hist(df.hist[,1], breaks = 60, main = "defaults rpart - defaults sklearn", xlab = "Difference in terms of performance")

#---------------------------
# Average performance plot 
#---------------------------

df2 = df.merge[,c(8, 2:7)]

# -------------------------
# renaming datasets using OpenML ids
# -------------------------
  
mapping = read.csv("data/mapping.csv")
mapping$UCI.name = as.character(mapping$UCI.name)

aux.name = lapply(1:nrow(df2), function(i) {
    id = which(mapping$UCI.name == rownames(df2)[i])
    if(length(id) == 0) {
      return(c(NA,NA))
    }
    return(mapping[id,1:2])
})

maps = cbind(do.call("rbind", aux.name), df2)

# -------------------------
# -------------------------

df = maps
# ordering df according to the defaults' perofrmance values
ids.order = order(df$defaultsSkl, decreasing = TRUE) 
df = df[ids.order,]
df[,1] = factor(df[,1], levels = df[,1])

df.melted = melt(df, id.vars = c(1,2)) 

g = ggplot(df.melted, aes(x = data.id, y = value, color = variable, group = variable,
    linetype = variable))
g = g + geom_line() + ylab("(balanced) accuracy") + xlab("Dataset id") + theme_bw()
g = g + scale_color_manual(values=CUSTOM.COLORS, labels = levels(df.melted$variable)) 
g = g + scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1))
g = g + scale_linetype_manual(values=CUSTOM.LINETYPES, labels = levels(df.melted$variable))
g = g + theme(legend.position = c(.07,.49), legend.background = element_rect(colour = "black"))
g = g + theme(legend.title = element_blank(), legend.text = element_text(size = 8))
g = g + theme(legend.key.height = unit(0.3, "cm"), legend.key.width = unit(0.4, "cm"))
g = g + theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1, size = 6))

ggsave(g, file = "plots/rpart_defaultsSkL_AvgPerf.pdf", dpi = 500, width = 7.5, height = 2.6, units = "in")
ggsave(g, file = "plots/rpart_defaultsSkL_AvgPerf.jpeg", dpi = 500, width = 7.5, height = 2.6, units = "in")
ggsave(g, file = "plots/rpart_defaultsSkL_AvgPerf.eps", dpi = 500, width = 7.5, height = 2.6, units = "in")

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
