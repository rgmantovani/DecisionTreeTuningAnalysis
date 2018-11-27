#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

mlr::configureMlr(on.learner.error = "warn")
mlr::configureMlr(show.info = TRUE)

BUDGET = 900

AVAILABLE.ALGOS = c("classif.J48", "classif.rpart", "classif.ctree")

TECHNIQUES = c("defaults", "PSO", "GA", "EDA", "RS", "SMBO", "Irace")

INNER.NAMES = c("defaults", "pso", "ga", "eda", "random", "mbo", "irace")

CUSTOM.COLORS = c("black", "purple", "green", "darkorange", "red",
  "darkblue", "darkcyan", "darkgreen", "darkmagenta", "darkgoldenrod",
  "antiquewhite4")

CUSTOM.LINETYPES = c("dotted", "solid", "solid", "solid", "solid",
  "solid", "solid", "dashed", "solid", "dashed", "solid")

#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------
