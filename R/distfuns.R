corr_dist <- function(method) {
  function(x) as.dist(1 - cor(t(x), method = method))
}

pearson <- corr_dist("pearson")
spearman <- corr_dist("spearman")
kendall <- corr_dist("kendall")
