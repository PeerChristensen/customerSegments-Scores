
# scree plot


h2o.init(nthreads = -1)

km_training <- as.h2o(rfmts_norm)
x = names(km_training)

# scree plot
max_groups <- 20
wss_h2o <- numeric(max_groups)

for(i in 1:max_groups){
  
  km1 <- h2o.kmeans(km_training, x= x, estimate_k = FALSE, k = i, standardize = FALSE, 
                    nfolds = 5, max_iterations = 25)   
  wss_h2o[i] <- h2o.tot_withinss(km1, xval = TRUE) # xval=TRUE means cross validation used
}

d <- tibble(n_clusters = 1:max_groups,wss_h2o)

plot(1:max_groups, wss_h2o, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", bty = "l")

d %>%
  ggplot(aes(n_clusters,wss_h2o)) +
  geom_line(size=1) +
  geom_point() +
  theme_minimal()