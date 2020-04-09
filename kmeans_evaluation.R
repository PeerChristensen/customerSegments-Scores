# customer K-means evaluation
# january 2020
# Peer Christensen

library(recipes)
library(h2o)
library(ggthemes)

# make sure to run this:
source("individual_scores_update.R")

rfmts_km <- df %>%
  select(Customer_Key,Type,Recency_km = RecencyDays, 
         Frequency_km = Orders, Monetary_km = DB2,
         Tenure_km = Duration, Streaming_km = Buckets) %>%
  mutate(Tenure_km = as.numeric(Tenure_km),
         Type = fct_recode(Type,"BTB" = "BTI")) # approx. 100 BTI, so we merge with BTB

# ---------------------------------------------------------------
# Find optimal K per customer type
# ---------------------------------------------------------------

km_results <- tibble()
plots <- list()

h2o.init(nthreads = -1)

for (type in unique(rfmts_km$Type)) {
  
  rfmts_km_type <- rfmts_km %>%
    filter(Type == type) %>%
    select(-Type)
  
  rfmts_recipe <- rfmts_km_type %>%
    select(ends_with("_km")) %>%
    recipe() %>%
    step_YeoJohnson(all_numeric()) %>%
    step_center(all_numeric()) %>%
    step_scale(all_numeric()) %>%
    prep(data = train_data)
  
  rfmts_norm <- bake(rfmts_recipe, new_data = rfmts_km_type)
  
  km_training <- as.h2o(rfmts_norm)
  x = names(km_training)
  
  # scree plots
  max_groups <- 7
  wss_h2o <- numeric(max_groups)
  
  for(i in 1:max_groups){
    
    km1 <- h2o.kmeans(km_training, x= x, estimate_k = FALSE, k = i, standardize = FALSE)   
    wss_h2o[i] <- h2o.tot_withinss(km1, xval = F) # xval=TRUE means cross validation used
  }
  
  d <- tibble(n_clusters = 1:max_groups,wss_h2o)
  
  #plot(1:max_groups, wss_h2o, type="b", xlab="Number of Clusters",
  #    ylab="Within groups sum of squares", bty = "l")
  
  p <- d %>%
    ggplot(aes(n_clusters,wss_h2o)) +
    ggtitle(glue::glue("{type}")) +
    geom_line(size=1) +
    geom_point() +
    theme_minimal()
  
  plots[[type]] <- p
  
}

gridExtra::grid.arrange(grobs = plots)
ggsave("segments_scree_plots.png")

# ---------------------------------------------------------------
# Plot centroids for all models and clusters
# ---------------------------------------------------------------

# april 2020: we choose 2-3 segments (k) for all customer types

centroids <- NULL

for (type in unique(rfmts_km$Type)) {
  
  rfmts_km_type <- rfmts_km %>%
    filter(Type == type) %>%
    select(-Type)
  
  rfmts_recipe <- rfmts_km_type %>%
    select(ends_with("_km")) %>%
    recipe() %>%
    step_YeoJohnson(all_numeric()) %>%
    step_center(all_numeric()) %>%
    step_scale(all_numeric()) %>%
    prep(data = train_data)

  rfmts_norm <- bake(rfmts_recipe, new_data = rfmts_km_type)

  km_training <- as.h2o(rfmts_norm)
  x = names(km_training)
  
  km <- h2o.kmeans(km_training, x= x, estimate_k = FALSE, k = 3, standardize = F)   
  
  model_centers <- km@model$centers %>% 
    as_tibble() %>% 
    mutate(type=type)
  
  centroids <- rbind(centroids,model_centers)
  
}

centroids %>%
   pivot_longer(ends_with("km"),names_to="metric") %>%
   mutate(metric = fct_relevel(metric, 
                              "recency_km","frequency_km","monetary_km","tenure_km","streaming_km")) %>%
  ggplot(aes(x=metric,y=value,
             group=centroid,colour = centroid)) +
   geom_line(size=1.5) +
   geom_point(size=2) +
   #ylim(-2,2) +
   theme_light() +
   scale_colour_tableau() +
   theme(legend.title = element_blank(),
         axis.text.x = element_text(angle=45,hjust=1)) +
   facet_wrap(~type,scales="free")

ggsave(glue::glue("centroids_{today()}.png"))

# ---------------------------------------------------------------
# Summary statistics
# ---------------------------------------------------------------

km_results <- tibble()

for (type in unique(rfmts_km$Type)) {
  
  rfmts_km_type <- rfmts_km %>%
    filter(Type == type) %>%
    select(-Type)
  
  Customer_Key <- rfmts_km_type %>% pull(Customer_Key)
  
  rfmts_recipe <- rfmts_km_type %>%
    select(ends_with("_km")) %>%
    recipe() %>%
    step_YeoJohnson(all_numeric()) %>%
    step_center(all_numeric()) %>%
    step_scale(all_numeric()) %>%
    prep(data = train_data)
  
  rfmts_norm <- bake(rfmts_recipe, new_data = rfmts_km_type)
  
  h2o.init(nthreads = -1)
  
  km_training <- as.h2o(rfmts_norm)
  x = names(km_training)
  
  km <- h2o.kmeans(training_frame = km_training, 
                   k = 3,
                   x = x,
                   standardize = F,
                   estimate_k = F,

                 #  cluster_size_constraints = c(round(nrow(rfmts_norm)*.2),
                  #                              round(nrow(rfmts_norm)*.2))
                   )
  
  #h2o.saveModel(km,path = glue::glue("models_{today()}/{type}"))
  
  cluster <- h2o.predict(km,km_training) %>% as_tibble() 
  
  rfmts_clusters <- tibble(Cluster = factor(cluster$predict+1)) %>% 
    mutate(Customer_Key = Customer_Key, Type = type) %>%
    select(Customer_Key,Type,Cluster)
  
  km_results = rbind(km_results,rfmts_clusters)
  
}

# n customers per type and segment

table(km_results$Type,km_results$Cluster)

km_results %>%
  group_by(Type,Cluster) %>%
  count() %>%
  ggplot(aes(Cluster,n)) +
  geom_col() +
  facet_wrap(~Type,scales="free")

# median values per type, segment and component
df %>%
  inner_join(km_results)


h2o.shutdown(prompt=F)

