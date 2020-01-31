# customer K-means training
# january 2020
# Peer Christensen

library(recipes)
library(h2o)
library(ggthemes)

rfmts_km <- list(recency,frequency,monetary,tenure,streaming) %>% 
  reduce(left_join) %>%
  select(-contains("Quan")) %>%
  left_join(df3) %>%
  drop_na() %>%
  mutate(Type = as.character(Type)) %>% 
  mutate(Type = replace_na(Type,"Unknown")) %>%
  select(Customer_Key,Type,everything())

# kmeans
km_results <- tibble()

for (type in unique(rfmts_km$Type)) {
  
  rfmts_km_type <- rfmts_km %>%
    filter(Type == type) %>%
    select(-Type)
  
  Customer_Key <- rfmts_km_type %>% pull(Customer_Key)
  
  rfmts_recipe <- rfmts_km_type %>%
    select(RecencyDays,Frequency,Monetary,Tenure,Streaming) %>%
    recipe() %>%
    step_YeoJohnson(all_numeric()) %>%
    step_center(all_numeric()) %>%
    step_scale(all_numeric()) %>%
    prep(data = train_data)
  
  rfmts_norm <- bake(rfmts_recipe, new_data = rfmts_km_type)
  
  # kmeans with H2O
  h2o.init(nthreads = -1)
  
  km_training <- as.h2o(rfmts_norm)
  x = names(km_training)

  file <- list.files(glue::glue("models/{type}"))
  model_path <- glue::glue("models/{type}/{file}")
 
 km <- h2o.loadModel(model_path)

  
  cluster <- h2o.predict(km,km_training) %>% as_tibble() 
  
  rfmts_clusters <- tibble(Cluster = factor(cluster$predict+1)) %>% 
    mutate(Customer_Key = Customer_Key, Type = type) %>%
    select(Customer_Key,Type,Cluster)
  
  km_results = rbind(km_results,rfmts_clusters)
  
}

h2o.shutdown(prompt=F)