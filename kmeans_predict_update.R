# customer K-means - predict
# january 2020
# Peer Christensen
# -----------------------------------------------------------

options(warn = -1)

library(recipes)
library(h2o)
library(ggthemes)

# ------------------------------------------------------------
# Prepare RFMTS data
# ------------------------------------------------------------

rfmts_km <- df %>%
  select(Customer_Key,Type,Recency_km = RecencyDays, 
         Frequency_km = Orders, Monetary_km = DB2,
         Tenure_km = Duration, Streaming_km = Buckets) %>%
  mutate(Tenure_km = as.numeric(Tenure_km))

# ------------------------------------------------------------
# K-means for each customer type with H20
# ------------------------------------------------------------

km_results <- tibble()

for (type in unique(rfmts_km$Type)) {
  
  rfmts_km_type <- rfmts_km %>%
    filter(Type == type) %>%
    select(-Type)
  
  # save customer keys
  Customer_Key <- rfmts_km_type %>% pull(Customer_Key)
  
  # define recipe for preprocessing the data
  rfmts_recipe <- rfmts_km_type %>%
    select(ends_with("_km")) %>%
    recipe() %>%
    step_YeoJohnson(all_numeric()) %>%
    step_center(all_numeric()) %>%
    step_scale(all_numeric()) %>%
    prep(data = train_data)
  
  # apply the recipe
  rfmts_norm <- bake(rfmts_recipe, new_data = rfmts_km_type)
  
  # kmeans with H2O
  h2o.init(nthreads = -1)
  
  km_training <- as.h2o(rfmts_norm)
  x = names(km_training)
  
  # load model
  file <- list.files(glue::glue("D:/R/RScripts/customerSegments_Scores/models/{type}"))
  model_path <- glue::glue("D:/R/RScripts/customerSegments_Scores/models/{type}/{file}")
  
  km <- h2o.loadModel(model_path)
  
  # predict on current data
  cluster <- h2o.predict(km,km_training) %>% as_tibble() 
  
  rfmts_clusters <- tibble(Cluster = factor(cluster$predict+1)) %>% 
    mutate(Customer_Key = Customer_Key, Type = type) %>%
    select(Customer_Key,Type,Cluster)
  
  km_results = rbind(km_results,rfmts_clusters)
  
}

h2o.shutdown(prompt=F)
