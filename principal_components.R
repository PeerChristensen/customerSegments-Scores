# Principal components

options(warn = -1)

pca <- tibble()

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
  
  
  pcomp <- prcomp(rfmts_norm, scale = FALSE) 

  pca_type <- tibble(type, 
                     Customer_Key = Customer_Key,
                     pc1 = pcomp$x[,1],
                     pc2 = pcomp$x[,2])
  
  pca <- rbind(pca,pca_type)
}
  
  