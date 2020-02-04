# run scripts
# ca. 37 sekunder

library(tictoc)

tic()
source("individual_scores_update.R")
toc()

tic()
source("kmeans_predict_update.R")
toc()

# join
output <- df %>%
  mutate(Type = as.character(Type)) %>%
  left_join(km_results) %>%
  select(Customer_Key,Type,Cluster,Recency,Frequency,Monetary,Tenure,Streaming,RFM_score,
         RFMS_score) %>%
  rename(Segment = Cluster)

# save to SQL
channel <-odbcConnect("saxo034", uid="R", pwd="sql2017")

sqlquery <- "TRUNCATE TABLE DataMartMisc.dbo.customerSegmentationOutput"

sqlQuery(channel, sqlquery)

sqlSave(channel, output, tablename = "dbo.customerSegmentationOutput",
        append=TRUE, fast = FALSE, rownames = FALSE)

close(channel)

