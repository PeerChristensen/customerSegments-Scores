# run scripts
# ca. 37 sekunder

library(tictoc)

tic()
source("individual_scores.R")
toc()

tic()
source("kmeans_predict.R")
toc()
# join

output <- rfmts %>%
  left_join(km_results) %>%
  select(Customer_Key,Type,Cluster,everything()) %>%
  rename(Recency = R_Quantile, Frequency = F_Quantile, Monetary = M_Quantile,
         Tenure = T_Quantile, Streaming = S_Quantile,RFMscore = RFM_score,
         RFMSscore = RFMS_score, Segment = Cluster)


sqlSave(channel,output, tablename = "customerSegmentation2020",rownames = F,safer=F)

close(channel)
