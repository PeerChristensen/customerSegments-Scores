
# Kundesegmentering og scores (RFMTS)
# Peer CHristensen
# Februar 2020
# ---------------------------------------------------------------------------------

# RFMTS segmentering og individuelle scores (kvartiler, 1-4) grupperet efter kundetype

# output:
# kunde id	
# kundetype	(BTC, BTB..)
# RFMTS segment via K-means clustering
# Recency score	- 4 = høj recency (kunder der har købt for nyligt)
# Frequency score	- købsfrekvens, 4 = høj
# Monetary value score - kundens værdi (DB2), 4 = høj	
# Tenure score - varighed af medlemsskab 
# Streaming score	- streamingfrekvens, 4 = høj
# RFM score	- R + F + M, min. = 3, max = 12
# RFMS score - R + F + M + S, min. = 4, max. = 16	
# ---------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------
# Individuelle og kombinerede scores
# ---------------------------------------------------------------------------------

source("D:/R/RScripts/customerSegments_Scores/individual_scores_update.R")


# update April 2020:

# we no lobger use kmeans, but divide customers into gold, silver and bronze 
# segments based on quantiles

# df %>% 
#   select(Customer_Key, Type, Recency, Frequency, Monetary, Tenure, Streaming,
#          RFM_score, RFMS_score) %>%
#   mutate(RFMTS_score = Recency+Frequency+Monetary+Tenure+Streaming) %>%
#  # mutate(Segment = case_when(RFMTS_score <= 9 ~ "Bronze",
# #                       RFMTS_score >= 10 & RFMTS_score <= 14 ~ "Silver",
# #                       RFMTS_score >= 15 ~ "Gold"))
#or Segment = #ntile(RFMTS_score,3))
# ---------------------------------------------------------------------------------
# K-means Clustering
# ---------------------------------------------------------------------------------

source("D:/R/RScripts/customerSegments_Scores/kmeans_predict_update.R")

# ---------------------------------------------------------------------------------
# Output
# ---------------------------------------------------------------------------------

# join data
output <- df %>%
  ungroup() %>%
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

