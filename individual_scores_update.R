# individual scores update



# customer scores
# january 2020
# Peer Christensen

library(RODBC)
library(lubridate)
library(tidyverse)

# -----------------------------------------------------------
# get the data

#credentials <- read_rds("credentials.rds")

channel <-odbcConnect("saxo034", uid="R", pwd="sql2017")

sqlquery <- "SELECT [Customer_Key]
      ,[IsActive]
      ,[Buckets]
      ,[FirstOrderDate]
      ,[LatestOrderDate]
      ,[Orders]
      ,[DB2]
      ,[Segment]
  FROM [DataMartMisc].[r].[CustomerSegmentationInput]"

df <- sqlQuery(channel, sqlquery)

df <- df %>%
  as_tibble() %>%
  mutate(FirstOrderDate = ymd(FirstOrderDate),
         LatestOrderDate = ymd(LatestOrderDate))

# ------------------------------------------------------------

df <- df %>%
  mutate(RecencyDays = as.numeric(today() - LatestOrderDate),
         Duration    = today() - FirstOrderDate) %>%
  mutate(Recency     = ntile(desc(RecencyDays), 4),
         Frequency   = ntile(Orders, 4),
         Monetary    = ntile(DB2, 4),
         Tenure      = ntile(Duration, 4),
         Streaming   = ntile(Buckets, 4)) %>%
  mutate(RFM_score   = Recency+Frequency+Monetary,
         RFMS_score  = Recency+Frequency+Monetary+Streaming) %>%
  select(Customer_Key, Type = Segment, everything())




