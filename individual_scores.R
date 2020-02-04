# customer scores
# january 2020
# Peer Christensen

library(RODBC)
library(lubridate)
library(tidyverse)

# -----------------------------------------------------------
# get the data

credentials <- read_rds("credentials.rds")

channel <-odbcConnect(credentials[1], uid=credentials[2], pwd=credentials[3])

#RFMT
sqlquery1 <- "SELECT [DateOrdered_Key], t1.[Customer_Key], [5_DB2],IsActive
              FROM [EDW].[fact].[OrderFact] [t1]
              inner JOIN [DataMartMisc].[temp].[PremiumSubscribers_Active] [t2] on t2.[Customer_Key] = t1.[Customer_Key]
                where t1.Customer_Key != -1
                and DateCancelled_Key = -1
                and [DateOrdered_Key] >= (SELECT CONVERT(INT, CONVERT(VARCHAR(8), GETDATE()-365 * 1, 112)))
                "

df <- sqlQuery(channel, sqlquery1)


# streaming
sqlquery2 <- "SELECT DISTINCT [Premium_Subscribed].[Customer_Key], ISNULL(t.[n],0) [n]
                FROM [DataMartMisc].[temp].[Premium_Subscribed] 
                LEFT JOIN 
                (
                SELECT [t1].[Customer_Key]
                      ,count([t1].[Customer_Key]) as [n]
                  FROM [EDW].[fact].[ReaderFact] [t1]
                  Where [Streaming_Key] = 1
                  and [IsBucketRead_Key] = 1
                  and [Date_Key] >= (SELECT CONVERT(INT, CONVERT(VARCHAR(8), GETDATE()-365 * 1, 112)))
                  and [Premium_Key] = 1
                 group by [t1].[Customer_Key]
                ) t ON [Premium_Subscribed].[Customer_Key] = t.[Customer_Key]
                ORDER BY [Premium_Subscribed].[Customer_Key]" 

df2 <- sqlQuery(channel, sqlquery2)

# segment types

sqlquery3 <- "SELECT DISTINCT Customer_Key,Segment as Type
              FROM [DataMartMisc].[r].[CLTVinput]"

df3 <- sqlQuery(channel, sqlquery3)


df <- df %>%
  as_tibble() %>%
  mutate(Date = ymd(DateOrdered_Key)) %>%
  select(Customer_Key, Date, DB2 = `5_DB2`)

# ------------------------------------------------------------

# RECENCY

last_date <- max(as_date(df$Date))

recency <- df %>%
  group_by(Customer_Key) %>%
  summarise(RecencyDays = as.numeric(last_date - max(as_date(Date)))) %>%
  mutate(R_Quantile = ntile(desc(RecencyDays), 4)) # quartile

# FREQUENCY

frequency <- df %>%
  group_by(Customer_Key) %>%
  summarise(Frequency = n()) %>%
  mutate(F_Quantile = ntile(Frequency, 4)) # quartile

# MONETARY

monetary <- df %>% 
  group_by(Customer_Key) %>%
  summarise(Monetary = sum(DB2)) %>%
  mutate(M_Quantile = ntile(Monetary, 4)) # quartile

# TENURE

tenure <- df %>%
  group_by(Customer_Key) %>%
  summarise(Tenure = as.numeric(last_date - min(as_date(Date)))) %>%
  mutate(T_Quantile = ntile(Tenure, 4)) # quartile

# STREAMING

streaming <- df2 %>%
  rename(Streaming = n) %>%
  mutate(S_Quantile = ntile(Streaming, 4))


# JOIN

rfmts <- list(recency,frequency,monetary,tenure,streaming) %>% 
  reduce(left_join) %>%
  select(contains("_")) %>% 
  left_join(df3) %>%
  mutate(Type = as.character(Type)) %>% 
  mutate(Type = replace_na(Type,"Unknown")) %>%
  select(Customer_Key,Type,everything()) %>%
  mutate(RFM_score = R_Quantile+F_Quantile+M_Quantile,
         RFMS_score =  R_Quantile+F_Quantile+M_Quantile+S_Quantile)





