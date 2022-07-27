### RFM MODEL ###

## In order not only to define segments of customers, but also to capture and analyse ##
## the flow of customers among the different segments from a reference period to another ##
## RFM analysis is executed separately on 2 consecutive reference periods.##
## Results are then compared ##

## The same preprocessing operations are applied to both reference periods and ##
## the same rules for defining the segments scores are used ##


#### PERIOD 2 ####


### RFM - MODEL PREPROCESSING ###

## 3 months reference period : 2019-02-01 to 2019-04-30 (last available date in the dataset) ##

## Customer base: customers who were active for the entire period, that is customers ##
## 1) whose fidelity card is active and that was activated before the start of the reference period ##
## 2) who made at least a purchase in the reference period ###


### Defining the customer base to fit the model on ###
customer_base <- df_1_cli_fid_last[df_1_cli_fid_last$STATUS_FID == 1 
                                   & df_1_cli_fid_last$DT_ACTIVE < '2019-02-01', ]
customer_base$ID_NEG <- NULL
customer_base_purchases <- left_join(df_7_tic_clean_final, customer_base, by = "ID_CLI")


### Selection of purchases made in the reference period ###
last_purchases <- customer_base_purchases[customer_base_purchases$TIC_DATE >= '2019-02-01',]
last_date <- max(customer_base_purchases$TIC_DATE)

## Removing transactions regarding products that were purchased and then refunded ##
## & transactions with net import = 0 ##
last_purchases <- last_purchases %>% 
  group_by(ID_CLI, COD_REPARTO, ID_NEG, ID_ARTICOLO) %>%
  filter(sum(as.numeric(IMPORTO_LORDO))!=0 & IMPORTO_NETTO > 0)  
cat("The total number of products in the specified reference period\n
    for the selected customer base is", nrow(last_purchases))



### RFM - MODEL FITTING ###

rfm <- last_purchases %>%
  group_by(ID_CLI) %>%
  summarise(Recency = as.numeric(difftime(time1 = last_date, 
                                          time2 = max(TIC_DATE), 
                                          units = "days")), 
            Frequency = n_distinct(ID_SCONTRINO), 
            Monetary = sum(IMPORTO_NETTO))
summary(rfm)

## exploratory plots ##
r <- ggplot(rfm) +
  geom_density(aes(x= Recency))
f <- ggplot(rfm) +
  geom_count(aes(x = Frequency))
m <- ggplot(rfm) +
  geom_density(aes(x = Monetary))

## Tackling with highly positively skewed distributions of Monetary and Frequency ##
# Keeping values below the 99% percentile of the distribution of Monetary and of Frequency #
rfm <- rfm[rfm$Monetary < quantile(rfm$Monetary, 0.99) 
            & rfm$Frequency < quantile(rfm$Frequency, 0.99), ]
# Scaling of the logarithm of the Monetary (min-max) #
rfm <- rfm %>% 
  mutate(Monetary_scaled = (log(Monetary)-min(log(Monetary))) / 
                                                    (max(log(Monetary))-min(log(Monetary))))
summary(rfm)

## exploratory plots after changes ##
r <- ggplot(rfm) +
  geom_density(aes(x= Recency))
f <- ggplot(rfm) +
  geom_density(aes(x = Frequency))
m <- ggplot(rfm) +
  geom_density(aes(x = Monetary_scaled))
grid.arrange(r, f, m, nrow = 3)




## Assigning R, F and M scores to each customer ##

# The scoring method may depend on the individual businesses. # 
# In this case, we decided to generate the score by binning the values of each of the #
# 3 deterministic variables into 3 categories: 1,2 and 3. Particularly, #
# a value of 1 is low, therefore Recency = 1 means that the customer last purchase was #
# 'long time ago'; 3 is a high values, hence Frequency = 3 means the customer falls among #
# the customers in the customer base that purchased more often in the reference period. #
#
# As far as Recency is concerned, given the 3-months reference period, we decided to #
# procede this way: #
# R = 1 iff the customer last purchase was within 1 month #
# R = 1 iff the customer last purchase was between 1 and 2 months ago #
# R = 1 iff the customer last purchase was between 2 and 3 months ago #

rfm$R_score <- 0
rfm$R_score[rfm$Recency < 29] <- 3
rfm$R_score[rfm$Recency >= 29 & rfm$Recency <= 58] <- 2
rfm$R_score[rfm$Recency > 58] <- 1

# The definition of the 3 categories for the Frequency and Monetary variables was obtained #
# based on the analysis of the distribution of their values. #
# Specifically, given that more than 50% of the customer base purchased in the 
# reference period only once, and more than 25% purchased in the reference period 
# from 2 to 3 times, we thought to be coherent to use this scoring method:
#
# Frequency = 1 was assigned to all those who purchased only once #
# Frequency = 2 to all those who purchased twice or 3 times, and finally #
# Frequency = 3 to those who purchased more than 3 times  ... #

rfm$F_score <- 0
rfm$F_score[rfm$Frequency == 1] <- 1
rfm$F_score[rfm$Frequency == 2 | rfm$Frequency == 3] <- 2
rfm$F_score[rfm$Frequency > 3] <- 3

# Regarding monetary, considering the above operations made to deal with its skewed #
# distribution and its final shape, we decided to assign the same score to all the #
# customers falling in the central - and greatest part - of the distribution: #
#
# M = 1 to customers with monetary value below the first quartile #
# M = 2 to customers with monetary value between the first and third quartile #
# M = 3 to customers with monetary value above the third quartile #

rfm$M_score <- 0
rfm$M_score[rfm$Monetary_scaled <= quantile(rfm$Monetary_scaled, 0.25)] <- 1
rfm$M_score[rfm$Monetary_scaled > quantile(rfm$Monetary_scaled, 0.25) & 
              rfm$Monetary_scaled < quantile(rfm$Monetary_scaled, 0.75)] <- 2
rfm$M_score[rfm$Monetary_scaled >= quantile(rfm$Monetary_scaled, 0.75)] <- 3


## Combining R, F and M scores into RFM score ##
rfm <- rfm %>% mutate(RFM_score = 100 *R_score +10 * F_score + M_score)

## Dividing customers into 7 different segments according to their RFM score ##
rfm$segment <- "0"
rfm$segment[which(rfm$RFM_score %in% c(333,233))] <- "Diamond"
rfm$segment[which(rfm$RFM_score %in% c(232,133,332))] <- "Gold"
rfm$segment[which(rfm$RFM_score %in% c(331,231,132,223,323))] <- "Silver"
rfm$segment[which(rfm$RFM_score %in% c(123,113,222,322,131))] <- "Bronze"
rfm$segment[which(rfm$RFM_score %in% c(213,313,122,112,221,321))] <- "Copper"
rfm$segment[which(rfm$RFM_score %in% c(312,212,121,111))] <- "Tin"
rfm$segment[which(rfm$RFM_score %in% c(311,211))] <- "Cheap"


# Distribution of customers among the segments #
table(rfm$segment)
# Plot #
ggplot(rfm) + 
  geom_bar(aes(x = segment, fill = segment))+
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(title = "Barplot for Segments of customers")

####





#### PERIOD 1 ####

### RFM - MODEL PREPROCESSING ###

## 3 months reference period : 2018-11-01 to 2019-01-31 ##

### Defining the customer base to fit the model on ###
customer_base2 <- df_1_cli_fid_last[df_1_cli_fid_last$STATUS_FID == 1 
                                   & df_1_cli_fid_last$DT_ACTIVE < '2018-11-01', ]
customer_base2$ID_NEG <- NULL
customer_base_purchases2 <- left_join(df_7_tic_clean_final, customer_base2, by = "ID_CLI")


### Selection of purchases made in the reference period ###
last_purchases2 <- customer_base_purchases2[customer_base_purchases2$TIC_DATE < '2019-02-01' &
                                            customer_base_purchases2$TIC_DATE >= '2018-11-01',]
last_date2 <- max(last_purchases2$TIC_DATE)

## Removing transactions regarding products that were purchased and then refunded ##
## & transactions with net import = 0 ##
last_purchases2 <- last_purchases2 %>% 
  group_by(ID_CLI, COD_REPARTO, ID_NEG, ID_ARTICOLO) %>%
  filter(sum(as.numeric(IMPORTO_LORDO))!=0 & IMPORTO_NETTO > 0)  
cat("The total number of products in the specified reference period\n
    for the selected customer base is", nrow(last_purchases2))



### RFM - MODEL FITTING ###

rfm2 <- last_purchases2 %>%
  group_by(ID_CLI) %>%
  summarise(Recency = as.numeric(difftime(time1 = last_date2, 
                                          time2 = max(TIC_DATE), 
                                          units = "days")), 
            Frequency = n_distinct(ID_SCONTRINO), 
            Monetary = sum(IMPORTO_NETTO))
summary(rfm2)

## exploratory plots ##
r2 <- ggplot(rfm2) +
  geom_density(aes(x= Recency))
f2 <- ggplot(rfm2) +
  geom_count(aes(x = Frequency))
m2 <- ggplot(rfm2) +
  geom_density(aes(x = Monetary))

## Tackling with highly positively skewed distributions of Monetary and Frequency ##
# Keeping values below the 99% percentile of the distribution of Monetary and of Frequency #
rfm2 <- rfm2[rfm2$Monetary < quantile(rfm2$Monetary, 0.99) 
           & rfm2$Frequency < quantile(rfm2$Frequency, 0.99), ]
# Scaling of the logarithm of the Monetary (min-max) #
rfm2 <- rfm2 %>% 
  mutate(Monetary_scaled = (log(Monetary)-min(log(Monetary))) / 
           (max(log(Monetary))-min(log(Monetary))))
summary(rfm2)

## exploratory plots after changes ##
r2 <- ggplot(rfm2) +
  geom_density(aes(x= Recency))
f2 <- ggplot(rfm2) +
  geom_density(aes(x = Frequency))
m2 <- ggplot(rfm2) +
  geom_density(aes(x = Monetary_scaled))
grid.arrange(r2, f2, m2, nrow = 3)
# The distributions are similar between the 2 periods, which justifies the following operations #



## Assigning R, F and M scores to each customer ##

# As a matter of coherence, the reasoning explained above was used to create the scores # 

# Recency #
rfm2$R_score <- 0
rfm2$R_score[rfm2$Recency < 29] <- 3
rfm2$R_score[rfm2$Recency >= 29 & rfm2$Recency <= 58] <- 2
rfm2$R_score[rfm2$Recency > 58] <- 1

# Frequency #
rfm2$F_score <- 0
rfm2$F_score[rfm2$Frequency == 1] <- 1
rfm2$F_score[rfm2$Frequency == 2 | rfm2$Frequency == 3] <- 2
rfm2$F_score[rfm2$Frequency > 3] <- 3

# Monetary #
rfm2$M_score <- 0
rfm2$M_score[rfm2$Monetary_scaled <= quantile(rfm2$Monetary_scaled, 0.25)] <- 1
rfm2$M_score[rfm2$Monetary_scaled > quantile(rfm2$Monetary_scaled, 0.25) & 
              rfm2$Monetary_scaled < quantile(rfm2$Monetary_scaled, 0.75)] <- 2
rfm2$M_score[rfm2$Monetary_scaled >= quantile(rfm2$Monetary_scaled, 0.75)] <- 3


## Combining R, F and M scores into RFM score ##
rfm2 <- rfm2 %>% mutate(RFM_score = 100 *R_score +10 * F_score + M_score)

## Dividing customers into 7 different segments according to their RFM score ##
rfm2$segment <- "0"
rfm2$segment[which(rfm2$RFM_score %in% c(333,233))] <- "Diamond"
rfm2$segment[which(rfm2$RFM_score %in% c(232,133,332))] <- "Gold"
rfm2$segment[which(rfm2$RFM_score %in% c(331,231,132,223,323))] <- "Silver"
rfm2$segment[which(rfm2$RFM_score %in% c(123,113,222,322,131))] <- "Bronze"
rfm2$segment[which(rfm2$RFM_score %in% c(213,313,122,112,221,321))] <- "Copper"
rfm2$segment[which(rfm2$RFM_score %in% c(312,212,121,111))] <- "Tin"
rfm2$segment[which(rfm2$RFM_score %in% c(311,211))] <- "Cheap"


# Distribution of customers among the segments #
table(rfm2$segment)
# Plot #
ggplot(rfm2) + 
  geom_bar(aes(x = segment, fill = segment))+
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(title = "Barplot for Segments of customers")



#### SANKEY DIAGRAM ####

## Merging the dataframes of the 2 periods, to find the common customers ##
final_rfm <- merge(rfm, rfm2, by = "ID_CLI", suffixes = c("_per2", "_per1"))

## Nodes ##
nodes <- data.frame(node = c(0:13), 
                    name = rep(c("Diamond", "Gold", "Silver", "Bronze", "Copper", "Tin", "Cheap"),2))

## Links ##

# Preprocessing #

sankplot1 <- final_rfm[, c("ID_CLI", "segment_per1")]
sankplot1 <- sankplot1 %>% 
  group_by(segment_per1) %>% 
  mutate(count1 = n_distinct(ID_CLI))

sankplot2 <- final_rfm[, c("ID_CLI", "segment_per2")]
sankplot2 <- sankplot2 %>% 
  group_by(segment_per2) %>% 
  mutate(count2 = n_distinct(ID_CLI))
# Merge #
sankjoin <- merge(sankplot1, sankplot2, by = "ID_CLI")
# Add joint column #
sankjoin$joint <- paste(sankjoin$segment_per1, sankjoin$segment_per2, sep="_")
sankjoin <- sankjoin %>% 
  group_by(joint) %>% 
  summarise(value = n_distinct(ID_CLI))
# Split joint column #
sankjoin[c('sou', 'tar')] <- stringr::str_split_fixed(sankjoin$joint, '_', 2)
# Remove joint column #
sankjoin$joint <- NULL
# Transform to plain dataframe #
links <- as.data.frame(sankjoin)
# Create source variable #
links$source[links$sou=='Diamond'] <- 0
links$source[links$sou=='Gold'] <- 1
links$source[links$sou=='Silver'] <- 2
links$source[links$sou=='Bronze'] <- 3
links$source[links$sou=='Copper'] <- 4
links$source[links$sou=='Tin'] <- 5
links$source[links$sou=='Cheap'] <- 6
# Create target variable #
links$target[links$tar=='Diamond'] <- 7
links$target[links$tar=='Gold'] <- 8
links$target[links$tar=='Silver'] <- 9
links$target[links$tar=='Bronze'] <- 10
links$target[links$tar=='Copper'] <- 11
links$target[links$tar=='Tin'] <- 12
links$target[links$tar=='Cheap'] <- 13


# Colors #
my_colors <- 'd3.scaleOrdinal() .domain(["Diamond", "Gold", "Silver", "Bronze", "Copper", "Tin", "Cheap"]) 
.range(["#698B69", "#EEC900" , "gainsboro", "#EE7621", "#EE6363", "#CD69C9", "#8B636C"])'


links$group <- as.factor(c(rep("Bronze", 7), rep("Cheap", 7), rep("Copper", 7), 
                           rep("Diamond", 7), rep("Gold", 7), rep("Silver", 7), rep("Tin", 7)))
nodes$group <- as.factor(c("Diamond", "Gold", "Silver", "Bronze", "Copper", "Tin", "Cheap"))

# Plot #
sankeyNetwork(Links = links, Nodes = nodes, 
              Source = 'source', 
              Target = 'target', 
              Value = 'value',
              NodeID = 'name',
              colourScale = my_colors,
              LinkGroup="group",
              NodeGroup = "group")

