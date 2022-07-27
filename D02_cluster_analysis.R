### CLUSTER ANALYSIS ###

## Creation of cluster variables ##

# A reference period is defined: #
# only events occurred after 1st of February 2019 are considered #
# Total number of receipts and products for each client #
# Average % discount for each client #
df7_art <- df_7_no_resi[df_7_no_resi$TIC_DATETIME >= '2019-02-01',] %>%
  group_by(ID_CLI) %>% 
  summarise(n_scontrini = n_distinct(ID_SCONTRINO), n_prodotti = n(), avg_discount = mean(SCONTO_PERCENTUALE))

# Average amount spent for receipts #
df7_scont_avg <- df_7_no_resi[df_7_no_resi$TIC_DATETIME >= '2019-02-01',] %>%  
  group_by(ID_CLI, ID_SCONTRINO) %>%
  summarise(somma_spesa_scontrini = sum(IMPORTO_NETTO)) %>%
  group_by(ID_CLI) %>%
  summarise(media_spesa_scontrini = mean(somma_spesa_scontrini))
df7_scont_avg <- df7_scont_avg[df7_scont_avg$media_spesa_scontrini < 5000,]

# Total number of clicks for each client #
df_clicks1 <- df_clicks[df_clicks$CLICK_DATE >='2019-02-01',] %>%
  group_by(ID_CLI) %>% 
  summarise(n_clicks_tot = sum(NUM_CLICKs))

df_clst <- left_join(df7_art, df7_scont_avg, by = "ID_CLI")

## Setting infinite values to NA and deleting NAs ##
df_clst <- do.call(data.frame,lapply(df_clst,function(x) replace(x, is.infinite(x), NA)))
df_clst <- na.omit(df_clst)

df_clst <- left_join(df_clst, df_clicks1, by = "ID_CLI")

## Setting NA values to 0 ##
df_clst[is.na(df_clst)] <- 0

## Evaluate correlation among cluster variables ##
M = cor(df_clst)
corrplot(M, method = 'number')

# Scale some cluster variables #
scale <- c("media_spesa_scontrini", "n_scontrini", "n_prodotti", "n_clicks_tot")
not_scale <- c("ID_CLI","avg_discount")
df_clst_scale <- scale(df_clst[scale])
df_kmeans_scale <- cbind(df_clst[,not_scale],df_clst_scale)
df_kmeans_scale

## K-MEANS WITH 5 CLUSTERS ##
set.seed(14219)
km_scale <- kmeans(df_kmeans_scale[2:6], centers = 5)
km_scale

df_cluster <- cbind(df_clst[2:6], cluster = km_scale$cluster)
df_cluster

aggregate(.~cluster, data=df_cluster, FUN= mean)

