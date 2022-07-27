### MARKET BASKET ANALYSIS ###

#Memory limit extention
memory.limit(30000)

library(MASS)
#Sometimes it gives an error: it works by rerunning it
products <- df_7_no_resi %>% group_by(ID_ARTICOLO) %>% summarise(reparto = paste(sort(unique(COD_REPARTO)),collapse=","), importo_medio = mean(IMPORTO_LORDO), perc_scontrini = n_distinct(ID_SCONTRINO)/num_scontrini)
products <- products[products$importo_medio>0,] 
products <- do.call(data.frame,lapply(products,function(x) replace(x, is.infinite(x), NA)))
products <- na.omit(products)

#Barplot of number of products by department
products %>%
  ggplot(aes(x = fct_infreq(reparto),fill=fct_infreq(reparto))) +
  geom_bar() +
  labs(x = "department", y='number of products') +
  scale_fill_manual(values=c("#B18FF9",
                             "#9062F0",
                             "#6935D6",
                             "#5F2BE2",
                             "#3B60AF",
                             "#658ADA",
                             "#97B2EC",
                             "#97ECC2",
                             "#61AB86",
                             "#428563",
                             "#008880",
                             "#558888",
                             "#888888",
                             "#CC8888"
                             )) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16),
    panel.background = element_rect(fill='white'),
    plot.background = element_rect(fill='white'))

#Barplot of average product prices by department
products %>%
  ggplot(aes(x = reorder(reparto,-importo_medio, median, order = TRUE), y = importo_medio,fill=fct_infreq(reparto))) +
  geom_boxplot() +
  scale_y_log10(breaks = scales::log_breaks(n = 10)) +
  labs(x = "department", y='product prices') +
  scale_fill_manual(values=c("#B18FF9",
                             "#9062F0",
                             "#6935D6",
                             "#5F2BE2",
                             "#3B60AF",
                             "#658ADA",
                             "#97B2EC",
                             "#97ECC2",
                             "#61AB86",
                             "#428563",
                             "#008880",
                             "#558888",
                             "#888888",
                             "#CC8888"
  )) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16),
    panel.background = element_rect(fill='white'),
    plot.background = element_rect(fill='white'))

#Number of total receipts
num_scontrini <- n_distinct(df_7_no_resi$ID_SCONTRINO)
num_scontrini

#Group by product and product support computation
df_7_tic_grouped <- df_7_no_resi %>% group_by(ID_ARTICOLO)
cont <- df_7_tic_grouped %>% summarise(conteggio = n_distinct(ID_SCONTRINO)/num_scontrini) %>% arrange(desc(conteggio))

#Product support distribution 
cont[cont$conteggio>0.01,]$ID_ARTICOLO# 3 articles with support higher that 1%
cont[cont$conteggio<=0.01 & cont$conteggio>0.001,]$ID_ARTICOLO # 313 articles with support lower that 1% and higher than 0.1%
cont[cont$conteggio<=0.001 & cont$conteggio>0.0001,]$ID_ARTICOLO# 8528 articles with support lower that 0.1% and higher than 0.01%
cont[cont$conteggio<=0.0001 & cont$conteggio>0.00001,]$ID_ARTICOLO # 36056 articles with support lower that 0.01% and higher than 0.001%
cont[cont$conteggio<=0.00001 &cont$conteggio>0.000001,]$ID_ARTICOLO # 51447 articles with support lower that 0.001% and higher than 0.0001%
cont[cont$conteggio<=0.000001,]$ID_ARTICOLO # 0 articles with support lower that 0.0001%

#Selection of high support products (>0.1%)
cont_1 <- cont[cont$conteggio>0.001, ]
cont_1
only_high_support <- df_7_no_resi[df_7_no_resi$ID_ARTICOLO %in% cont_1$ID_ARTICOLO,]

#Test high support products department uniqueness and average product price computation 
high_support_products <- only_high_support %>% group_by(ID_ARTICOLO) %>% summarise(reparto = paste(sort(unique(COD_REPARTO)),collapse=","), importo_medio = mean(IMPORTO_NETTO), perc_scontrini = n_distinct(ID_SCONTRINO)/num_scontrini)

#Histogram of high support product prices
ggplot(high_support_products, aes(x=importo_medio)) + 
  geom_histogram(color="black", fill="lightblue")

#Barplot of number of high support products by department
high_support_products %>%
  ggplot(aes(x = fct_infreq(reparto))) +
  geom_bar(color="black", fill='pink') +
  labs(x = "reparto") +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16),
    panel.background = element_rect(fill='white'),
    plot.background = element_rect(fill='white'))

#Average product prices by department
aggregate(.~reparto, data=high_support_products, FUN= mean) %>% arrange(desc(importo_medio))


#Group by receipts, number of products computation and binary variables (presence/absence of each product)
list_scontrini <- only_high_support %>% group_by(ID_SCONTRINO) %>% summarise(num_articoli = n_distinct(ID_ARTICOLO),art = paste(sort(unique(ID_ARTICOLO)),collapse=",")) %>% arrange(desc(num_articoli))
list_scontrini2 <- list_scontrini %>% ungroup() %>% cSplit_e("art", type =  "character",sep = ",", fill = FALSE) #[, dcast(.SD, ID_SCONTRINO ~ type)]
list_scontrini2
type_index = which(colnames(mtcars)=="art")
list_scontrini3 <- list_scontrini2[,-which(names(list_scontrini2) %in% c("art","ID_SCONTRINO","num_articoli"))]
list_scontrini3

magic_fun <- function(x){ifelse(x==1,TRUE,FALSE)}
list_scontrini3 <- data.frame(lapply(list_scontrini3,magic_fun))

#Number of receipts with at least one high support product (>0.1%)
num_scontrini_2 <- n_distinct(only_high_support$ID_SCONTRINO)

#Adding records of receipts with low support products only - just for correct supports computation
n_columns_to_add <- num_scontrini-num_scontrini_2
n_columns_to_add
len_articoli <- length(colnames(list_scontrini3))
list_scontrini4 <- data.frame(matrix(FALSE,ncol = len_articoli, nrow = n_columns_to_add))
x <- colnames(list_scontrini3)
colnames(list_scontrini4) <- x
list_scontrini5 <- rbind(list_scontrini3,list_scontrini4)

#Market Basket Analysis with thresholds: support > 0.1%  and confidence > 65%, sets of 1 to 3 products

##If association rule generation function doesn't work, run the next four lines
detach("package:arulesViz", unload=TRUE)
detach("package:arules", unload=TRUE)
library("arules", lib.loc="[wherever your R libraries go]")
library(arules)

rules <- apriori(list_scontrini5,parameter = list(supp = 0.001, conf = 0.65, minlen = 1, maxlen = 3, target = "rules")) 
inspect(rules)

#Redundant association rules deletion --> any redundant rules
redundant <- is.redundant(rules, measure="confidence")
which(redundant)
rules.pruned <- rules[!redundant]
rules.pruned <- sort(rules.pruned, by="lift")
inspect(rules.pruned)

#Association rules representation by support, lift and confidence
library(arulesViz)
plot(rules, method = NULL, measure = "support", shading = "lift",colors=c("#143B56", "#90CAF2"))+
  geom_point(size=4, shape=16) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 16))

subrules <- head(rules, n = 15, by = "lift")
plot(subrules, method = "graph",  engine = "visNetwork", selection_menu = FALSE, measure="lift")

#Average driving and driven product prices computation and associated products department extraction
df_mbcr = data.frame(
  lhs = labels(lhs(rules)),
  rhs = labels(rhs(rules)), 
  rules@quality)
df_mbcr$lhs_reparto <- NA
df_mbcr$rhs_reparto <- NA
df_mbcr$lhs_importo_medio <- NA
df_mbcr$rhs_importo_medio <- NA

for(i in 1:nrow(df_mbcr)){
  rhs_product <- high_support_products[high_support_products$ID_ARTICOLO == as.integer(substr(df_mbcr$rhs[i],6,13)),]
  df_mbcr$rhs_reparto[i] <- rhs_product$reparto
  df_mbcr$rhs_importo_medio[i] <- rhs_product$importo_medio
  if(nchar(df_mbcr$lhs[i])==27){
    lhs_product_1 <- high_support_products[high_support_products$ID_ARTICOLO == as.integer(substr(df_mbcr$lhs[i],6,13)),]
    lhs_product_2 <- high_support_products[high_support_products$ID_ARTICOLO == as.integer(substr(df_mbcr$lhs[i],19,26)),]
    df_mbcr$lhs_importo_medio[i] <- (lhs_product_1$importo_medio + lhs_product_2$importo_medio)/2
    if (lhs_product_1$reparto == lhs_product_2$reparto){
      df_mbcr$lhs_reparto[i] <- lhs_product_1$reparto
    }
    else{
      df_mbcr$lhs_reparto[i] <- paste(lhs_product_1$reparto,lhs_product_2$reparto,sep=' ')
    }
  }
  else{
    lhs_product <- high_support_products[high_support_products$ID_ARTICOLO == as.integer(substr(df_mbcr$lhs[i],6,13)),]
    df_mbcr$lhs_reparto[i] <- lhs_product$reparto
    df_mbcr$lhs_importo_medio[i] <- lhs_product$importo_medio
    }
}  

#Distribution of associated products average prices
ggplot(df_mbcr, aes(x=lhs_importo_medio, y=rhs_importo_medio)) +
  geom_point(size=2.5, shape=16, color='#0E4E7A') +
  geom_smooth(method=lm,color='black',size=0.7, fullrange=TRUE, se=FALSE) +
  xlim(1.25,27) + ylim(1.25,27) +
  scale_x_continuous(breaks = round(seq(min(df_mbcr$lhs_importo_medio), max(df_mbcr$lhs_importo_medio), by = 2),0)) +
  scale_y_continuous(breaks = round(seq(min(df_mbcr$rhs_importo_medio), max(df_mbcr$rhs_importo_medio), by = 2),0)) +
  labs(x = "Driving product prices",
       y = "Driven product price") +
  theme(
    axis.text = element_text(size = 12),
    panel.background = element_rect(fill='white'),
    plot.background = element_rect(fill='white'),
    axis.line = element_line(size = 0.1, colour = "black", linetype=1),
    axis.title = element_text(size = 16))
