### CHURN MODEL ###

#Set Seed
set.seed(1234)

#Selection of last year buyers (the others are inactive)
last_year_products <- df_7_tic_clean_final %>% group_by(ID_CLI) %>% summarise(cont = n_distinct(ID_ARTICOLO)) #$ID_CLI
last_year_clients <- last_year_products$ID_CLI
df1_active_buyers <- df_1_cli_fid_clean[df_1_cli_fid_clean$ID_CLI %in% last_year_clients,]

#Selection of clients that activated the card after 2018-11-01 (they weren't active from the beginning of lookback period)
df1_churn <- df1_active_buyers[df1_active_buyers$FIRST_DT_ACTIVE >='2018-11-01',]

#Deleting clients with disabled cards and any purchases after 2019-02-01 (there are no information about the date when they became churners)
record_before <- df_7_tic_clean_final[df_7_tic_clean_final$TIC_DATETIME >='2019-02-01',] %>% group_by(ID_CLI) %>% summarise(cont = n_distinct(ID_ARTICOLO))
churner_nochurner <- c(record_before[record_before$ID_CLI %in% df1_churn[df1_churn$LAST_STATUS_FID==0,]$ID_CLI,]$ID_CLI,df1_churn[df1_churn$LAST_STATUS_FID==1,]$ID_CLI)
churner_nochurner
df1_churn <- df1_churn[df1_churn$ID_CLI %in% churner_nochurner,]

#Binary Churn variable: holdout period churners are active clients that d sono coloro che hanno disattivato la carta ma hanno fatto acquisti dopo il 2019-02-01 
df1_churn$churner <- ifelse(df1_churn$ID_CLI %in% record_before[record_before$ID_CLI %in% df1_churn[df1_churn$LAST_STATUS_FID==1,]$ID_CLI,]$ID_CLI,0,1)


#Purchasing behaviour variable in lookback period (from 2018-11-01 to 2019-01-31)
lookback_period_products <- df_7_tic_clean_final[df_7_tic_clean_final$TIC_DATETIME <'2019-02-01' & df_7_tic_clean_final$TIC_DATETIME >='2018-11-01',] 
lookback_period_products <- lookback_period_products[lookback_period_products$ID_CLI %in% df1_churn$ID_CLI,]

##Number of receipts for each client
##Total amount spent
##Average % discount 
lookback_period_purchases <- lookback_period_products[lookback_period_products$IMPORTO_NETTO>0,]
client_purchases <- lookback_period_purchases %>% group_by(ID_CLI) %>% summarise(num_scontrini = n_distinct(ID_SCONTRINO), costo_totale = sum(IMPORTO_NETTO), sconto_medio = mean(SCONTO_PERCENTUALE))
client_purchases
df1_churn <- merge(df1_churn,client_purchases,by="ID_CLI",all.x=TRUE,incomparables=0)

##Binary return variable (1 = at least a return, 0 = no returns)
lookback_period_returns <- lookback_period_products[lookback_period_products$IMPORTO_NETTO<0,]
client_returns <- lookback_period_returns %>% group_by(ID_CLI) %>% summarise(num_resi = n_distinct(ID_SCONTRINO))
client_returns
df1_churn$resi <- ifelse(df1_churn$ID_CLI %in% client_returns$ID_CLI,1,0)

##Privacy subscription
holdout_client_privacy <- df_4_cli_privacy_clean[df_4_cli_privacy_clean$ID_CLI %in% df1_churn$ID_CLI,]
df1_churn <- merge(df1_churn,holdout_client_privacy,by="ID_CLI",all.x=TRUE,incomparables=0)

##Number of campaigns during callback period
holdout_callback_campaigns <- df_6_camp_event_clean_final[df_6_camp_event_clean_final$ID_CLI %in% df1_churn$ID_CLI,]
holdout_callback_campaigns <- holdout_callback_campaigns[holdout_callback_campaigns$SEND_DATE<'2019-02-01' & holdout_callback_campaigns$SEND_DATE>='2018-11-01',]
holdout_callback_campaigns_by_client <- holdout_callback_campaigns %>% group_by(ID_CLI) %>% summarise(n_campagne = n_distinct(ID_CAMP),percentuale_aperte=sum(OPENED)/n_distinct(ID_CAMP), percentuale_cliccate=sum(CLICKED)/n_distinct(ID_CAMP)) 
holdout_callback_campaigns_by_client
df1_churn <- merge(df1_churn,holdout_callback_campaigns_by_client,by="ID_CLI",all.x=TRUE,incomparables=0)
df1_churn[is.na(df1_churn)] <- 0

#Unused variables deletion (df1)
df1_churn
drop <- c("ID_FID","LAST_COD_FID","LAST_TYP_CLI_FID","LAST_STATUS_FID","LAST_DT_ACTIVE","FIRST_ID_NEG","FIRST_DT_ACTIVE","NUM_FIDs","rfm_label")
df1_churn = df1_churn[,!(names(df1_churn) %in% drop)]

#Collinearity absence check
var_quantitative <- c("num_scontrini","costo_totale","sconto_medio","n_campagne","percentuale_aperte","percentuale_cliccate")
M = cor(df1_churn[var_quantitative])
corrplot(M, method = 'number')

#Continuous variables standardization
scaling <- preProcess(df1_churn[var_quantitative], method = c("scale", "BoxCox"))
var_quantitative_2 = predict(scaling, newdata = df1_churn[var_quantitative])
df1_churn[var_quantitative] =  var_quantitative_2

#Categorical variables dummization
var_categoriali <- c("churner","resi","FLAG_PRIVACY_1","FLAG_PRIVACY_2","FLAG_DIRECT_MKT")
for (i in var_categoriali){
  df1_churn[,i] <- as.factor(df1_churn[,i])
}

#Train set and validation set split 
intrain<- createDataPartition(df1_churn$churner,p=0.8,list=FALSE)
train_set<- df1_churn[intrain,]
test_set<- df1_churn[-intrain,]

#Model training (balanced by class)

#Class weights 
prop.table(table(train_set$churner))
model_weights <- ifelse(train_set$churner == "Not_Churner",
                        (1/table(train_set$churner)[1]),
                        (1/table(train_set$churner)[2]))
length(model_weights)

train_set_churners <- train_set[train_set$churner=='Churner',]

levels(train_set$churner) <- c("Not_Churner", "Churner")
levels(test_set$churner) <- c("Not_Churner", "Churner")

##LOGISTIC

#training and tuning
Control <- trainControl(method= "cv", number=5, summaryFunction = twoClassSummary, classProbs = TRUE)
glm <- train(churner ~ resi + FLAG_PRIVACY_1 + FLAG_PRIVACY_2 + FLAG_DIRECT_MKT + n_campagne + percentuale_aperte + percentuale_cliccate + num_scontrini + costo_totale + sconto_medio, data=train_set, method = "glm", metric='Spec', trControl = Control, tuneLength=5, trace=FALSE, na.action = na.pass, maximize=TRUE,weights = model_weights)
glm
#training results
summary(glm)
y_glm <- predict(glm, train_set, type = "raw")
y_glm
confusionMatrix(y_glm, train_set$churner)

##DECISION TREE

#training and tuning
Control_2 <- trainControl(method = "cv", number=10, summaryFunction=twoClassSummary, search="grid", classProbs = TRUE)
tree <- train(churner ~ resi + FLAG_PRIVACY_1 + FLAG_PRIVACY_2 + FLAG_DIRECT_MKT + n_campagne + percentuale_aperte + percentuale_cliccate + num_scontrini + costo_totale + sconto_medio, data = train_set, metric='Spec', method = "rpart", trControl = Control_2, tuneLength=5, weights = model_weights)
tree
#training results
predicted_tree<-predict(tree, train_set, type = 'raw')
table_acc <- table(predicted_tree, train_set$churner); table_acc
accuracy_tree <- sum(diag(table_acc)) / nrow(train_set)
print(paste("Accuracy:", round(accuracy_tree, 4)))
y_tree <- predict(tree, train_set, type = "raw")
y_tree
confusionMatrix(y_tree, train_set$churner)

##RANDOM FOREST

#training and tuning
Control_3 = trainControl(method="cv", number=10, search="grid", summaryFunction = twoClassSummary, classProbs = TRUE)
tunegrid = expand.grid(.mtry=c(3:5))
forest = train(churner ~ resi + FLAG_PRIVACY_1 + FLAG_PRIVACY_2 + FLAG_DIRECT_MKT + n_campagne + percentuale_aperte + percentuale_cliccate + num_scontrini + costo_totale + sconto_medio, data=train_set, method="rf", metric='Spec', tuneGrid=tunegrid, ntree=50, trControl=Control_3, weights = model_weights)
forest
#training results
plot(forest)
forest$results
getTrainPerf(forest)
y_forest <- predict(forest, train_set, type = "raw")
y_forest
confusionMatrix(y_forest, train_set$churner)
#variable importance
varImp(forest)

##NEURAL NETWORK

#training and tuning
Control_4 = trainControl(method="cv", number=5,  classProbs = TRUE, summaryFunction = twoClassSummary, search="grid")
nnetwork <- train(churner ~ resi + FLAG_PRIVACY_1 + FLAG_PRIVACY_2 + FLAG_DIRECT_MKT + n_campagne + percentuale_aperte + percentuale_cliccate + num_scontrini + costo_totale + sconto_medio, data=train_set, method = "nnet", metric="Spec", tuneLength = 4, preProcess = "range", trControl=Control_4, trace = FALSE, maxit = 100, weights = model_weights)
nnetwork
#training results
y_nnetwork <- predict(nnetwork, train_set, type = "raw")
y_nnetwork
confusionMatrix(y_nnetwork, train_set$churner)


#Roc curves - Models comparison

y=test_set$churner
y_glm = predict(glm, test_set, "prob")[,1]
predglm <- prediction(y_glm,y)
roc.glm = performance(predglm, measure="tnr",x.measure="fnr")
y_tree = predict(tree, test_set, "prob")[,1]
predtree <- prediction(y_tree,y)
roc.tree = performance(predtree, measure="tnr",x.measure="fnr")
y_rf = predict(forest, test_set, "prob")[,1]
predrf <- prediction(y_rf,y)
roc.rf = performance(predrf, measure="tnr",x.measure="fnr")
y_nn = predict(nnetwork, test_set, "prob")[,1]
prednn <- prediction(y_nn,y)
roc.nn = performance(prednn, measure="tnr",x.measure="fnr")

#ROC curves plot
plot(roc.glm, lwd = 2, cex.lab = 1.3)
plot(roc.tree,add=T,col="red", lwd = 2)
plot(roc.rf,add=T,col="green", lwd = 2)
plot(roc.nn,add=T,col="purple", lwd = 2)
legend("bottomright", c("Logistic", "Tree", "Random Forest", "Neural Network"), lty=1,lwd = 2,
       col = c("black","red", "green","purple"), bty="n", inset=c(0.05,0.05))

#Validation set metrics computation - Accuracy, Specificity, Sensitivity and AUC
y_glm_test <- predict(glm, test_set, type = "raw")
confusionMatrix(y_glm_test, test_set$churner)
auc(test_set$churner, y_glm)

y_tree_test <- predict(tree, test_set, type = "raw")
confusionMatrix(y_tree_test, test_set$churner)
auc(test_set$churner, y_tree)

y_rf_test <- predict(forest, test_set, type = "raw")
confusionMatrix(y_rf_test, test_set$churner)
auc(test_set$churner, y_rf)

y_nnetwork_test <- predict(nnetwork, test_set, type = "raw")
confusionMatrix(y_nnetwork_test, test_set$churner)
auc(test_set$churner, y_nn)

#Best Churn classification model selection

#Random Forest reach the best performances: both accuracy and AUC are greater than 80% 
#Also it classifies well both churners and non churners (both sensitivity and specificity are greater than 80% too!!)

