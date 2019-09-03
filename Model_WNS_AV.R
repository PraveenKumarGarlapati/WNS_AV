#Model on WNS AV Hack



# Lin Models --------------------------------------------------------------

lm1 <- lm(is_click ~ ., tr[, -c(1,2,3,10,11,12,13)])
lm1_preds <- predict(lm1, ts)%>%as.data.frame()

colnames(lm1_preds) <- "is_click"

plotROC(ts$is_click, lm1_preds$is_click)

lm1_preds <- lm1_preds%>%
  mutate(is_click = ifelse(is_click < 0.053, 0, 1))

lm1_sub <- data.frame(sample_submission$impression_id, lm1_preds$is_click)
colnames(lm1_sub) <- colnames(sample_submission)
write_csv(lm1_sub, "lm1_sub.csv")

summary(lm1)

# Logistic Model ----------------------------------------------------------

train$is_click <- as.numeric(train$is_click)

glm1 <- glm(is_click ~ ., 
            family=binomial(link='logit'),
            data=train[, -c(1,2,3,10,11,12,13)])

glm1_preds <- plogis(predict(glm1, test))%>%as.data.frame()
colnames(glm1_preds) <- "is_click"

# glm1_preds%>%
#   mutate(is_click = ifelse(is_click < 0.053, 0, 1))%>%
#   count(is_click)

glm1_preds <- glm1_preds%>%
  mutate(is_click = ifelse(is_click < -2.9, 0, 1))

glm1_sub <- data.frame(sample_submission$impression_id, glm1_preds$is_click)
colnames(glm1_sub) <- colnames(sample_submission)
write_csv(glm1_sub, "glm1_sub.csv")


# Split the data and test again  ------------------------------------------
y <- train$is_click

set.seed(100)
valid <- createDataPartition(y, p=0.8, list=FALSE)  
tr <- train[valid, ]
ts <- train[-valid, ]


# Running a Random Forest -------------------------------------------------


tr[ ,c(4,7,14,15)] <- map(tr[ ,c(4,7,14,15)], as.factor)

glimpse(tr)
glimpse(tr[, -c(1,2,3,10,11,12,13)])

#trainbck <- train

rf1 <- randomForest(is_click~ .,
                    data = tr[, -c(1,2,3,10,11,12,13)])



# Run an XGBoost Model ----------------------------------------------------

train_m <- train[, -c(1,2,3,10,11,13)]
train_m[ ,-c(4,5,6)] <- map(train_m[ ,-c(4,5,6)], as.factor)

test$is_click <- NA
test_m <- test[, -c(1,2,3,7,8,10)]
test_m <- test_m%>%
  select(app_code, os_version, is_4G, is_click, freq_user_website,
         freq_userlevel,day,hour,mins)
test_m[ ,-c(4,5,6)] <- map(test_m[ ,-c(4,5,6)], as.factor)

all <- rbind(train_m, test_m)

all_ohe <- dummy_cols(all, select_columns = c("app_code","os_version","is_4G","day","hour","mins"))
all_ohe <- all_ohe%>%
  select(-app_code, -os_version, 
         -is_4G, -day, -hour, -mins)

all_ohe%>%colnames()

#Breaking down back to train and test
test_m_ohe <- all_ohe%>%
  filter(is.na(is_click))

trainm_ohe <- all_ohe%>%
  filter(!is.na(is_click))

# 
# 
# train_m_ohe <- dummy_cols(train_m, select_columns = c("app_code","os_version","is_4G","day","hour","mins"))
# train_m_ohe <- train_m_ohe%>%
#   select(-app_code, -os_version, 
#          -is_4G, -day, -hour, -mins)
# 
# y <- train_m_ohe$is_click

# set.seed(100)
# valid <- createDataPartition(y, p=0.8, list=FALSE)  
# tr <- train_m_ohe[valid, ]
# ts <- train_m_ohe[-valid, ]
# 
# y = tr$is_click
# 
# tr <- as.matrix(tr)
# ts <- as.matrix(ts)
# dtrain <- xgb.DMatrix(data = tr[ ,-1], label = tr[ ,1])
# dtest <- xgb.DMatrix(data = ts[ ,-1], label = ts[ ,1])
# watchlist <- list(train=dtrain, test=dtest)

trainm_ohe <- as.matrix(trainm_ohe)
test_m_ohe <- as.matrix(test_m_ohe)
dtrain <- xgb.DMatrix(data = trainm_ohe[ ,-1], label = trainm_ohe[ ,1])
dtest <- xgb.DMatrix(data = test_m_ohe[ ,-1])

xgb <- xgb.train(data = dtrain, 
               eta = 0.1,
               max_depth = 15, 
               nround=25, 
               eval_metric = "auc",
               objective = "binary:logistic",
               verbose = 1)

xgb_preds <- predict(xgb, dtest)%>%as.data.frame()
colnames(xgb_preds) <- "is_click"

xgb_sub <- data.frame(sample_submission$impression_id, xgb_preds$is_click)
colnames(xgb_sub) <- colnames(sample_submission)
write_csv(xgb_sub, "xgb_sub.csv")

names <- dimnames(data.matrix(trainm_ohe[,-1]))[[2]]
importance_matrix <- xgb.importance(names, model = xgb)
# Nice graph
xgb.plot.importance(importance_matrix[1:10,])

# Iteration to find the best f1 score -------------------------------------


#Finding the best F1 Score
df <- data.frame()
for (i in seq(0.01,0.99,0.001)){
  threshold <- i
  
  res <- confusionMatrix(ts$is_click, lm1$is_click, threshold = i)
  
  prec <- res[2,2]/(res[2,1] + res[2,2])
  recall <- res[2,2]/(res[1,2] + res[2,2])
  f1score <- 2*prec*recall/(prec+recall)
  
  sensitivity <- sensitivity(ts$is_click, lm1_preds$is_click, threshold = i)
  specificity <- specificity(ts$is_click, lm1_preds$is_click, threshold = i)
  
  temp_df <- data.frame(threshold,prec, recall,sensitivity,specificity,f1score)
  df <- rbind(df, temp_df)
}
print(df)

#df%>%write_clip()
df%>%
  filter(f1score == max(f1score, na.rm = T))








# FinalDay Model ----------------------------------------------------------

train <- train%>%
  left_join(views2day)

train <- train%>%
  left_join(allviewstill2day)

train <- train%>%
  left_join(unqitem2day)

test <- test%>%
  left_join(views2day)

test <- test%>%
  left_join(allviewstill2day)

test <- test%>%
  left_join(unqitem2day)

train_m <- train[, -c(1,2,3,10,11,13)]
train_m[ ,-c(4,5,6,10,11,12)] <- map(train_m[ ,-c(4,5,6,10,11,12)], as.factor)

test$is_click <- NA
test_m <- test[, -c(1,2,3,7,8,10)]
test_m <- test_m%>%
  select(app_code, os_version, is_4G, is_click, freq_user_website,
         freq_userlevel,day,hour,mins, views2day,
         allviewstill2day,unqitem2day)
test_m[ ,-c(4,5,6,10,11,12)] <- map(test_m[ ,-c(4,5,6,10,11,12)], as.factor)

train_m[is.na(train_m)] <- 0

all <- rbind(train_m, test_m)

all_ohe <- dummy_cols(all, select_columns = c("app_code","os_version","is_4G","day","hour","mins"))
all_ohe <- all_ohe%>%
  select(-app_code, -os_version, 
         -is_4G, -day, -hour, -mins)

all_ohe%>%colnames()

#Breaking down back to train and test
test_m_ohe <- all_ohe%>%
  filter(is.na(is_click))

trainm_ohe <- all_ohe%>%
  filter(!is.na(is_click))

# fwrite(trainm_ohe, "trainmohe_v2.csv")
# fwrite(test_m_ohe, "testmohe_v2.csv")

trainm_ohe <- fread("trainmohe_v2.csv")
test_m_ohe <- fread("testmohe_v2.csv")

trainm_ohe <- as.matrix(trainm_ohe)
test_m_ohe <- as.matrix(test_m_ohe)
dtrain <- xgb.DMatrix(data = trainm_ohe[ ,-1], label = trainm_ohe[ ,1])
dtest <- xgb.DMatrix(data = test_m_ohe[ ,-1])

xgb <- xgboost(data = dtrain, 
                 eta = 0.1,
                 max_depth = 15, 
                 nround=25, 
                 eval_metric = "auc",
                 objective = "binary:logistic")

xgb_preds <- predict(xgb, dtest)%>%as.data.frame()
colnames(xgb_preds) <- "is_click"

xgb_sub <- data.frame(sample_submission$impression_id, xgb_preds$is_click)
colnames(xgb_sub) <- colnames(sample_submission)
write_csv(xgb_sub, "xgb_sub3.csv")



