# install.packages("RMySQL")
library(DBI)
library(RMySQL)
library(readr)
library(dplyr)
library(caret)

# Connect to AWS RDS
con <- dbConnect(RMySQL::MySQL(),
                host = "stroke-db.cpcsqk8849iz.ap-southeast-1.rds.amazonaws.com",
                port = 3306,
                user = "admin",
                password = "Qwe12345#",
                dbname = "stroke_project")

# Read CSV
stroke_df <- read_csv("stroke_data.csv")

# Upload to MySQL (RDS)
dbWriteTable(con, name = "stroke_data", value = stroke_df, row.names = FALSE)

# Check connect data
dbGetQuery(con, "SELECT COUNT(*) FROM stroke_data")
stroke_df <- dbGetQuery(con, "SELECT * FROM stroke_data")

# Disconnect
dbDisconnect(con)

# Check data
dim(stroke_df)
glimpse(stroke_df)
summary(stroke_df)

# Missing values
stroke_df$bmi[stroke_df$bmi == "N/A"] <- NA
colSums(is.na(stroke_df))

stroke_df$bmi <- as.numeric(stroke_df$bmi)
summary(stroke_df$bmi)

# Mean Imputation
stroke_df$bmi[is.na(stroke_df$bmi)] <- mean(stroke_df$bmi, na.rm = TRUE)

# Converting to Factor
stroke_df$gender <- as.factor(stroke_df$gender)
stroke_df$ever_married <- as.factor(stroke_df$ever_married)
stroke_df$work_type <- as.factor(stroke_df$work_type)
stroke_df$Residence_type <- as.factor(stroke_df$Residence_type)
stroke_df$smoking_status <- as.factor(stroke_df$smoking_status)
stroke_df$stroke <- as.factor(stroke_df$stroke)
stroke_df$id <- NULL

# split data
split_data <- function(data) {
  set.seed(30)
  n <- nrow(data)
  id <- sample(1:n, size = 0.7*n)
  train_df <- data[id, ]
  test_df <- data[-id, ]
  return( list(train = train_df,
               test = test_df) )
}

prep_df <- split_data(stroke_df)

# ML Model
set.seed(30)
ctrl <- trainControl(method = "cv", 
                     number = 5, 
                     verboseIter = TRUE)
logic_model <- train(stroke ~ ., 
                     data = prep_df$train,
                     method = 'glm',
                     trControl = ctrl)
tree_model <- train(stroke ~ .,
                    data = prep_df$train,
                    method = 'rpart',
                    trControl = ctrl)
rf_model <- train(stroke ~ .,
                   data = prep_df$train,
                   method = 'rf',
                   trControl = ctrl)

# score $ Evaluate
p_logic <- predict(logic_model, newdata = prep_df$test)
p_logic_prob <- predict(logic_model, newdata = prep_df$test, type = "prob")
p_tree <- predict(tree_model, newdata = prep_df$test)
p_tree_prob <- predict(tree_model, newdata = prep_df$test, type = "prob")
p_rf <- predict(rf_model, newdata = prep_df$test)
p_rf_prob <- predict(rf_model, newdata = prep_df$test, type = "prob")

library(pROC)
roc_logic <- roc(prep_df$test$stroke, p_logic_prob[, "1"])
roc_tree <- roc(prep_df$test$stroke, p_tree_prob[, "1"])
roc_rf <- roc(prep_df$test$stroke, p_rf_prob[, "1"])

auc_logic <- auc(roc_logic)
auc_tree <- auc(roc_tree)
auc_rf <- auc(roc_rf)
acc_logic <- mean(p_logic == prep_df$test$stroke)
acc_tree <- mean(p_tree == prep_df$test$stroke)
acc_rf <- mean(p_rf == prep_df$test$stroke)

# Variable Importance
varImp(logic_model)

# plot ROC Curve
plot(roc_logic, col = "blue", main = "ROC Curve - Logistic Model")

# save model
write.csv(stroke_df, "stroke_df.csv", row.names = F)
saveRDS(logic_model, "logic_model.rds")
