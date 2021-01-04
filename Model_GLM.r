### Dependencies ---------------------------------------------------------------
load("data.RData")
list.of.packages <- c("Metrics","glmnet")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
rm(list = c("list.of.packages","new.packages"))


refCol <- "insurance_median"

RemCols <- function(d, colsRem){
  return(na.omit(d[,-which(names(d) %in% colsRem)]))
}

colsRem <- c("insurance_comp_1","insurance_comp_2","insurance_comp_3",
             "insurance_comp_4","insurance_comp_5","insurance_comp_6",
             "insurance_comp_7","insurance_median", "model_point_id",
             "vehicle_brand", "insurer_ZIP_code")
colsRem <- setdiff(colsRem, refCol)

data <- RemCols(data, colsRem)




set.seed(500) 
#################################################
#####Holdout for validation and testing##########
#################################################
# Split data to (training + validation) and testing datasets
n <- nrow(data)
# Randomly shuffle the data #nahodne zamichat, keby boli nahodou zoradene, hlavne kvoli CV
data <- data[sample(n), ]
#generate index of training set 
nr_trval <- round(0.80 * n)
index_trval <- sample(1:n, nr_trval)

data_trval <- data[index_trval, ]
data_test <- data[-index_trval, ]

# Split training_trval data to training and validation datasets (two steps split for teaching purpose otherwise you can split all at once)
nr_train <- round(0.75 * nr_trval) #total split 60:20:20
index_train <- sample(1:nr_trval, nr_train)
data_train <- data_trval[index_train, ]
data_validation <- data_trval[-index_train, ]

# Adjust data to glment package format
data_model_matrix_train <- model.matrix(insurance_median ~ ., data_train) 
data_model_matrix_validation <- model.matrix(insurance_median ~ ., data_validation)
data_model_matrix_test <- model.matrix(insurance_median ~ ., data_test)
data_model_matrix_trval <- model.matrix(insurance_median ~ ., data_trval) 

#define grid to tune the parameters
hyper_grid <- expand.grid(
  alpha = seq(from = 0 , to = 1, by = 0.05),
  lambda = seq(from = 0, to = 100, length.out = 50),
  rmse_ho = NA #for hold out
 # rmse_cv = NA #for cross validation
)

#get estimate of the validation error for each couple on the grid
for(i in 1:nrow(hyper_grid)){ #cyklus pres vsechny radky hyper_gridu
  #i = 2

  # Fit elastic net regression
  fit_elnet_reg <- glmnet(
    x = data_model_matrix_train, 
    y = data_train$insurance_median,
    alpha = hyper_grid[i,"alpha"],
    lambda = hyper_grid[i,"lambda"],
    standardize = TRUE, 
    intercept = TRUE,
    family = "gaussian")

  #calcualte predictions
  preds = predict(
    object = fit_elnet_reg,
    newx =  data_model_matrix_validation)
  #calculate rmse and 
  hyper_grid[i,"rmse_ho"]<- rmse(data_validation$insurance_median, preds)
}

opt_row_ho = which.min(hyper_grid[,"rmse_ho"]) #0.35	95.91837	1083.066

#Estimate rmse on test data using train and validation data for training
fit_elnet_reg_ho <- glmnet(
  x = data_model_matrix_trval, #uz pouzijem cele tren.data aj s val., ziskam 20% naviac
  y = data_trval$insurance_median,
  alpha = hyper_grid[opt_row_ho,"alpha"],
  lambda = hyper_grid[opt_row_ho,"lambda"],
  standardize = TRUE, 
  intercept = TRUE,
  family = "gaussian")

#Evaluate rmse on testing data #otestujem chybu na testovacim souboru
rmse_test_ho = rmse(data_test$insurance_median,  predict(object = fit_elnet_reg_ho, newx =  data_model_matrix_test))
#1879

# Results show:
# The number of nonzero coeffcients (Df),
# The value of λ (Lambda). 
fit_elnet_reg_ho$lambda #extract grid of lambda
coef(fit_elnet_reg_ho) #extract parameters for each lambda
#plot(fit_elnet_reg_ho, xvar = "lambda", label = T)

# Calculate predictions on test data
data_test$predicted_elnet_reg_ho = predict(
  object = fit_elnet_reg_ho,
  newx =  data_model_matrix_test, 
  s = fit_elnet_reg_ho$lambda) # s - selected lambda

rmse_model <-  rmse(data_test$insurance_median, data_test$predicted_elnet_reg_ho)
mae_model  <- mean(abs(data_test$insurance_median- data_test$predicted_elnet_reg_ho))
mape_model <- mean(ifelse(is.na(data_test$insurance_median), NA,
  abs(data_test$insurance_median - data_test$predicted_elnet_reg_ho)/data_test$insurance_median),na.rm = TRUE)



####################################################
#####Cross validation and hold out for testing######
####################################################
#replace hold out of validation part with 10-fold CV

K = 10
# Create 10 equally sized folds
#don't forget that data were randomly shuffled

folds <- cut(seq(1, nrow(data_trval)), breaks = K, labels = F)
rmse_fold = c() #create vector for fold results
# For each fold create data_train and data_validation => fit model

#get estimate of the validation error for each couple on the grid
for(i in 1:nrow(hyper_grid)){
  for(j in 1:K) {
    #j = 1#!!!! 
    validation_row_indexes <- which(folds == j, arr.ind = T) #this fold is for validation
    data_validation_fold <- data_trval[validation_row_indexes, ] #this fold is for validation
    data_train_fold <- data_trval[-validation_row_indexes, ] #remaining folds are for training
    
    # Create elastic net model
    # Adjust data to glment package format
    data_model_matrix_train_fold <- model.matrix(insurance_median ~ ., data_train_fold) #region, smoker
    data_model_matrix_validation_fold <- model.matrix(insurance_median ~ ., data_validation_fold)
    
    # Fit model
    fit_lasso_cv_reg <- glmnet(
      x = data_model_matrix_train_fold, 
      y = data_train_fold$insurance_median,
      alpha = hyper_grid[i,"alpha"],
      lambda = hyper_grid[i,"lambda"],
      standardize = TRUE, 
      intercept = TRUE,
      family = "gaussian")
    
    #calcualte predictions
    preds = predict(
      object = fit_lasso_cv_reg,
      newx =  data_model_matrix_validation_fold)
    #calculate rmse for the fold
    rmse_fold[j] = rmse(data_validation_fold$insurance_median, preds)
    #calculate rmse and 
  } #end fold
  hyper_grid[i,"rmse_cv"]<- mean(rmse_fold)
}#end hyper grid  

opt_row_cv = which.min(hyper_grid[,"rmse_cv"])

opt_par = hyper_grid[c(opt_row_ho, opt_row_cv),]
print(opt_par)
#Parameters are quite different, but validation rmse is similar


#Estimate rmse on test data using train and validation data for training
fit_lasso_reg_cv <- glmnet(
  x = data_model_matrix_trval, 
  y = data_trval$insurance_median,
  alpha = hyper_grid[i,"alpha"],
  lambda = hyper_grid[opt_row_cv,"lambda"],
  standardize = TRUE, 
  intercept = TRUE,
  family = "gaussian")

rmse_test_cv = rmse(data_test$insurance_median,  predict(object = fit_lasso_reg_cv, newx =  data_model_matrix_test))
mae_test_cv  <- mean(abs(data_test$insurance_median- data_test$fit_lasso_reg_cv))

print(rmse_test_ho)
print(rmse_test_cv)


# Results show:
# The number of nonzero coeffcients (Df),
# The value of λ (Lambda). 
fit_lasso_reg_cv$lambda #extract grid of lambda
coef(fit_lasso_reg_cv) #extract parameters for each lambda
#plot(fit_elnet_reg_ho, xvar = "lambda", label = T)

# Calculate predictions on test data
data_test$predicted_lasso_reg_cv = predict(
  object = fit_lasso_reg_cv,
  newx =  data_model_matrix_test, 
  s = fit_lasso_reg_cv$lambda) # s - selected lambda

rmse_model3 <-  rmse(data_test$insurance_median, data_test$predicted_lasso_reg_cv)
mae_model3  <- mean(abs(data_test$insurance_median- data_test$predicted_lasso_reg_cv))
mape_model3 <- mean(ifelse(is.na(data_test$insurance_median), NA,
    abs(data_test$insurance_median - data_test$predicted_lasso_reg_cv)/data_test$insurance_median),na.rm = TRUE)


###Cross validation using package
#only provides validation for different lambdas and alpha fixed

#using alpha opimized in ho
fit_elnet_cvp <- cv.glmnet(
  x = data_model_matrix_trval, 
  y = data_trval$insurance_median,
  lambda = c(seq(from = 0, to = 50, length.out = 5)),
  alpha = hyper_grid[opt_row_ho,"alpha"], # number from interval (0,1)
  family = "gaussian", 
  nfolds = 10)

rmse_test_cvp = rmse(data_test$insurance_median,  
                     predict(object = fit_elnet_cvp, 
                             newx =  data_model_matrix_test))

print(rmse_test_cvp)
# Results show:
# The number of nonzero coeffcients (Df),
# The value of λ (Lambda). 
lambda_opt=fit_elnet_cvp$lambda.min #value of lambda that gives minimum cvm
coef(fit_elnet_cvp) #extract parameters for each lambda

# Calculate predictions on test data
data_test$predicted_elnet_cvp = predict(
  object = fit_elnet_cvp,
  newx =  data_model_matrix_test, 
  s = lambda_opt) # s - selected lambda

rmse_model2 <-  rmse(data_test$insurance_median, data_test$predicted_elnet_cvp)
mae_model2 <- mean(abs(data_test$insurance_median - data_test$predicted_elnet_cvp))
mape_model2 <- mean(ifelse(is.na(data_test$insurance_median), NA,
  abs(data_test$insurance_median - data_test$predicted_elnet_cvp)/data_test$insurance_median),na.rm = TRUE)


