# load and if needed also install packages
list.of.packages <- c("pdp","Metrics","gbm")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
rm(list = c("list.of.packages","new.packages"))

# main function running for all insurance company prices
runWholeGBM <- function(refCol, data_train, data_validation){
  
  # function to remove redundant columns and rows with missing values
  RemCols <- function(d, colsRem){
    return(na.omit(d[,-which(names(d) %in% colsRem)]))
  }
  
  # following columns are to be removed except reference column with price data
  colsRem <- c("insurance_comp_1","insurance_comp_2","insurance_comp_3",
               "insurance_comp_4","insurance_comp_5","insurance_comp_6",
               "insurance_comp_7","insurance_median")
  # remove reference column from previous set
  colsRem <- setdiff(colsRem, refCol)
  
  # clean datasets
  data_train <- RemCols(data_train, colsRem)
  data_validation <- RemCols(data_validation, colsRem)
  
  # calculate distance matrix 
  dist_mat1 <- eval(parse(text=sprintf(
                 "dist(data_train$%s[!is.na(data_train$%s)],method='euclidean')"
                  ,refCol,refCol)))
  # hierarchical clustering dependent on price data, then cut to 20 clusters
  whclust <- hclust(dist_mat1, method = 'ward.D2') 
  wcut <- cutree(whclust, k = 20)
  
  # dependency of brands/ZIPCodes on cluster 
  vals <- table(data_train$vehicle_brand, wcut)
  valz <- table(data_train$insurer_ZIP_code, wcut)
  
  # assign the most frequent group as reference group
  ref_ZIP   <- as.data.frame(cbind(rownames(valz), 
                                   colnames(valz)[apply(valz,1,which.max)]))
  ref_brand <- as.data.frame(cbind(rownames(vals), 
                                   colnames(vals)[apply(vals,1,which.max)]))
  colnames(ref_ZIP)   <- c("insurer_ZIP_code","ins_ZIPCODE_group")
  colnames(ref_brand) <- c("vehicle_brand"   ,"vehicle_brand_group")
  ref_ZIP$ins_ZIPCODE_group     <- as.factor(ref_ZIP$ins_ZIPCODE_group)
  ref_brand$vehicle_brand_group <- as.factor(ref_brand$vehicle_brand_group)
  # assign derived groups as new columns in datasets
  data_train <- merge(data_train, ref_ZIP,   by = "insurer_ZIP_code")
  data_train <- merge(data_train, ref_brand, by = "vehicle_brand")
  data_validation <- merge(data_validation, ref_ZIP, by = "insurer_ZIP_code")
  data_validation <- merge(data_validation, ref_brand, by = "vehicle_brand")
  rm(vals, valz, dist_mat1, whclust, wcut) # clean variable set
  
  # drop no more needed variables
  colsRem <- c("vehicle_brand", "insurer_ZIP_code", "model_point_id")
  # assign modelpoints used for validation to outer space
    # used for final comparison of effectivity of models
  model_IDs[[refCol]] <<- data_validation$model_point_id
  
  # clean datasets
  data_train <- RemCols(data_train, colsRem)
  data_validation <- RemCols(data_validation, colsRem)
  
  #set the formula 
  formula.mod <- eval(parse(text=sprintf("formula(%s~.)",refCol))) #
  
  
  #HYPERPARAMETERS to be used for model enhancing
  hyper.grid <- expand.grid(shrinkage = c(0.001,0.01, 0.1),
                            interaction.depth = c(3, 10),
                            n.minobsinnode = c(2,6))
  
  hg_out <- data.frame(shrinkage = double(),
                       interaction.depth = integer(),
                       n.minobsinnode = integer(),
                       opt.n.trees = integer(),
                       rmse = double())
  
  RunGBM <- function(x){
    # fit model for ith hyperparameter combination
    fit <- gbm(formula = formula.mod,
               distribution = "gaussian",
               data = data_train,
               n.trees = 5000,
               shrinkage = x["shrinkage"],
               interaction.depth = x["interaction.depth"],
               n.minobsinnode = x["n.minobsinnode"],
               cv.folds = 5
    )
    # optimal number of trees
    opt.n.trees <- gbm.perf(object = fit,method = "cv", plot.it = F) 
    # store predictions for evaluation of model
    preds <- predict(object = fit, newdata = data_validation, 
                     n.trees = opt.n.trees, type = "response")
    # calc RMSE as main model quality indicator 
    rmse <- eval(parse(text=sprintf(
                "rmse(actual = data_validation$%s, predicted = preds)",refCol)))
    # hypergrid with opt number of trees and RMSE for final comparison
    hg_out <<- rbind(hg_out, c(x,opt.n.trees,rmse))
  }
  
  set.seed(500) #fix the random number generator
  apply(hyper.grid, MARGIN = 1, RunGBM) # fastest? way to run models in parallel
  
  colnames(hg_out) <- c("shrinkage", "interaction.depth", "n.minobsinnode", 
                        "opt.n.trees", "rmse")
  
  # find optimal parameters and store them to be used for final model
  opt <- which.min(hg_out$rmse) 
  n.trees.opt <- hg_out[opt, "opt.n.trees"]
  shrinkage.opt <- hg_out[opt, "shrinkage"]
  n.minobsinnode.opt <- hg_out[opt, "n.minobsinnode"]
  interaction.depth.opt <- hg_out[opt, "interaction.depth"]
  
  #fit model with optimal parametres and calc rmse on validation part of data
  set.seed(500) #fix the random number generator
  boost.model.opt <- gbm(formula = formula.mod,
                         distribution = "gaussian",
                         data = data_train,
                         #for unknown reason parameters for trees dont work well
                         n.trees = eval(parse(text = "n.trees.opt")),
                         shrinkage = shrinkage.opt, #learning rate
                         interaction.depth = interaction.depth.opt,
                         n.minobsinnode = n.minobsinnode.opt
  )
  # calculate predictions for validation data
  preds = predict(object = boost.model.opt, newdata = data_validation, 
                  n.trees =n.trees.opt, type = "response")
  # store predictions to outer space for model comparison
  preds[[refCol]] <<- preds
  par(las = 1, mar = c(5,17,4,2))
  summary(boost.model.opt) #print variable relative importance
  par(las = 1, mar = c(5,4,4,2)) # par() back to default
  attach(data_validation)
  #calc RMSE, MAE, MAPE
  eval(parse(text=sprintf("RMSE <- rmse(actual = %s, predicted=preds)",refCol)))
  eval(parse(text=sprintf("MAPE <- mean(abs(%s-preds)/%s)",refCol,refCol)))
  eval(parse(text=sprintf("MAE  <- mean(abs(%s-preds))",refCol)))
  detach(data_validation)
  # store final model to outer space
  models[[refCol]] <<- boost.model.opt
  return(c(RMSE, MAPE, MAE)) # return main indicators to compare models
}

# variables to be obtained from inner space of function
models <- list()
model_IDs <- list()
preds <- list()

# vector of variables we calculate model for
ins_companies <- c("insurance_comp_1","insurance_comp_2","insurance_comp_3",
                   "insurance_comp_4","insurance_comp_5","insurance_comp_6",
                   "insurance_comp_7","insurance_median")
# initialization of DF to store results
Results <- data.frame(RMSE = double(),
                      MAPE = double(),
                      MAE  = double())
# loop through variables model will be applied to
  # apply does not work here,but this is not bottleneck of algorithm at all
for (i in ins_companies) {
  Results[i,] <- runWholeGBM(i, data_train, data_validation)
  print(Results)
}
rownames(Results) <- ins_companies 
