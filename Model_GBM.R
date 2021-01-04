list.of.packages <- c("pdp","Metrics","gbm")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
rm(list = c("list.of.packages","new.packages"))


runWholeGBM <- function(refCol, data_train, data_validation){
  
  RemCols <- function(d, colsRem){
    return(na.omit(d[,-which(names(d) %in% colsRem)]))
  }
  
  colsRem <- c("insurance_comp_1","insurance_comp_2","insurance_comp_3",
               "insurance_comp_4","insurance_comp_5","insurance_comp_6",
               "insurance_comp_7","insurance_median", "model_point_id")
  colsRem <- setdiff(colsRem, refCol)
  
  data_train <- RemCols(data_train, colsRem)
  data_validation <- RemCols(data_validation, colsRem)
  
  
  dist_mat1 <- eval(parse(text=sprintf(
                  "dist(data_train$%s[!is.na(data_train$%s)],method='euclidean')"
                  ,refCol,refCol)))
  whclust <- hclust(dist_mat1, method = 'ward.D2')
  wcut <- cutree(whclust, k = 20)
  
  vals <- table(data_train$vehicle_brand, wcut)
  valz <- table(data_train$insurer_ZIP_code, wcut)
  
  ref_ZIP   <- as.data.frame(cbind(rownames(valz), colnames(valz)[apply(valz,1,which.max)]))
  ref_brand <- as.data.frame(cbind(rownames(vals), colnames(vals)[apply(vals,1,which.max)]))
  colnames(ref_ZIP)   <- c("insurer_ZIP_code","ins_ZIPCODE_group")
  colnames(ref_brand) <- c("vehicle_brand"   ,"vehicle_brand_group")
  ref_ZIP$ins_ZIPCODE_group     <- as.factor(ref_ZIP$ins_ZIPCODE_group)
  ref_brand$vehicle_brand_group <- as.factor(ref_brand$vehicle_brand_group)
  data_train <- merge(data_train, ref_ZIP,   by = "insurer_ZIP_code")
  data_train <- merge(data_train, ref_brand, by = "vehicle_brand")
  data_validation <- merge(data_validation, ref_ZIP, by = "insurer_ZIP_code")
  data_validation <- merge(data_validation, ref_brand, by = "vehicle_brand")
  rm(vals, valz, dist_mat1, whclust, wcut)
  
  
  colsRem <- c("vehicle_brand", "insurer_ZIP_code")
  
  data_train <- RemCols(data_train, colsRem)
  data_validation <- RemCols(data_validation, colsRem)
  
  #set the formula 
  formula.mod <- eval(parse(text=sprintf("formula(%s~.)",refCol))) #
  
  
  #HYPERPARAMETRY KTERE CHCEME OPTIMALIZOVAT
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
    # save.rmse
    opt.n.trees <- gbm.perf(object = fit,method = "cv", plot.it = F)
    preds <- predict(object = fit, newdata = data_validation, 
                     n.trees = opt.n.trees, type = "response")
    rmse <- eval(parse(text=sprintf("rmse(actual = data_validation$%s, predicted = preds)",refCol)))
    hg_out <<- rbind(hg_out, c(x,opt.n.trees,rmse))
  }
  
  set.seed(500) #fix the random number generator
  apply(hyper.grid, MARGIN = 1, RunGBM)
  
  colnames(hg_out) <- c("shrinkage", "interaction.depth", "n.minobsinnode", 
                        "opt.n.trees", "rmse")
  
  
  opt <- which.min(hg_out$rmse) 
  n.trees.opt <- hg_out[opt, "opt.n.trees"]
  shrinkage.opt <- hg_out[opt, "shrinkage"]
  n.minobsinnode.opt <- hg_out[opt, "n.minobsinnode"]
  interaction.depth.opt <- hg_out[opt, "interaction.depth"]
  
  #fitneme model s optimalnymi parametrami a spocitame rmse na validacnom subore
  set.seed(500) #fix the random number generator
  boost.model.opt <- gbm(formula = formula.mod,
                         distribution = "gaussian",
                         data = data_train,
                         n.trees = eval(parse(text = "n.trees.opt")),
                         shrinkage = shrinkage.opt, #learning rate
                         interaction.depth = interaction.depth.opt,
                         n.minobsinnode = n.minobsinnode.opt
  )
  
  preds = predict(object = boost.model.opt, newdata = data_validation, 
                  n.trees =n.trees.opt, type = "response")
  par(las = 1, mar = c(5,17,4,2))
  summary(boost.model.opt) #just variable importance
  par(las = 1, mar = c(5,4,4,2)) # back to default
  attach(data_validation)
  #print RMSE, MAE, MAPE
  eval(parse(text=sprintf("RMSE <- rmse(actual = %s, predicted=preds)",refCol)))
  eval(parse(text=sprintf("MAPE <- mean(abs(%s-preds)/%s)",refCol,refCol)))
  eval(parse(text=sprintf("MAE  <- mean(abs(%s-preds))",refCol)))
  detach(data_validation)
  
  return(c(RMSE, MAPE, MAE))
}



ins_companies <- c("insurance_comp_1","insurance_comp_2","insurance_comp_3",
                    "insurance_comp_4","insurance_comp_5","insurance_comp_6",
                    "insurance_comp_7")

Results <- data.frame(RMSE = double(),
                      MAPE = double(),
                      MAE  = double())

for (i in ins_companies) {
  Results[i,] <- runWholeGBM(i, data_train, data_validation)
  print(Results)
}
rownames(Results) <- ins_companies