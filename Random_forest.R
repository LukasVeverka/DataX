# Training sample
# set.seed(123)
# train = sample(1:nrow(Data), nrow(Data)/3*2)
# test = setdiff(c(1:nrow(Data)),train)
# valid = sample(train,length(train)*0.2)
# train = setdiff(train,valid)

load("Data_RF_cluster.RData")
load("index_row_train_test.RData")

data_train = Data[train,]
data_valid = Data[valid,]
data_test = Data[test,]

# Priprava dat
data_train$vehicle_model = NULL
data_train$insurer_place = NULL
data_train$insurer_ZIP_code = NULL
data_train$vehicle_brand = NULL

data_train$Cluster_insurer_ZIP = NULL
data_train$ZIP_target_encoded = NULL

data_valid$vehicle_model = NULL
data_valid$insurer_place = NULL
data_valid$insurer_ZIP_code = NULL
data_valid$vehicle_brand = NULL

data_valid$Cluster_insurer_ZIP = NULL
data_valid$ZIP_target_encoded = NULL

data_test$vehicle_model = NULL
data_test$insurer_place = NULL
data_test$insurer_ZIP_code = NULL
data_test$vehicle_brand = NULL

data_test$Cluster_insurer_ZIP = NULL
data_test$ZIP_target_encoded = NULL

set.seed(123)
library(parallel)
library(randomForest)
library(pdp)

tic = Sys.time()

for (i in 1:7) {
  time_to_end = difftime(Sys.time(),tic,units = "hours")/i*(7-i)
print(paste(i,"/7 TIME REMAINING:",round(time_to_end, digits = 2),"hours"))

# Dependent = "insurance_comp_1"
Vynechat = c("insurance_comp_1","insurance_comp_2","insurance_comp_3","insurance_comp_4","insurance_comp_5","insurance_comp_6","insurance_comp_7",
             "insurance_median","model_point_id", "model_point_period","model_point_Period","group_engine_volume","group_engine_power","insurer_age_group")
Dependent = Vynechat[i]

# Specifikovani rovnice
form <- as.formula(paste(Dependent," ~ ",paste0(setdiff(colnames(data_test),c(Vynechat)),collapse=" + ")))

hyper.grid <- expand.grid(
  mtry = c(2,5,10,13),
  n.trees = c(1000,2500),
  nodesize = c(10, 20),
  sampsize = round(c(0.75, 1)*nrow(data_train[!is.na(data_train[,Dependent]),])),
  opt.n.trees = NA,
  rmse = NA
)

opti_param_search <- function(i.grid,
                              hyper.grid,
                              data_train,
                              data_validation,
                              formula.mod) {
  # Vyradim NA hodnoty v dependent
  data_input = data_train[!is.na(data_train[,Dependent]),]

  fit <- randomForest(formula = formula.mod,
                      data = data_input,
                      ntrees =hyper.grid$n.trees[i.grid],
                      mtry = hyper.grid$mtry[i.grid],
                      nodesize = hyper.grid$nodesize[i.grid],
                      sampsize = hyper.grid$sampsize[i.grid]
  )

  #find opt ntree
  hyper.grid$opt.n.trees[i.grid] <- which.min(fit$mse)

  #generate predictions for the validation data
  prediction = predict(fit, newdata = data_validation, ntrees = which.min(fit$mse))
  rmse = sqrt(mean((prediction[!is.na(data_validation[,Dependent])] - data_validation[!is.na(data_validation[,Dependent]),Dependent] )^2))

  hyper.grid[i.grid,"rmse"] = rmse
  return(hyper.grid[,c("rmse","opt.n.trees")])
}

# opti_param_search(1,hyper.grid,data_train,data_valid,form)
# results <- t(sapply(seq_len(nrow(hyper.grid)),function(x) opti_param_search(x,hyper.grid,data_train,data_valid,form)))


# Pokusne ke zprovozneni funkce
# results <- t(sapply(1:2,function(x) opti_param_search(x,hyper.grid,data_train,data_valid,form)))

fx = function(x) opti_param_search(x,hyper.grid,data_train,data_valid,form)

cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS

clusterExport(cluster, "hyper.grid") # import data_train into cluster
clusterExport(cluster, "data_train") # import data_train into cluster
clusterExport(cluster, "data_valid") # import data_train into cluster
clusterExport(cluster, "form") # import data_train into cluster
clusterExport(cluster, "opti_param_search") # import data_train into cluster
clusterExport(cluster, "fx") # import data_train into cluster
clusterExport(cluster, "Dependent") # import data_train into cluster

clusterEvalQ(cluster, { library(randomForest) })  # import required packages into nodes

paralell_results <- parallel::parLapply(cluster,seq_len(nrow(hyper.grid)),fun = fx)

stopCluster(cluster)

for (j in 1:length(paralell_results)) {
  hyper.grid$opt.n.trees[j] = paralell_results[[j]][j,][2]
  hyper.grid$rmse[j] = paralell_results[[j]][j,][1]
}

opt_set = hyper.grid[which.min(hyper.grid$rmse),]

data_input = data_train[!is.na(data_train[,Dependent]),]

# Random forest
# Mohl jsem misto tvoreni individualniho, nechat zapsat nejlepsi ale nedostal jsem se k tomu
rf.model = randomForest(form, data = data_input,
                        mtry=opt_set$mtry,
                        ntree = opt_set$n.trees,
                        nodesize = opt_set$nodesize,
                        sampsize = opt_set$sampsize,
                        importance =TRUE)

save(list=c("rf.model"), file = paste("RF_insur_comp_",i,".Rdata",sep = ""))

rm(list=setdiff(ls(), c("Data", "data_test","data_train","data_valid","test","train","valid","tic")))
}

Vyslekdy = data.frame(
  RMSE_train = rep(0,7),
  RMSE_test = rep(0,7),
  R2_train = rep(0,7),
  R2_test = rep(0,7),
  MAPE_train = rep(0,7),
  MAPE_test = rep(0,7)
)

i = i+1

# for (i in 1:7) {
  # print(i)
  # setwd("~/Documents/GIT/DataX")
  load(paste("RF_insur_comp_",i,".Rdata", sep = ""))
  Dependent = paste("insurance_comp_",i,sep = "")
  
  # plot(rf.model)
  
  # RMSE train
  # sqrt(mean((predict(rf.model,newdata = Data[-train,]) - data.test)^2))
  Vyslekdy$RMSE_train[i] = sqrt(mean((predict(rf.model) - data_train[!is.na(data_train[,Dependent]),Dependent])^2))
  # RMSE test
  Vyslekdy$RMSE_test[i] = sqrt(mean((predict(rf.model,data_test,ntrees = which.min(rf.model$mse))[!is.na(data_test[,Dependent])] - data_test[!is.na(data_test[,Dependent]),Dependent])^2))
  # R^2 train
  Vyslekdy$R2_train[i] = cor(predict(rf.model),data_train[!is.na(data_train[,Dependent]),Dependent])^2
  # R^2 test
  Vyslekdy$R2_test[i] = cor(predict(rf.model,data_test,ntrees = which.min(rf.model$mse))[!is.na(data_test[,Dependent])],data_test[!is.na(data_test[,Dependent]),Dependent])^2
  # MAPE train
  Vyslekdy$MAPE_train[i] = mean(abs((predict(rf.model)-data_train[!is.na(data_train[,Dependent]),Dependent])/data_train[!is.na(data_train[,Dependent]),Dependent]))
  # MAPE test
  Vyslekdy$MAPE_test[i] = mean(abs((predict(rf.model,data_test,ntrees = which.min(rf.model$mse))[!is.na(data_test[,Dependent])]-data_test[!is.na(data_test[,Dependent]),Dependent])/data_test[!is.na(data_test[,Dependent]),Dependent]))

  
# }

# Random Forest nebo Bagging uz nejde vizualizovat, ale jde rict, ktere promenne jsou nejdulezitejsi
# Prvni sloupec (IncMSE) ukazuje prumerny pokles MSE (tzn rozptylu). Druhy pokles node purity (tzn RSS - celkove chyby)
VarImp_data_MSE = data.frame(MSE = as.numeric(importance(rf.model)[,1]),
                             Variable = rownames(importance(rf.model)))
# VarImp_data_Purity = data.frame(Purity = as.numeric(importance(rf.model)[,2]),
#                                 Variable = rownames(importance(rf.model)))

# Graficke vyjadreni dulezitsti je tento graf
# varImpPlot (rf.model)
# %IncMSE se pocita z MSE (squared error) zatimco NodePurity z Gini index

# setwd("~/Documents/GIT/DataX/Grafy")

pdf(paste("VarImp_MSE_insur_",i,".pdf", sep = ""),width = 8.5, height = 5) #export
ggplot(data = VarImp_data_MSE,
       aes(x = reorder(Variable, MSE), y = MSE))+
  geom_bar(stat = "identity")+
  coord_flip() +
  xlab("Variable") +
  ylab("MSE reduction") +
  ggtitle(paste("VarImpPlot Insurance comp.",i))
dev.off() #export

# ggplot(data = VarImp_data_Purity,
#        aes(x = reorder(Variable, Purity), y = Purity))+
#   geom_bar(stat = "identity")+
#   coord_flip() +
#   xlab("Variable") +
#   ylab("Node purity")

# data_input = data_train[!is.na(data_train[,Dependent]),]
# 
# pp <- partial(rf.model, 
#               pred.var = "ZIP_num",
#               # train = Data[train,-10],
#               plot = TRUE,
#               plot.engine = "ggplot2"
# )
# pp + ylab(Dependent)


TOP = VarImp_data_MSE[order(VarImp_data_MSE$MSE,decreasing = T),]
data_input = data_train[!is.na(data_train[,Dependent]),]

# PDP_overview = list()
op <- par(mfrow=c(3,3))
# par(mar = c(7, 4, 2, 2) + 0.2)

# pdf(paste("PDP_insur_",i,".pdf", sep = ""),width = 8.5, height = 5) #export
for (j in 1:9){
  partialPlot(rf.model,
              pred.data = data_input,
              x.var = as.character(TOP$Variable[j]),
              xlab="",#colnames(Data)[j],
              main=paste(#"Partial Dependence on", 
                as.character(TOP$Variable[j])),
              # main = NULL,
              # las = 2
  )
}
# dev.off() #export

par(op)

# }

# p3d1 <- partial(rf.model_crsv, 
#                 pred.var = c("TRP", "hour"), 
#                 train = Data[train,-10],
#                 plot = TRUE,
#                 contour = TRUE,
#                 plot.engine = "ggplot2"
# )
# p3d1

# save(list=c("Vyslekdy"), file = "Vysledky.Rdata")

