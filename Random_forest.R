# Training sample
# set.seed(123)
# train = sample(1:nrow(data), nrow(data)/3*2)
# Testovaci vzorek vysvetlovane promenne
data.test = Data$insurance_comp_1[-train]

Dependent = "insurance_comp_1"
Vynechat = c("insurance_comp_1","insurance_comp_2","insurance_comp_3","insurance_comp_4","insurance_comp_5","insurance_comp_6","insurance_comp_7",
             "insurance_median","model_point_id", "model_point_period","group_engine_volume","group_engine_power","insurer_age_group")

Data = data[,!(names(data) %in% Vynechat)]
Data[,ncol(Data)+1] = data[,Dependent]
colnames(Data)[ncol(Data)] = Dependent

# Data = Data[!is.na(Data[,ncol(Data)]),]

for (i in 1:ncol(Data)) {
  Data = Data[!is.na(Data[,i]),]
}

# Specifikovani rovnice
form <- as.formula(paste(Dependent," ~ ",paste0(setdiff(colnames(Data),c(Dependent)),collapse=" + ")))

# Random forest
rf.model = randomForest(form, data=Data, mtry=5, ntree = 1000 ,importance =TRUE)

plot(rf.model)

# MSE
sqrt(mean((predict(rf.model) - Data[,ncol(Data)])^2))
cor(predict(rf.model),Data[,ncol(Data)])^2
# Potreba dodelat odhad na testovacich datech
mean((predict(rf.model,Data[,]) - data.test)^2)


# Random Forest nebo Bagging uz nejde vizualizovat, ale jde rict, ktere promenne jsou nejdulezitejsi
# Prvni sloupec (IncMSE) ukazuje prumerny pokles MSE (tzn rozptylu). Druhy pokles node purity (tzn RSS - celkove chyby)
VarImp_data_MSE = data.frame(MSE = as.numeric(importance(rf.model)[,1]),
                             Variable = rownames(importance(rf.model)))
VarImp_data_Purity = data.frame(Purity = as.numeric(importance(rf.model)[,2]),
                                Variable = rownames(importance(rf.model)))

# Graficke vyjadreni dulezitsti je tento graf
varImpPlot (rf.model)
# %IncMSE se pocita z MSE (squared error) zatimco NodePurity z Gini index

ggplot(data = VarImp_data_MSE,
       aes(x = reorder(Variable, MSE), y = MSE))+
  geom_bar(stat = "identity")+
  coord_flip() +
  xlab("Variable") +
  ylab("MSE reduction")

ggplot(data = VarImp_data_Purity,
       aes(x = reorder(Variable, Purity), y = Purity))+
  geom_bar(stat = "identity")+
  coord_flip() +
  xlab("Variable") +
  ylab("Node purity")

pp <- partial(rf.model, 
              pred.var = "insurer_age",
              # train = Data[train,-10],
              plot = TRUE,
              plot.engine = "ggplot2"
)
pp + ylab(Dependent)


TOP = VarImp_data_MSE[order(VarImp_data_MSE$MSE,decreasing = T),]


# PDP_overview = list()
op <- par(mfrow=c(3,3))
# par(mar = c(7, 4, 2, 2) + 0.2)
for (i in 1:9){
  partialPlot(rf.model,
              pred.data = Data,
              x.var = as.character(TOP$Variable[i]),
              xlab="",#colnames(Data)[i],
              main=paste(#"Partial Dependence on", 
                as.character(TOP$Variable[i])),
              # main = NULL,
              # las = 2
  )
}
par(op)


# p3d1 <- partial(rf.model_crsv, 
#                 pred.var = c("TRP", "hour"), 
#                 train = Data[train,-10],
#                 plot = TRUE,
#                 contour = TRUE,
#                 plot.engine = "ggplot2"
# )
# p3d1