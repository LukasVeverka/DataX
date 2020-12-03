library(dplyr)
library(ggplot2)

Data = data

# Priprava dat
data$vehicle_model = NULL
data$insurer_place = NULL

set.seed(123)
train = sample(1:nrow(data), nrow(data)/3*2)

data = data[train,]

# data$vehicle_brand seskupeni
plot(data$vehicle_brand,data$insurance_median)
sort(table(data$vehicle_brand),decreasing = T)

TOP = names(sort(table(data$vehicle_brand),decreasing = T)[1:10])
pokus = data %>% filter(!vehicle_brand %in% TOP)
# sort(table(pokus$vehicle_brand),decreasing = T)
# TOP
# Napocitani za kazdy brand jeho prumernou cenu a variabilitu podle ktere se pak bude delat cluster
pokus = pokus %>% group_by(vehicle_brand) %>% summarise(prum_pojist = mean(insurance_median), var_pojist = var(insurance_median))
set.seed(123)
# k-means clustering, musi byt prednim scaling dat, protoze variance je v hroznych radech
kmeans_model = kmeans(scale(pokus[,-1]),5,nstart = 50)

# Transformace cisla klasteru do pismen, abychom pak vedeli co je zlusterovane a co ne
temp = inner_join(as.data.frame(kmeans_model$cluster), data.frame(CL = c(1,2,3,4,5),brand = c("A","B","C","D","E")), by = c("kmeans_model$cluster" = "CL")) %>% select(brand)
pokus$CL = temp$brand

graf = pokus %>%
  select(prum_pojist,var_pojist,CL)
Graf = ggplot(graf, aes(x = prum_pojist, y = var_pojist, col = CL))+
  geom_point()
Graf
# Neresim predikci, ta by se kdyztak musela udelat jako check znacky v TOP a pak prirazeni k clusteru

# Zapis do dat
data = left_join(data,pokus %>% select(vehicle_brand,CL), by = "vehicle_brand")
data$CL = as.character(data$CL)
data$CL[which(is.na(data$CL))] = as.character(data$vehicle_brand[which(is.na(data$CL))])
# Cisla znaci puvodni znacku (TOP 10 v poctu) a pismena clustery
sort(table(data$CL),decreasing = T)

data$vehicle_brand = as.factor(data$CL)
data$CL = NULL
rm(pokus, temp)




# data$insurer_ZIP_code seskupeni
plot(data$insurer_ZIP_code,data$insurance_median)
sort(table(data$insurer_ZIP_code),decreasing = T)

pokus = data
pokus = pokus %>% group_by(insurer_ZIP_code) %>% summarise(prum_pojist = mean(insurance_median), var_pojist = var(insurance_median))
pokus$var_pojist[which(is.na(pokus$var_pojist))] = min(pokus$var_pojist, na.rm = T)
kmeans_model = kmeans(scale(pokus[,-1]),10,nstart = 50, method = "ward.D2")

pokus$CL = as.factor(kmeans_model$cluster)

graf = pokus %>%
  select(prum_pojist,var_pojist,CL)
Graf = ggplot(graf, aes(x = prum_pojist, y = var_pojist, col = CL))+
  geom_point()
Graf
# Neresim predikci, ta by se kdyztak musela udelat jako check znacky v TOP a pak prirazeni k clusteru

data = left_join(data,pokus %>% select(insurer_ZIP_code,CL), by = "insurer_ZIP_code")
sort(table(data$CL),decreasing = T)

data$insurer_ZIP_code = as.factor(data$CL)
data$CL = NULL
rm(pokus,TOP,kmeans_model,graf,Graf)

Data_upraveno_cluster = data
Data_origo = Data
