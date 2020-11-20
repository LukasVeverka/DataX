
# Dependencies
list.of.packages <- c("scatterplot3d","plotly")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
rm(list = c("list.of.packages","new.packages"))


# nastavíme velkosť výberu s ktorým pracujeme
Ssize <- 1200
RS <- sample(1:nrow(data),Ssize,replace = FALSE)

# vzorka dát
RS_data <- data.frame(data[RS,])
rm(RS)

#plot premenné ktoré má význam upravovať alebo z nich niečo vytiahnúť
cols<- c("vehicle_engine_volume","vehicle_weight","vehicle_age",
           "insurer_age","vehicle_engine_power",
           "insurer_number_of_previous_insured_months",
           "insurer_number_of_previous_accidents")
par(mfrow = c(3,3))
for (i in cols){
  plot(as.vector(RS_data[,i]),RS_data$insurance_median, type = "p", 
       na.rm = TRUE,xlab = i)
}
rm(cols)

# pre prehľadnosť zmeníme trošku vek
ageR <- RS_data$insurer_age + rnorm(Ssize, 0,.5) 

par(mfrow = c(1,1))
plot(ageR,RS_data$insurance_median, type = "p",xlab = "ageR")
rm(ageR)

# zopbrazenie grafov s oddelením novo vzniknutých skupín
ages <- c(0,24,26,30,35,40,45,50,60,70)
for (i in ages){
  abline(v = i, col = "lightblue")
}
rm(ages)

plot(RS_data$vehicle_engine_volume,RS_data$insurance_median, type = "p")
vols <- c(0,1000, 1350, 1600, 1850, 2000, 2200, 2600)
for (i in vols){
  abline(v = i, col = "lightblue")
}

plot(RS_data$vehicle_engine_power,RS_data$insurance_median, type = "p")
vols <- c(50,75,93,110,150,180,5000)
for (i in vols){
  abline(v = i, col = "lightblue")
}


# z psc nic nevidíme ale ukážeme si grafík
psc <- substr(RS_data$insurer_ZIP_code,1,3)
psc2 <- substr(RS_data$insurer_ZIP_code,1,2)

plot(psc, RS_data$insurance_median)
plot(psc2, RS_data$insurance_median)


# 3D grafy pre bonus-malus 
colsBM <- c("insurer_number_of_previous_insured_months",
            "insurer_npaR",
            "insurance_median")

# malé znáhodnenie pre lešpiu prehľadnosť grafu
RS_data$insurer_npaR <- 
  RS_data$insurer_number_of_previous_accidents+rnorm(Ssize,0,.2)

# ofarbíme pekne
colors <- c("#999999", "#E69F00", "#56B4E9")
colors <- colors[as.numeric(RS_data$insurer_number_of_previous_accidents)+1]
# listok vektorov pre pohľady z rôznych uhlov na 3D scatter 
listicek <- list("a" = c(1,2,3), 
                 "b" = c(2,1,3), 
                 "c" = c(3,1,2), 
                 "d" = c(3,2,1))
# všetky 4 spolu
par(mfrow = c(2,2))
for (i in listicek){
  scatterplot3d(RS_data[,colsBM[i]], pch = 16, color=colors,
                grid=TRUE, box=FALSE)
}
# po 1 grafe - treba sa preklikať
par(mfrow = c(1,1))
for (i in listicek){
  scatterplot3d(RS_data[,colsBM[i]], pch = 16, color=colors,
                grid=TRUE, box=FALSE)
}
# BM, ktorý sa nám pozdával ako zmysluplný
plot(RS_data$insurer_number_of_previous_insured_months-
       36*RS_data$insurer_number_of_previous_accidents,
     RS_data$insurance_median, xlab = "Prev. ins. mths - 36*accidents")

# neviem prečo v loope plotly nefunguje... nižšie je to rozpísané
companies <-  c("insurance_comp_1","insurance_comp_2",
                "insurance_comp_3","insurance_comp_4",
                "insurance_comp_5","insurance_comp_6",
                "insurance_comp_7","insurance_median")

for (c in companies){
  eval(parse( text =
        sprintf("fig <- plot_ly(RS_data, 
                               x = ~ insurer_number_of_previous_insured_months,
                               y = ~ insurer_number_of_previous_accidents,
                               z = ~ %s, 
                               marker = list(size = 4),
                               color = ~insurer_number_of_previous_accidents)
                add_markers(fig, 
                            x = RS_data$insurer_number_of_previous_insured_months,
                            y = RS_data$insurer_number_of_previous_accidents,
                            z = RS_data$%s)
               fig",c,c)))
  fig
}

# rozpísané generovanie plotly 3D grafu pre všetky poisťovne - nič z toho nevidím 
plotText <- "fig <- plot_ly(RS_data, 
                             x = ~ insurer_number_of_previous_insured_months,
                             y = ~ insurer_number_of_previous_accidents,
                             z = ~ %s, 
                             marker = list(size = 4),
                             color = ~insurer_number_of_previous_accidents)
              add_markers(fig, 
                          x = RS_data$insurer_number_of_previous_insured_months,
                          y = RS_data$insurer_number_of_previous_accidents,
                          z = RS_data$%s)
              layout(fig, title = '%s')
              fig"

eval(parse( text = sprintf(plotText,companies[1],companies[1],companies[1])))
eval(parse( text = sprintf(plotText,companies[2],companies[2],companies[2])))
eval(parse( text = sprintf(plotText,companies[3],companies[3],companies[3])))
eval(parse( text = sprintf(plotText,companies[4],companies[4],companies[4])))
eval(parse( text = sprintf(plotText,companies[5],companies[5],companies[5])))
eval(parse( text = sprintf(plotText,companies[6],companies[6],companies[6])))
eval(parse( text = sprintf(plotText,companies[7],companies[7],companies[7])))
eval(parse( text = sprintf(plotText,companies[8],companies[8],companies[8])))

# remove unnecessary variables from memory
rm(c,colors,colsBM,companies,fig,i,listicek,plotText,psc,psc2,Ssize,vols)

