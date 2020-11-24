
# Dependencies
list.of.packages <- c("odbc","DBI","dbplyr","tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
rm(list = c("list.of.packages","new.packages"))

# Drivery pro připojení k databázi

#odbc::odbcListDrivers()

# Pokud v seznamu po odkomentování příkazu výše není "MySQL ODBC 8.0 Unicode Driver",
  #tak je potřeba nainstalovat z odkazu https://dev.mysql.com/downloads/connector/odbc/

# Define server connection
con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "MySQL ODBC 8.0 Unicode Driver",
                      Server   = "compass-db-server.cgbcuxp6tr07.eu-central-1.rds.amazonaws.com",
                      UID      = "studentX",
                      PWD      = rstudioapi::askForPassword("Database password"),
                      Port     = 3306)

# Příklad dotazování "SQL style"
data <- dbGetQuery(con,' SELECT *
                               FROM MP_university_DATAX.MP_database AS MP
                               LEFT JOIN MP_university_DATAX.MP_brand_model AS BM
                               ON MP.model_point_id = BM.model_point_id ')

# Příklad dotazování "R style"
data <- 
  tbl(con, in_schema("MP_university_DATAX","MP_database")) %>% 
  collect()
# remove connection
rm(con)


# odstranění pořadového sloupce, který vznikl při přeukládání
# odstranění očividně zbytečných proměnných
# "vehicle_category", "vehicle_date_of_first_registration", "insurer_birth_date" - NA hodnoty, nebo jen jedna hladina
# "model_poin_source" - bylo řečeno, že nám pro analýzu k ničemu nebude
data$X = NULL
data$vehicle_category = NULL
data$vehicle_date_of_first_registration = NULL
data$insurer_birth_date = NULL
data$model_poin_source = NULL


# výpočet medianu pro insurance_comp
attach(data)
data$insurance_median = apply(matrix(c(insurance_comp_1,insurance_comp_2,
                                        insurance_comp_3,insurance_comp_4,
                                        insurance_comp_5,insurance_comp_6,
                                        insurance_comp_7),
                                      ncol = 7, byrow = F),
                               1,median, na.rm = T)
detach(data)

# vyberem iba data kde je medián ceny
data <- as.data.frame(data[!is.na(data$insurance_median),])


# prevod vybraných proměnných na faktor
chci_faktor = c("model_point_id", "model_point_Period", 
                "vehicle_brand", "vehicle_model", 
                "vehicle_fuel_type", "vehicle_type_of_usage", 
                "insurer_legal_form", "insurer_ZIP_code", "insurer_place", 
                "policy_payment_frequency")
for(i in 1:length(chci_faktor)){
  data[,chci_faktor[i]] = as.factor(data[,chci_faktor[i]])
}
rm(chci_faktor,i)

# prevod veku na číslo 
data$insurer_age <- as.numeric(data$insurer_age)

# vektory jednotlivých hraníc intervalov
volume_breaks<- c(0,1000, 1350, 1600, 1850, 2000, 2200, 2600)
power_breaks <- c(0,50,75,93,110,150,180,5000)
age_breaks   <- c(0,24,26,30,35,40,45,50,60,70)

# funkcia ktorá vytvorí mená skupín
getLabels <- function(vec){
  retVec <- c()
  for (i in 2:length(vec)-1){
    retVec[i-1] <- sprintf("%s-%s",vec[i-1]+1,vec[i])
  }
  retVec[i] <- paste0(vec[i]+1,"+")
  return(retVec)
}

data$malusBonus36 <- (data$insurer_number_of_previous_insured_months - 
                        (36 * data$insurer_number_of_previous_accidents))

data$group_engine_volume <- cut(data$vehicle_engine_volume, 
                                breaks = volume_breaks,
                                labels =getLabels(volume_breaks))

data$group_engine_power <- cut(data$vehicle_engine_power, 
                               breaks = power_breaks,
                               labels =getLabels(power_breaks))

data$insurer_age_group <- cut(data$insurer_age, 
                              breaks = age_breaks,
                              labels =getLabels(age_breaks))
# Na pro vek u firem
data$insurer_age[which(data$insurer_legal_form == 3)] = NA

rm(age_breaks,power_breaks,volume_breaks,getLabels)



