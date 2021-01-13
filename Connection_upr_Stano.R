
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


# drop order number created by load 
data$X = NULL
# drop vehicle category - almost all data are "For personal use" (CODE: 1)
data$vehicle_category = NULL
# no information - holds only one value
data$model_poin_source = NULL
# droppped due to NaNs
data$insurer_birth_date = NULL
data$insurer_place = NULL
# rather used vehicle age
data$vehicle_date_of_first_registration = NULL
# too many models, not applicable in model -> drop
data$vehicle_model = NULL


# calc median for all insurance_comp-anies
attach(data)
data$insurance_median = apply(matrix(c(insurance_comp_1,insurance_comp_2,
                                        insurance_comp_3,insurance_comp_4,
                                        insurance_comp_5,insurance_comp_6,
                                        insurance_comp_7),
                                      ncol = 7, byrow = F),
                               1,median, na.rm = T)
detach(data)

# keep only values where at least one price is non-null
data <- as.data.frame(data[!is.na(data$insurance_median),])


# factorize given variables
chci_faktor = c("model_point_id", "model_point_Period", 
                "vehicle_brand", "vehicle_fuel_type", 
                "vehicle_type_of_usage", "insurer_legal_form", 
                "insurer_ZIP_code", "policy_payment_frequency")
for(i in 1:length(chci_faktor)){
  data[,chci_faktor[i]] = as.factor(data[,chci_faktor[i]])
}
rm(chci_faktor,i)

# convert Age to numeric 
data$insurer_age <- as.numeric(data$insurer_age)

# values discovered as "boundary" for given variables by EDA
volume_breaks<- c(0,1000, 1350, 1600, 1850, 2000, 2200, 2600)
power_breaks <- c(0,50,75,93,110,150,180,5000)
age_breaks   <- c(0,24,26,30,35,40,45,50,60,70)

# function to create named groups by its boundaries
getLabels <- function(vec){
  retVec <- c()
  for (i in 2:length(vec)-1){
    retVec[i-1] <- sprintf("%s-%s",vec[i-1]+1,vec[i])
  }
  retVec[i] <- paste0(vec[i]+1,"+")
  return(retVec)
}
# Bonus-Malus appears to be dependent as (nr of insured month)-(nr of claims)*36
data$malusBonus36 <- (data$insurer_number_of_previous_insured_months - 
                        (36 * data$insurer_number_of_previous_accidents))
# create variable number of whole years insured
data$ins_nr_prev_insured_whole_years <- 
        round(data$insurer_number_of_previous_insured_months/12 - .5)

# grouped variables
data$group_engine_volume <- cut(data$vehicle_engine_volume, 
                                breaks = volume_breaks,
                                labels =getLabels(volume_breaks))

data$group_engine_power <- cut(data$vehicle_engine_power, 
                               breaks = power_breaks,
                               labels =getLabels(power_breaks))

data$insurer_age_group <- cut(data$insurer_age, 
                              breaks = age_breaks,
                              labels =getLabels(age_breaks))
# Companies are not allowed to have age - set to NULL, Age_group set to "ICO"
data$insurer_age[which(data$insurer_legal_form == 3)] = NA
levels(data$insurer_age_group) <- c(levels(data$insurer_age_group),"ICO")
data$insurer_age_group[is.na(data$insurer_age_group)] = "ICO"

rm(age_breaks,power_breaks,volume_breaks,getLabels) # clean variable set

# create fix train and validation split
set.seed(123) # fix random number generator
train = sample(1:nrow(data), nrow(data)*.8)
data_train = data[train,]
data_validation = data[-train,]
rm(train)
