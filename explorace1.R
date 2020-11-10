### EXPLORACE 1

# write.csv(moje_data,"dataMTPL.csv")
# přeuložní dat, aby šel vyvolat markdown
dataMTPL = read.csv("dataMTPL.csv")

# odstranění pořadového sloupce, který vznikl při přeukládání
dataMTPL$X = NULL

# odstranění očividně zbytečných proměnných
# "vehicle_category", "vehicle_date_of_first_registration", "insurer_birth_date" - NA hodnoty, nebo jen jedna hladina
# "model_poin_source" - bylo řečeno, že nám pro analýzu k ničemu nebude

# ověření statistik a odstranění
dataMTPL$model_poin_source = as.factor(dataMTPL$model_poin_source)
summary(dataMTPL[,c("vehicle_category", "vehicle_date_of_first_registration", "insurer_birth_date","model_poin_source")])

dataMTPL$vehicle_category = NULL
dataMTPL$vehicle_date_of_first_registration = NULL
dataMTPL$insurer_birth_date = NULL
dataMTPL$model_poin_source = NULL


# prevod vybraných proměnných na faktor
chci_faktor = c( "model_point_id", "model_point_Period", "vehicle_brand", "vehicle_model", 
                 "vehicle_fuel_type", "vehicle_type_of_usage", "insurer_legal_form",
                 "insurer_ZIP_code", "insurer_place", "policy_payment_frequency",
                 "insurer_number_of_previous_insured_months",
                 "insurer_number_of_previous_accidents")
for(i in 1:length(chci_faktor)){
  sloupec = chci_faktor[i]
  dataMTPL[,sloupec] = as.factor(dataMTPL[,sloupec])
}

# výpočet medianu pro insurance_comp
attach(dataMTPL)
dataMTPL$insurance_median = apply(matrix(c(insurance_comp_1,insurance_comp_2,
                                           insurance_comp_3,insurance_comp_4,
                                           insurance_comp_5,insurance_comp_6,
                                           insurance_comp_7),ncol = 7, byrow = F),1,median, na.rm = T)
detach(dataMTPL)
attach(dataMTPL)

## první náhled na data

library("skimr")
skim(dataMTPL)

# lze pozorovat, že máme 4121 NA hodnot pro insurance_median
# tato pozorovaní pro nás nemají žádnou hodnotu -> odstranit

dataMTPL = dataMTPL[!is.na(insurance_median),]
detach(dataMTPL)
attach(dataMTPL)

# druhý pohled na data
skim(dataMTPL)

### EXPLORATIVNÍ ANALÝZA JEDNOTLIVÝCH PROMĚNNÝCH

## model_point_id, model_point_Period
# společně jsou primarním klíčem tabulky
# id - číslo klienta
# Period - měsíc - duben/květen/červen

length(unique(model_point_id))
table(model_point_Period)
# 
# nemame pro každého klienta 3 měsíční, většinou jsou za 2 měsíce  (23 400 různých lidí)

### VEHICLE
## vehicle_brand, vehicle_model

table(vehicle_brand)
hist(as.numeric(vehicle_brand))
length(unique(vehicle_model))
hist(as.numeric(vehicle_model))

plot(as.numeric(vehicle_brand), as.numeric(vehicle_model), pch = 16, cex = 0.5)
# lze pozorovat problém v datech
# kdy model auta č. 379 je ve  všech značkách
# (OTAZKA K ZAMYSLENI č. 1 - Co s opakujícím se modelem - NA hodnoty?/ odstranit pozorování?)

## vehicle_engine_volume, vehicle_engine_power
summary(dataMTPL[,c("vehicle_engine_volume", "vehicle_engine_power", "vehicle_weight")])     

hist(vehicle_engine_volume, main = "vehicle_engine_volume")
nrow(dataMTPL[vehicle_engine_volume>2600,])
nrow(dataMTPL[vehicle_engine_volume>2600,])/nrow(dataMTPL)
# nad 2600 máme 2 % pozorování

par(mfrow=c(1,2))
hist(vehicle_engine_power, main = "engine power \n původní hodnoty")
hist(sqrt(vehicle_engine_power), main = "engine power \n odmocnina")
# odmocnina vylepší tvar rozdělení hodnot blíže k normálnímu
# (OTAZKA K ZAMYSLENI č. 2 - chceme odmocnit power? pokud ano, musí se na to myslet při modelování)
# ? ocasek do jednoho intervalu
par(mfrow=c(1,1))
plot(vehicle_engine_power,vehicle_engine_volume, pch = ".", cex = 2)
# (OTAZKA K ZAMYSLENI č. 3 - co může za takto vypadající graf?)
# (OTAZKA K ZAMYSLENI č. 4 - máme 3 odlehlé nízké hodnoty pro power a další 3 pro volume
          ## co s nimi? odstranit pozorování? nechat)

## vehicle_weight, vehicle_fuel_type
summary(dataMTPL[,c("vehicle_weight", "vehicle_fuel_type")])
hist((vehicle_weight))
nrow(dataMTPL[vehicle_weight<1000,])
min(vehicle_weight)

# náhodný obrázky zda někně neuvidímě nějakou korelaci
plot(vehicle_engine_volume, vehicle_weight, pch = ".",cex = 2, col = (as.numeric(vehicle_fuel_type)+1))
cor(vehicle_engine_volume, vehicle_weight)
# volume, weight, fuel type jsou vzajemne nezavisle
plot(vehicle_brand,vehicle_engine_volume, xlab = "n. brand", ylab = "volume")
plot(vehicle_brand,vehicle_engine_power, xlab = "n. brand", ylab = "power")
# nezdá se, že by se volume a power nějak vírazně lišili mezi značkou auta 
# když by na to přišlo, šlo by na to najít test
# (OTAZKA K ZAMYSLENI č. 5 - zajímá nás to dost na to, abych pro to hledala testování? :D )


### vehicle_age, vehicle_type_of_usage
summary(dataMTPL[,c("vehicle_age", "vehicle_type_of_usage")])
par(mfrow=c(1,2))
hist(vehicle_age, main = "vehicle age \n puvodni hodnoty")
hist(sqrt(vehicle_age), main = "vehicle age \n odmocnina")
par(mfrow=c(1,1))
# (OTAZKA K ZAMYSLENI č. 6 - chceme odmocnit věk a mít hodnoty z rozdělení bližšího normálnímu?)
# zbytecne

table(vehicle_type_of_usage)
# pravděpodobně nás zajímá jen type 2 (do ostatních tříd spadají taxíky, historická vozidla a podobně)
# (OTAZKA K ZAMYSLENI č. 7 - chceme odtranit pozorováníz ostatních hladin?
plot(vehicle_age,vehicle_engine_power, pch = ".")
plot(vehicle_age,vehicle_engine_volume, pch = ".")
### INSURER

## insurer_legal_form, insurer_age
summary(dataMTPL[,c("insurer_legal_form","insurer_age")])

# insurer_legal_form: 1 - fyzická osoba, 2 - živnostníci, 3 - firmy
# UKOL 1 - pro firmy je potřeba dát jako věk pojištěnce hodnoty NA, neboť tam je ten věk irelevantní
hist(as.numeric(insurer_age))
# krásný rozložení hodnot!

## insurer_ZIP_code, insurer_place

length(unique(insurer_ZIP_code))
length(unique(insurer_place))
# UKOL 2 - vyřešit přepočet dle počtu obyvatel/rozlohy/hustoty
# place - nevyužijem

## insurer_number_of_previous_insured_months, insurer_number_of_previous_accidents
# = bonusy
summary(dataMTPL[,c("insurer_number_of_previous_insured_months", "insurer_number_of_previous_accidents")])
par(mfrow=c(1,2))
hist(as.numeric(insurer_number_of_previous_insured_months), main = "zaplacených měsíců \n původ. hodnoty")
hist(sqrt(as.numeric(insurer_number_of_previous_insured_months)),main = "zaplacených měsíců \n odnocněný hodnoty")
par(mfrow=c(1,1))
# (OTAZKA K ZAMYSLENI č. 8 - chcem odmocninu?) 
table(insurer_number_of_previous_accidents)

## policy_payment_frequency
table(policy_payment_frequency)
 
plot(as.numeric( as.numeric(insurer_number_of_previous_insured_months, insurer_number_of_previous_accidents)), pch= ".")

### INSURANCE COMPANIES

hist(insurance_median)
hist(log(insurance_median))
#rozdělení výrazně pozitivně šikmé ... log(log())?
summary(insurance_median)
sd(insurance_median)

## VZTAHY MEZI VYSVĚTLOVANOU A VYSVĚTLUJÍCÍMI PROMĚNNÝMI

plot(insurance_median~as.numeric(model_point_id), pch = ".")
plot(insurance_median~vehicle_brand)
plot(insurance_median~vehicle_model, pch = ".")
plot(insurance_median~vehicle_engine_volume, pch = ".")
cor(insurance_median,vehicle_engine_volume)
cor(log(insurance_median),vehicle_engine_volume)
# tu bude nějaká positivní závislost
plot(insurance_median~vehicle_engine_power, pch = ".")
cor(log(insurance_median),vehicle_engine_power)
plot(insurance_median~vehicle_weight, pch = ".")
cor(log(insurance_median),vehicle_weight)
plot(insurance_median~vehicle_fuel_type)
plot(insurance_median~vehicle_age)
cor(log(insurance_median),vehicle_age)
# zajímavé, věk vozidla nehraje roli
plot(insurance_median~insurer_age)
# podezdřelost ve věku 40
plot(insurance_median~insurer_ZIP_code)
plot(insurance_median~insurer_number_of_previous_insured_months)
cor(log(insurance_median), as.numeric(insurer_number_of_previous_insured_months))
# vliv
plot(insurance_median~insurer_number_of_previous_accidents)
# vliv
plot(insurance_median~policy_payment_frequency)


detach(dataMTPL) 
