###################################
#    Autor: Octavio Lafourcade    #
###################################

#---------------------------------------------------------
# Para limpiar el workspace, por si hubiera algun dataset 
# o informacion cargada
rm(list = ls())

# Semilla para replicar el codigo
set.seed(1)

#---------------------------------------------------------
# Cambiar el directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


# Cargamos las librerias a utilizar
library(ggplot2)
library(lattice)
library(caret)
library(stringr)
library(dplyr)
library(psych)
library(ROSE)
library(colorspace)
library(grid)
library(data.table)
library(VIM)
library(DMwR)
library(corrplot)


# Cargando el data set
data <- read.csv("./diabetic_data.csv")
attach(data)
#Revisión general del data set
str(data)
View(data)

# Presencia de Missing Values ####
# Se identifica la presencia de Missing values bajo los valores "?" y "Unknown/Invalid"
table(weight)
table(payer_code)
table(diag_1)
table(diag_2)
table(diag_3)
table(race)
table(gender)
table(medical_specialty)

data2 <- data

# Pasos a seguir
# 1. Recodificación de Datos Perdidos 
# 2. Diagnóstico de Datos Perdidos, MCAR o MAR?
# 3. Imputación de Datos Perdidos

# 1. Recodificación de Datos Perdidos ####

data2$weight <- replace(data2$weight, data2$weight=="?", NA)
data2$payer_code <- replace(data2$payer_code, data2$payer_code=="?", NA)
data2$diag_1 <- replace(data2$diag_1, data2$diag_1=="?", NA)
data2$diag_2 <- replace(data2$diag_2, data2$diag_2=="?", NA)
data2$diag_3 <- replace(data2$diag_3, data2$diag_3=="?", NA)
data2$race <- replace(data2$race, data2$race=="?", NA)
data2$gender <- replace(data2$gender, data2$gender=="Unknown/Invalid", NA)
data2$medical_specialty <- replace(data2$medical_specialty, data2$medical_specialty=="?", NA)

#Para ver que columnas tienen valores perdidos
which(colSums(is.na(data2))!=0)

#Para ver el porcentaje de valores perdidos en las columnas
colmiss=c(3,4,6,11,12,19,20,21)
per.miss.col=100*colSums(is.na(data2[,colmiss]))/dim(data2)[1]
per.miss.col

# 2. Diagnóstico de datos perdidos ####
# A simple vista se puede apreciar que las variables weight, payer_code y medical_specialty tienen una considerable 
# presencia de valores faltantes. Dado el alto porcentaje, no justificaría realizar ninguna técnica de imputación
# si no mas bien, retirar estas variables del análisis.
# En cambio, las variables race, gender, diag_1, diag_2 y diag_3 tienen una presencia de valores faltantes inferior al 5% por lo
# que se podría aplicar una técnica de imputación.

# En este sentido, la presencia de datos perdidos se puede deber a que el instrumento de recolección
# no funcionaba bien y no registró cierta información. 
# Otras alternativas es que el analista haya provocado los datos perdidos, no tenga información
# o es inconsistente y lo eliminó. También puede ser porque los datos no se ingresaron,
# el historial no se registró adecuadamente y se perdió esa información. 
# En algunos casos va a ser necesario estimar esos valores (como en el caso de race, gender, diag_1, diag_2 y diag_3)
# y en otros va a ser mejor no estimarlos y dejarlos allí.

# Identificando el mecanismo generador de valores perdidos
# MCAR: valores faltantes completamente al azar
# MAR: valores faltantes al azar como consecuencia de otros atributos
# NMAR: valores faltantes no al azar

# Usando la librería VIM
a=aggr(data2,numbers=T)
a
summary(a)
aggr(data2,numbers=T, sortComb=TRUE, sortVar=TRUE, only.miss=TRUE)
windows()
matrixplot(data2)

#¿MCAR, MAR o NMAR?
VIM::pbox(data2[6,11], pos=1)
t.test(data2$weight ~ is.na(data2$payer_code), data=data2)
marginplot(data2[,c("Weight", "Payer Code")])

# Del marginplot se puede apreciar que weight, payer_code y medical_specialty siguen un mecanismo generador de valores
# perdidos NMAR.

# 3.Imputación de datos perdidos ####
# A partir del diagnóstico previo se decidió retirar weight, payer_code y medical_specialty 
# e imputar race, gender, diag_1, diag_2 y diag_3.
# Dado que el porcentaje de valores perdidos en estas 4 variables es inferior al 5% podemos imputar por la media, 
# la moda o la mediana (Si la variable es numérica contínua con la media, numérica discreta con la mediana, 
# categórica con la moda).

# Retirando weight, payer_code y medical_specialty
data3 <- data2[,-c(6,11,12)]

# Imputando
data3<-centralImputation(data3)
data3<-initialise(data3,method="median")


# Validando
which(colSums(is.na(data3))!=0)

# Selección de variables dentro del contexto del negocio ####

# 1. Se ha identificado que un paciente (patient_nbr) puede tener más de un encuentro (encounter_id) y la unidad de 
#    análisis es el paciente. De este modo, se tomó la decisión de retirar la variable encounter_id

# 2. Se aprecia que las variables examide y citoglipton tienen solo un nivel "No" por lo que se retiran del análisis

# 3. Se decide trabajar con el diagnóstico principal por lo que los diagnósticos secundarios 2 y 3 son retirados

data3 <- data3[,-c(1,17,18,37,38)]

# Feature Engineering ####
# En esta etapa se va a realizar la recodificación de variables

# admission_type_id: Integer identifier corresponding to 9 distinct values: ####
  # 1:emergency, 2:urgent, 3:elective, 4:newborn, 5:not available, 6:NULL, 7: Trauma Center, 8: Not Mapped
  # Al analizar estas categorías se podría concluir que hay agrupaciones que se pueden realizar:
  # 1: emergency, urgent, Trauma Center
  # 3: elective
  # 4: newborn
  # 5: not available, NULL, Not Mapped
barplot(table(data3$admission_type_id))
data4 <- data3
  # De este modo, procedemos a recategorizar:
data4$admission_type_id <- 
  ifelse(data3$admission_type_id == 1,1,
         ifelse(data3$admission_type_id==2,1,
                ifelse(data3$admission_type_id==7,1,
                       ifelse(data3$admission_type_id==6,5,
                              ifelse(data3$admission_type_id==8,5,
                                     data3$admission_type_id)))))

  # Cambiamos el nombre a las categorías y convertimos en factor
data4$admission_type_id <- str_replace(data4$admission_type_id,"1","Emergency")
data4$admission_type_id <- str_replace(data4$admission_type_id,"5","Other")
data4$admission_type_id <- str_replace(data4$admission_type_id,"3","Elective")
data4$admission_type_id <- str_replace(data4$admission_type_id,"4","Newborn")

data4$admission_type_id <- as.factor(data4$admission_type_id)


windows()
par(mfrow=c(1, 2))
barplot(table(data3$admission_type_id), main="Antes")
barplot(table(data4$admission_type_id), main="Después")

# discharge_disposition_id: Integer identifier corresponding to 29 distinct values, for example, discharged to home, expired, and not available ####

# Categorizar en este caso se vuelve más complejo pero necesario por la atomización de categorías. 
# Esta variable tiene las siguientes categorías:
  # 1: Discharged to home, 
  # 2: Discharged/transferred to another short term hospital, 
  # 3: Discharged/transferred to SNF, 4: Discharged/transferred to ICF,
  # 5: Discharged/transferred to another type of inpatient care institution,
  # 6: Discharged/transferred to home with home health service
  # 7: Left AMA
  # 8: Discharged/transferred to home under care of Home IV provider
  # 9: Admitted as an inpatient to this hospital
  # 10: Neonate discharged to another hospital for neonatal aftercare
  # 11: Expired*
  # 12: Still patient or expected to return for outpatient services
  # 13: Hospice / home*
  # 14: Hospice / medical facility*
  # 15: Discharged/transferred within this institution to Medicare approved swing bed
  # 16: Discharged/transferred/referred another institution for outpatient services
  # 17: Discharged/transferred/referred to this institution for outpatient services
  # 18: NULL
  # 19: Expired at home. Medicaid only, hospice.*
  # 20: Expired in a medical facility. Medicaid only, hospice.*
  # 21: Expired, place unknown. Medicaid only, hospice.*
  # 22: Discharged/transferred to another rehab fac including rehab units of a hospital
  # 23: Discharged/transferred to a long term care hospital.
  # 24: Discharged/transferred to a nursing facility certified under Medicaid but not certified under Medica...
  # 25: Not Mapped
  # 26: Unknown/Invalid <- Fue imputada
  # 27: Discharged/transferred to a federal health care facility.
  # 28: Discharged/transferred/referred to a psychiatric hospital of psychiatric distinct part unit of a hos...
  # 29: Discharged/transferred to a Critical Access Hospital (CAH).
  # 30: Discharged/transferred to another Type of Health Care Institution not Defined Elsewhere
barplot(table(data3$discharge_disposition_id))
  # Hay tres aspectos a considerar: 
    # Por un lado, no se puede perder de vista que el objetivo es predecir si un paciente
    # es readmitido. En este sentido, hay categorías que se relacionan con la muerte o que un paciente ha sido deshauciado
    # y enviado a un asilo o a su casa. Tales categorías son la 11, 13, 14, 19, 20, or 21. Lo adecuado sería retirar los registros
    # de estos pacientes del data set.
data4 <- data4[ !(data4$discharge_disposition_id %in% c(11, 13, 14, 19, 20, 21)), ]

    # Se tiene la presencia de categorías con NULL y Not Mapped (18, 25) por lo que se procede a imputar
data4$discharge_disposition_id <- replace(data4$discharge_disposition_id, data4$discharge_disposition_id==18, NA)
data4$discharge_disposition_id <- replace(data4$discharge_disposition_id, data4$discharge_disposition_id==25, NA)

colmiss=c(6,7)
per.miss.col=100*colSums(is.na(data4[,colmiss]))/dim(data4)[1]
per.miss.col
    # Se aprecia que las categorías 18 y 25 corresponden al 4.71% del total de registros por lo que aún se puede utilizar una técnica
    # de imputación.
data4<-centralImputation(data4)
data4<-initialise(data4,method="median")

    # Por otro lado, se aprecia una disparidad considerable entre la categoría 1 respecto al resto de categorías. Además, el resto de
    # variables dan a entender que el paciente fue derivado a otro hospital o entidad. De este modo, se podría generar una categoría
    # que haga referencia a Discharged/transferred ... y agrupar al resto de categorías en ella.
    
data4$discharge_disposition_id <- 
  ifelse(data4$discharge_disposition_id == 3,2,
         ifelse(data4$discharge_disposition_id==4,2,
                ifelse(data4$discharge_disposition_id==5,2,
                       ifelse(data4$discharge_disposition_id==6,2,
                              ifelse(data4$discharge_disposition_id==7,2,
                                     ifelse(data4$discharge_disposition_id==8,2,
                                            ifelse(data4$discharge_disposition_id==9,2,
                                                   ifelse(data4$discharge_disposition_id==10,2,
                                                          ifelse(data4$discharge_disposition_id==12,2,
                                                                 ifelse(data4$discharge_disposition_id==15,2,
                                                                        ifelse(data4$discharge_disposition_id==16,2,
                                                                               ifelse(data4$discharge_disposition_id==17,2,
                                                                                      ifelse(data4$discharge_disposition_id==22,2,
                                                                                             ifelse(data4$discharge_disposition_id==23,2,
                                                                                                    ifelse(data4$discharge_disposition_id==24,2,
                                                                                                           ifelse(data4$discharge_disposition_id==27,2,
                                                                                                                  ifelse(data4$discharge_disposition_id==28,2,
                                                                                                                      data4$discharge_disposition_id)))))))))))))))))



  # Finalmente, podemos apreciar la diferencia en la categorización
windows()
par(mfrow=c(1, 2))
barplot(table(data3$discharge_disposition_id), main="Antes")
barplot(table(data4$discharge_disposition_id), main="Después")
str(data4)

  # Renombramos las variables
data4$discharge_disposition_id <- case_when(data4$discharge_disposition_id %in% "1" ~ "Home",TRUE ~ "Other")
  
  # Convirtiendo a factor
data4$discharge_disposition_id <- as.factor(data4$discharge_disposition_id)



#admission_source_id: Integer identifier corresponding to 21 distinct values, for example, physician referral, emergency room, and transfer from a hospital ####
# Esta variable tiene las siguientes categorías:
  # 1: Physician Referral
  # 2: Clinic Referral
  # 3: HMO Referral
  # 4: Transfer from a hospital
  # 5: Transfer from a Skilled Nursing Facility (SNF)
  # 6: Transfer from another health care facility
  # 7: Emergency Room
  # 8: Court/Law Enforcement
  # 9: Not Available
  # 10: Transfer from critial access hospital
  # 11: Normal Delivery
  # 12: Premature Delivery-
  # 13: Sick Baby-
  # 14: Extramural Birth
  # 15: Not Available-
  # 17: NULL
  # 18: Transfer From Another Home Health Agency-
  # 19: Readmission to Same Home Health Agency-
  # 20: Not Mapped
  # 21: Unknown/Invalid-
  # 22: Transfer from hospital inpt/same fac reslt in a sep claim
  # 23: Born inside this hospital-
  # 24: Born outside this hospital-
  # 25: Transfer from Ambulatory Surgery Center
  # 26: Transfer from Hospice-


table(data4$admission_source_id)
table(data$admission_source_id)
windows()
barplot(table(data4$admission_source_id), main="Antes")

# Del Barplot se puede apreciar que las categorías 1 y 7 concentran la mayor parte de los registros.
# Las categorías 1,2 y 3 se tratan de referidos por lo que se podrían categorizarse en una categoría.
# Las categorías 4,5,6,10,22,25  se tratan de transferencias desde otra entidad y podrían categorizarse en una categoría.
# La categoría 7 estaría individualizada.
# El resto de categorías podrían agruparse bajo "Otros"

data4$admission_source_id <- 
  ifelse(data4$admission_source_id == 1,1,
         ifelse(data4$admission_source_id==2,1,
                ifelse(data4$admission_source_id==3,1,
                       ifelse(data4$admission_source_id==4,4,
                              ifelse(data4$admission_source_id==5,4,
                                     ifelse(data4$admission_source_id==6,4,
                                            ifelse(data4$admission_source_id==10,4,
                                                   ifelse(data4$admission_source_id==22,4,
                                                          ifelse(data4$admission_source_id==25,4,
                                                                 ifelse(data4$admission_source_id==7,7,
                                                                        ifelse(data4$admission_source_id==8,8,
                                                                               ifelse(data4$admission_source_id==9,8,
                                                                                      ifelse(data4$admission_source_id==11,8,
                                                                                             ifelse(data4$admission_source_id==13,8,
                                                                                                    ifelse(data4$admission_source_id==14,8,
                                                                                                           ifelse(data4$admission_source_id==17,8,
                                                                                                                  ifelse(data4$admission_source_id==20,8,
                                                                                                                    data4$admission_source_id)))))))))))))))))
data4$admission_source_id <- case_when(data4$admission_source_id %in% c("1") ~ "Referral",
                                       data4$admission_source_id %in% c("4") ~   "Transfers", 
                                       data4$admission_source_id %in% c("8") ~   "Other", 
                                    TRUE ~ "Emergency Room")                                          

data4$admission_source_id <- as.factor(data4$admission_source_id)
windows()
par(mfrow=c(1, 2))
barplot(table(data3$admission_source_id), main="Antes")
barplot(table(data4$admission_source_id), main="Después")

#diag_1: The primary diagnosis (coded as first three digits of ICD9); 848 distinct values ####
# Se identificó una fuente de información que brinda una mayor noción sobre las categorías y un insight sobre la manera en la cual se podrían agrupar
# Las categorías que se crearían serían:
  # Circulatory: Diseases of the circulatory system. Códigos: 390-459, 785
  # Respiratory: Diseases of the respiratory system. Códigos: 460-519, 786
  # Digestive: Diseases of the digestive system. Códigos: 520-579, 787
  # Diabetes: Diabetes mellitus. Códigos: 250
  # Injury: Injury and poisoning. Códigos: 800-999
  # Musculoskeletal: Diseases of the musculoskeletal system and connective tissue. Códigos: 710-739
  # Genitourinary: Diseases of the genitourinary system. Códigos: 580-629, 788
  # Neoplasms: Neoplasms. Códigos: 140-239
  # Other: around 17% of the rest are considered here.

data5 <- data4

# Se requiere convertir a caracter para abordar los registros que tienen letras
data5$diag_1 <- as.character(data5$diag_1)

data5<- mutate(data5, diagnostic =
                 ifelse(str_detect(diag_1, "V") | str_detect(diag_1, "E"),"Other", # Se identificaron que algunos registros empiezan con V y E
                        ifelse(str_detect(diag_1, "250"), "Diabetes", # Se aprecia que el valor es del tipo 250.xx
                               ifelse((as.integer(diag_1) >= 390 & as.integer(diag_1) <= 459) | as.integer(diag_1) == 785, "Circulatory",
                                      ifelse((as.integer(diag_1) >= 460 & as.integer(diag_1) <= 519) | as.integer(diag_1) == 786, "Respiratory", 
                                             ifelse((as.integer(diag_1) >= 520 & as.integer(diag_1) <= 579) | as.integer(diag_1) == 787, "Digestive", 
                                                    ifelse((as.integer(diag_1) >= 580 & as.integer(diag_1) <= 629) | as.integer(diag_1) == 788, "Genitourinary",
                                                           ifelse((as.integer(diag_1) >= 140 & as.integer(diag_1) <= 239), "Neoplasms",  
                                                                  ifelse((as.integer(diag_1) >= 710 & as.integer(diag_1) <= 739), "Musculoskeletal",          
                                                                         ifelse((as.integer(diag_1) >= 800 & as.integer(diag_1) <= 999), "Injury",                    
                                                                                "Other"))))))))))

data5$diag_1 <- as.factor(data5$diag_1)

windows()
par(mfrow=c(1, 2))
barplot(table(data4$diag_1), main="Antes")
barplot(table(data5$diagnostic), main="Después")

data5 <- subset(data5, select = -c(diag_1))

str(data5)
barplot(table(data$age))

# Age: Grouped in 10-year intervals: [0, 10), [10, 20), . . ., [90, 100) ####
# Se puede apreciar que los rangos etarios avanzan de 10 en 10. Se podría categorizar las edades de 0 a 10 y 10 a 20 en una categoría al igual que de
# 80 a 90 y 90 a 100 en otra y el resto de categorías tendrían un rango de 20 años también.
# Se aprecia también que la distribución tiene una asimetría negativa.
data5$age <- data4$age
data5$age <- 
  ifelse(data5$age == "[0-10)","[0-20)",
         ifelse(data5$age=="[10-20)","[0-20)",
                ifelse(data5$age=="[20-30)","[20-40)",
                       ifelse(data5$age=="[30-40)","[20-40)",
                              ifelse(data5$age=="[40-50)","[40-60)",
                                     ifelse(data5$age=="[50-60)","[40-60)",
                                          ifelse(data5$age=="[60-70)","[60-80)",
                                                ifelse(data5$age=="[70-80)","[60-80)",
                                                      ifelse(data5$age=="[80-90)","[80-100)",
                                                            ifelse(data5$age=="[90-100)","[80-100)",
                                                                data5$age))))))))))

data5$age <- as.factor(data5$age)
windows()
par(mfrow=c(1, 2))
barplot(table(data4$age), main="Antes")
barplot(table(data5$age), main="Después")

# Variables adicionales que requerirían ser retiradas del análisis ####
# Se aprecia cierta duplicidad en cuanto a la medicación prescrita. 
# Por ejemplo, se tiene las variables glyburide y metformin por separado pero a la vez se tiene la variable glyburide.metformin que hace referencia
# a la prescripción de ambos fármacos en simultáneo.

# En consecuencia, se tomó la decisión de eliminar del análisis a glyburide.metformin, glipizide.metformin, glimepiride.pioglitazone,
# metformin.rosiglitazone y metformin.pioglitazone

data5 <- subset(data5, select = -c(glyburide.metformin, 
                                   glipizide.metformin, 
                                   glimepiride.pioglitazone, 
                                   metformin.rosiglitazone, 
                                   metformin.pioglitazone))

# Registros duplicados que deben ser removidos ####
# Se aprecia la presencia de duplicidad en los registros para un mismo patient_nbr en un porcentaje aproximado de 30%.
# Para poder remover los registros duplicados se utiliza !duplicated().
data5 <- data5[!duplicated(data5$patient_nbr),]

# Variable TARGET readmitted: 30 days, ">30" if the patient was readmitted in more than 30 days, and "No" for no record of readmission.####
# El análisis se centra en la readmisión anticipada (<30 días), se puede categorizar en:
  # 1: readmitido dentro de los 30 días posiblemente porque el tratamiento no fue el apropiado.
  # 0: cualquier otro caso
data5$readmitted <- data4$readmitted
data5$readmitted <- case_when(data5$readmitted %in% c(">30","NO") ~ "0",
                              TRUE ~ "1")
data5$readmitted <- as.factor(data5$readmitted)         


# Tratamiento de las variables numéricas (Outlier, Correlación, etc) ####
# Revisión general ####
windows()
par(mfrow = c(4,2))
boxplot(data5$time_in_hospital, main = "time_in_hospital")
boxplot(data5$num_lab_procedures, main = "num_lab_procedures")
boxplot(data5$num_procedures, main = "num_procedures")
boxplot(data5$num_medications, main = "num_medications")
boxplot(data5$number_outpatient, main = "number_outpatient")
boxplot(data5$number_emergency, main = "number_emergency")
boxplot(data5$number_inpatient, main = "number_inpatient")
boxplot(data5$number_diagnoses, main = "number_diagnoses")

# Se aprecia presencia de outliers en todas las variables numéricas analizadas
# Aplicando Capping ####
# time_in_hospital ####
data6 <- data5
x <- data6$time_in_hospital
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
data6$time_in_hospital <- x

# Observando los outliers identificados
outliers1 <- boxplot(x)$out
outliers1 ; length(outliers1)


# Observando el antes y el después
windows()
par(mfrow=c(1, 2))
boxplot(data5$time_in_hospital, main="Antes")
boxplot(data6$time_in_hospital, main="Despues")
mtext("Time in Hospital", outer = TRUE, side = 3, line = -1)


str(data5)
barplot(table(data5$readmitted))
######################################################

# Hacemos una 
data4 <- data3
data4$metformin.pioglitazone <- as.numeric(data3$metformin.pioglitazone)
table(data4$metformin.pioglitazone)
table(data4$glyburide)
data4$glyburide <- as.numeric(data3$glyburide)
cor(data4$metformin.pioglitazone,data4$glyburide)

corre <- cor(data4,method = c("spearman"))
data4[,1:ncol(data4)] <- lapply(data4[,1:ncol(data4)],as.character)
data4[,1:ncol(data4)] <- lapply(data4[,1:ncol(data4)],as.numeric)

corre <- correlacionS(corre)
corre$filtro <- ifelse(abs(corre$cor)>0.6,1,0)
View(corre)

data4$race <- as.numeric(as.factor(data4$race))
str(data3)

table(diag_1)
