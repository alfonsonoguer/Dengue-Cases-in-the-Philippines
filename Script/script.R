# -librarys ----------------------------------------------------------------

pacman::p_load(dplyr,tidyr,plyr,ggplot2,caret,corrplot,randomForest,zoo)

# -comments ---------------------------------------------------------------

# City and date indicators

# city – City abbreviations: sj for San Juan and iq for Iquitos
# week_start_date – Date given in yyyy-mm-dd format
# NOAA's GHCN daily climate data weather station measurements
# station_max_temp_c – Maximum temperature
# station_min_temp_c – Minimum temperature
# station_avg_temp_c – Average temperature
# station_precip_mm – Total precipitation
# station_diur_temp_rng_c – Diurnal temperature range

# PERSIANN satellite precipitation measurements (0.25x0.25 degree scale)

# precipitation_amt_mm – Total precipitation

# NOAA's NCEP Climate Forecast System Reanalysis measurements (0.5x0.5 degree
# scale)

# reanalysis_sat_precip_amt_mm – Total precipitation
# reanalysis_dew_point_temp_k – Mean dew point temperature
# reanalysis_air_temp_k – Mean air temperature
# reanalysis_relative_humidity_percent – Mean relative humidity
# reanalysis_specific_humidity_g_per_kg – Mean specific humidity
# reanalysis_precip_amt_kg_per_m2 – Total precipitation
# reanalysis_max_air_temp_k – Maximum air temperature
# reanalysis_min_air_temp_k – Minimum air temperature
# reanalysis_avg_temp_k – Average air temperature
# reanalysis_tdtr_k – Diurnal temperature range

# Satellite vegetation - Normalized difference vegetation index (NDVI) 
# - NOAAs CDR Normalized Difference Vegetation Index (0.5x0.5 degree scale)
# measurements

# ndvi_se – Pixel southeast of city centroid
# ndvi_sw – Pixel southwest of city centroid
# ndvi_ne – Pixel northeast of city centroid
# ndvi_nw – Pixel northwest of city centroid

# 1 data --------------------------------------------------------------------

train.cases<-read.csv("Data/dengue_features_train.csv")
train.labels<-read.csv("Data/dengue_labels_train.csv")


# 2 Preprocessing ---------------------------------------------------------


Denge <- merge(x = train.cases,y = train.labels,by = names(train.labels)[1:3])
is.na(Denge)
table(is.na(Denge))



Denge <- na.locf(Denge)
table(is.na(Denge))
topredict<-apply(is.na(Denge), 2, which)



Denge3 <- subset(Denge,select = c(names(Denge)[1:3],
                                  "reanalysis_min_air_temp_k",
                                  "reanalysis_precip_amt_kg_per_m2",
                                  "reanalysis_relative_humidity_percent",
                                  "reanalysis_dew_point_temp_k",
                                  "station_precip_mm",
                                  "total_cases"
                                  ))
Denge3$control <- seq(1:nrow(Denge3))
# eliminamos la fecha completa ya que no queremos trabajar con ella en los 
# modelos
Denge2 <- Denge[,-4]

# we find all the missing rows and we filter in 2 diferent DF 
Denge2_missing <-  Denge2[rowSums(is.na(Denge2)) > 0,]
Denge2_nomissing<- Denge2[rowSums(is.na(Denge2)) == 0,]

Denge3_missing <-  Denge3[rowSums(is.na(Denge3)) > 0,]
Denge3_nomissing<- Denge3[rowSums(is.na(Denge3)) == 0,]
names(Denge3_missing[, colSums(is.na(Denge3_missing)) > 0])

Denge3_nomissing
# i move the number of new contagions for sick 
Denge3_nomissing$sick<- c(0,head(Denge3_nomissing$total_cases,-1))
Denge$sick<- c(0,head(Denge$total_cases,-1))

# round 2
Denge_iq <- Denge[Denge$city=="iq", ]
Denge_sj <- Denge[Denge$city=="sj", ]


Denge3_no_iq <- Denge3_nomissing[Denge3_nomissing$city=="iq", ]
Denge3_no_sj <- Denge3_nomissing[Denge3_nomissing$city=="sj", ]

Denge3_no_iq_sample <- createDataPartition(Denge3_no_iq$total_cases,p = 0.75,
                                          list = FALSE,times = 1)
Denge3_no_sj_sample <- createDataPartition(Denge3_no_sj$total_cases,p = 0.75,
                                          list = FALSE,times = 1)
# ROUND2
Denge_iq_sample <- createDataPartition(Denge_iq$total_cases,p = 0.75,
                                          list = FALSE,times = 1)
Denge_sj_sample <- createDataPartition(Denge_sj$total_cases,p = 0.75,
                                          list = FALSE,times = 1)

Denge3_no_iq_train <- Denge3_no_iq[Denge3_no_iq_sample,]
Denge3_no_sj_train <- Denge3_no_sj[Denge3_no_sj_sample,]
Denge3_no_iq_test <- Denge3_no_iq[-Denge3_no_iq_sample,]
Denge3_no_sj_test <- Denge3_no_sj[-Denge3_no_sj_sample,]
# round 2 
Denge_iq_train <- Denge_iq[Denge_iq_sample,]
Denge_sj_train <- Denge_sj[Denge_sj_sample,]
Denge_iq_test <- Denge_iq[-Denge_iq_sample,]
Denge_sj_test <- Denge_sj[-Denge_sj_sample,]

# 2.1 Rellenar NA WiP-----------------------------------------------------------

# 
# ctr <- trainControl(method = "CV",number = 5)
# train(
#   data = Denge2_nomissing, method = "rf",trControl = ctr)
# 



# 2.2 eliminar alta correlacion -------------------------------------------

Denge2_nomissing.cor <- cor(Denge2_nomissing[,-1])
corrplot(Denge2_nomissing.cor)
highcorr<-findCorrelation(Denge2_nomissing.cor, cutoff = 0.85, verbose = FALSE,
                names = TRUE,
                exact = ncol(Denge2_nomissing.cor) < 100)


Denge2_v1 <- subset(Denge2_nomissing,select 
                    = !(names(Denge2_nomissing) %in% highcorr))


# 2.3 seleccion de atributos para modelado --------------------------------
# funcion2 mejores resultados
# funcion0 <- (total_cases ~ .-city-year-weekofyear)
# tc1<-trainControl(method = "CV", number =  10,)
# model_0 <- train(funcion0,
#                  data = Denge2_v1, method = "rf",
#                  trControl = tc1, tuneLength = 5, importance = TRUE)
# model_1 <- train(funcion1,
#                  data = Denge2_v1, method = "rf",
#                  trControl = tc1, tuneLength = 5, importance = TRUE)
model_2 <- train(funcion2,
                 data = Denge2_v1, method = "rf",
                 trControl = tc1, tuneLength = 5, importance = TRUE)
# model_3 <- train(funcion3,
#                  data = Denge2_v1, method = "rf",
#                  trControl = tc1, tuneLength = 5, importance = TRUE)
# model_4 <- train(funcion4,
#                  data = Denge2_v1, method = "rf",
#                  trControl = tc1, tuneLength = 5, importance = TRUE)

# model_0.1 <- randomForest(funcion0, data = Denge2_v1,ntree=500,importance= TRUE)
# variables<- varImp(model_0)
# varImp(model_4)
# funcion1 <-total_cases ~ reanalysis_min_air_temp_k + 
#   reanalysis_dew_point_temp_k + ndvi_nw + reanalysis_precip_amt_kg_per_m2 + 
#   reanalysis_relative_humidity_percent + station_precip_mm + ndvi_ne + 
#   station_diur_temp_rng_c + station_max_temp_c

funcion2 <-total_cases ~ reanalysis_min_air_temp_k + 
  reanalysis_dew_point_temp_k + reanalysis_precip_amt_kg_per_m2 + 
  reanalysis_relative_humidity_percent + station_precip_mm 
  
# funcion3 <-total_cases ~ reanalysis_min_air_temp_k + 
#   reanalysis_dew_point_temp_k + reanalysis_precip_amt_kg_per_m2 + 
#   reanalysis_relative_humidity_percent
#   
# funcion4 <-total_cases ~ reanalysis_min_air_temp_k + 
#   reanalysis_dew_point_temp_k+ reanalysis_precip_amt_kg_per_m2 + 
#   reanalysis_relative_humidity_percent + station_precip_mm 
varImp(model_2)
sort(varImp(model_0.1),decreasing = TRUE)




# 3 modeling --------------------------------------------------------------



model1_iq<- train( total_cases~reanalysis_specific_humidity_g_per_kg + ,
                 data = Denge3_no_iq_train,
                 method = "rf",
                 trainControl = tc1,
                 tuneLength = 5
)

model2_iq<- train( total_cases~ .-city,
                   data = Denge3_no_iq_train,
                   method = "svmRadial",
                   trainControl = tc1,
                   tuneLength = 5
)

model1_sj<- train( total_cases~ .-city,
                 data = Denge3_no_sj_train,
                 method = "rf",
                 trainControl = tc1,
                 tuneLength = 5
)

model2_sj<- train( total_cases~ .-city,
                   data = Denge3_no_sj_train,
                   method = "svmRadial",
                   trainControl = tc1,
                   tuneLength = 5
)

# modelingr2 --------------------------------------------------------------


# round2
model2_iq<- train( total_cases~reanalysis_specific_humidity_g_per_kg + 
                     reanalysis_dew_point_temp_k + 
                     station_min_temp_c + 
                     station_avg_temp_c,
                   data = Denge_iq_train,
                   method = "rf",
                   trainControl = tc1,
                   tuneLength = 5
)

model2_sj<- train( total_cases~reanalysis_specific_humidity_g_per_kg + 
                     reanalysis_dew_point_temp_k + 
                     station_min_temp_c + 
                     station_avg_temp_c,
                   data = Denge_sj_train,
                   method = "rf",
                   trainControl = tc1,
                   tuneLength = 5
)

Denge3_no_iq_test$prediction<- predict(model1_iq, Denge3_no_iq_test)
Denge3_no_sj_test$prediction<- predict(model1_sj, Denge3_no_sj_test)
postResample(Denge3_no_iq_test$sick,Denge3_no_iq_test$prediction)
postResample(Denge3_no_sj_test$sick,Denge3_no_sj_test$prediction)

# round2
Denge_iq_test$prediction<- predict(model2_iq, Denge_iq_test)
Denge_sj_test$prediction<- predict(model2_sj, Denge_sj_test)
postResample(Denge_iq_test$sick,Denge_iq_test$prediction)
postResample(Denge_sj_test$sick,Denge_sj_test$prediction)

Denge3_predictions<- bind_rows(Denge3_no_iq_test,Denge3_no_sj_test)

# comprobacion contra todos los datos
Denge_nomissing<- Denge[rowSums(is.na(Denge3)) == 0,]
Denge_nomissing$sick<- c(0,head(Denge_nomissing$total_cases,-1))
Denge_no_iq <- Denge_nomissing[Denge3_nomissing$city=="iq", ]
Denge_no_sj <- Denge_nomissing[Denge3_nomissing$city=="sj", ]


Denge_no_iq$prediction<- predict(model1_iq, Denge_no_iq)
Denge_no_sj$prediction<- predict(model1_sj, Denge_no_sj)
results <- bind_rows(Denge_no_iq,Denge_no_sj)

ggplot(results, aes(week_start_date)) +
  geom_point(aes(y=total_cases, color="red"))+
  geom_point(aes(y=prediction, color="blue"))


# 4 final predictions -----------------------------------------------------

test  <- read.csv("Data/dengue_features_test.csv")

test <- na.locf(test)
table(is.na(test))
test$sick <- 0
test_iq <- test[test$city=="iq", ]
test_sj <- test[test$city=="sj", ]

# Denge_nomissing$sick<- c(0,head(Denge_nomissing$total_cases,-1))

# Denge_no_iq$prediction<- predict(model1_iq, Denge_no_iq)
# Denge_no_sj$prediction<- predict(model1_sj, Denge_no_sj)

test_iq$prediction <- 0 

test_iq <- predict(model2_iq, test_iq)
test_sj <- predict(model2_sj, test_sj)
results <- c(test_iq,test_sj)
for (i in 1:nrow(test_iq)){
  test_iq$prediction[1:i]<- predict(model2_iq, test_iq[1:i, ])
  test_iq$sick <- c(0,head(test_iq$prediction,-1))
  }


for (i in 1:nrow(test_sj)){
  test_sj$prediction[1:i]<- predict(model2_sj, test_sj[1:i, ])
  test_sj$sick <- c(0,head(test_sj$prediction,-1))
}
results_final  <- bind_rows(test_iq,test_sj)


ggplot(results_final, aes(week_start_date)) +
  # geom_point(aes(y=total_cases, color="red"))+
  geom_point(aes(y=prediction, color="blue"))

submit <- read.csv("Data/submission_format.csv")

submit2 <- merge(x=submit[,1:3],y=results_final[,c(1:3,26)], by=names(submit)[1:3], all.x = TRUE)
names(submit2)<- names(submit) 


submit2$total_cases <- round(submit2$total_cases)
results <- round(results)
write.csv(results,"Round3.csv", row.names = FALSE)
