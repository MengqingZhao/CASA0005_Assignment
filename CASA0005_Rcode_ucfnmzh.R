#CASA0005-Rscript-ucfnmzh by Mengqing Zhao
library(highcharter)
library(downloader)
library(ggplot2)
library(reshape2)
library(raster)
library(tidyverse)
library(tmap)
library(geojsonio)
library(plotly)
library(rgdal)
library(broom)
library(mapview)
library(crosstalk)
library(sf)
library(sp)
library(spdep)
library(car)
library(fs)
library(janitor)
londonshape <- st_read("data/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")
summary(londonshape)
mycsv <-  read_csv("data/data_edit.csv")  
mycsv
#merge boundaries and data
londonleftjoin <- londonshape%>%
  left_join(.,
            mycsv,
            by = c("GSS_CODE" = "code"))
tmap_mode("view")
qtm(londonleftjoin, 
    fill = "co2_emission", 
    borders = NULL,  
    fill.palette = "Reds")
qtm(londonleftjoin, 
    fill = "average_income", 
    borders = NULL,  
    fill.palette = "Blues")
qtm(londonleftjoin, 
    fill = "employment_rate", 
    borders = NULL,  
    fill.palette = "Greens")
qtm(londonleftjoin, 
    fill = "active_enterprises", 
    borders = NULL,  
    fill.palette = "Oranges")
qtm(londonleftjoin, 
    fill = "population", 
    borders = NULL,  
    fill.palette = "Purples")

# Descriptive Statistics
summary(mycsv)
ggplot(londonleftjoin,aes(y=co2_emission)) + 
  geom_boxplot()
ggplot(londonleftjoin,aes(y=average_income)) + 
  geom_boxplot()
ggplot(londonleftjoin,aes(y=employment_rate)) + 
  geom_boxplot()
ggplot(londonleftjoin,aes(y=active_enterprises)) + 
  geom_boxplot()
ggplot(londonleftjoin,aes(y=population)) + 
  geom_boxplot()

# normal distribution 
ggplot(londonleftjoin, aes(x=co2_emission)) + 
  geom_histogram(aes(y = ..density..)) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

ggplot(londonleftjoin, aes(x=average_income)) + 
  geom_histogram(aes(y = ..density..)) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)
ggplot(londonleftjoin, aes(x=employment_rate)) + 
  geom_histogram(aes(y = ..density..)) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)
ggplot(londonleftjoin, aes(x=active_enterprises)) + 
  geom_histogram(aes(y = ..density..)) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)
ggplot(londonleftjoin, aes(x=population)) + 
  geom_histogram(aes(y = ..density..)) + 
  geom_density(colour="red", 
               size=1, 
               adjust=1)

# OLS Regression model
OLSRegressiondata<- londonleftjoin%>%
  clean_names()%>%
  dplyr::select(co2_emission, 
                average_income,
                employment_rate,
                active_enterprises,
                population)
#now model
OLSmodel <- OLSRegressiondata %>%
  lm(co2_emission ~
       average_income + employment_rate + active_enterprises + population,
     data=.)
#show the summary of those outputs
tidy(OLSmodel)
glance(OLSmodel)

# No Multicolinearity
library(corrr)
Correlation <- londonleftjoin %>%
  st_drop_geometry()%>%
  dplyr::select(co2_emission,
                average_income,
                employment_rate,
                active_enterprises,
                population) %>%
  correlate() %>%
  # just focus on GCSE and house prices
  focus(-co2_emission, mirror = TRUE)
Correlation
rplot(Correlation)

# VIF
vif(OLSmodel)

#save the residuals into your dataframe
OLSmodel_data <- OLSmodel %>%
  augment(., OLSRegressiondata)
#plot residuals
OLSmodel_data%>%
  dplyr::select(.resid)%>%
  pull()%>%
  qplot()+ 
  geom_histogram()
# also add them to the shapelayer
londonleftjoin <- londonleftjoin %>%
  mutate(OLSmodelresids = residuals(OLSmodel))
#print some model diagnositcs
par(mfrow=c(2,2))    #plot to 2 by 2 array
plot(OLSmodel)
#now plot the residuals
tmap_mode("view")
qtm(londonleftjoin, fill = "OLSmodelresids")

#calculate Moran
#calculate the centroids of all Wards in London
coordsW <- londonleftjoin%>%
  st_centroid()%>%
  st_geometry()
plot(coordsW)
#or nearest neighbours
knn_wards <-coordsW %>%
  knearneigh(., k=4)
LWard_knn <- knn_wards %>%
  knn2nb()
Lward.knn_4_weight <- LWard_knn %>%
  nb2listw(., style="C")

OLS_model_Moran <- londonleftjoin %>%
  st_drop_geometry()%>%
  dplyr::select(OLSmodelresids)%>%
  pull()%>%
  moran.test(., Lward.knn_4_weight)%>%
  tidy()
OLS_model_Moran

#GWR
library(spgwr)
st_crs(londonleftjoin) = 27700
londonleftjoinSP <- londonleftjoin %>%
  as(., "Spatial")
st_crs(coordsW) = 27700
coordsWSP <- coordsW %>%
  as(., "Spatial")
coordsWSP
#calculate kernel bandwidth
GWRbandwidth <- gwr.sel(co2_emission ~
                          average_income + employment_rate + active_enterprises + population, 
                        data = londonleftjoinSP, 
                        coords=coordsWSP,
                        adapt=T)
#run the gwr model
gwr.model = gwr(co2_emission ~
                  average_income + employment_rate + active_enterprises + population, 
                data = londonleftjoinSP, 
                coords=coordsWSP, 
                adapt=GWRbandwidth, 
                hatmatrix=TRUE, 
                se.fit=TRUE)
#print the results of the model
gwr.model
results <- as.data.frame(gwr.model$SDF)
names(results)
#attach coefficients to original SF
londonleftjoin2 <- londonleftjoin %>%
  mutate(coef_average_income = results$average_income,
         coef_employment_rate = results$employment_rate,
         coef_active_enterprises = results$active_enterprises,
         coef_population = results$population)
tm_shape(londonleftjoin2) +
  tm_polygons(col = "coef_average_income", 
              palette = "RdBu", 
              alpha = 0.5)
tm_shape(londonleftjoin2) +
  tm_polygons(col = "coef_employment_rate", 
              palette = "RdBu", 
              alpha = 0.5)
tm_shape(londonleftjoin2) +
  tm_polygons(col = "coef_active_enterprises", 
              palette = "RdBu", 
              alpha = 0.5)
tm_shape(londonleftjoin2) +
  tm_polygons(col = "coef_population", 
              palette = "RdBu", 
              alpha = 0.5)
#run the significance test
sigTest_average_income = abs(gwr.model$SDF$"average_income")-2 * gwr.model$SDF$"average_income_se"
sigTest_employment_rate = abs(gwr.model$SDF$"employment_rate")-2 * gwr.model$SDF$"employment_rate_se"
sigTest_active_enterprises = abs(gwr.model$SDF$"active_enterprises")-2 * gwr.model$SDF$"active_enterprises_se"
sigTest_population = abs(gwr.model$SDF$"population")-2 * gwr.model$SDF$"population_se"
#store significance results
londonleftjoin2 <- londonleftjoin2 %>%
  mutate(GWR_average_income_Sig = sigTest_average_income)
londonleftjoin2 <- londonleftjoin2 %>%
  mutate(GWR_employment_rate_Sig = sigTest_employment_rate)
londonleftjoin2 <- londonleftjoin2 %>%
  mutate(GWR_active_enterprises_Sig = sigTest_active_enterprises)
londonleftjoin2 <- londonleftjoin2 %>%
  mutate(GWR_population_Sig = sigTest_population)
tm_shape(londonleftjoin2) +
  tm_polygons(col = "GWR_average_income_Sig", 
              palette = "RdYlBu")
tm_shape(londonleftjoin2) +
  tm_polygons(col = "GWR_employment_rate_Sig", 
              palette = "RdYlBu")
tm_shape(londonleftjoin2) +
  tm_polygons(col = "GWR_active_enterprises_Sig", 
              palette = "RdYlBu")
tm_shape(londonleftjoin2) +
  tm_polygons(col = "GWR_population_Sig", 
              palette = "RdYlBu")

