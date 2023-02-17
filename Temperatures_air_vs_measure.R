#########LIBRARIES##########

library(rgee)
library(googledrive)
library(sf)
library(tidyverse)
library(ggplot2)
library(readxl)
library(lubridate)
library(weathermetrics)

#########FILES##########
product <- read_xlsx("DB_Limpo.xlsx") %>% 
  rename(Temp= `Temperatura Receção Logística (ºC)`) %>% 
  rename(day=Data)


########Data cleaning###########
boxplot(product$Temp) # a lot of outliers

quartiles <- quantile(product$Temp, probs=c(.25,.75),na.rm=FALSE)
IQR <- IQR(product$Temp)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] +1.5*IQR

product_clean <- subset(product, product$Temp>Lower & product$Temp<Upper)
# Library a usar para earth engine= rgee
# Guia instalacao - https://github.com/r-spatial/rgee
# Instalacao gcloud services: https://cloud.google.com/sdk/docs/install
# Mas da alguns problemas
# Solucoes para alguns que encontrei:
#  1 - instalar o google earth engine no conda - https://developers.google.com/earth-engine/guides/python_install-conda#windows_5
#  2-  conda a pedir o OpenSSL - https://github.com/conda/conda/issues/11795
#  3 - conda nao encontra gcloud instalado - https://gis.stackexchange.com/questions/445457/gcloud-command-not-found-when-authenticating-google-earth-engine
#  4 - R nao encontra credenciais/gcloud - https://github.com/r-spatial/rgee/issues/269#issuecomment-1246880456

####################
# Guia a seguir: https://developers.google.com/earth-engine/tutorials/community/ph-ug-temp
# https://cran.r-project.org/web/packages/rgee/rgee.pdf
# https://r-spatial.github.io/rgee/
###
# 1o definir borders, queria so peniche, mas nao vamos ser muito ambiciosos logo portanto definir PT
# Image collection a usar para definir borders:
# https://developers.google.com/earth-engine/datasets/catalog/USDOS_LSIB_SIMPLE_2017 --> este para pais
# https://developers.google.com/earth-engine/datasets/catalog/FAO_GAUL_2015_level1#description --> 1º nivel
# https://developers.google.com/earth-engine/datasets/catalog/FAO_GAUL_2015_level2#description --> 2º nivel
# Aparentemente, consigo ir as close as Leiria --> ver ficheiro FAO GAUL (excel csv)
# https://data.apps.fao.org/catalog/dataset/gaul-codes/resource/cfdaf156-26b9-46c2-aab2-eb437fc16622
###
# Image collection a usar com daily temps:
# ee.ImageCollection("ECMWF/ERA5/DAILY") 
# mean_2m_air_temperature (em K, necessario converter em ºC)
# Necessario definir start time e end, comparar com dados BD
###
# Converter temp de  para ºC
###
# Mapear como uma time series as temperaturas a considerar

############
### Analise temp bss vs temp ar diarias
# Quero graph time series com duas linhas temp
# Preciso testes estat para comparar os dois - var quantitativas

#############
# Inicio do codigo a serio
rgee::ee_Initialize()

#####################
nc <- st_read("Cont_AAD_CAOP2020", geometry_column = "geometry") %>% 
  subset(Freguesia %in% c("Peniche")) %>% 
  subset(TAA %in% c("ÁREA PRINCIPAL"))

temps_ar <- ee$ImageCollection("ECMWF/ERA5/DAILY") %>%
  ee$ImageCollection$filterDate("2017-01-02", "2022-02-01") %>%
  ee$ImageCollection$map(function(x) x$select("mean_2m_air_temperature")) %>% # Select only temperature bands
  ee$ImageCollection$toBands() # from imagecollection to image

ee_nc_temp <- ee_extract(
    x = temps_ar,
    y = nc["geometry"], #Isto tem de ser alterado depois para o que quero mesmo
    scale = 250,
    fun = ee$Reducer$mean(),
    sf = TRUE
  )  


# Ficamos com df com colunas Xyyyymmdd_mean_2m_air_temperature -> portanto colunas desde dia 2/1/2017 a 9/7/2020 pq era o que tinhamos no earth engine
# Which means, preciso de pegar no pivot_longer e por isto como deve ser, em que as rows sao datas

ee_nc_temp <- ee_nc_temp %>% 
  pivot_longer(X20170102_mean_2m_air_temperature:X20200709_mean_2m_air_temperature, names_to = "day", values_to = "K") %>%
  mutate(day, day=gsub("X", "", day)) %>% 
  mutate(day, day=gsub("_mean_2m_air_temperature", "", day)) 


ee_nc_temp %>%
  ggplot(aes(x = day, y = K, color = K)) +
  geom_line(alpha = 0.4, group=1) +
  xlab("Day") +
  ylab("Temperature (K)") +
  theme_minimal()

temp_date <- ee_nc_temp %>%  
  st_drop_geometry()

boxplot(temp_date$Celsius) #no outliers

product_clean %>%
  ggplot(aes(x = Data, y = Temp, color = Temp)) +
  geom_line(alpha = 0.4) +
  xlab("Day") +
  ylab("Temperature (C)") +
  theme_minimal()


temp_date$day <- ymd(temp_date$day)

product_clean$Data <- ymd(product_clean$day)
temp_date$Celsius <- kelvin.to.celsius(temp_date$K, round=1)

temp_date %>%
  ggplot(aes(x = day, y = Celsius, color = Celsius)) +
  geom_line(alpha = 0.4) +
  xlab("Day") +
  ylab("Temperature (ºC)") +
  theme_minimal()

Air_product <- left_join(product_clean, temp_date) %>% 
  subset(select = c(day, Temp, Celsius))

Air_product %>% 
  ggplot(aes(x=day)) +
  geom_line(aes(y=Celsius, colour="Air")) +
  geom_line(aes(y=Temp, colour="Product"))
  

# time series depois

Air_sem_NA <- Air_product %>% drop_na()

# First, i should remove the outliers in the product variable and only then can i carry on the analysis

ks.test(Air_sem_NA$Temp, "pnorm") #p<2.2e-16, not normal
ks.test(Air_sem_NA$Celsius, "pnorm") #p<2.2e-16, not normal

corre <- cor(Air_sem_NA$Temp, Air_sem_NA$Celsius, method="spearman")
corre #0.205 
