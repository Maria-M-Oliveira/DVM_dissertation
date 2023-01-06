library(rgee)
library(googledrive)
library(sf)
library(tidyverse)
library(ggplot2)


# Library a usar para earth engine= rgee
# Guia instalacao - https://github.com/r-spatial/rgee
# Instalacao gcloud services: https://cloud.google.com/sdk/docs/install
# Mas da alguns problemas
# Solucoes para alguns que encontrei:
#  1 - instalar o google earth engine no conda - https://developers.google.com/earth-engine/guides/python_install-conda#windows_5
#  2-  conda a pedir o OpenSSL - https://github.com/conda/conda/issues/11795
#  3 - conda nao encontra gcloud instalado - https://gis.stackexchange.com/questions/445457/gcloud-command-not-found-when-authenticating-google-earth-engine
#  4 - R nao encontra credenciais/gcloud - https://github.com/r-spatial/rgee/issues/269#issuecomment-1246880456

rgee::ee_Initialize()

#####################
# codigo de teste do pacote

nc <- st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

terraclimate <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE") %>%
  ee$ImageCollection$filterDate("2001-01-01", "2002-01-01") %>%
  ee$ImageCollection$map(function(x) x$select("pr")) %>% # Select only precipitation bands
  ee$ImageCollection$toBands() %>% # from imagecollection to image
  ee$Image$rename(sprintf("PP_%02d",1:12)) # rename the bands of an image

ee_nc_rain <- ee_extract(
  x = terraclimate,
  y = nc["NAME"],
  scale = 250,
  fun = ee$Reducer$mean(),
  sf = TRUE
)  %>% 
  pivot_longer(PP_01:PP_12, names_to = "month", values_to = "pr") %>%
  mutate(month, month=gsub("PP_", "", month))

ee_nc_rain %>%
  ggplot(aes(x = month, y = pr, group = NAME, color = pr)) +
  geom_line(alpha = 0.4) +
  xlab("Month") +
  ylab("Precipitation (mm)") +
  theme_minimal()

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
# Exportar ficheiro temps como csv bc why not, pode dar jeito
###
# Mapear como uma time series as temperaturas a considerar


############
### Analise temp bss vs temp ar diarias
# Quero graph time series com duas linhas temp
# Preciso testes estat para comparar os dois - var quantitativas

#############
# Inicio do codigo a serio

temps_ar <- ee$ImageCollection("ECMWF/ERA5/DAILY") %>%
  ee$ImageCollection$filterDate("2017-01-02", "2022-02-01") %>%
  ee$ImageCollection$map(function(x) x$select("mean_2m_air_temperature")) %>% # Select only precipitation bands
  ee$ImageCollection$toBands() # from imagecollection to image

peniche <- ee$FeatureCollection("FAO/GAUL/2015/level2") %>% 
  ee$FeatureCollection$select("ADM1_NAME", "ADM1_CODE", "Shape_Area", "Shape_Length")$filter("ADM1_NAME"="Leiria") %>% 
  ee$FeatureCollection$getInfo()
# Da erro, nao e bem isto
# https://csaybar.github.io/rgee-examples/#Geometry%2c%20Feature%2c%20FeatureCollection --> investigar
# Tenho de transformar isto num objeto sf para conseguir fazer alguma coisa dele ACHO
# Mas queria so a infor de leiria,o resto pouco me interessa, how to escolher so leiria

# Outra coisa que posso fazer, e ir pelos dados portugeses mesmo, ja tenho shp com esses dados algures
# Provavelmente ia dar ao mesmo --> tentar

ee_nc_temp <- ee_extract(
    x = temps_ar,
    y = nc["NAME"], #Isto tem de ser alterado depois para o que quero mesmo
    scale = 250,
    fun = ee$Reducer$mean(),
    sf = TRUE
  )  
# Ficamos com df com colunas Xyyyymmdd_mean_2m_air_temperature -> portanto colunas desde dia 2/1/2017 a 1/2/2022
# Which means, preciso de pegar no pivot_longer e por isto como deve ser, em que as rows sao datas

ee_nc_temp <- ee_nc_temp %>% 
  pivot_longer(X20170102_mean_2m_air_temperature:X20200709_mean_2m_air_temperature, names_to = "day", values_to = "K") %>%
  mutate(day, day=gsub("X", "", day)) %>% 
  mutate(day, day=gsub("_mean_2m_air_temperature", "", day)) 
# Erro 1: so extraiu ate 09-07-2020 por alguma razao, pensar what do and why

ee_nc_temp %>%
  ggplot(aes(x = day, y = K, group = NAME, color = K)) +
  geom_line(alpha = 0.4) +
  xlab("Day") +
  ylab("Temperature (K)") +
  theme_minimal()
# Funciona, demora bues pq tenho 128500 observacoes pq  nao e so uma localidade
# Quando substituir por so uma, vai correr mais rapido
# Point is, funciona
