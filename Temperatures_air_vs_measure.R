library(rgee)
library(googledrive)
library(sf)

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
srtm <- rgee::ee$Image("USGS/SRTMGL1_003")

viz <- list(
  max = 4000,
  min = 0,
  palette = c("#000000","#5AAD5A","#A9AD84","#FFFFFF")
)

rgee::Map$addLayer(
  eeObject = srtm,
  visParams =  viz,
  name = 'SRTM',
  # legend = TRUE
)
#####################

####################
# Guia a seguir: https://developers.google.com/earth-engine/tutorials/community/ph-ug-temp
###
# 1o definir borders, queria so peniche, mas nao vamos ser muito ambiciosos logo portanto definir PT
# Image collection a usar para definir borders:
# https://developers.google.com/earth-engine/datasets/catalog/USDOS_LSIB_SIMPLE_2017
###
# Image collection a usar com daily temps:
# ee.ImageCollection("ECMWF/ERA5/DAILY") 
# mean_2m_air_temperature (em K, necessario converter em ºC)
# Necessario definir start time e end, comparar com dados BD
###
# Converter temp de  para ºC
###
# Exportar ficheiro temps bc why not
###
# Mapear como uma time series as temperaturas a considerar


############
### Analise temp bss vs temp diarias
# Quero graph time series com duas linhas temp
# Preciso testes estat para comparar os dois - var quantitativas

