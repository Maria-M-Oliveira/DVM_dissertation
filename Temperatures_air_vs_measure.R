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
