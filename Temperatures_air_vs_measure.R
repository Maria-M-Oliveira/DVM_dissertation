# library(weatherData)
# weatherdata package is currently not working (: need to try another
# getWeatherForDate(station_id = "LPPT" ,"2022-01-02", end_date = "2022-03-01")


# Usar outra library
library(rgee)
library(googledrive)

rgee::ee_Initialize(user = 'oliveira.maria.miguel@gmail.com', drive = TRUE, gcs = FALSE)
ee_install_upgrade()

ee_users()

images  <- ee$ImageCollection("ECMWF/ERA5/DAILY")$
  filterDate("2017-01-01", "2018-01-31")$
  select("mean_2m_air_temperature")

images <- images$map(function(i) i$unmask(-1))
array <- images$toArray()
bandNames <- images$aggregate_array("system:index")
image <- array$arrayProject(list(0))$arrayFlatten(list(bandNames))

print(image$getInfo())

bandNames <- image$bandNames()
print(bandNames$getInfo())

addTime <- function(image) {
  return(image$addBands(image$metadata("system:time_start")))
}

conditional <- function(image) {
  return(ee$Algorithms$If(
    ee$Number(image$get("SUN_ELEVATION"))$gt(40),
    image,
    ee$Image(0)
  ))
}

collection <- ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
  filter(ee$Filter$eq("WRS_PATH", 44))$
  filter(ee$Filter$eq("WRS_ROW", 34))

print(collection$map(addTime)$getInfo())

collection <- ee$ImageCollection("LANDSAT/LC8_L1T_TOA")$
  filter(ee$Filter$eq("WRS_PATH", 44))$
  filter(ee$Filter$eq("WRS_ROW", 34))


print(collection$map(conditional)$getInfo())


viz <- list(
  max = 4000,
  min = 0,
  palette = c("#000000","#5AAD5A","#A9AD84","#FFFFFF")
)

rgee::Map$addLayers(
  eeObject = images,
  # visParams =  viz,
  name = 'SRTM',
  # legend = TRUE
)
