# library(weatherData)
# weatherdata package is currently not working (: need to try another
# getWeatherForDate(station_id = "LPPT" ,"2022-01-02", end_date = "2022-03-01")


# Usar outra library
library(rgee)
library(googledrive)
library(sf)

rgee::ee_Initialize(user = 'oliveira.maria.miguel@gmail.com', drive = TRUE, gcs = FALSE)

nc <- st_read(system.file("shape/nc.shp", package = "sf")) %>%
  st_transform(4326) %>%
  sf_as_ee()

ee_s2 <- ee$ImageCollection("ECMWF/ERA5/DAILY")$
  filterDate("2016-01-01", "2016-01-31")$
  filterBounds(nc)

ee_s2 <- ee$ImageCollection(ee_s2$toList(2))

Map$centerObject(nc$geometry())
m5 <- Map$addLayers(
  ee_s2$filterDate("2016-01-03"),
  visParams
  )
m5

visParams <- list(
  min = 250,
  max = 320,
  palette = c('#000080', '#0000D9', '#4000FF', '#8000FF', '#0080FF', '#00FFFF', '#00FF80',
    '#80FF00', '#DAFF00', '#FFFF00', '#FFF500', '#FFDA00', '#FFB000', '#FFA400',
    '#FF4F00', '#FF2500', '#FF0A00', '#FF00FF')
  )

