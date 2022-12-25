# library(weatherData)
# weatherdata package is currently not working (: need to try another
# getWeatherForDate(station_id = "LPPT" ,"2022-01-02", end_date = "2022-03-01")


# Usar outra library
library(rgee)

ee_Initialize(user = 'oliveira.maria.miguel@gmail.com', drive = TRUE, gcs = FALSE)
ee_install_upgrade()

ee_users()
ee_clean_credentials("ndef")
