#' Get sun events from latitude, longitude and date
#'
#' \code{get_sun_events} calculates nautical and civil dawn and dusk as well as sunrise and sunset, daylength, nautical length and civil length.
#' @param data a data.frame with column longitude, latitude and dateTime
#' @return a data.frame containing both data and new columns with dawn, dusk, sunrise, sunset ...
#' @examples
#' get_sun_events(data)
#' @export
#


get_sun_events <- function(data){
  latPROJ <- "+proj=longlat +datum=WGS84 +no_defs"
  dawn <- with(data,maptools::crepuscule(crds=cbind(longitude,latitude),dateTime=dateTime,proj4string=sp::CRS(latPROJ),direction="dawn",solarDep=12,POSIXct.out=T))
  data$nautical_dawn <- dawn$time
  dusk <- with(data,maptools::crepuscule(crds=cbind(longitude,latitude),dateTime=dateTime,proj4string=sp::CRS(latPROJ),direction="dusk",solarDep=12,POSIXct.out=T))
  data$nautical_dusk <- dusk$time
  dawn <- with(data,maptools::crepuscule(crds=cbind(longitude,latitude),dateTime=dateTime,proj4string=sp::CRS(latPROJ),direction="dawn",solarDep=6,POSIXct.out=T))
  data$civil_dawn <- dawn$time
  dusk <- with(data,maptools::crepuscule(crds=cbind(longitude,latitude),dateTime=dateTime,proj4string=sp::CRS(latPROJ),direction="dusk",solarDep=6,POSIXct.out=T))
  data$civil_dusk <- dusk$time
  sunrise <- with(data,maptools::sunriset(crds=cbind(longitude,latitude),dateTime=dateTime,proj4string=sp::CRS(latPROJ),direction="sunrise",POSIXct.out=T))
  data$sunrise <- sunrise$time
  sunset <- with(data,maptools::sunriset(crds=cbind(longitude,latitude),dateTime=dateTime,proj4string=sp::CRS(latPROJ),direction="sunset",POSIXct.out=T))
  data$sunset <- sunset$time
  data <- mutate(data,day_length=as.numeric(difftime(sunset,sunrise,units='hours')),
                 civil_length=as.numeric(difftime(civil_dusk,civil_dawn,units='hours')),
                 nautical_length=as.numeric(difftime(nautical_dusk,nautical_dawn,units='hours')))
  data<- as.data.frame(data)
  return(data)
}

