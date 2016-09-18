#' Get day or night
#'
#' \code{get_day_night} calculates nautical and civil dawn and dusk as well as
#' sunrise and sunset, daylength, nautical length and civil length.
#' @param subdf a data.frame with column longitude, latitude and dateTime and
#'   julday. As well as idcol.
#' @param sun_events a sun_events dataframe generated from \code{get_sun_events}
#' @return a data.frame with for each observation night attribute and time2sunevent.
#' @examples
#' get_day_night(subdf,sun_events,id="burstname")
#' @export
#
#
# Sys.setenv(TZ="GMT")
# # Load packages
# UTMstring<-"+proj=utm +zone=35 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
# latPROJ <- "+proj=longlat +datum=WGS84 +no_defs"
# TZ <- "Africa/Harare"
# lions <- read.csv("../../These/Hwange/Lions/data/Formatted/Lions_raw.csv")
# lions <- dplyr::mutate(lions,dateTime=as.POSIXct(strptime(paste(dateTime), "%Y-%m-%d %H:%M:%S", tz=TZ)))
#
# df <- lions;rm(lions)
# Jan1 =  as.POSIXct(strptime(paste("2000","-01-01 00:00:01",sep=""), "%Y-%m-%d %H:%M:%S"),tz=TZ)
# df <- mutate(df,julday=floor(difftime(dateTime,Jan1,units='day')),julday=as.numeric(julday))
#
# sun_events <- group_by(filter(df,!is.na(latitude)),id,julday) %>% summarise(latitude=mean(latitude,na.rm=T),longitude=mean(longitude,na.rm=T),dateTime=first(dateTime))
# sun_events <- as.data.frame(sun_events)
# # df <- sun_events
# sun_events <- crepuscule::get_sun_events(sun_events)
# subdf <- dplyr::filter(df,id=="CATF1")
# idcol="id"
# longitudecol="longitude"
# timecol="dateTime"

get_day_night  <- function(subdf,sun_events,idcol="burstname",timecol="realTime",longitudecol="x"){
  indiv <- first(subdf[,idcol])
  subsun <- eval(parse(text=paste('dplyr::filter(sun_events,',idcol,'==indiv)',sep='')))
  subdf2 <- dplyr::left_join(subdf,dplyr::select(subsun,julday,sunrise,sunset),by=c("julday"))
  subdf2 <- eval(parse(text=paste('dplyr::filter(subdf2,!is.na(',longitudecol,'))',sep="")))
  subdf2 <- eval(parse(text=paste('dplyr::mutate(subdf2,night=day_or_night(',timecol,',sunrise,sunset),time2sunevent=get_time2sunevent(',timecol,',sunrise,sunset))',sep="")))
  subdf2 <- dplyr::select(subdf2,index,night,time2sunevent)
  subdf <- dplyr::left_join(subdf,subdf2)
  return(subdf)
}

#' Get day, night and time to sunrise or sunset
#'


day_or_night <- Vectorize(function(time,sunrise,sunset){
  if(time < sunrise) return("night")
  if(time >= sunrise & time <= sunset) return("day")
  if(time > sunset) return("night")
  return(NA)
})

#' Get day, night and time to sunrise or sunset
#'

get_time2sunevent <- Vectorize(function(time,sunrise,sunset){
  hour <- as.numeric(format(time,"%H"))
  if(time < sunrise) return(as.numeric(difftime(time,sunrise,units='hours')))
  if(time >= sunrise & time <= sunset & hour < 13) return( as.numeric(difftime(time,sunrise,units='hours')))
  if(time >= sunrise & time <= sunset & hour >= 13) return( as.numeric(difftime(time,sunset,units='hours')))
  if(time > sunset) return( as.numeric(difftime(time,sunset,units='hours')))
  return(NA)
})
