% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tools_day_night.R
\name{get_day_night}
\alias{get_day_night}
\title{Get day or night}
\usage{
get_day_night(df, sun_events, idcol = "burstname", timecol = "realTime",
  longitudecol = "x")
}
\arguments{
\item{df}{a data.frame with column longitude, latitude and dateTime and
julday. As well as idcol.}

\item{sun_events}{a sun_events dataframe generated from \code{get_sun_events}}
}
\value{
a data.frame with for each observation night attribute and time2sunevent.
}
\description{
\code{get_day_night} calculates nautical and civil dawn and dusk as well as
sunrise and sunset, daylength, nautical length and civil length.
}
\examples{
get_day_night(subdf,sun_events,id="burstname")
}

