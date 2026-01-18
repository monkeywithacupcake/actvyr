# handle strava data

#' Clean Strava File
#'
#' This is an opinionated function using Imperial units, returning temperature
#' in F, distance in miles, and duration in minutes.
#'
#' @param fpath path to the activities.csv from your strava download. Strava
#'  gives you an entire folder of data when you download your data. The path
#'  just to the activities.csv is desired here.
#' @param this_year numeric. like 2025.
#' @param this_tz string. like UTC. Only use this if all of your activities are
#' from the same time zone that you know.
#' @param has_peloton boolean. if you want to label rides with zero elevation
#' as peloton ride.
#'
#' @importFrom readr read_csv parse_datetime
#' @importFrom magrittr "%>%"
#' @import dplyr
#' @importFrom lubridate year month
#'
#' @return data frame of strava activites
#' @export
#'
#' @examples \dontrun{
#' strava_df <- clean_strava_file(fpath = file.path("dir", "activities.csv"),
#'                                this_year = year(Sys.Date())-1)
#' }
clean_strava_file <- function(fpath,
                              this_year = NULL,
                              this_tz = NULL,
                              has_peloton = FALSE){
  if(is.null(this_tz)){this_tz = "UTC"}
  if(!is.null(this_year)){# this year should be a number
    this_year <- as.numeric(this_year)
    stopifnot(!is.na(this_year))
  }
  activities <- readr::read_csv(fpath)
  pattern <- "^([^,]*,[^,]*),"
  # weather
  weather <- data.frame(
    code = c(1,2,3,5),
    condition = c("clear","partly cloudy","cloudy","rain")
  )
  df <- activities %>%
    mutate(datetime = format(readr::parse_datetime(`Activity Date`,
                                            "%b %d, %Y, %H:%M:%S %p"),
                             tz=,usetz=FALSE),
           date = as.Date(datetime),
           #start_hour = format(parse_datetime(datetime,"%F %T"), "%H"),
           year = as.numeric(lubridate::year(date)),
           month = lubridate::month(date, label=TRUE),
           code = `Weather Condition`,
           temp = (`Apparent Temperature`* (9/5)) + 32,
           elev  = `Elevation Gain` * 3.28084, # feet
           distance = `Distance...7`*0.621372,
           duration = `Moving Time`/60,
           pace = duration/distance) %>%
    { if(!is.null(this_year)) filter(., year == this_year) else (.) }%>%
    left_join(weather) %>%
    select(date, month, name = `Activity Name`, type = `Activity Type`,
           duration, pace, distance, elev, condition, temp)

  if(has_peloton) {
    df <- df %>%
      mutate(type = ifelse(elev == 0 & type == "Ride",
                           "Peloton Ride", type))
  }

  return(df)
}
