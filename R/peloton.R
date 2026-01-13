# peloton


#' Known timezones
known_timezones <- list(
  "AKT" = "-09",
  "ADT" = "-08",
  "PST" = "-08",
  "PDT" = "-07",
  "MST" = "-07",
  "MDT" = "-06",
  "CST" = "-06", # there is a China time that is CST +08
  "CDT" = "-05",
  "EST" = "-05",
  "EDT" = "-04",
  "AST" = "-04",
  "WET" = "+00",
  "GMT" = "+00",
  "CET" = "+01",
  "FET" = "+03",
  "CXT" = "+07",
  "KST" = "+09"
)

get_known_timezones <- function(){
  return(known_timezones)
}

#' Decode timezones
#' @param x vector of three character timezones
#' @param known_timezones list of three character timezones
#' that you should definitely pass if you know what your data
#' have in them
#'
decode_timezones <- function(x,tzs = NULL){
  if(is.null(tzs)){tzs = get_known_timezones()}
  y <- as.character(unname(plyr::revalue(x, tzs,
                                    warn_missing = FALSE)))
  return(y)
}

#' Replace timezones in timestamps
#' @param x vector of timestamps
#' @param tzs list of three character timezones
#' that you should definitely pass if you know what your data
#' have in them
#'
replace_timezones_in_timestamps <- function(x, tzs = NULL){
  x_paren <- substr(x, nchar(x)-3, nchar(x)-1)
  ox <- substr(x, 1, nchar(x)-6)
  nx <- decode_timezones(x = x_paren, tzs)
  return(paste0(ox, " (",nx,")"))
}

convert_to_utc <- function(x, tzs = NULL){
  y = replace_timezones_in_timestamps(x, tzs)
  z = parse_datetime(y, "%F %T (%z)")
  return(z)
}

# functions to handle peloton downloaded data
#' Clean Peloton Data from File
#'
#' @param fpath path to the csv
#' @param this_year numeric year defaults to NULL
#' if NULL there is no filter
#' @param prefer_utc boolean for if the timestamp should be
#' converted to UTC. default is FALSE (local at time of activity)
#' conversion to UTC has challenges, namely that three digit
#' timezones are not unique (CST used in US, China, and sometimes Oz)
#' @param tzs list of three character timezones
#' that you should definitely pass if you know what your data
#' have in them
#'
#' @importFrom readr read_csv
#' @import dplyr
#'
#' @return dataframe
#' @export
#'
#' @examples \dontrun{
#' peloton_df <- clean_peloton_file(fpath = file.path("dir", "peloton.csv"),
#'                                  this_year = year(Sys.Date())-1)
#' }
clean_peloton_file <- function(fpath,
                               this_year = NULL,
                               prefer_utc = FALSE,
                               tzs = NULL){
  if(!is.null(this_year)){# this year should be a number
    this_year <- as.numeric(this_year)
    stopifnot(!is.na(this_year))
  }
  df <- readr::read_csv(fpath)

  if(prefer_utc){ # convert `Workout Timestamp` to UTC
    df$`Workout Timestamp` <- convert_to_utc(df$`Workout Timestamp`)
  }

  # if the activity is a distance workout, Length is NA, so calculate
  df <- df %>%
    mutate(date = as.Date(substr(`Workout Timestamp`,0,10)),
           duration = suppressWarnings(ifelse(`Length (minutes)` == "None",
                             `Distance (mi)`*60*(1/`Avg. Speed (mph)`),
                             as.numeric(`Length (minutes)`)))
    ) %>%
    { if(!is.null(this_year)) filter(.,substr(date, 1, 4) == this_year) else (.) }%>%
    select(date,
           type = `Fitness Discipline`,
           duration,
           distance = `Distance (mi)`)
}



