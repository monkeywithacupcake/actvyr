#' Get Clean Activity Data
#' 
#' Combines Strava and Peloton data into a single data set that avoids 
#' duplicating workouts shared from Peloton to Strava. Will work if no peloton 
#' file is had, creating grouped data.
#'
#' @param strava_df df output of clean_strava_file. required.
#' @param peloton_df df output of clean_peloton_file. optional
#' @param other vector of strings that are other workouts
#' 
#' @import dplyr
#'
#' @return data frame with single entry for each activity and a group
#' @export
#'
clean_activity_df <- function(strava_df, peloton_df = NULL,
                               other = c("Stretching", "Cardio", "Meditation")
                               ) {
  
  actv_df <- strava_df %>%
    filter(!type %in% "Workout") # workout does not have data
  
  if(!is.null(peloton_df)) {
    actv_df <- actv_df %>%
      mutate(x = ifelse(type == 'Peloton Ride', 
                        paste(date, round(distance,1)), NA)) 
    actv_df <- actv_df %>%
      bind_rows(peloton_df %>%
                  mutate(month = lubridate::month(date, label=TRUE),
                         x = ifelse(type == 'Cycling',
                                    paste(date, round(distance,1)), NA)) %>%
                  filter(is.na(x) | !x %in% actv_df$x) %>%
                  mutate(type = ifelse(type == 'Cycling', 'Peloton Ride', type))
      ) %>%
      select(-x) 
  }
  
  actv_df <- actv_df %>%
    mutate(group = case_when(
      grepl("Run", type) ~ "Run",
      grepl("Ride", type) ~ "Ride",
      type %in% other ~ "Other",
      .default = type)
    )
  return(actv_df)
}