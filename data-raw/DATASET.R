## code to prepare `DATASET` dataset goes here

# Read raw data
df <- readr::read_csv("data-raw/ex_peloton.csv")
df <- df[, colSums(is.na(df)) != nrow(df)]
c("Workout Timestamp", "Length (minutes)", "Distance (mi)", "Avg. Speed (mph)", "Fitness Discipline")
ex_peloton_data <- df
usethis::use_data(ex_peloton_data, overwrite = TRUE)

df <- readr::read_csv("data-raw/ex_strava.csv")
df <- df[, colSums(is.na(df)) != nrow(df)]
df <- df[, c("Activity Date", "Weather Condition","Apparent Temperature",
             "Distance...7", "Moving Time", "Activity Name", "Elevation Gain")]

ex_strava_data <- df

usethis::use_data(ex_strava_data, overwrite = TRUE)

ex_actv_df <- clean_activity_df(clean_strava_file("data-raw/ex_strava.csv",
                                                  this_year = year(Sys.Date())-1),
                             clean_peloton_file("data-raw/ex_peloton.csv",
                                                this_year = year(Sys.Date())-1)
)

usethis::use_data(ex_actv_df, overwrite = TRUE)

