get_type_details <- function(df, type){
  tmp <- df %>%
    filter(type == type) %>%
    group_by(month) %>%
    summarise(distance = sum(distance), # miles
              elev = sum(elev),         # ?
              time = sum(duration)/60,    # hours
              avgdist = mean(distance, na.rm = TRUE), #mi/run
              avgelev = mean(elev, na.rm = TRUE), # ft/run
              avgpace = mean(pace, na.rm = TRUE), # min/mile
              count = n(),
              .groups = "drop")
}
