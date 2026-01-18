activity_group_colors <- function() {
  x <- data.frame(
  group = c("Hike", "Walk", "Run", "Ride", "Yoga", "Strength", "Other"),
  color = c("#4daf4a","#beaed4","#984ea3", "#375eb8", "#f9bb90","#0af9fa","#909090")
)
  return(x)
}
activity_type_colors <- function() {
  x <- data.frame(
  type = c("Hike", "Walk", "Run", "Ride", "E-Bike Ride", "Peloton Ride"),
  color = c("#4daf4a","#beaed4","#984ea3", "#375eb8", "#377eb8","#80b1d3")
)
  return(x)
}

#' Make Daily Activities by Month Plot
#'
#' Creates a horizontal plot with a breakdown for each month with number of
#' activities and summarizes by duration
#'
#' @param actv_df data.frame out of clean_activity_df. could be created from
#'  another source. Is expected to have columns: c('date','month','name',
#'  'type','duration','pace','distance','elev','condition','temp','group')
#' @param this_year numeric. like 2025
#' @param groupcolors data.frame of colors by group
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom stringr str_pad
#'
#' @return plot
#' @export
#'
#' @examples \dontrun{
#' # use the example data
#' make_daily_activities_by_month_plot <- function(ex_actv_df, 2025)
#' }
make_daily_activities_by_month_plot <- function(actv_df, this_year,
                                                groupcolors = NULL) {
  if(is.null(groupcolors)) {
    groupcolors <- activity_group_colors()
  }
  ddf <- actv_df %>%
    group_by(month, date, group) %>%
    summarise(duration = sum(duration), .groups = "drop") %>%
    group_by(date) %>%
    mutate(dtot = sum(duration, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(month) %>%
    mutate(avg = mean(dtot, na.rm = TRUE),
           y_max = max(3, max(duration/60))) %>% ungroup() %>%
    mutate(lbl = paste(stringr::str_pad(month(date), 2, "left", "0"),
                       month,"~ avg:", round(avg, 0), "min"))

  ddf$group <- factor(ddf$group, levels=groupcolors$group)

  p <- ggplot(ddf, aes(x = date, y = duration/60, fill = group)) +
    geom_col() +
    geom_hline(aes(yintercept = avg/60)) +
    labs(title = paste("Active Days,", this_year),
         subtitle = paste("Avg activity duration:",
                          round(mean(actv_df$duration,
                                     na.rm = TRUE),0), "min |",
                          "Avg daily total duration:",
                          round(mean(ddf$dtot, na.rm = TRUE),0)),
         caption = "includes only activities tracked in peloton or strava apps, duplicates removed",
         x = "", y = "Hours of Activity",fill = "")+
    scale_fill_manual(labels = groupcolors$group, values = groupcolors$color) +
    scale_x_date(date_labels = "%d") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    facet_wrap(.~lbl, scales = "free") +
    geom_blank(aes(y = y_max)) +
    guides(fill=guide_legend(nrow=1,byrow=TRUE))

  return(p)
}



make_yearly_activities_plot <- function(actv_df, this_year,
                                        typecolors = NULL) {
  if(is.null(typecolors)) {
    typecolors <- activity_type_colors()
  }


}


#' Make Daily Activities (with distance) by Month Data
#'
#' Used in plots
#'
#' @param df data.frame out of clean_activity_df. could be created from
#'  another source. Is expected to have columns: c('date','month',
#'  'type','duration','pace','distance','elev')
#' @param typecolors data.frame of colors by type.
#' defaults to activity_type_colors()
#'
#' @import dplyr
#'
#' @return data.frame summarized
#' @export
#'
#' @examples \dontrun{
#' # use the example data
#' clean_monthly_type_summary(df = ex_actv_df)
#' }
clean_monthly_type_summary <- function(df, typecolors = NULL) {
  if(is.null(typecolors)) {
    typecolors <- activity_type_colors()
  }
  df <- df[df$type %in% typecolors$type,]
  df$type <- factor(df$type,
                    levels=typecolors$type)
  tmp <- df %>%
    mutate(elevation = elev,
           pace = coalesce(pace, duration/distance),
           act_dist = distance) %>% # rename for mean
    group_by(month, type) %>%
    summarise(distance = sum(distance, na.rm = TRUE), # miles
              elev = sum(elev, na.rm = TRUE),         # ?
              time = sum(duration, na.rm = TRUE)/60,    # hours
              avgdist = mean(act_dist, na.rm = TRUE), #mi/run
              avgelev = mean(elevation, na.rm = TRUE), # ft/run
              avgpace = mean(pace, na.rm = TRUE), # min/mile
              count = n(),
              .groups = "drop") %>%
    mutate(avgelev = coalesce(avgelev, 0))
  return(tmp)
}

#' Make Monthly Distance by Type Plot
#'
#' Creates a bar chart of miles by type
#'
#' @param df data.frame expected to have columns: c('month', 'type'
#' 'distance'). output of clean_monthly_type_summary() is best.
#' @param typecolors data.frame of colors by type
#'
#' @import dplyr
#' @import ggplot2
#'
#' @return plot
#' @export
#'
#' @examples \dontrun{
#' # use the example data
#' mo_df <- clean_monthly_type_summary(df = ex_actv_df)
#' make_mo_dist_type_plot(df = mo_df)
#' }
make_mo_dist_type_plot <- function(df, typecolors = NULL) {
  if(is.null(typecolors)) {
    typecolors <- activity_type_colors()
  }
  p <- ggplot(df, aes(month, distance, fill = type)) +
    geom_col() +
    labs(
      x = "",
      y = "Distance (in Miles)") +
    scale_fill_manual(labels = typecolors$type,
                      values = typecolors$color) +
    theme_minimal() +
    theme(legend.position="top", legend.title=element_blank()) +
    guides(fill = guide_legend(nrow = 1))
  return(p)
}

#' Make Monthly Elevation by Type Plot
#'
#' Creates a bar chart of vert by type
#'
#' @param df data.frame expected to have columns: c('month', 'type'
#' 'elev'). output of clean_monthly_type_summary() is best.
#' @param typecolors data.frame of colors by type
#'
#' @import ggplot2
#'
#' @return plot
#' @export
#'
#' @examples \dontrun{
#' # use the example data
#' mo_df <- clean_monthly_type_summary(df = ex_actv_df)
#' make_mo_elev_type_plot(df = mo_df)
#' }
make_mo_elev_type_plot <- function(df, typecolors = NULL) {
  if(is.null(typecolors)) {
    typecolors <- activity_type_colors()
  }
  p <- ggplot(df, aes(month, elev, fill = type)) +
    geom_col() +
    labs(x = "",
         y = "Elevation Gain (in Feet)") +
    scale_fill_manual(labels = typecolors$type,
                      values = typecolors$color) +
    theme_minimal() +
    theme(legend.position="top", legend.title=element_blank())
  return(p)
}
