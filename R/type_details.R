make_type_p <- function(df, type = "Run", typecolors = NULL) {

  if(is.null(typecolors)) {
    typecolors <- activity_type_colors()
  }
  df <- df[tolower(df$type) == tolower(type),] # case insensitive
  this_color <- typecolors[type]
  avg_pace <- mean(df$pace, na.rm = TRUE)
  total_time <- sum(df$duration, na.rm = TRUE)
  total_dist <- sum(df$distance, na.rm = TRUE)
  total_elev <- sum(df$elev, na.rm = TRUE)

  m_df <- clean_monthly_type_summary(df)
  month_plot <- ggplot(m_df, aes(month, distance)) +
    geom_col(fill = this_color) +
    geom_text(aes(label = paste0(round(distance),"\nmiles")),
              nudge_y = 6,
              color = this_color) +
    labs(x = "", y = "Distance (in Miles)") +
    expand_limits(y=100)+
    theme_minimal() +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y= element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  pace_plot <- ggplot(df, aes(distance, pace)) +
    geom_point(color = this_color, alpha = .5, size = 5) +
    labs(subtitle = paste("Average pace:",
                          round(avg_pace, 1),"minutes per mile"),
      y = "Pace (minutes per mile)",
      x = "Length (in miles)") +
    theme_minimal() +
    theme(legend.position="none", legend.title=element_blank())

  library(patchwork)
  p <- month_plot + inset_element(pace_plot, left = 0,
                                  bottom = 0.6, right = .55, top = .9)

  p <- p + plot_annotation(
    title = paste(type, "Log -", nrow(df), "Tracked",
                  paste0(type, "s!")),
    theme = theme(plot.title = element_text(size = 18,
                                            face = "bold")),
    subtitle = paste(type,
                     scales::label_comma()(total_dist),
                     "miles,",
                     scales::label_comma()(total_elev),
                     "feet vert,",
                     paste0("(",
                     scales::label_comma()(total_time),
                     "hours)")
                     )
  )
  return(p)
}
