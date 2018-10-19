# Function for positioning the titles with the ggplotly graphs

helper_plotly_title <- function(plot){

  title_info <- list(
    text = plot$labels$title,
    xref = "paper",
    yref = "paper",
    yanchor = "bottom",
    xanchor = "center",
    align = "center",
    x = 0.5,
    y = 1,
    showarrow = FALSE
  )

  return(title_info)

}


