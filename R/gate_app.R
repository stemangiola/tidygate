#' Create Shiny App UI
#' 
#' @importFrom shiny fluidPage
#' @importFrom shiny actionButton
#' @importFrom plotly plotlyOutput
#' @return Fluid UI container
#' @export
ui <- 
  shiny::fluidPage(
    shiny::actionButton("continue_button", "Continue"),
    # shiny::actionButton("deselect_button", "Deselect all"),
    plotly::plotlyOutput("plot")
  )

#' Run Shiny App for interactive gating 
#' 
#' @importFrom plotly renderPlotly
#' @importFrom plotly event_data
#' @importFrom plotly ggplotly
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 theme_bw
#' @importFrom purrr map_lgl
#' @importFrom shiny observe
#' @importFrom shiny stopApp
#' @param input Server input parameter
#' @param output Server output parameter
#' @param session Server session parameter
#' @return NA
#' @export
server <- 
  function(input, output, session) {

    output$plot <- plotly::renderPlotly({
      
      # Begin recording selection and brush information
      select_data <- plotly::event_data("plotly_selected")
      brush_data <- plotly::event_data("plotly_brushed")
      
      # Save selection and brush information
      assign("select_data", tidygate_env$input_data, envir = tidygate_env)
      assign("brush_data", brush_data, envir = tidygate_env)
      
      # Set selected points to TRUE
      if (!is.null(select_data)) {
        tidygate_env$input_data <-
          tidygate_env$input_data |>
          mutate(.selected = ifelse(.key %in% as.numeric(select_data$key), TRUE, .selected))
      }
      
      # Create plot
      if (is.null(tidygate_env$custom_plot)) {
        plot <-
          tidygate_env$input_data |>
          ggplot2::ggplot(ggplot2::aes(x = dimension_x, y = dimension_y, colour = .selected, key = .key)) +
          ggplot2::geom_point(alpha = tidygate_env$input_data$.alpha[[1]], size = tidygate_env$input_data$.size[[1]]) +
          ggplot2::theme_bw()
        
      # Or load supplied plot
      } else {
        plot <-
          tidygate_env$custom_plot
      }

      # Draw plot
      plot |>
        plotly::ggplotly() |>
        plotly::layout(dragmode = "lasso") 
    })
    
    # Close Shiny App with button press
    shiny::observe({
      if(input$continue_button > 0){
        shiny::stopApp()
      }
      # if(input$deselect_button > 0){
      #   tidygate_env$input_data$.selected <- FALSE
      # }
    })
  }
