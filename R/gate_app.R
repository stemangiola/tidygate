#' Create Shiny App UI
#' 
#' @importFrom shiny fluidPage
#' @importFrom shiny actionButton
#' @importFrom shiny verbatimTextOutput
#' @importFrom plotly plotlyOutput
#' @return Fluid UI container
#' @export
ui <- fluidPage(
  shiny::actionButton("continue_button", "Continue"),
  plotly::plotlyOutput("plot"),
  shiny::verbatimTextOutput("select"),
  shiny::verbatimTextOutput("brush")
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
#' @importFrom dplyr left_join
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate
#' @importFrom dplyr pull
#' @importFrom shiny observeEvent
#' @importFrom shiny renderPrint
#' @importFrom shiny stopApp
#' @param input Server input parameter
#' @param output Server output parameter
#' @param session Server session parameter
#' @return NA
#' @export
server <- function(input, output, session) {
  
  # Fix CRAN note
  key <- NULL
  
  select_data <- tibble()
  brush_data <- tibble()
  
  # Draw plot
  output$plot <- plotly::renderPlotly({
    tidygate_env$input_plot |>
        plotly::ggplotly() |>
        plotly::layout(dragmode = "lasso") |>
        plotly::config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "select2d"))      
  })

  # Get selection information
  output$select <- shiny::renderPrint({
    select_event <- plotly::event_data("plotly_selected")
    if (!is.null(select_event)) {
      select_event$.gate <- tidygate_env$event_count
      select_data <<- rbind(select_data, select_event)
    }
    select_data
  })

  # Get brush information
  output$brush <- shiny::renderPrint({
    brush_event <- plotly::event_data("plotly_brushed")
    if (!is.null(brush_event)) {
      brush_event <- tibble(x = brush_event$x, y = brush_event$y)
      brush_event$.gate <- tidygate_env$event_count
      brush_data <<- rbind(brush_data, brush_event)
      tidygate_env$event_count <- tidygate_env$event_count + 1
    }
    brush_data
  })

  # Close
  shiny::observeEvent(input$continue_button, {

    # Save supplementary data
    tidygate_env$select_data <- select_data
    tidygate_env$brush_data <- brush_data
    
    # Return vector of gate values
    shiny::stopApp({
      tidygate_env$input_data |>
        dplyr::left_join(
          tidygate_env$select_data |>
            dplyr::mutate(key = as.integer(key)) |>
            dplyr::group_by(key) |>
            dplyr::summarise(.gate = list(.gate)),
          by = c(".key" = "key")) |>
        dplyr::pull(.gate)
    })
  })
}
