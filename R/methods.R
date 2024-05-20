#' Label points within a scatter plot drawing a gate
#'
#' \lifecycle{maturing}
#'
#' @description gate() takes as input a `tbl` formatted as | <DIMENSION 1> | <DIMENSION 2> | <...> | and calculates the rotated dimensional space of the feature value.
#'
#' @importFrom rlang enquo
#' @importFrom rlang quo_is_null
#' @importFrom magrittr "%>%"
#'
#' @name gate_chr
#'
#'
#' @param .dim1 A column symbol. The x dimension
#' @param .dim2 A column symbol. The y dimension
#' @param .color A column symbol. Colour of points
#' @param .shape A column symbol. Shape of points
#' @param .size A column symbol. Size of points
#' @param opacity A number between 0 and 1. The opacity level of the data points
#' @param how_many_gates An integer. The number of gates to label
#' @param .group_by A column symbol. The column that is used to calculate distance (i.e., normally genes)
#' @param gate_list A list of gates. It is returned by gate function as attribute \"gate\". If you want to create this list yourself, each element of the list is a data frame with x and y columns. Each row is a coordinate. The order matter.
#' @param ... Further parameters passed to the function gatepoints::fhs
#'
#' @details This function allow the user to label data points in inside one or more 2D gates. This package is based on on the package gatepoints.
#'
#' @return An character vector, with "0" for elements outside gates and "1..N" for the elements inside the N gates. 
#'
#'
#' @examples
#'
#' \donttest{
#' # Standard use - interactive
#' 
#'   if(interactive()){
#'
#'  tidygate::tidygate_data  %>%
#'  distinct(`ct 1` , `ct 2`, Dim1, Dim2) %>%
#'  mutate(gate = gate_chr( Dim1, Dim2)) 
#'
#'   }
#'
#' }
#'
#' library(magrittr)
#' library(dplyr)
#' 
#' # Standard use - programmatic
#' res_distinct =
#'  tidygate::tidygate_data  %>%
#'  distinct(`ct 1` , `ct 2`, Dim1, Dim2) %>%
#'  mutate(gate = gate_chr( Dim1, Dim2,gate_list = tidygate::gate_list)) 
#'
#' # Grouping - programmatic
#' res =
#'  tidygate::tidygate_data  %>%
#'    mutate(gate = gate_chr( 
#'      Dim1, Dim2,
#'      .group_by = c(`ct 1` , `ct 2`), 
#'      gate_list = tidygate::gate_list
#'    ))
#'
#'
#' @docType methods
#' @rdname gate_chr-methods
#' @export
#'
#'
#'
#'
gate_chr <- function(.dim1,
                     .dim2,
                     .color = NULL,
                     .shape = NULL,
                     .size = NULL,
                     opacity = 1,
                     how_many_gates = 1,
                     .group_by = NULL,
                     
                     gate_list = NULL,
                     ...) {
  UseMethod("gate_chr")
}


#' gate_chr
#' 
#' @inheritParams gate_chr
#' 
#' @return An character vector, with "0" for elements outside gates and "1..N" for the elements inside the N gates. 
#' 
#' @export
gate_chr.numeric = 	function(                     .dim1,
                                                   .dim2,
                                                   .color = NULL,
                                                   .shape = NULL,
                                                   .size = NULL,
                                                   opacity = 1,
                                                   how_many_gates = 1,
                                                   .group_by = NULL,
                                                   
                                                   gate_list = NULL,
                                                   ...) {

  
  .gate_chr_int(
    .dim1 = .dim1,
    .dim2 = .dim2,
    .color = .color,
    .shape = .shape,
    .size = .size,
    opacity = opacity,
    how_many_gates = how_many_gates,
    .group_by = .group_by,
    gate_list = gate_list,
    output_type = "chr",
    ...
  )
  
}


#' gate_int
#' 
#' @name gate_int
#' 
#' @inheritParams gate_chr
#' @docType methods
#' @rdname gate_chr-methods
#' 
#' @return An integer vector, with 0 for elements outside gates and 1..N for the elements inside the N gates. 
#' 
#' @export
gate_int <- function(.dim1,
                     .dim2,
                     .color = NULL,
                     .shape = NULL,
                     .size = NULL,
                     opacity = 1,
                     how_many_gates = 1,
                     .group_by = NULL,
                     gate_list = NULL,
                     ...) {
  UseMethod("gate_int")
}


#' gate_int
#' 
#' @inheritParams gate_chr
#' 
#' @return An integer vector, with 0 for elements outside gates and 1..N for the elements inside the N gates. 
#' 
#' @export
gate_int.numeric = 	function(  .dim1,
                                                   .dim2,
                                                   .color = NULL,
                                                   .shape = NULL,
                                                   .size = NULL,
                                                   opacity = 1,
                                                   how_many_gates = 1,
                                                   .group_by = NULL,
                                                   
                                                   gate_list = NULL,
                                                   ...) {
  
  .gate_chr_int(
    .dim1 = .dim1,
    .dim2 = .dim2,
    .color = .color,
    .shape = .shape,
    .size = .size,
    opacity = opacity,
    how_many_gates = how_many_gates,
    .group_by = .group_by,
    gate_list = gate_list,
    output_type = "int",
    ...
  )
  
}

#' Interactively gate data with a simple scatter plot
#' 
#' Launch an interactive scatter plot, based on the input X and Y coordinates. Points on this plot 
#' can then be gated.
#' 
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @importFrom rlang env
#' @importFrom shiny shinyApp
#' @importFrom shiny runApp
#' @param dimension_x A column symbol representing the X dimension. 
#' @param dimension_y A column symbol representing the Y dimension.
#' @param alpha The opacity of points, with 1 being completely opaque and 0 being completely
#' transparent.
#' @param size The size of points.
#' @return A vector with TRUE for elements inside gate points and FALSE for elements outside gate 
#' points. A record of the selected points is stored in `tidygate_env$select_data` and a record of 
#' the gates is stored in `tidygate_env$brush_data`.
#' @examples
#' \donttest{
#' library(dplyr)
#' library(ggplot2)
#' 
#' mtcars |>
#'   dplyr::mutate(selected = gate_simple(dimension_x = mpg, dimension_y = wt)) |>
#'   print()
#'}
#' @export
gate_simple <-
  
  function(dimension_x, dimension_y, alpha = 1, size = 1) {
    
    message("tidygate says: this feature is in early development and may undergo changes or contain bugs.")
    
    # Add needed columns to input data
    data <- 
      tibble::tibble(dimension_x, dimension_y) |>
      dplyr::mutate(.key = dplyr::row_number()) |>
      dplyr::mutate(.selected = FALSE) |>
      dplyr::mutate(.alpha = alpha) |>
      dplyr::mutate(.size = size)
    
    # Create environment and save input variables
    tidygate_env <<- rlang::env()
    tidygate_env$input_data <- data
    tidygate_env$custom_plot <- NULL
    
    # Launch Shiny App
    app <- shiny::shinyApp(ui, server)
    shiny::runApp(app, port = 1234) # Specify a port if needed
    
    return(tidygate_env$input_data$.selected)
  }

#' Interactively gate data with a custom plot
#' 
#' Launch an interactive scatter plot, based on a user-defined `ggplot2`. Points on this plot can
#' then be gated.
#' 
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom rlang env
#' @importFrom purrr pluck
#' @importFrom ggplot2 ggplot_build
#' @importFrom shiny shinyApp
#' @importFrom shiny runApp
#' @param custom_plot A ggplot object. Must contain a row index in the `.key` column set as key.
#' @return A vector with TRUE for elements inside gate points and FALSE for elements outside gate 
#' points. A record of the selected points is stored in `tidygate_env$select_data` and a 
#' record of the gates is stored in `tidygate_env$brush_data`.
#' @examples
#' \donttest{
#' library(dplyr)
#' library(ggplot2)
#' 
#' scaled_plot <- 
#'   mtcars |> 
#'   mutate(.key = row_number()) |>
#'   ggplot(aes(x = mpg, y = wt, key = .key)) + 
#'   scale_y_log10() +
#'   geom_point() +
#'   theme_dark()
#'   
#' mtcars |>
#'   mutate(selected = gate_interactive(dimension_x = mpg, dimension_y = wt, custom_plot = scaled_plot)) |>
#'   print()
#' }
#' @export
gate_custom <-
  
  function(custom_plot) {
    
    message("tidygate says: this feature is in early development and may undergo changes or contain bugs.")
    
    # Create tibble with .key column
    data <- 
      custom_plot |>
      ggplot2::ggplot_build() |>
      purrr::pluck(1, 1) |>
      tibble::as_tibble() |>
      dplyr::select(key) |>
      dplyr::rename(.key = "key") |>
      dplyr::mutate(.selected = FALSE)
      
    # Create environment and save input variables
    tidygate_env <<- rlang::env()
    tidygate_env$input_data <- data
    tidygate_env$custom_plot <- custom_plot
    
    # Launch Shiny App
    app <- shiny::shinyApp(ui, server)
    shiny::runApp(app, port = 1234)
    
    return(tidygate_env$input_data$.selected)
  }

