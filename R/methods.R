#' Label points within a scatter plot drawing a gate
#'
#' \lifecycle{maturing}
#'
#' @description gate() takes as input a `tbl` formatted as | <DIMENSION 1> | <DIMENSION 2> | <...> | and calculates the rotated dimensional space of the feature value.
#'
#' @importFrom rlang enquo
#' @importFrom rlang quo_is_null
#' @importFrom magrittr "%>%"
#' @importFrom lifecycle deprecate_warn
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
#' \dontrun{
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
  lifecycle::deprecate_warn("1.0.0", "tidygate::gate_chr()")
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
#' @importFrom lifecycle deprecate_warn
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
  lifecycle::deprecate_warn("1.0.0", "tidygate::gate_int()")
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
#' Create an interactive scatter plot based on user-defined X and Y coordinates. Colour, shape, size 
#' and alpha can be defined as constant values, or can be controlled by values in a specified 
#' column. 
#' 
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @importFrom rlang env
#' @importFrom rlang quo_is_null
#' @importFrom rlang quo_is_symbol
#' @importFrom purrr map_chr
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 scale_shape_manual
#' @importFrom ggplot2 scale_alpha_manual
#' @importFrom ggplot2 scale_shape_manual
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 theme
#' @importFrom shiny shinyApp
#' @importFrom shiny runApp
#' 
#' @param x A vector representing the X dimension. 
#' @param y A vector representing the Y dimension.
#' @param colour A vector representing the point colour. Or, a colour code string compatible
#' with ggplot2.
#' @param shape A vector representing the point shape, coercible to a factor. Or, a shape code 
#' numeric compatible with ggplot2 (0-127)
#' @param alpha A vector representing the point alpha, coercible to a factor. Or, an alpha numeric 
#' compatible with ggplot2 (0-1). 
#' @param size A vector representing the point size, coercible to a factor. Or, a size numeric 
#' compatible with ggplot2 (0-20).
#' @return A vector of strings, of the gates each X and Y coordinate pair is within. A record 
#' of the selected points is stored in `tidygate_env$select_data` and a record of the gates is 
#' stored in `tidygate_env$brush_data`.
#' @examples
#' \dontrun{
#' mtcars |>
#'   mutate(selected = gate_interactive(x = mpg, y = wt, shape = am))
#'}
#' @export
gate_interactive <-
  
  function(x, y, colour = NULL, shape = NULL, alpha = 1, size = 2) {

    x_name <- quo_name(enquo(x))
    y_name <- quo_name(enquo(y))
    colour_quo <- enquo(colour)
    shape_quo <- enquo(shape)
    alpha_quo <- enquo(alpha)
    size_quo <- enquo(size)
    
    # Fix CRAN note
    .key <- NULL
    
    # Create basic input data tibble
    data <-
      tibble::tibble(x, y) |>
      dplyr::mutate(.key = dplyr::row_number())

    # Create basic plot
    plot <-
      data |>
      ggplot2::ggplot(ggplot2::aes(x = x, y = y, key = .key)) +
      ggplot2::geom_point() +
      ggplot2::labs(x = x_name, y = y_name) +
      theme_bw()
    
    # Add colour 
    if (!rlang::quo_is_null(colour_quo)) {
      plot <- plot + ggplot2::aes(colour = !!colour_quo) # Set aesthetic
      if (!rlang::quo_is_symbol(colour_quo)) { # Set to equal constant if not a column symbol
        colour_fixed <- rlang::eval_tidy(colour_quo)
        plot <- 
          plot + 
          ggplot2::scale_colour_manual(values = colour_fixed) +
          ggplot2::guides(colour = FALSE)
      }
    }
    
    # Add shape 
    if (!rlang::quo_is_null(shape_quo)) {
      plot <- plot + ggplot2::aes(shape = as.factor(!!shape_quo))
      if (!rlang::quo_is_symbol(shape_quo)) {
        fixed_shape <- rlang::eval_tidy(shape_quo)
        plot <- 
          plot + 
          ggplot2::scale_shape_manual(values = fixed_shape) +
          ggplot2::guides(shape = FALSE)
      }
    }
    
    # Add alpha
    if (!rlang::quo_is_null(alpha_quo)) {
      plot <- plot + ggplot2::aes(alpha = as.factor(!!alpha_quo))
      if (!rlang::quo_is_symbol(alpha_quo)) {
        alpha_fixed <- rlang::eval_tidy(alpha_quo)
        plot <- 
          plot + 
          ggplot2::scale_alpha_manual(values = alpha_fixed) +
          ggplot2::guides(alpha = FALSE)
      }
    }
    
    # Add size
    if (!rlang::quo_is_null(size_quo)) {
      plot <- plot + ggplot2::aes(size = as.factor(!!size_quo))
      if (!rlang::quo_is_symbol(size_quo)) {
        size_fixed <- rlang::eval_tidy(size_quo)
        plot <- 
          plot + 
          ggplot2::scale_size_manual(values = size_fixed) +
          ggplot2::guides(size = FALSE)
      }
    }
    
    # Create environment and save input variables
    tidygate_env <<- rlang::env()
    tidygate_env$input_data <- data
    tidygate_env$input_plot <- plot
    tidygate_env$event_count <- 1
    
    # Launch Shiny App
    app <- shiny::shinyApp(ui, server)
    gate_vector <- 
      shiny::runApp(app, port = 1234) |> 
      purrr::map_chr(~ .x |> paste(collapse = ","))
    
    return(gate_vector)
  }

#' Programmatically gate data with pre-recorded lasso selection coordinates
#'
#' A helpful way to repeat previous interactive lasso selections to enable reproducibility. 
#' Programmatic gating is based on the package [gatepoints](https://github.com/wjawaid/gatepoints)
#' by Wajid Jawaid. 
#'
#' @importFrom purrr map
#' @importFrom purrr when
#' @importFrom purrr pluck
#' @param x A vector representing the X dimension. 
#' @param y A vector representing the Y dimension.
#' @param brush_data A `data.frame` of the gate brush data, as saved in `tidygate_env$brush_data`. 
#' The column `x` records X coordinates, the column `y` records Y coordinates and the column `.gate` 
#' records the gate number.  
#' @return A vector of strings, of the gates each X and Y coordinate pair is within. 
#' @export
gate_programmatic <-
  function(x, y, brush_data) {

    # Format input
    data <- 
      data.frame(x, y) |>
      as.matrix()
    
    # Loop over gates
    gate_vector <-
      brush_data |>
      data.frame() |>
      split(brush_data$.gate) |>
      purrr::map(
        ~ .x %>%
          purrr::when("data.frame" %in% class(.) ~ .as_matrix(.), ~ (.)) %>%
          applyGate(data, .) %>%
          which() %>%
          as.character() %>%
          
          # Avoid error for empty gates
          purrr::when(!is.null(.) ~ (.) %>% add_attr(.x, "gate"))
      ) |>
      
      parse_gate_list(data)
      
    # Format output
    gate_vector <- ifelse(gate_vector == "0", "", gate_vector)
    
    return(gate_vector)
  }
