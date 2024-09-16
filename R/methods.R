# Define otherwise undefined global variables for R CMD check
utils::globalVariables(c(".key", ".rs.invokeShinyPaneViewer"))

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
#' @examples
#'
#' # Standard use - interactive
#' 
#' if(interactive()){
#'
#'   tidygate::tidygate_data  %>%
#'     distinct(`ct 1` , `ct 2`, Dim1, Dim2) %>%
#'     mutate(gate = gate_chr( Dim1, Dim2)) 
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
  lifecycle::deprecate_warn("1.0.0", "tidygate::gate_chr()", with = "tidygate::gate()")
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
  lifecycle::deprecate_warn("1.0.0", "tidygate::gate_int()", with = "tidygate::gate()")
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
#' @importFrom utils globalVariables
#' @importFrom tibble tibble
#' @importFrom dplyr mutate
#' @importFrom rlang env
#' @importFrom rlang quo_is_null
#' @importFrom rlang quo_is_symbol
#' @importFrom rlang quo_name
#' @importFrom rlang eval_tidy
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
#' @param colour A single colour code string compatible with ggplot2. Or, a vector representing the 
#' point colour.
#' @param shape A single ggplot2 shape numeric ranging from 0 to 127. Or, a vector representing the 
#' point shape, coercible to a factor of 6 or less levels.
#' @param alpha A single ggplot2 alpha numeric ranging from 0 to 1. Or, a vector representing the 
#' point alpha, either a numeric or factor of 6 or less levels.
#' @param size A single ggplot2 size numeric ranging from 0 to 20. Or, a vector representing the 
#' point size, either a numeric or factor of 6 or less levels.
#' @return A vector of strings, of the gates each X and Y coordinate pair is within. If gates are
#' drawn interactively, they are temporarily saved to `tidygate_env$gates`
gate_interactive <-
  function(x, y, colour = NULL, shape = NULL, alpha = 1, size = 2) {
    
    # Check input values are valid
    if (!rlang::quo_is_null(shape)) {
      if (rlang::quo_is_symbol(shape)) {
        shape_factor_length <- 
          shape |>
          rlang::eval_tidy() |>
          as.factor() |>
          levels() |>
          length()
        if (shape_factor_length > 6) {
          stop("tidygate says: shape factor level count exceeds the limit of 6") 
        }
      } else {
        if (rlang::eval_tidy(shape) < 0 | rlang::eval_tidy(shape) > 127)
          stop("tidygate says: shape numeric outside of the range 0 to 127") 
        }      
      }

    if (!rlang::quo_is_null(alpha)) {
      if (rlang::quo_is_symbol(alpha)) {
        if (is.factor(rlang::eval_tidy(alpha))) {
          alpha_factor_length <- 
            alpha |>
            rlang::eval_tidy() |>
            as.factor() |>
            levels() |>
            length()
          if (alpha_factor_length > 6) {
            stop("tidygate says: alpha factor level count exceeds the limit of 6") 
          }
        }
      } else {
        if (rlang::eval_tidy(alpha) < 0 | rlang::eval_tidy(alpha) > 1)
          stop("tidygate says: alpha numeric outside of the range 0 to 1") 
        }      
      }

    if (!rlang::quo_is_null(size)) {
      if (rlang::quo_is_symbol(size)) {
        if (is.factor(rlang::eval_tidy(size))) {
          size_factor_length <- 
            size |>
            rlang::eval_tidy() |>
            as.factor() |>
            levels() |>
            length()
          if (size_factor_length > 6) {
            stop("tidygate says: size factor level count exceeds the limit of 6") 
          }
        }
      } else {
        if (rlang::eval_tidy(size) < 0 | rlang::eval_tidy(size) > 20)
          stop("tidygate says: size numeric outside of the range 0 to 20") 
        }      
    }
    
    # Create basic input data tibble
    data <-
      tibble::tibble(x = rlang::eval_tidy(x), y = rlang::eval_tidy(y)) |>
      dplyr::mutate(.key = dplyr::row_number())
  
    # Create basic plot
    plot <-
      data |>
      ggplot2::ggplot(ggplot2::aes(x = x, y = y, key = .key)) +
      ggplot2::geom_point() +
      ggplot2::labs(x = rlang::quo_name(x), y = rlang::quo_name(y)) +
      theme_bw()

    # Add colour 
    # Set colour to equal column value if provided
    if (!rlang::quo_is_null(colour)) {
      
      if (rlang::quo_is_symbol(colour)) { 
        plot <- 
          plot + 
          ggplot2::aes(colour = !!colour) +
          ggplot2::scale_colour_distiller(palette="Spectral")
        
      # Set to equal constant if not a column symbol and remove legend
      } else { 
        plot <- 
          plot + 
          ggplot2::aes(colour = !!colour) +
          ggplot2::scale_colour_manual(values = rlang::eval_tidy(colour)) +
          ggplot2::guides(colour = "none")
      }
    }
    
    # Add shape 
    if (!rlang::quo_is_null(shape)) {
      if (rlang::quo_is_symbol(shape)) {
        plot <- 
          plot + 
          ggplot2::aes(shape = as.factor(!!shape)) +
          ggplot2::guides(shape = ggplot2::guide_legend(title = rlang::quo_name(shape)))
      } else {
        plot <- 
          plot + 
          ggplot2::aes(shape = as.factor(!!shape)) +
          ggplot2::scale_shape_manual(values = rlang::eval_tidy(shape)) +
          ggplot2::guides(shape = "none")
      }
    }
    
    # Add alpha
    if (!rlang::quo_is_null(alpha)) {
      if (rlang::quo_is_symbol(alpha)) {
        plot <- 
          plot + 
          ggplot2::aes(alpha = !!alpha) +
          ggplot2::guides(alpha = ggplot2::guide_legend(title = rlang::quo_name(alpha)))
      } else {
        plot <- 
          plot + 
          ggplot2::aes(alpha = "fixed_alpha") +
          ggplot2::scale_alpha_manual(values = rlang::eval_tidy(alpha)) +
          ggplot2::guides(alpha = "none")
      }
    }
    
    # Add size
    if (!rlang::quo_is_null(size)) {
      if (rlang::quo_is_symbol(size)) {
        plot <- 
          plot + 
          ggplot2::aes(size = !!size) +
          ggplot2::guides(size = ggplot2::guide_legend(title = rlang::quo_name(size)))
      } else {
        plot <- 
          plot + 
          ggplot2::aes(size = "fixed_size") +
          ggplot2::scale_size_manual(values = rlang::eval_tidy(size)) +
          ggplot2::guides(size = "none")
      }
    }
    
    # Create environment and save input variables
    tidygate_env <<- rlang::env()
    tidygate_env$input_data <- data
    tidygate_env$input_plot <- plot
    tidygate_env$event_count <- 1
    
    # Set RStudio Viewer as browser if in RStudio
    if (.Platform$GUI == "Rstudio") {
      options(shiny.launch.browser = .rs.invokeShinyPaneViewer)
    }
    
    # Launch Shiny App
    app <- shiny::shinyApp(ui, server)
    gate_vector <- 
      shiny::runApp(app, port = 1234) |> 
      purrr::map_chr(~ .x |> paste(collapse = ",")) |>
      purrr::map_chr(~ ifelse(.x == "", NA, .x))
    
    message("tidygate says: interactively drawn gates are temporarily saved to tidygate_env$gates")
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
#' @param programmatic_gates A `data.frame` of the gate brush data, as saved in 
#' `tidygate_env$gates`. The column `x` records X coordinates, the column `y` records Y 
#' coordinates and the column `.gate` records the gate number.  
#' @return A vector of strings, of the gates each X and Y coordinate pair is within. 
gate_programmatic <-
  function(x, y, programmatic_gates) {

    # Format input
    data <- 
      data.frame(x, y) |>
      as.matrix()
    
    # Loop over gates
    gate_vector <-
      programmatic_gates |>
      data.frame() |>
      split(programmatic_gates$.gate) |>
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
    gate_vector <- ifelse(gate_vector == "0", NA, gate_vector)
    
    return(gate_vector)
  }

#' Gate points
#' 
#' @description
#' Gate points based on their X and Y coordinates. By default, this function launches an interactive
#' scatter plot. Colour, shape, size and alpha can be defined as constant values, or can be 
#' controlled by the values of a specified column. 
#' 
#' If previously drawn gates are supplied to the `programmatic_gates` argument, points will be gated 
#' programmatically. This feature allows the reproduction of previously drawn interactive gates.
#' Programmatic gating is based on the package gatepoints by Wajid Jawaid. 
#' 
#' @param x A vector representing the X dimension. 
#' @param y A vector representing the Y dimension.
#' @param colour A single colour code string compatible with ggplot2. Or, a vector representing the 
#' point colour.
#' @param shape A single ggplot2 shape numeric ranging from 0 to 127. Or, a vector representing the 
#' point shape, coercible to a factor of 6 or less levels.
#' @param alpha A single ggplot2 alpha numeric ranging from 0 to 1. Or, a vector representing the 
#' point alpha, either a numeric or factor of 6 or less levels.
#' @param size A single ggplot2 size numeric ranging from 0 to 20. Or, a vector representing the 
#' point size, either a numeric or factor of 6 or less levels.
#' @param programmatic_gates A `data.frame` of the gate brush data, as saved in 
#' `tidygate_env$gates`. The column `x` records X coordinates, the column `y` records Y coordinates and the column `.gate` 
#' records the gate number. When this argument is supplied, gates will be drawn programmatically.
#' @return A vector of strings, of the gates each X and Y coordinate pair is within. If gates are
#' drawn interactively, they are temporarily saved to `tidygate_env$gates`.
#' @examples 
#' library(dplyr)
#' data("demo_gate_data", package = "tidygate")
#' 
#' # Gate points interactively
#' if(interactive()) {
#'   mtcars |>
#'     mutate(gated = gate(x = mpg, y = wt, shape = am))
#' }
#' 
#' # Gate points programmatically
#' mtcars |>
#'   mutate(gated = gate(x = mpg, y = wt, programmatic_gates = demo_gate_data))
#' @export
gate <-
  function(x, y, colour = NULL, shape = NULL, alpha = 1, size = 2, programmatic_gates = NULL) {
    
    if (is.null(programmatic_gates)) {
      gate_interactive(x = enquo(x), y = enquo(y), colour = enquo(colour), shape = enquo(shape), 
                       alpha = enquo(alpha), size = enquo(size))
    } else {
      gate_programmatic(x = x, y = y, programmatic_gates = programmatic_gates)
    }
  }
    