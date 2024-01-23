parse_gate_list = function(.data, my_df){
  
  # Comply with CRAN NOTES
  point = NULL
  
  .data |>
    imap( ~ tibble(gate = .y, point = .x)) |>
    reduce(.f = full_join, by = "point") |>
    
    # Add all points
    full_join(tibble(point = as.character(1:nrow(my_df))), by = "point") |>
    arrange(as.numeric(point)) |>
    
    # Unite in case of a point belonging to multiple gates
    tidyr::unite(contains("gate"),
                 col = "gate",
                 sep = ",",
                 na.rm = TRUE) |>
    
    # Replace NAs
    mutate(gate := if_else(gate == "", "0", gate)) |>
    
    # Pull
    pull(gate)
}

#' @importFrom rlang quo_is_symbol
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom viridis viridis
#' @import dplyr
#' @import tidyr
#' @importFrom scales rescale
#' @importFrom rlang :=
#' @importFrom graphics axis
#' @importFrom graphics legend
#' @importFrom graphics par
#' @importFrom utils head
#' @importFrom stringr str_pad
#' @importFrom scales alpha

pretty_plot_chr_int = function(.data,
                               .dim1,
                               .dim2,
                               .color = NULL,
                               .shape = NULL,
                               .size = NULL,
                               opacity = 1,
                               is_size_fixed) {
  # Comply with CRAN NOTES
  . = NULL
  color_hexadecimal = NULL
  
  # Get column names
  .dim1 = enquo(.dim1)
  .dim2 = enquo(.dim2)
  .color = enquo(.color)
  .shape = enquo(.shape)
  .size = enquo(.size)
  my_size_range = c(1, 3)
  
  
  .data_formatted <- .data
  
  # Define COLOR
  # If not defined
  if (pull(.data_formatted, !!.color) |> unique() |> is.na() |> all()) {
    .data_formatted <- .data_formatted |> mutate(.color = "grey25", color_hexadecimal = "#3B3B3B")
  }
  # If continuous
  else if (quo_is_symbol(.color) && select(.data_formatted, !!.color) |> sapply(class) |> `%in%`(c("numeric", "integer", "double")) |> any()) {
    order_ <- .data_formatted |> pull(!!.color) |> sort() |> findInterval(pull(.data_formatted, !!.color))
    .data_formatted <- .data_formatted |> mutate(color_hexadecimal = grDevices::colorRampPalette(viridis(n = 5))(max(order_))[order_])
  }
  # If discrete
  else if (quo_is_symbol(.color)) {
    how_many_colors <- .data_formatted |> distinct(!!.color) |> nrow()
    .data_formatted <- .data_formatted |> mutate(color_hexadecimal =
                                                   grDevices::colorRampPalette(RColorBrewer::brewer.pal(min(9, how_many_colors), "Set1"))(how_many_colors)[factor(pull(.data_formatted, !!.color))])
  }
  
  # Define SIZE
  # If not defined
  if (pull(.data_formatted, !!.size) |> unique() |> is.na() |> all()) {
    .data_formatted <- .data_formatted |> mutate(.size = 2)
  }
  # If it is a number and not a column name
  else if (exists("is_size_fixed") && is_size_fixed) {
    .data_formatted <- .data_formatted |> mutate(.size = !!.size)
  }
  # If continuous
  else if (quo_is_symbol(enquo(.size)) && select(.data_formatted, !!enquo(.size)) |> sapply(class) |> `%in%`(c("numeric", "integer", "double")) |> any()) {
    .data_formatted <- .data_formatted |> mutate(.size = pull(., !!enquo(.size)) |> rescale(to = my_size_range))
  }
  # If discrete
  else if (quo_is_symbol(enquo(.size))) {
    warning("tidygate says: .size has to be a continuous variable. .size has been ignored")
    .data_formatted <- .data_formatted |> mutate(.size = 2)
  }
  else {
    stop("tidygate says: the parameter .size must be NULL, numeric or a symbolic column name")
  }
  
  # Define SHAPE
  # If not defined
  if (pull(.data_formatted, !!.shape) |> unique() |> is.na() |> all()) {
    .data_formatted <- .data_formatted |> mutate(.shape = 19)
  }
  # If continuous
  else if (quo_is_symbol(.shape) && select(.data_formatted, !!.shape) |> sapply(class) |> `%in%`(c("numeric", "integer", "double")) |> any()) {
    warning("tidygate says: .shape has to be a discrete variable. .shape has been ignored")
    .data_formatted <- .data_formatted |> mutate(.shape = 19)
  }
  # If discrete
  else if (quo_is_symbol(.shape)) {
    .data_formatted <- .data_formatted |> mutate(.shape = c(19, 17, 15, 18, 3, 4, 8, 10, 5)[factor(pull(., !!.shape))])
  }
  
  
  # Plot
  plot(
        .data_formatted |> pull(!!.dim1),
        .data_formatted |> pull(!!.dim2),
        xlim = range(.data_formatted |> pull(!!.dim1)),
        ylim = range(.data_formatted |> pull(!!.dim2)),
        bty = 'l',
        pch = .data_formatted |> pull(.shape),
        cex = .data_formatted |> pull(.size),
        col = .data_formatted |> pull(color_hexadecimal) |> alpha(opacity),
        xlab = quo_names(.dim1) |> paste(collapse = " "),
        ylab = quo_names(.dim2) |> paste(collapse = " "),
        xaxt = 'n',
        yaxt = 'n'
      )

  
  axis(1, tck = 1,  col.ticks = "light gray")
  axis(1,
       tck = -0.015,
       col.ticks = "black",
       labels = FALSE)
  axis(
    2,
    tck = 1,
    col.ticks = "light gray",
    lwd.ticks = "1",
    las = 1
  )
  axis(
    2,
    tck = -0.015,
    col.ticks = "black",
    las = 1,
    labels = FALSE
  )
  
  # Max length of the legends titles
  color_title = quo_name(.color)
  shape_title = quo_name(.shape)
  size_title = quo_name(.size)
  max_length_titles = max(nchar(color_title), nchar(shape_title), nchar(size_title))
  color_title = stringr::str_pad(color_title, width = max_length_titles, side = "both")
  shape_title = stringr::str_pad(shape_title, width = max_length_titles, side = "both")
  size_title = stringr::str_pad(size_title, width = max_length_titles, side = "both")
  
  # Add legend to top right, outside plot region
  inset_y = 0
  
  # Check if the 'color' column in .data is not all NA (missing values)
  if (pull(.data, !!.color) |> unique() |> is.na() |> not() |> all()) {
    
    # Get the class/type of the 'color' column in .data_formatted
    color_class <- pull(.data_formatted, !!.color) |> class()
    
    # If the 'color' column is of numeric, integer, or double type (continuous data)
    if (color_class %in% c("numeric", "integer", "double")) {
      # Create a legend for continuous color data
      color_legend <- .data_formatted |> 
        arrange(!!.color) |> 
        slice(1, n()) |> 
        pull(!!.color) |> 
        round(digits = 4)
      color_col <- .data_formatted |> 
        arrange(!!.color) |> 
        slice(1, n()) |> 
        pull(color_hexadecimal)
    } else {
      # Create a legend for non-continuous (discrete) color data
      color_legend <- .data_formatted |> 
        arrange(!!.color) |> 
        distinct(!!.color) |> 
        pull(!!.color)
      color_col <- .data_formatted |> 
        arrange(!!.color) |> 
        distinct(color_hexadecimal) |> 
        pull(color_hexadecimal)
    }
    
    # Add the color legend to the plot
    legend(
      "topleft",
      inset = c(1.05, inset_y),
      legend = color_legend,
      pch = 19,
      col = color_col,
      title = color_title,
      box.col = "white",
      xjust = 0
    )
    
    # Adjust the position for the next legend
    inset_y <- inset_y + (.data_formatted |> distinct(!!.color, .color) |> nrow() * 0.1)
  }
  
  # Check if the 'size' column in .data is not all NA and is numeric
  if (pull(.data, !!.size) |> unique() |> is.na() |> not() |> all() &&
      (.data |> select(!!enquo(.size)) |> sapply(class) |> `%in%`(c("numeric", "integer", "double")) |> any())) {
    # Create a legend for the size variable
    size_legend <- .data_formatted |> 
      distinct(!!enquo(.size)) |> 
      pull(!!enquo(.size)) |> 
      range()
    
    # Add the size legend to the plot
    legend(
      "topleft",
      inset = c(1.05, inset_y),
      legend = size_legend,
      pch = 19,
      col = "black",
      pt.cex = my_size_range,
      title = size_title,
      box.col = "white",
      xjust = 0
    )
    
    # Adjust the position for the next legend
    inset_y <- inset_y + 0.3
  }
  
  # Check if the 'shape' column in .data is not all NA
  if (pull(.data, !!.shape) |> unique() |> is.na() |> not() |> all()) {
    # Create a legend for the shape variable
    shape_legend <- .data |> distinct(!!.shape) |> pull(!!.shape)
    shape_pch <- .data_formatted |> distinct(!!.shape, .shape) |> pull(.shape)
    
    # Add the shape legend to the plot
    legend(
      "topleft",
      inset = c(1.05, inset_y),
      legend = shape_legend,
      pch = shape_pch,
      col = "black",
      title = shape_title,
      box.col = "white",
      yjust = 0
    )
    
    # Adjust the position for the next legend
    inset_y <- inset_y + (.data_formatted |> distinct(!!.shape, .shape) |> nrow() * 0.1)
  }
  
  
  
}

#' Get points within a user drawn gate
#'
#' @keywords internal
#'
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @importFrom graphics plot
#' @importFrom purrr imap
#' @importFrom purrr map
#' @importFrom purrr reduce
#' @importFrom rlang quo_is_symbol
#'
#' @param .data A tibble
#' @param .dim1 A column symbol. The x dimension
#' @param .dim2 A column symbol. The y dimension
#' @param .color A column symbol. Color of points
#' @param .shape A column symbol. Shape of points
#' @param .size A column symbol. Size of points
#' @param opacity A number between 0 and 1. The opacity level of the data points
#' @param how_many_gates An integer. The number of gates to label
#' @param is_size_fixed A logical value indicating if the size of points is fixed, or it is a column name.
#' @param ... Further parameters passed to the function gatepoints::fhs
#'
#' @return A tibble with additional columns
#'
gate_interactive_chr_int <-
  function(.data,
           .dim1,
           .dim2,
           .color = NA,
           .shape = NULL,
           .size = NULL,
           opacity = 1,
           how_many_gates = 1,
           is_size_fixed,
           ...) {
    # Comply with CRAN NOTES
    . = NULL
    value = NULL
    
    # Get column names
    .dim1 = enquo(.dim1)
    .dim2 = enquo(.dim2)
    .color = enquo(.color)
    .shape = enquo(.shape)
    .size = enquo(.size)
    
    name = "gate"
    
    # Check if dimensions are NA and create my_df
    my_df <- .data |>
      check_dimensions(!!.dim1, !!.dim2)
    
    # Create my_matrix from my_df
    my_matrix <- my_df |>
      select(!!.dim1, !!.dim2) |>
      as.matrix()
    
    # Add extra space to right of plot area; change clipping to figure
    if ((pull(.data, !!.color) |> unique() |> is.na() |> not() |> all()) |
        (pull(.data, !!.shape) |> unique() |> is.na() |> not() |> all()) |
        (pull(.data, !!.size) |> unique() |> is.na() |> not() |> all() &&
         (.data |> select(!!.size) |> sapply(class) |> `%in%`(c("numeric", "integer", "double")) |> any()))) {
      # Reset par on exit
      opar <- par(no.readonly = TRUE)
      on.exit(par(opar))
      
      # Set the new par
      par(mar = c(5.1, 4.1, 4.1, 8.1),
          xpd = TRUE,
          tck = -.01) # Reduce tick length
    }
    
    # Plot
    my_df |> 
      pretty_plot_chr_int(
        !!.dim1, !!.dim2,
        .color = !!.color,
        .shape = !!.shape,
        .size = !!.size,
        opacity = opacity,
        is_size_fixed = is_size_fixed
      )
    
    # Loop over gates and create gate_list
    gate_list <- map(1:how_many_gates,
                     ~ my_matrix |> fhs(mark = TRUE, ...))
    
    # Save gate list
    temp_file <- sprintf("%s.rds", tempfile())
    message(sprintf("tidygate says: the gates have been saved in %s", temp_file))
    gate_list |> 
      map(~ attr(.x, "gate")) |>
      saveRDS(temp_file)
    
    # Return the parsed gate list
    gate_list |> parse_gate_list(my_df)
  }


#' Get points within a user drawn gate
#'
#' @keywords internal
#'
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @importFrom graphics plot
#' @importFrom purrr imap
#' @importFrom purrr map
#'
#' @param .data A tibble
#' @param .dim1 A column symbol. The x dimension
#' @param .dim2 A column symbol. The y dimension
#' @param gate_list A list of gates. Each element of the list is a data frame with x and y columns. Each row is a coordinate. The order matter.
#'
#' @return A tibble with additional columns
#'
gate_programmatic_chr_int <-
  function(.data,
           .dim1,
           .dim2,
           gate_list
           ) {
    # Comply with CRAN NOTES
    . = NULL
    value = NULL
    
    # Get column names
    .dim1 = enquo(.dim1)
    .dim2 = enquo(.dim2)
    name = "gate"
    
    # my df
    my_df =
      .data |>
      
      # Check if dimensions are NA
      check_dimensions(!!.dim1,!!.dim2)
    
    my_matrix	=
      my_df |>
      select(!!.dim1,!!.dim2) |>
      .as_matrix()
    
    # Loop over gates # Variable needed for recalling the attributes later
    # Loop over gates
    gate_list_result <- map(gate_list, function(.x) {
      # Check the class of the item
      if ("data.frame" %in% class(.x)) {
        result <- .as_matrix(.x)
      } else {
        result <- .x
      }
      
      # Apply gate and convert to character
      result <- applyGate(my_matrix, result) |>
        which() |>
        as.character()
      
      # Avoid error for empty gates
      if (!is.null(result)) {
        result <- result |>
          add_attr(.x, "gate")
      }
      
      return(result)
    })
    
    
    # Return
    gate_list_result |>
      
      parse_gate_list(my_df)
  
    
  }


.gate_chr_int = 		function(.dim1,
                           .dim2,
                           .color = NULL,
                           .shape = NULL,
                           .size = NULL,
                           opacity = 1,
                           how_many_gates = 1,
                           .group_by = NULL,
                           gate_list = NULL,
                           output_type = "chr",
                           ...)
{
  
  # Comply with CRAN NOTES
  . = NULL
  point = NULL
  
  # If gouping is complex multicolumn
  if (!is.null(.group_by)) {
    .group_by <- .group_by |> 
      matrix(ncol = length(.group_by) / length(.dim1)) |> 
      as.data.frame() |> 
      unite(".group_by", everything(), sep = "___") |> 
      pull(.group_by)
  }
  
    
  # Create tibble
  input_df = 
    tibble(
    .dim1 = .dim1,
    .dim2 = .dim2
  ) |> 
    mutate(.color = NA, .shape = NA, .size = NA)
  
  if(!is.null(.color)) input_df = input_df |> mutate(., .color = !!.color)
  if(!is.null(.shape)) input_df = input_df |> mutate(., .shape = !!.shape)
  if(!is.null(.size)) input_df = input_df |> mutate(., .size = !!.size)
  if(!is.null(.group_by)) input_df = input_df |> mutate(., .group_by = !!.group_by)
  
  
  if (!is.null(.group_by)) {
    unique_df <- input_df |> 
      nest(data___ = .group_by)
  } else {
    unique_df <- input_df
  }
  
  # Apply 'distinct()' outside of the if-else block
  unique_df <- unique_df |> distinct()
  
  # Interactive
  if(is.null(gate_list))
  result_vector = 
    unique_df |> 
    gate_interactive_chr_int(
      .dim1 = .dim1,
      .dim2 = .dim2,
      .color = .color,
      .shape = .shape,
      
      # size can be number of column
      .size =  .size,
      
      opacity = opacity,
      how_many_gates = how_many_gates,
      is_size_fixed = 
        class(.size) %in%  c("numeric", "integer", "double") &
        length(.size) < length(.dim1) &
        length(.size) == 1,
      ...
    )
  
  # Programmatic
  else if(is.list(gate_list))
    result_vector = 
    unique_df |> 
    gate_programmatic_chr_int(
      .dim1 = .dim1,
      .dim2 = .dim2,
      gate_list = gate_list
    )
  
  else 
    stop(
      "tidygate says: the parameter gate_list has to be NULL or a list of data frames"
    )
  
  # Convert
  if(output_type == "chr") result_vector = result_vector |> as.character()
  else if(output_type == "int") result_vector = result_vector |> as.integer()

  # Integrate maintaining order in case of nesting
  input_df |>
    left_join(
      unique_df |>
        mutate(gate = result_vector)
    ) |> 
    suppressMessages() |> 
    pull(gate)
}