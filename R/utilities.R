check_data_unique = function(.data,
                             .element,
                             .dim1,
                             .dim2) {
  # Get column names
  .element = enquo(.element)
  .dim1 = enquo(.dim1)
  .dim2 = enquo(.dim2)
  
  if (.data %>%
      select(!!.element,!!.dim1,!!.dim2) %>%
      distinct %>%
      
      # Count
      group_by_at(vars(!!.element,!!.dim1,!!.dim2)) %>%
      tally() %>%
      ungroup() %>%
      
      # Check
      pull(n) %>%
      max %>%
      `>` (1))
  stop(sprintf(
    "tidygate says: %s must be unique for each row for the calculation",
    quo_names(.element)
  ))
}


reattach_internals = function(.data,
                              .data_internals_from = NULL,
                              .name = "gate") {
  if (.data_internals_from %>% is.null)
    .data_internals_from = .data
  
  .data %>% add_attr(.data_internals_from %>% attr(.name), .name)
}

#' Add attribute to abject
#'
#' @keywords internal
#'
#'
#' @param var A tibble
#' @param attribute An object
#' @param name A character name of the attribute
#'
#' @return A tibble with an additional attribute
add_attr = function(var, attribute, name) {
  attr(var, name) <- attribute
  var
}

check_dimensions = function(.data, .dim1, .dim2) {
  .dim1 = enquo(.dim1)
  .dim2 = enquo(.dim2)
  
  .data %>%
    when(# If NAs in dimensions
      (.) %>%
        filter(!!.dim1 %>% is.na | !!.dim2 %>% is.na) %>%
        nrow() %>%
        `>` (0) ~ {
          warning(
            "tidygate says: you have some elements with non-valid dimensions. Those elements points will be filtered out"
          )
          (.) %>%
            filter(!!.dim1 %>% is.na | !!.dim2 %>% is.na) %>%
            capture.output() %>%
            paste0(collapse = "\n") %>%
            message()
          
          # Return
          (.) %>%	filter(!(!!.dim1 %>% is.na | !!.dim2 %>% is.na))
        },
      
      # Otherwise
      ~ (.))
}

format_gatepoints = function(.data, .element, name, .idx) {
  # Comply CRAN check
  value = NULL
  
  # Column name
  .element = enquo(.element)
  
  .data %>%
    as.character %>%
    as_tibble() %>%
    
    # Reconstitute columns
    separate(value, quo_names(.element), sep = "___") %>%
    
    mutate(!!as.symbol(sprintf("%s%s", name, .idx)) := .idx)
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

pretty_plot = function(.data,
                       .dim1,
                       .dim2,
                       .color = NULL,
                       .shape = NULL,
                       .size = NULL,
                       opacity = 1) {
  # Comply with CRAN NOTES
  . = NULL
  
  # Get column names
  .dim1 = enquo(.dim1)
  .dim2 = enquo(.dim2)
  .color = enquo(.color)
  .shape = enquo(.shape)
  
  my_size_range = c(1, 3)
  
  
  
  .data_formatted =
    .data %>%
    
    # Define COLOR
    when(
      # If continuous
      quo_is_symbol(.color) &&
        (.) %>%
        select(!!.color) %>%
        sapply(class) %in% c("numeric", "integer", "double") ~ {
          order_ = findInterval(pull(., !!.color), sort(pull(., !!.color)))
          (.) %>% mutate(.color = grDevices::colorRampPalette(viridis(n = 5))(n())[order_])
        },
      
      # If discrete
      quo_is_symbol(.color) ~ {
        how_many_colors = .data %>% distinct(!!.color) %>% nrow
        (.) %>%
          mutate(.color =
                   grDevices::colorRampPalette(RColorBrewer::brewer.pal(min(
                     9, how_many_colors
                   ), "Set1"))(how_many_colors)[factor(!!.color)])
      },
      
      # If not defined
      ~ (.) %>% mutate(.color = "grey25")
    ) %>%
    
    # Define SIZE
    when(
      # If not defined
      is.null(.size) ~ (.)  %>% mutate(.size = 2),
      
      # If it is a number and not a column name
      class(.size) == "numeric" ~ (.)  %>% mutate(.size := !!.size),
      
      # If continuous
      quo_is_symbol(enquo(.size)) &&
        (.) %>%
        select(!!enquo(.size)) %>%
        sapply(class) %in% c("numeric", "integer", "double") ~ 	(.) %>%	mutate(.size := !!enquo(.size) %>% rescale(to = my_size_range)),
      
      # If discrete
      quo_is_symbol(enquo(.size)) ~ {
        warning("tidygate says: .size has to be a continuous variable. .size has been ignored")
        (.) %>% mutate(.size = 2)
      },
      
      ~ stop(
        "tidygate says: the parameter .size must be NULL, numeric or a symbolic column name"
      )
    ) %>%
    
    # Define SHAPE
    when(
      # If continuous
      quo_is_symbol(.shape) &
        (.) %>%
        select(!!.shape) %>%
        sapply(class) %in% c("numeric", "integer", "double") ~ {
          warning("tidygate says: .shape has to be a discrete variable. .shape has been ignored")
          (.) %>% mutate(.shape = 19)
        }	,
      
      # If discrete
      quo_is_symbol(.shape) ~ (.) %>%	mutate(.shape := c(19, 17, 15, 18, 3, 4, 8, 10, 5)[factor(!!.shape)]),
      
      # If not defined
      ~ (.)  %>% mutate(.shape = 19)
    )
  
  # Plot
  .data_formatted %>%
    {
      plot(
        (.) %>% pull(!!.dim1),
        (.) %>% pull(!!.dim2),
        xlim = range((.) %>% pull(!!.dim1)),
        ylim = range((.) %>% pull(!!.dim2)),
        bty = 'l',
        pch = (.) %>% pull(.shape),
        cex = (.) %>% pull(.size),
        col = (.) %>% pull(.color) %>% alpha(opacity),
        xlab = quo_names(.dim1) %>% paste(collapse = " "),
        ylab = quo_names(.dim2) %>% paste(collapse = " "),
        xaxt = 'n',
        yaxt = 'n'
      )
    }
  
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
  if (quo_is_symbol(.color)) {
    legend(
      "topleft",
      inset = c(1.05, inset_y),
      legend = distinct(.data_formatted,!!.color) %>% pull(!!.color),
      pch = 19,
      col = distinct(.data_formatted,!!.color, .color) %>% pull(.color),
      title = color_title,
      box.col = "white",
      xjust = 0
    )
    inset_y = inset_y + distinct(.data_formatted,!!.color, .color) %>% nrow %>% magrittr::multiply_by(.1)
  }
  if (quo_is_symbol(enquo(.size)) &&
      (.data %>% select(!!enquo(.size)) %>% sapply(class) %in% c("numeric", "integer", "double"))) {
    legend(
      "topleft",
      inset = c(1.05, inset_y),
      legend = distinct(.data_formatted,!!enquo(.size)) %>% pull(!!enquo(.size)) %>% range,
      pch = 19,
      col = "black",
      pt.cex = my_size_range,
      title = size_title,
      box.col = "white",
      xjust = 0
    )
    inset_y = inset_y + 0.3
  }
  if (quo_is_symbol(.shape)) {
    legend(
      "topleft",
      inset = c(1.05, inset_y),
      legend = distinct(.data_formatted,!!.shape) %>% pull(!!.shape),
      pch = distinct(.data_formatted,!!.shape, .shape) %>% pull(.shape),
      col = "black",
      title = shape_title,
      box.col = "white",
      yjust = 0
    )
    inset_y = inset_y + distinct(.data_formatted,!!.shape, .shape) %>% nrow %>% magrittr::multiply_by(.1)
  }
  
  
  
}


