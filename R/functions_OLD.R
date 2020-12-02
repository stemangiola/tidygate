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
#' @param .element A column symbol. The column that is used to calculate distance (i.e., normally genes)
#' @param .dim1 A column symbol. The x dimension
#' @param .dim2 A column symbol. The y dimension
#' @param .color A column symbol. Color of points
#' @param .shape A column symbol. Shape of points
#' @param .size A column symbol. Size of points
#' @param opacity A number between 0 and 1. The opacity level of the data points
#' @param  how_many_gates An integer. The number of gates to label
#' @param name A character string. The name of the new column
#' @param ... Further parameters passed to the function gatepoints::fhs
#'
#' @return A tibble with additional columns
#'
gate_interactive <-
  function(.data,
           .element,
           .dim1,
           .dim2, 
           .color = NULL,
           .shape = NULL,
           .size = NULL,
           opacity = 1,
           how_many_gates = 1,
           name = "gate", ...) {
    
    # Comply with CRAN NOTES
    . = NULL
    value = NULL
    
    # Get column names
    .element = enquo(.element)
    .dim1 = enquo(.dim1)
    .dim2 = enquo(.dim2)
    .color = enquo(.color)
    .shape = enquo(.shape)
    
    
    # Error if elements with coordinates are not unique
    .data %>% check_data_unique(!!.element,  !!.dim1, !!.dim2)
    
    # my df
    my_df = 
      .data %>%
      select(!!.element, get_specific_annotation_columns(.data, !!.element)) %>%
      distinct %>%
      
      # Check if dimensions are NA
      check_dimensions(!!.dim1, !!.dim2) 
    
    my_matrix	=
      my_df %>%
      select(!!.element, !!.dim1, !!.dim2) %>%
      .as_matrix(rownames = !!.element) 
    
    # Add extra space to right of plot area; change clipping to figure
    if(
      quo_is_symbol(.color) | 
      quo_is_symbol(.shape) | 
      (quo_is_symbol(enquo(.size))  && (.data %>% select(!!enquo(.size)) %>% sapply(class) %in% c("numeric", "integer", "double")))
    ){
      # Reset par on exit
      opar <- par(no.readonly =TRUE)  
      on.exit(par(opar)) 
      
      # Set the new par
      par(
        mar=c(5.1, 4.1, 4.1, 8.1),
        xpd=TRUE,
        tck = -.01 # Reduce tick length
      )
    }
    
    
    # Plot
    my_df %>% pretty_plot(
      !!.dim1, 
      !!.dim2,
      .color = !!.color,
      .shape = !!.shape,
      
      # size can be number or column
      .size = .size %>% when(is.null(.size) | class(.) == "numeric" ~ (.), ~ !!enquo(.)),
      
      opacity = opacity
    )
    
    # Loop over gates # Variable needed for recalling the attributes later
    gate_list = map(1:how_many_gates,  ~ my_matrix %>% gatepoints::fhs(mark = TRUE, ...))
    
    
    
    # Return
    gate_list %>%
      
      # Format
      imap( ~ .x %>% format_gatepoints(!!.element, name, .y)) %>%
      purrr::reduce(full_join, by = quo_names(.element)) %>%
      unite(col = !!name,
            contains(name),
            sep = ",",
            na.rm = TRUE) %>%
      
      # Correct column types
      # Keep classes for compatibility
      imap(
        ~ .x %>%
          when(
            .y %in% colnames(my_df) &&
              class(my_df %>% select(.y) %>% pull(1)) == "numeric" ~ as.numeric(.),
            .y %in% colnames(my_df) &&
              class(my_df %>% select(.y) %>% pull(1)) == "integer" ~ as.integer(.),
            .y %in% colnames(my_df) &&
              class(my_df %>% select(.y) %>% pull(1)) == "logical" ~ as.logical(.),
            .y %in% colnames(my_df) &&
              class(my_df %>% select(.y) %>% pull(1)) == "factor" ~ as.factor(.),
            ~ (.)
          )
      ) %>%
      do.call(bind_cols, .) %>%
      
      # Join with the dataset
      right_join(my_df %>% select(!!.element),
                 by = quo_names(.element)) %>%
      
      # Replace NAs
      mutate(!!name := replace_na(!!as.symbol(name), 0)) %>%
      
      # Add internals the list of gates
      add_attr(map(gate_list, ~ attr(.x, "gate")), "gate") 
    
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
#' @param .element A column symbol. The column that is used to calculate distance (i.e., normally genes)
#' @param .dim1 A column symbol. The x dimension
#' @param .dim2 A column symbol. The y dimension
#' @param gate_list A list of gates. Each element of the list is a data frame with x and y columns. Each row is a coordinate. The order matter.
#' @param name A character string. The name of the new column
#' @param ... Further parameters passed to the function gatepoints::fhs
#'
#' @return A tibble with additional columns
#'
gate_programmatic <- 
  function(.data,
           .element,
           .dim1,
           .dim2, 
           gate_list,
           name = "gate") {
    
    # Comply with CRAN NOTES
    . = NULL
    value = NULL
    
    # Get column names
    .element = enquo(.element)
    .dim1 = enquo(.dim1)
    .dim2 = enquo(.dim2)
    
    # Error if elements with coordinates are not unique
    .data %>% check_data_unique(!!.element,  !!.dim1, !!.dim2)
    
    # my df
    my_df = 
      .data %>%
      select(!!.element, get_specific_annotation_columns(.data, !!.element)) %>%
      distinct %>%
      
      # Check if dimensions are NA
      check_dimensions(!!.dim1, !!.dim2) 
    
    my_matrix	=
      my_df %>%
      select(!!.element, !!.dim1, !!.dim2) %>%
      .as_matrix(rownames = !!.element) 
    
    # Loop over gates # Variable needed for recalling the attributes later
    gate_list_result = map(
      gate_list,  
      ~ my_matrix[applyGate(my_matrix,.x),] %>%
        rownames() %>% 
        
        # Avoid error for empty gates
        when(!is.null(.) ~ (.) %>% add_attr(.x, "gate") )
    )
    
    # Return
    gate_list_result %>%
      
      # Format
      imap( ~ .x %>% format_gatepoints(!!.element, name, .y)) %>%
      purrr::reduce(full_join, by = quo_names(.element)) %>%
      unite(col = !!name,
            contains(name),
            sep = ",",
            na.rm = TRUE) %>%
      
      # Correct column types
      # Keep classes for compatibility
      imap(
        ~ .x %>%
          when(
            .y %in% colnames(my_df) &&
              class(my_df %>% select(.y) %>% pull(1)) == "numeric" ~ as.numeric(.),
            .y %in% colnames(my_df) &&
              class(my_df %>% select(.y) %>% pull(1)) == "integer" ~ as.integer(.),
            .y %in% colnames(my_df) &&
              class(my_df %>% select(.y) %>% pull(1)) == "logical" ~ as.logical(.),
            .y %in% colnames(my_df) &&
              class(my_df %>% select(.y) %>% pull(1)) == "factor" ~ as.factor(.),
            ~ (.)
          )
      ) %>%
      do.call(bind_cols, .) %>%
      
      # Join with the dataset
      right_join(my_df %>% select(!!.element),
                 by = quo_names(.element)) %>%
      
      # Replace NAs
      mutate(!!name := replace_na(!!as.symbol(name), 0)) %>%
      
      # Add internals the list of gates
      add_attr(gate_list, "gate") 
    
  }