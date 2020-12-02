# Negation
not = function(is){	!is }

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





