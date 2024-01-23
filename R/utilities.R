# Negation
not = function(is){	!is }

check_data_unique = function(.data, .element, .dim1, .dim2) {
  # Get column names as quosures
  .element = enquo(.element)
  .dim1 = enquo(.dim1)
  .dim2 = enquo(.dim2)
  
  # Check if the combination of .element, .dim1, and .dim2 is unique for each row
  if (
    # Select the specified columns
    .data |> 
    select(!!.element, !!.dim1, !!.dim2) |> 
    
    # Remove duplicate rows
    distinct() |>                         
    
    # Group by these columns
    group_by_at(vars(!!.element, !!.dim1, !!.dim2)) |> 
    
    # Count the number of rows in each group
    tally() |>                            
    
    # Remove the grouping
    ungroup() |>                          
    
    # Extract the count column
    pull(n) |>                            
    
    # Check if the maximum count is greater than 1
    max() > 1
  ) {
    
    # If duplicates are found, stop the function and return an error message
    stop(sprintf(
      "tidygate says: %s must be unique for each row for the calculation",
      quo_names(.element)
    ))
  }
}




reattach_internals = function(.data,
                              .data_internals_from = NULL,
                              .name = "gate") {
  if (.data_internals_from |> is.null())
    .data_internals_from = .data
  
  .data |> add_attr(.data_internals_from |> attr(.name), .name)
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

#' @importFrom utils capture.output
check_dimensions = function(.data, .dim1, .dim2) {
  .dim1 = enquo(.dim1)
  .dim2 = enquo(.dim2)
  
  # Check if there are NAs in dimensions
  if (
    .data |>
    filter( is.na(!!.dim1) | is.na(!!.dim2)) |>
    nrow() > 0
  ) {
    warning("tidygate says: you have some elements with non-valid dimensions. Those elements points will be filtered out")
    
    # Capture and message the filtered data
    filtered_data <- .data |>
      filter(is.na(!!.dim1) | is.na(!!.dim2))
    capture_output <- capture.output(print(filtered_data)) |> 
      paste0(collapse = "\n") |> 
      message()
    
    # Filter out non-valid elements
    .data <- .data |> filter(!is.na(!!.dim1) & !is.na(!!.dim2))
  }
  
  return(.data)
}


format_gatepoints = function(.data, .element, name, .idx) {
  # Comply CRAN check
  value = NULL
  
  # Column name
  .element = enquo(.element)
  
  .data |>
    as.character() |>
    as_tibble() |>
    
    # Reconstitute columns
    separate(value, quo_names(.element), sep = "___") |>
    
    mutate(!!as.symbol(sprintf("%s%s", name, .idx)) := .idx)
}





