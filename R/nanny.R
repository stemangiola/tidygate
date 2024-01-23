
#' Convert array of quosure (e.g. c(col_a, col_b)) into character vector
#' 
#' @keywords internal
#'
#' @importFrom rlang quo_name
#' @importFrom rlang quo_squash
#'
#' @param v A array of quosures (e.g. c(col_a, col_b))
#'
#' @return A character vector
quo_names <- function(v) {
  
  v = quo_name(quo_squash(v))
  gsub('^c\\(|`|\\)$', '', v) |> 
    strsplit(', ') |> 
    unlist()
}

#' @importFrom magrittr equals
#' @import dplyr
get_specific_annotation_columns = function(.data, .col){
  
  
  # Comply with CRAN NOTES
  . = NULL
  
  # Make col names
  .col = enquo(.col)
  
  # x-annotation df
  n_x = .data |> distinct_at(vars(!!.col)) |> nrow()
  
  # element wise columns
  .data = 
    .data |>
    select(-!!.col) |>
    colnames() |>
    map(
      ~ {
        current_col <- .x
        # Check condition
        if (nrow(.data |> distinct_at(vars(!!.col, current_col))) == n_x) {
          result <- current_col
        } else {
          result <- NULL
        }
        result
      }
    ) 
  
  # Drop null
  .data[lengths(.data) != 0] |> 
    unlist()
  
  
}

#' @import dplyr
#' @import tidyr
#' @importFrom magrittr set_rownames
#' @importFrom rlang quo_is_null
#' @importFrom rlang quo_is_symbolic
.as_matrix = function(.data, rownames = NULL, do_check = TRUE, sep_rownames = "___") {
  
  # Comply with CRAN NOTES
  variable = NULL
  rn = NULL
  
  rownames = enquo(rownames)
  
  # Process data based on conditions
  if (do_check) {
    .data_check = .data
    
    if (!quo_is_null(rownames)) {
      # If rownames are not null, select columns except rownames
      .data_check <- .data_check |> select(-!!rownames)
    }
    
    # Check for non-numeric columns
    if (any(!unique(.data_check |> summarise_all(class) |> gather(variable, class) |> pull(class)) %in% c("numeric", "integer"))) {
      warning("tidygate says: there are NON-numerical columns, the matrix will NOT be numerical")
    }
  }
  
  # Merge columns for rownames if rownames are not null
  if (!quo_is_null(rownames)) {
    .data <- .data |> unite(col = "rn", !!rownames, sep = sep_rownames)
  }
  
  .data = .data |>  as.data.frame()
  
  # Convert to data frame and then to matrix, handling rownames if present
  if (!quo_is_null(rownames)) {
    .data <- .data |> 
      set_rownames(pull(.data, rn)) |> 
      select(-rn)
  } 
  
  .data |> as.matrix()
}


#' Convert array of quosure (e.g. c(col_a, col_b)) into character vector
#' 
#' @keywords internal
#'
#' @importFrom rlang quo_name
#' @importFrom rlang quo_squash
#'
#' @param v A array of quosures (e.g. c(col_a, col_b))
#'
#' @return A character vector
quo_names <- function(v) {
  
  v = quo_name(quo_squash(v))
  gsub('^c\\(|`|\\)$', '', v) |> 
    strsplit(', ') |> 
    unlist()
}