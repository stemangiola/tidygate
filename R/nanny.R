
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
  gsub('^c\\(|`|\\)$', '', v) %>% 
    strsplit(', ') %>% 
    unlist 
}

#' @importFrom magrittr equals
#' @import dplyr
get_specific_annotation_columns = function(.data, .col){
  
  
  # Comply with CRAN NOTES
  . = NULL
  
  # Make col names
  .col = enquo(.col)
  
  # x-annotation df
  n_x = .data %>% distinct_at(vars(!!.col)) %>% nrow
  
  # element wise columns
  cols = .data %>%
    select(-!!.col) %>%
    colnames %>%
    map(~ {
      if (.data %>%
            distinct_at(vars(!!.col, .x)) %>%
            nrow %>%
            equals(n_x)) {
        .x
      } else {
        NULL
      }
    })
  
  # Drop NULL
  cols[lengths(cols) != 0] %>%
    unlist
  
}

#' @import dplyr
#' @import tidyr
#' @importFrom magrittr set_rownames
#' @importFrom rlang quo_is_null
#' @importFrom rlang quo_is_symbolic
.as_matrix = function(.data,
                      rownames = NULL,
                      do_check = TRUE,
                      sep_rownames = "___") {
  
  # Comply with CRAN NOTES
  variable = NULL
  
  rownames = enquo(rownames)
  
  
  out = .data
  
  # Through warning if data frame is not numerical beside the rownames column (if present)
  if (do_check) {
    check_df = out
    if (!quo_is_null(rownames)) {
      check_df = check_df %>% select(-!!rownames)
    }
    if (check_df %>%
          dplyr::summarise_all(class) %>%
          tidyr::gather(variable, class) %>%
          pull(class) %>%
          unique() %>%
          `%in%`(c("numeric", "integer")) %>% `!`() %>% any()) {
      warning("tidygate says: there are NON-numerical columns, the matrix will NOT be numerical")
    }
  }
  
  # If rownames multiple enquo (e.g., c(col1, col2)) merge them
  if (!quo_is_null(rownames)) {
    out = out %>% unite(col = "rn", !!rownames, sep = sep_rownames)
  }
  
  out = out %>% as.data.frame()
  
  # Deal with rownames column if present
  if (!quo_is_null(rownames)) {
    out = out %>%
      set_rownames(out %>% pull(rn)) %>%
      select(-rn)
  }
  
  # Convert to matrix
  as.matrix(out)
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
  gsub('^c\\(|`|\\)$', '', v) %>% 
    strsplit(', ') %>% 
    unlist 
}