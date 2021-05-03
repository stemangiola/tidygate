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







