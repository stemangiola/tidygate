#' DEPRECATED - use gate_chr, gate_int instead inside a dplyr::mutate() statement
#'
#' \lifecycle{maturing}
#'
#' @keywords internal
#' 
#' @description gate() takes as input a `tbl` formatted as | <DIMENSION 1> | <DIMENSION 2> | <...> | and calculates the rotated dimensional space of the feature value.
#'
#' @importFrom rlang enquo
#' @importFrom magrittr "%>%"
#' @importFrom lifecycle deprecate_warn
#'
#' @name gate
#'
#'
#' @param .data A tibble
#' @param .element A column symbol. The column that is used to calculate distance (i.e., normally genes)
#' @param .dim1 A column symbol. The x dimension
#' @param .dim2 A column symbol. The y dimension
#' @param .color A column symbol. Color of points
#' @param .shape A column symbol. Shape of points
#' @param .size A column symbol. Size of points
#' @param opacity A number between 0 and 1. The opacity level of the data points
#' @param how_many_gates An integer. The number of gates to label
#' @param gate_list A list of gates. It is returned by gate function as attribute \"gate\". If you want to create this list yourself, each element of the list is a data frame with x and y columns. Each row is a coordinate. The order matter.
#' @param name A character string. The name of the new column
#' @param action A character string. Whether to join the new information to the input tbl (add), or just get the non-redundant tbl with the new information (get).
#' @param ... Further parameters passed to the function gatepoints::fhs
#'
#' @details This function allow the user to label data points in inside one or more 2D gates. This package is based on on the package gatepoints.
#'
#' @return A tbl object with additional columns for the inside gate information. additional columns for the rotated dimensions. The rotated dimensions will be added to the original data set as `<NAME OF DIMENSION> rotated <ANGLE>` by default, or as specified in the input arguments.
#'
#'
#' @examples
#'
#' \donttest{
#' 
#'   if(interactive()){
#' 
#'     tidygate::tidygate_data %>%
#'     gate( .element = c(`ct 1`, `ct 2`), Dim1, Dim2 )
#'
#'   }
#' 
#' }
#' 
#' library(magrittr)
#' 
#' tidygate::tidygate_data  %>%
#'  gate(
#'    .element = c(`ct 1`, `ct 2`),
#'    Dim1, Dim2,
#'    gate_list = tidygate::gate_list
#'  )
#'
#' @docType methods
#' @rdname gate-methods
#' @export
#'
#'
#'
#'
gate <- function(.data,
                 .element,
                 .dim1,
                 .dim2, 
                 .color = NULL,
                 .shape = NULL,
                 .size = NULL,
                 opacity = 1,
                 how_many_gates = 1,
                 gate_list = NULL,
                 name = "gate",
                 action =	"add", ...) {
  UseMethod("gate")
}



# Set internal
.gate = 		function(.data,
                   .element,
                   .dim1,
                   .dim2, 
                   .color = NULL,
                   .shape = NULL,
                   .size = NULL,
                   opacity = 1,
                   how_many_gates = 1,				
                   gate_list = NULL,
                   name = "gate",
                   action =	"add", ...)
{
  
  deprecate_warn("0.3.0", "gate()", "gate_chr()")
  
  # Get column names
  .element = enquo(.element)
  .dim1 = enquo(.dim1)
  .dim2 = enquo(.dim2)
  .color = enquo(.color)
  .shape = enquo(.shape)
  
  .data_processed =
    
    .data %>% 
    when(
      
      # Interactive
      is.null(gate_list) ~ (.) %>% 
        gate_interactive(
          .element = !!.element,
          .dim1 = !!.dim1,
          .dim2 = !!.dim2,
          .color = !!.color,
          .shape = !!.shape,
          
          # size can be number of column
          .size =  .size %>% when(is.null(.size) | is(., "numeric") ~ (.), ~ !!enquo(.)),
          
          opacity = opacity,
          how_many_gates = how_many_gates,
          name = name,
          ...
        ),
      
      # Programmatic
      is.list(gate_list) ~ (.) %>% 
        gate_programmatic(
          .element = !!.element,
          .dim1 = !!.dim1,
          .dim2 = !!.dim2,
          gate_list = gate_list,
          name = name
        ),
      
      # Else error
      ~ stop("tidygate says: the parameter gate_list has to be NULL or a list of data frames")
    )
  
  
  if (action == "add"){
    
    .data %>%
      dplyr::left_join(	.data_processed,	by = quo_names(.element)	) %>%
      
      # Reattach internals
      reattach_internals(.data_processed)
    
  }
  else if (action == "get"){
    
    .data %>%
      
      # Selecting the right columns
      select(
        !!.element,
        get_specific_annotation_columns(.data, !!.element)
      ) %>%
      distinct() %>%
      
      dplyr::left_join(	.data_processed,	by = quo_names(.element)	) %>%
      
      # Reattach internals
      reattach_internals(.data_processed)
    
  }
  else if (action == "only") .data_processed
  else
    stop(
      "tidygate says: action must be either \"add\" for adding this information to your data frame or \"get\" to just get the information"
    )
}

#' gate
#' @docType methods
#' @rdname gate-methods
#' @export
#' @return A tbl object with additional columns for the reduced dimensions. additional columns for the rotated dimensions. The rotated dimensions will be added to the original data set as `<NAME OF DIMENSION> rotated <ANGLE>` by default, or as specified in the input arguments.
gate.spec_tbl_df = gate.tbl_df = .gate

#' gate
#' @docType methods
#' @rdname gate-methods
#' @export
#' @return A tbl object with additional columns for the reduced dimensions. additional columns for the rotated dimensions. The rotated dimensions will be added to the original data set as `<NAME OF DIMENSION> rotated <ANGLE>` by default, or as specified in the input arguments.
gate.tbl_df = .gate