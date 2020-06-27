#' Label points within a scatter plot drawing a gate
#'
#' \lifecycle{maturing}
#'
#' @description gate() takes as input a `tbl` formatted as | <DIMENSION 1> | <DIMENSION 2> | <...> | and calculates the rotated dimensional space of the feature value.
#'
#' @importFrom rlang enquo
#' @importFrom magrittr "%>%"
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
#' @param  how_many_gates An integer. The number of gates to label
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
#' \dontrun{
#' 
#'  mtcars_tidy_MDS = reduce_dimensions(mtcars_tidy, car_model, feature, value, method="MDS")
#'  
#'  gate(mtcars_tidy_MDS, car_model, `Dim1`, `Dim2`)
#'  
#' }
#'
#' @docType methods
#' @rdname gate-methods
#' @export
#'
setGeneric("gate", function(.data,
																			 .element,
																			 .dim1,
																			 .dim2, 
																			 .color = NULL,
																			 .shape = NULL,
																			 .size = NULL,
																			 how_many_gates = 1,
																			 name = "inside_gate",
																			 action =	"add", ...)
	standardGeneric("gate"))

# Set internal
.gate = 		function(.data,
                              .element,
                              .dim1,
                              .dim2, 
                              .color = NULL,
                              .shape = NULL,
                              .size = NULL,
                              how_many_gates = 1,
                              name = "inside_gate",
                              action =	"add", ...)
{
	
	# Get column names
	.element = enquo(.element)
	.dim1 = enquo(.dim1)
	.dim2 = enquo(.dim2)
	.color = enquo(.color)
	.shape = enquo(.shape)
	.size = enquo(.size)
	
	.data_processed =
		
		.data %>% 

		# Run calculation
		gate_(
			.element = !!.element,
			.dim1 = !!.dim1,
			.dim2 = !!.dim2,
			.color = !!.color,
			.shape = !!.shape,
			.size = !!.size,
			how_many_gates = how_many_gates,
			name = name,
			...
		)
	
	if (action == "add"){
		
		.data %>%
			dplyr::left_join(	.data_processed,	by = quo_names(.element)	) 
		
	}
	else if (action == "get"){
		
		.data %>%
			
			# Selecting the right columns
			select(
				!!.element,
				get_specific_annotation_columns(.data, !!.element)
			) %>%
			distinct() %>%
			
			dplyr::left_join(	.data_processed,	by = quo_names(.element)	) 
		
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
#' @return A tbl object with additional columns for the reduced dimensions. additional columns for the rotated dimensions. The rotated dimensions will be added to the original data set as `<NAME OF DIMENSION> rotated <ANGLE>` by default, or as specified in the input arguments.
setMethod("gate", "spec_tbl_df", .gate)

#' gate
#' @docType methods
#' @rdname gate-methods
#' @return A tbl object with additional columns for the reduced dimensions. additional columns for the rotated dimensions. The rotated dimensions will be added to the original data set as `<NAME OF DIMENSION> rotated <ANGLE>` by default, or as specified in the input arguments.
setMethod("gate", "tbl_df", .gate)

