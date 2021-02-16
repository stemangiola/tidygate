# 
# 
# test_that("gate dimensions", {
# library(dplyr)
# res =
# tidygate::tidygate_data %>%
#  mutate(sh = factor(hierarchy)) %>%
#   mutate(
#     gate =  gate_chr(
#       Dim1, Dim2,
#       .color = hierarchy,
#       .shape = sh,
#       .size = hierarchy,
#       how_many_gates = 2
#     )
#   )

#   
#   res2 =
#     tidygate::tidygate_data  %>%
#     mutate(sh = factor(hierarchy)) %>%
#     gate(
#       .element = c(`ct 1`, `ct 2`),
#       Dim1, Dim2,
#     gate_list = attr(res, "gate")
#     )
#   
#   identical(res, res2)
# 
# })

test_that("gate dimensions", {
  library(dplyr)

  res =
    tidygate::tidygate_data  %>%
    distinct(`ct 1` , `ct 2`, Dim1, Dim2) %>%
    mutate(gate = gate_chr( Dim1, Dim2,gate_list = tidygate::gate_list))
    
  
  expect_equal(ncol(res) , 5)
  
  
  res =
    tidygate::tidygate_data  %>%
    distinct(`ct 1` , `ct 2`, Dim1, Dim2) %>%
    mutate(gate = gate_int(Dim1, Dim2, gate_list = tidygate::gate_list))
  
  expect_equal(ncol(res) , 5)
  
})

test_that("gate grouping", {
  library(dplyr)
  
  res_distinct =
    tidygate::tidygate_data  %>%
    distinct(`ct 1` , `ct 2`, Dim1, Dim2) %>%
    mutate(gate = gate_chr( Dim1, Dim2,gate_list = tidygate::gate_list)) 
  
  res =
    tidygate::tidygate_data  %>%
    mutate(gate = gate_chr( Dim1, Dim2, .group_by = c(`ct 1` , `ct 2`), gate_list = tidygate::gate_list)) %>%
    distinct(`ct 1` , `ct 2`, Dim1, Dim2, gate)
  
  
  expect_equal(res_distinct$gate, res$gate)
  
})

# test_that("gate DEPRECATED", {
#   library(dplyr)
#   
#   data <- mutate(tidygate_data, sh = factor(hierarchy))
#   lifecycle::expect_deprecated(
#     gate(
#       data, 
#       .element = c(`ct 1`, `ct 2`), 
#       Dim1, 
#       Dim2, 
#       gate_list = gate_list
#     )
#   )
# })
