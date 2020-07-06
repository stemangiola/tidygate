# 
# 
# test_that("gate dimensions", {
# library(dplyr)
  # res =
  #  tidygate::tidygate_data %>%
  #   mutate(sh = factor(hierarchy)) %>%
  #   gate(
  #     .element = c(`ct 1`, `ct 2`),
  #     Dim1, Dim2,
  #     .color = hierarchy,
  #     .shape = sh,
  #     .size = hierarchy,
  #     how_many_gates = 2
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
    mutate(sh = factor(hierarchy)) %>%
    gate(
      .element = c(`ct 1`, `ct 2`),
      Dim1, Dim2,
      gate_list = tidygate::gate_list
    )
  
  expect_equal(ncol(res) , 10)
  
})