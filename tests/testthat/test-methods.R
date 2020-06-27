

# test_that("gate dimensions", {
# library(dplyr)
#   res =
#     nanny::reduce_dimensions(tidygate::test_data2 , c(`ct 1`, `ct 2`), cancer_ID, relation, method="MDS")  %>%
#     mutate(sh = factor(hierarchy)) %>%
#     gate(
#       .element = c(`ct 1`, `ct 2`),
#       Dim1, Dim2,
#       .color = hierarchy,
#       .shape = sh,
#       .size = hierarchy
#     )
#   expect_equal(ncol(res) , 10)
# 
# })
