

# test_that("gate dimensions", {
# 
#   res =
#     reduce_dimensions(tidygate::test_data2 , c(`ct 1`, `ct 2`), cancer_ID, relation, method="MDS") %>%
#     gate_dimensions(.element = c(`ct 1`, `ct 2`), Dim1, Dim2 )
#   expect_equal(ncol(res) , 10)
# 
# })
