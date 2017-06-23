library(utilitiesAdamExample)
library(datasets)

context('Testig aggregating columns')

DT <- as.data.table(mtcars)

DT_aggr <- aggregate_cols(  DT
                            , functions = list(sum)
                            , variables_aggregation = c('cyl')
                            , variables_function = c('disp'))

test_that("Statistics are preserved", {
  
    expect_equal(  DT[, sum2(disp)]
               , DT_aggr[, sum2(disp)])
})
