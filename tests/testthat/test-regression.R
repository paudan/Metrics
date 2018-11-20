context('Regression metrics.')

test_that('bias is calculated correctly', {
    expect_equal(bias(1, 1), 0)
    expect_equal(bias(c(-1, -100, 17.5), c(0, 0, 0)), mean(c(-1, -100, 17.5)))
})

test_that('percent_bias is calculated correctly', {
    expect_equal(percent_bias(c(1, 2, 3), c(1, 3, 2)), mean(c(0, -1/2, 1/3)))
    expect_equal(percent_bias(c(1, 2, 0), c(1, 2, 1)), -Inf)
    expect_equal(percent_bias(0, 0), NaN)
    expect_equal(percent_bias(c(-1.1, 1.1), c(-1, 1)), 0)
})

test_that('squared error is calculated correctly', {
    expect_equal(se(3.4, 4.4), 1)
    expect_equal(se(9:11, 11:9), c(4, 0, 4))
})

test_that('sum of squared errors is calculated correctly', {
    expect_equal(sse(c(1, 3, 2), c(2, 3, 4)), 5)
})

test_that('mean squared error is calculated correctly', {
    expect_equal(mse(1:4, c(2, 3, 4, 4)), 3 / 4)
})

test_that('root mean squared error is calculated correctly', {
    expect_equal(rmse(1:4, c(1, 2, 3, 5)), sqrt(1 / 4))
    expect_equal(rmse(1:4, 1:4), 0)
})

test_that('absolute error is calculated correctly', {
    expect_equal(ae(3.4, 4.4), 1)
    expect_equal(ae(9:11, 11:9), c(2, 0, 2))
})

test_that('mean absolute error is calculated correctly', {
    expect_equal(mae(1:4, c(1, 2, 3, 5)), 0.25)
})

test_that('median absolute error is calculated correctly', {
    expect_equal(mdae(1:4, c(1, 2, 4, 50)), 0.5)
})

test_that('absolute percent error is calculated correctly', {
    expect_equal(ape(0:3, 1:4), c(Inf, 1, 1/2, 1/3))
    expect_equal(ape(0:2, c(0, 0, 0)), c(NaN, 1, 1))
    expect_equal(ape(c(-1.1, 1.1), c(-1, 1)), c(1 / 11, 1 / 11))
})

test_that('mean absolute percent error is calculated correctly', {
    expect_equal(mape(1:3, 2:4), mean(c(1, 1/2, 1/3)))
    expect_equal(mape(c(-1.1, 1.1), c(-1, 1)), 1 / 11)
})

test_that('symmetric mean absolute percent error is calculated correctly', {
    expect_equal(smape(0, 0), NaN)
    expect_equal(smape(1, -1), 2)
    expect_equal(smape(1, 0), 2)
    expect_equal(smape(c(1, 2, 3), c(2, 5, 4)), smape(c(2, 5, 4), c(1, 2, 3)))
})

test_that('squared log error is calculated correctly', {
    expect_equal(sle(c(0, 1, 3.4), c(1, 0, 3.4)), c(log(2), log(2), 0) ^ 2)
    expect_equal(sle(exp(2) - 1,exp(1) - 1), 1)
})

test_that('mean squared log error is calculated correctly', {
    expect_equal(msle(c(1, 2, exp(1) - 1),c(1, 2, exp(2)-1)), 1 / 3)
})

test_that('root mean squared log error is calculated correctly', {
    expect_equal(rmsle(c(exp(5) - 1), c(exp(1) - 1)), 4)
})

test_that('relative absolute error is calculated correctly', {
    expect_equal(rae(0:10, 30:40), 11)
    expect_equal(rae(seq(0,2,0.5), seq(0,2,0.5)), 0.0)
    expect_equal(rae(1:4, c(1,2,3,5)), 0.25)
})

test_that('root relative squared error is calculated correctly', {
    expect_equal(rrse(0:10, 2:12), sqrt(0.4))
    expect_equal(rrse(seq(0,2,0.5), seq(0,2,0.5)), 0.0)
    expect_equal(rrse(1:4, c(1,2,3,5)), sqrt(0.2))
})

test_that('relative squared error is calculated correctly', {
    expect_equal(rse(0:10, 2:12), 0.4)
    expect_equal(rse(seq(0,2,0.5), seq(0,2,0.5)), 0.0)
    expect_equal(rse(1:4, c(1,2,3,5)), 0.2)
})


test_that('explained variation is calculated correctly', {
  expect_equal(explained_variation(0:10, 2:12), 0.6)
  expect_equal(explained_variation(seq(0,2,0.5), seq(0,2,0.5)), 1.0)
  expect_equal(explained_variation(1:4, c(1,2,3,5)), 0.8)
})

## Tests with na.rm setting

test_that('bias is calculated correctly with na.rm = T', {
  expect_equal(bias(c(1, NA), c(1, NA), na.rm = T), 0)
  expect_equal(bias(c(NA, 1), c(1, NA), na.rm = T), NaN)
  expect_equal(bias(c(-1, -100, 17.5, NA), c(0, 0, 0, NA), na.rm = T), mean(c(-1, -100, 17.5)))
})

test_that('percent_bias is calculated correctly with na.rm = T', {
  expect_equal(percent_bias(c(1, 2, 3, NA, 6), c(1, 3, 2, 4, NA), na.rm = T), mean(c(0, -1/2, 1/3)))
  expect_equal(percent_bias(c(1, 2, 0, NA, 6), c(1, 2, 1, 6, NA), na.rm = T), -Inf)
  expect_equal(percent_bias(c(NA, 0), c(NA, 0), na.rm = T), NaN)
  expect_equal(percent_bias(c(0, NA), c(0, NA), na.rm = T), NaN)
  expect_equal(percent_bias(c(-1.1, 1.1, NA), c(-1, 1, 0), na.rm = T), 0)
})

test_that('squared error is calculated correctly with na.rm = T', {
  expect_equal(se(9:11, c(11:9, NA, NA), na.rm = T), c(4, 0, 4))
  expect_equal(se(c(9:11, NA, NA), 11:9, na.rm = T), c(4, 0, 4))
})

test_that('sum of squared errors is calculated correctly with na.rm = T', {
  expect_equal(sse(c(1, 3, 2, NA, 2), c(2, 3, 4, 2, NA), na.rm = T), 5)
  expect_equal(sse(c(1, 3, 2, NA, NA), c(2, 3, 4, 2, NA), na.rm = T), 5)
})

test_that('mean squared error is calculated correctly with na.rm = T', {
  expect_equal(mse(c(1:4, NA, 3), c(2, 3, 4, 4, 3, NA), na.rm = T), 3 / 4)
  expect_equal(mse(c(1:4, NA, NA), c(2, 3, 4, 4, NA, NA), na.rm = T), 3 / 4)
})

test_that('root mean squared error is calculated correctly with na.rm = T', {
  expect_equal(rmse(c(1:4, NA, 3), c(1, 2, 3, 5, NA, NA), na.rm = T), sqrt(1 / 4))
  expect_equal(rmse(c(1:4, NA, NA, NA), c(1:4, 3, 2, 6), na.rm = T), 0)
})

test_that('absolute error is calculated correctly with na.rm = T', {
  expect_equal(ae(c(9:11, 3, NA, 3, NA), c(11:9, NA, NA, NA, 3), na.rm = T), c(2, 0, 2))
})

test_that('mean absolute error is calculated correctly with na.rm = T', {
  expect_equal(mae(c(1:4, 3, 65), c(1, 2, 3, 5, NA, NA), na.rm = T), 0.25)
})

test_that('median absolute error is calculated correctly with na.rm = T', {
  expect_equal(mdae(c(1:4, 3, 6, NA, 3), c(1, 2, 4, 50, NA, NA, 3, NA), na.rm = T), 0.5)
})

test_that('absolute percent error is calculated correctly with na.rm = T', {
  expect_equal(ape(c(0:3, NA), c(1:4, 6), na.rm = T), c(Inf, 1, 1/2, 1/3))
  expect_equal(ape(c(0:2, NaN), c(0, 0, 0, NA), na.rm = T), c(NaN, 1, 1))
  expect_equal(ape(c(-1.1, 1.1, NaN, NaN), c(-1, 1, NA, NA), na.rm = T), c(1 / 11, 1 / 11))
})

test_that('mean absolute percent error is calculated correctly with na.rm = T', {
  expect_equal(mape(c(1:3, NA), c(2:4, NA), na.rm = T), mean(c(1, 1/2, 1/3)))
  expect_equal(mape(c(-1.1, 1.1, 3, 6), c(-1, 1, NA, NA), na.rm = T), 1 / 11)
})

test_that('symmetric mean absolute percent error is calculated correctly with na.rm = T', {
  expect_equal(smape(0, 0), NaN)
  expect_equal(smape(c(1, NA, NA), c(-1, 3, 6), na.rm = T), 2)
  expect_equal(smape(c(1, NA, 3), c(0, 3, NaN), na.rm = T), 2)
  expect_equal(smape(c(1, 2, 3, NA, 3, NA, 3), c(2, 5, 4, 6, NaN, 9, NaN), na.rm = T),
               smape(c(2, 5, 4, NA, 78, NA, 45), c(1, 2, 3, 6, NaN, 9, NaN), na.rm = T))
})

test_that('squared log error is calculated correctly with na.rm = T', {
  expect_equal(sle(c(0, 1, 3.4, NA), c(1, 0, 3.4, NA), na.rm = T), c(log(2), log(2), 0) ^ 2)
  expect_equal(sle(c(exp(2) - 1, NA),c(exp(1) - 1, NA), na.rm = T), 1)
})

test_that('mean squared log error is calculated correctly with na.rm = T', {
  expect_equal(msle(c(1, 2, exp(1) - 1, 6),c(1, 2, exp(2)-1, NA), na.rm = T), 1 / 3)
})

test_that('root mean squared log error is calculated correctly with na.rm = T', {
  expect_equal(rmsle(c(exp(5) - 1, NA), c(exp(1) - 1, NA), na.rm = T), 4)
})

test_that('relative absolute error is calculated correctly with na.rm = T', {
  expect_equal(rae(c(0:10, 6, NA), c(30:40, NA, 5), na.rm = T), 11)
})

test_that('root relative squared error is calculated correctly with na.rm = T', {
  expect_equal(rrse(c(0:10, NA), c(2:12, 6), na.rm = T), sqrt(0.4))
})

test_that('relative squared error is calculated correctly with na.rm = T', {
  expect_equal(rse(c(0:10, NA), c(2:12, 6), na.rm = T), 0.4)
})


test_that('explained variation is calculated correctly with na.rm = T', {
  expect_equal(explained_variation(c(0:10, NA), c(2:12, 3), na.rm = T), 0.6)
})

