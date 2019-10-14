context("Function output check")
library(RGPR)

# devtools::test()

data("frenkeLine00")
x <- frenkeLine00


test_that("envelope > OK", {
  expect_true(max(envelope(x)) <=  max(abs(x)))
  expect_true(max(envelope(x)) >=  0)
})

# getGainSEC

test_that("gainSEC > OK", {
  expect_equal(as.matrix(gainSEC(x)), as.matrix(x * getGainSEC(x)))
})

