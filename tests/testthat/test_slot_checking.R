context("Slot checking")
library(RGPR)

# devtools::test()

data("frenkeLine00")
x <- frenkeLine00

y <- x
y2 <- x
vel(y) <- NULL
y@antsep <- numeric(0)
antsep(y2) <- numeric(0)

test_that("timeCorOffset > empty/uncorrect slots", {
  expect_error(timeCorOffset(y))
  expect_error(timeCorOffset(y2))
})



