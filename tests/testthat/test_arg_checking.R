context("Argument + @proc checking")
library(RGPR)

# devtools::test()

data("frenkeLine00")
x <- frenkeLine00

#------------------------------------------------------------------------------#

test_that("dcshift > OK", {
  expect_true(class(dcshift(x)) == "GPR")
  expect_true(class(dcshift(x, u = 1:100, FUN = mean)) == "GPR")
  expect_length(dcshift(x, u = 1:100, FUN = mean)@proc, 1)
  expect_length(dcshift(x, u = 1:100, FUN = mean, na.rm = TRUE, track = FALSE)@proc, 0)
})

test_that("dcshift > uncorrect arguments", {
  expect_error(dcshift(x, u = 1:10000, FUN = mean))
  expect_error(dcshift(x, u = 1:100, FUN = "mean"))
  expect_error(dcshift(x, u = -1:100, FUN = "mean"))
  expect_error(dcshift(x, u = -1:100, FUN = NULL))
})

#------------------------------------------------------------------------------#

test_that("firstBreak > OK", {
  expect_length(firstBreak(x), ncol(x))
  expect_length(firstBreak(x, thr = 0.4, method = "threshold"), ncol(x))
  expect_length(firstBreak(x, w = 20, ns = 30, method = "coppens"), ncol(x))
  expect_length(firstBreak(x, w = 48, ns = 1.5*48, method = "coppens"), ncol(x))
})

# NOT OK
test_that("firstBreak > uncorrect arguments", {
  expect_error(firstBreak(x, thr = 1.4, method = "threshold"))
  expect_error(firstBreak(x, w = 49, ns = 1.5*49, method = "coppens"))
  expect_error(firstBreak(x, w = 72.8, method = "coppens"))
  expect_error(firstBreak(x, w = 163.76, method = "coppens"))
  expect_error(firstBreak(x, w = 163.76, method = "ccoppens"))
})

#------------------------------------------------------------------------------#

ts <- runif(ncol(x), min = -10, max = 10)
test_that("traceShift > OK", {
  expect_true(class(traceShift(x, ts = -20)) == "GPR")
  expect_true(class(traceShift(x, ts = 20)) == "GPR")
  expect_true(class(traceShift(x, ts = ts)) == "GPR")
  expect_true(class(traceShift(x, ts = 20, method = "none")) == "GPR")
  expect_true(class(traceShift(x, ts = 20, method = "pchip", crop = TRUE)) == "GPR")
  expect_warning(traceShift(x, ts = 0))
  expect_length(traceShift(x, ts = 20)@proc, 1)
  expect_length(traceShift(x, ts = 20, track = FALSE)@proc, 0)
})

# NOT OK
test_that("traceShift > uncorrect arguments", {
  expect_error(traceShift(x, ts = rep(20, 4), method = "median", crop = 10))
})

#------------------------------------------------------------------------------#

z1 <- seq(from = 0, by = 0.3, to = max(x@depth) )
z2 <- seq(from = 0, by = 0.3, length.out = nrow(x) )
test_that("interpTrace > OK", {
  expect_true(class(interpTrace(x, z = 0.4)) == "GPR")
  expect_true(class(interpTrace(x, z = z1)) == "GPR")
  expect_true(class(interpTrace(x, z = z2, method = "spline")) == "GPR")
  expect_true(class(interpTrace(x, z = z1, method = "linear", crop = FALSE)) == "GPR")
  expect_true(class(interpTrace(x, z = 0.5, method = "pchip", crop = TRUE)) == "GPR")
  expect_length(interpTrace(x, z = 0.4)@proc, 1)
  expect_length(interpTrace(x, z = 0.4, track = FALSE)@proc, 0)
})

# NOT OK
test_that("interpTrace > uncorrect arguments", {
  expect_error(interpTrace(x, z = 0))
  expect_error(interpTrace(x, z = -0.5))
  expect_error(interpTrace(x, z = rep(0.4, 4), method = "median", crop = 10))
})

#------------------------------------------------------------------------------#

test_that("estimateTime0 > OK", {
  expect_true(class(estimateTime0(x)) == "GPR")
  expect_true(class(estimateTime0(x, FUN = median)) == "GPR")
  expect_true(class(estimateTime0(x, c0 = 1)) == "GPR")
  expect_length(estimateTime0(x)@proc, 1)
  expect_length(estimateTime0(x, track = FALSE)@proc, 0)
})

# NOT OK
test_that("estimateTime0 > uncorrect arguments", {
  expect_error(estimateTime0(x, FUN = "median", w = 0, ns = -1))
})

#------------------------------------------------------------------------------#

test_that("time0Cor > OK", {
  expect_true(class(time0Cor(x)) == "GPR")
  expect_length(time0Cor(x)@proc, 1)
  expect_length(time0Cor(x, track = FALSE)@proc, 0)
})

# NOT OK
test_that("time0Cor > uncorrect arguments", {
  expect_error(time0Cor(x, method = "ilkite", crop = 10))
})

#------------------------------------------------------------------------------#

test_that("timeCorOffset > OK", {
  expect_true(class(timeCorOffset(x)) == "GPR")
  expect_true(class(timeCorOffset(x, t0 = NULL)) == "GPR")
  expect_warning(timeCorOffset(x, t0 = 0))
  expect_length(timeCorOffset(x)@proc, 1)
  expect_length(timeCorOffset(x, track = FALSE)@proc, 0)
})

# NOT OK   - no arguments
# test_that("timeCorOffset > uncorrect arguments", {
#   expect_error(timeCorOffset(x))
# })

#------------------------------------------------------------------------------#

test_that("dewow > OK", {
  expect_true(class(dewow(x, type = "runmed", w = 20)) == "GPR")
  expect_true(class(dewow(x, type = "runmean", w = 20)) == "GPR")
  expect_true(class(dewow(x, type = "gaussian", w = 20)) == "GPR")
  expect_true(class(dewow(x, type = "Gaussian", w = 20)) == "GPR")
  expect_warning(dewow(x, type = "mad", w = 20))
  expect_true(class(dewow(x)) == "GPR")
  expect_warning(dewow(x, type = "MAD", w = 20))
  expect_length(dewow(x)@proc, 1)
  expect_length(dewow(x, track = FALSE)@proc, 0)
})

# NOT OK
test_that("time0Cor > uncorrect arguments", {
  expect_error(dewow(x, type = "ilkite", w = -10))
})

#------------------------------------------------------------------------------#

test_that("gainSEC > OK", {
  expect_true(class(gainSEC(x, a = 0)) == "GPR")
  expect_true(class(gainSEC(x, a = 0.01, b = 0)) == "GPR")
  expect_true(class(gainSEC(x, a = 0.01, t0 = 10)) == "GPR")
  expect_length(gainSEC(x, a = 0.01, t0 = 10)@proc, 1)
  expect_length(gainSEC(x, a = 0.01, t0 = 10, track = FALSE)@proc, 0)
})

# NOT OK
test_that("gainSEC > uncorrect arguments", {
  expect_error(gainSEC(x, a = "ilkite", t0 = rep(2, 8)))
})

#------------------------------------------------------------------------------#

test_that("getGainSEC > OK", {
  expect_true(class(getGainSEC(x, a = 0)) == "GPR")
  expect_true(class(getGainSEC(x, a = 0.01, b = 0)) == "GPR")
  expect_true(class(getGainSEC(x, a = 0.01, t0 = 10)) == "GPR")
  expect_length(getGainSEC(x, a = 0.01, t0 = 10)@proc, 1)
  expect_length(getGainSEC(x, a = 0.01, t0 = 10, track = FALSE)@proc, 0)
})

# NOT OK
test_that("getGainSEC > uncorrect arguments", {
  expect_error(getGainSEC(x, a = "ilkite", t0 = rep(2, 8)))
})

#------------------------------------------------------------------------------#

test_that("gainAGC > OK", {
  expect_true(class(gainAGC(x, w = 10)) == "GPR")
  expect_true(class(gainAGC(x, w = 20, p = 0, r = 0)) == "GPR")
  expect_true(class(gainAGC(x, w = 4, p = 1, r = 1)) == "GPR")
  expect_length(gainAGC(x, w = 4, p = 1, r = 1)@proc, 1)
  expect_length(gainAGC(x, w = 4, p = 1, r = 1, track = FALSE)@proc, 0)
})

# NOT OK
test_that("gainAGC > uncorrect arguments", {
  expect_error(gainAGC(x, w = 0, p = -1, r = c(-3,1)))
})

#------------------------------------------------------------------------------#

test_that("filter1D > OK", {
  expect_true(class(filter1D(x, type = "gaussian", w = 20)) == "GPR")
  expect_true(class(filter1D(x, type = "Gaussian", w = 20)) == "GPR")
  expect_true(class(filter1D(x, type = "hampel", w = 20)) == "GPR")
  expect_true(class(filter1D(x, type = "Hampel", w = 20)) == "GPR")
  expect_warning(filter1D(x, type = "MAD", w = 20))
  expect_equal(as.matrix(x - filter1D(x, type = "runmed", w = 20) ),
               as.matrix(dewow(x, type = "runmed", w = 20) ) )
  expect_length(filter1D(x)@proc, 1)
  expect_length(filter1D(x, track = FALSE)@proc, 0)
})

# NOT OK
test_that("filter1D > uncorrect arguments", {
  expect_error(filter1D(x, w = 0, p = -1, r = c(-3,1)))
})



#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#----------------- @proc
test_that("@proc > OK", {
  expect_length((x %>% dcshift() %>% estimateTime0() %>% time0Cor())@proc, 3)
  expect_length((x %>% dcshift(track = FALSE) %>% estimateTime0(track = FALSE) %>% time0Cor(track = FALSE))@proc, 0)
  expect_length((time0Cor(estimateTime0(dcshift(x))))@proc, 3)
  expect_length((time0Cor(estimateTime0(dcshift(x, track = FALSE), track = FALSE), track = FALSE))@proc, 0)
})
