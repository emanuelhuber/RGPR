# 
# 
LINES <- file.path("/mnt/data/huber/Documents/RESEARCH/PROJECTS/RGPR/CODE/DEVELOPMENT/FILE_FORMAT/DT1/2011_10_10_flagogna",
                   paste0("XLINE", sprintf("%03d", 0:5), ".DT1"))
# 
# 
 x <- GPRsurvey(LINES, verbose = FALSE)
# 
# #-------------------------------- spBuffer ------------------------------------#
# xb <-spBuffer(x, d = 5)
# plot(xb)
# xb <-spBuffer(x, d = 5, combine = FALSE)
# plot(xb)
# xb <-spBuffer(x[[2]], d = 5, combine = FALSE)
# plot(xb)
# xb <-spBuffer(x[[2]], d = 5, combine = TRUE)
# plot(xb)
# plot(as.sf(x[[2]]))
# #-------------------------------- spBuffer ------------------------------------#


test_that("test 'spBuffer()",{
  expect_warning(xb <-spBuffer(x, d = 5))
  expect_silent(xb <-spBuffer(x[2:6], d = 5))
  expect_silent(xb <-spBuffer(x[2:6], d = 5, combine = FALSE))
  expect_silent(xb <-spBuffer(x[[2]], d = 5, combine = FALSE))
  expect_silent(xb <-spBuffer(x[[2]], d = 5, combine = TRUE))
})