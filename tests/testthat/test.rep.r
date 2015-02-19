s <- rep(c(4,6,3),times=10)
test_that("the list has 30 items", {
  expect_equal(length(s), 30)
})

s <- rep(c(4,6,3), len=31)
test_that("the last item is 4", {
  expect_equal(s[31], 4)
})

s <- c(rep(4,times=10), rep(6,times=20), rep(3, times=30))
test_that("the list has 60 items", {
  expect_equal(length(s), 60)
})