# exercise 2 

s <- seq(3, 6, by=0.1)
ecos <- function (x) { exp(x)*cos(x) }
t <- ecos(s)

test_that("list has 31 items", {
  expect_equal(length(t), 31)
})