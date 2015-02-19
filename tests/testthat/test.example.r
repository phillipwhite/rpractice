s <- seq(3,6, by=0.1)
test_that("the last item is 6", {  
  expect_identical(tail(s,n=1), 6)  
})