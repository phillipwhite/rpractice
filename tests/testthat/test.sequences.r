s <- 2^(1:25)/(1:25)
test_that("item is what I think", {
  expect_equal(s[21], (2^21)/21)
})