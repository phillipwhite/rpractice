s <- 2^(1:25)/(1:25)
test_that("vector indexing works as I think", {
  expect_equal(s[21], (2^21)/21)
})

set.seed(50)
x = sample(0:999, 250, replace=T)
y = sample(0:999, 250, replace=T)
a = y - x
b = sin(y[1:length(y)-1])/cos(x[2:length(x)])

test_that("my understanding of this sequence is correct", {
  expect_equal(b[1], sin(y[1])/cos(x[2]))
  expect_equal(b[249], sin(y[249])/cos(x[250]))
})