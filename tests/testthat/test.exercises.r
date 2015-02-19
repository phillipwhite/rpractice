# 3(b) 
s <- 2^(1:25)/(1:25)
test_that("vector indexing works as I think", {
  expect_equal(s[21], (2^21)/21)
})

# exercise 6(b)
set.seed(50)
x = sample(0:999, 250, replace=T)
y = sample(0:999, 250, replace=T)
a = y - x
b = sin(y[1: length(y) - 1])/cos(x[2: length(x)])

test_that("my understanding of this sequence is correct", {
  expect_equal(b[1], sin(y[1]) / cos(x[2]))
  expect_equal(b[249], sin(y[249]) / cos(x[250]))
})

# exercise 6(c). create this vector:
# x(1) + 2x(2) - x(3), ..., x(n-2) + 2x(n-1) - x(n)
# solution. we need 3 vectors.
#   x1 goes from 1 to n - 2
#   x2 goes from 2 to n - 1
#   x3 goes from 3 to n
x1 = x[1: (length(x) - 2)]
x2 = x[2: (length(x) - 1)]
x3 = x[3: length(x)]
x123 = x1 + 2*x2 - x3

test_that("I'm looping right", {
  expect_equal(x123[1], x[1] + 2*x[2] - x[3])
  expect_equal(x123[189], x[189] + 2*x[190] - x[191])
  expect_equal(x123[length(x) - 2], x1[length(x1)] + 2*x2[length(x2)] - x3[length(x3)])
  expect_equal(length(x123), length(x) - 2)
})

# 6(d)
# calculate sum_{i=1}^{n-1} of e^{-x(i+1)} / x(i) + 10
# -x(i+1) can be expressed as x[-1] because
#    i+1 merely skips the first element
# x(i) however must have the last excluded because
# otherwise it won't match the number of items in the
# sequence x(i+1)
r = sum(exp(-x[-1]) / (x[-length(x)] + 10))
