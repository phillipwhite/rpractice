# 3(b) 
s = 2^(1:25)/(1:25)
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

# 7(d)
r = sqrt(abs(x - mean(x)))
# in this case, the test below doesn't have a connection
# with the r-expression computed. if we change the
# computation, we'll have to adjust the test.
# i don't have any better idea. but this test at 
# least makes sure that what I expect from the 
# expression is what I get. 
test_that("my understanding here is okay", {
  expect_identical(sqrt(abs(x[1] - mean(x[1]))), 0)
})

# 7(e) how many values in y are within 200 of max(y)?
# solution. to be within 200 of max(y) means what?
# it means that |x - max(y)| < 200 is true.
bs = y[abs(y - max(y)) < 200]

# the book solution is very different. they do:
book_res = sum(y > max(y) - 200)

test_that("my solution matches the book's", {
  expect_identical(length(bs), book_res)
})

# 7(f)
# x[x %% 2 == 0]
# again, book style is different
test_that("my solution matches book's", {
  expect_identical(length(x[x %% 2 == 0]), 
                   sum(x %% 2 == 0))
})

# 7(g) sort the numbers in x in the order of 
# increasing values in y. what does that even mean?
# i don't get how order() works.

# 7(h)
r = y[seq(1,length(y), by=3)]
# book solution is wild. y[c(T,F,F)]. how does it work?
test_that("book solution works", {
  expect_identical(r, y[c(T,F,F)])
})

# 8. 
# apparently we must use cumprod to compute each 
# of the inner sequences first.
num = seq(2,38, by=2)
den = seq(3,39, by=2)
r = 1 + sum(cumprod(num/den))

