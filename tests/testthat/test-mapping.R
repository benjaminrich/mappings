test_that("from is matched as a string", {
  f <- mapping("1", "A", ch.as.fact=FALSE)
  expect_equal(f(1), "A")
  expect_equal(f("1"), "A")

  f <- mapping(1, "A", ch.as.fact=FALSE)
  expect_equal(f(1), "A")
  expect_equal(f("1"), "A")

  f <- mapping(c("1"="A"), ch.as.fact=FALSE)
  expect_equal(f(1), "A")
  expect_equal(f("1"), "A")

  f <- mapping(c(`1`="A"), ch.as.fact=FALSE)
  expect_equal(f(1), "A")
  expect_equal(f("1"), "A")
})

test_that("mapping works with numerics", {
  f <- mapping(1:4, 5:8)
  expect_equal(f(c(NA, 3, NA, 1, 4, 3, 2, 5)), c(NA, 7, NA, 5, 8, 7, 6, NA))

  f <- mapping(1:4, 5:8, ch.as.fact=FALSE)
  expect_equal(f(c(NA, 3, NA, 1, 4, 3, 2, 5)), c(NA, 7, NA, 5, 8, 7, 6, NA))

  f <- mapping(c("1"=5, "2"=6, "3"=7, "4"=8))
  expect_equal(f(c(NA, 3, NA, 1, 4, 3, 2, 5)), c(NA, 7, NA, 5, 8, 7, 6, NA))
})

test_that("mapping works with a single named vector", {
  f <- mapping(c("A"="X", "B"="Y", "C"="Z"))
  expect_equal(
    f(c(NA, "B", "B", "C", "A", "D")),
    factor(c(NA, "Y", "Y", "Z", "X", NA), levels=c("X", "Y", "Z")))
})

test_that("mapping works with a two vectors", {
  # In the case, the names of from are ignored
  f <- mapping(c("D"="A", "E"="B", "F"="C"), c("X", "Y", "Z"))
  expect_equal(
    f(c(NA, "B", "B", "C", "A", "D")),
    factor(c(NA, "Y", "Y", "Z", "X", NA), levels=c("X", "Y", "Z")))
})

test_that("mapping handles factor levels correctly", {
  f <- mapping(c("C"="Z", "A"="X", "E"="W", "B"="Y"))
  expect_equal(
    f(c(NA, "B", "B", "C", "A", "D")),
    factor(c(NA, "Y", "Y", "Z", "X", NA), levels=c("Z", "X", "W", "Y")))
})

test_that("mapping works with duplicates", {
  f <- mapping(c("A"="X", "B"="Y", "C"="X"))
  expect_equal(
    f(c(NA, "B", "B", "C", "A", "D")),
    factor(c(NA, "Y", "Y", "X", "X", NA), levels=c("X", "Y")))

  f <- mapping(c("A"="X", "B"="Y", "A"="Z"))
  expect_equal(
    f(c(NA, "B", "B", "C", "A", "D")),
    factor(c(NA, "Y", "Y", NA, "X", NA), levels=c("X", "Y", "Z")))
})

test_that("mapping works with NA", {
  f <- mapping(c("A"="X", "B"=NA, "C"="Z"), na="W")
  expect_equal(
    f(c(NA, "B", "B", "C", "A", "D")),
    factor(c("W", NA, NA, "Z", "X", NA), levels=c("X", "Z", "W")))

  f <- mapping(c("A", "B", "C", NA), c("X", NA, "Z", "W"))
  expect_equal(
    f(c(NA, "B", "B", "C", "A", "D")),
    factor(c("W", NA, NA, "Z", "X", NA), levels=c("X", "Z", "W")))

  f <- mapping(c("A", "B", "C"), c("X", NA, "Z"), na="W")
  expect_equal(
    f(c(NA, "B", "B", "C", "A", "D")),
    factor(c("W", NA, NA, "Z", "X", NA), levels=c("X", "Z", "W")))

  f <- mapping(c("A", "B", "C"), c("X", NA, "Z"), na="W", ch.as.fact=F)
  expect_equal(
    f(c(NA, "B", "B", "C", "A", "D")),
    c("W", NA, NA, "Z", "X", NA))

  f <- mapping(c("A", "B", "C", NA), c("X", NA, "Z", "W"), na="V")
  expect_equal(
    f(c(NA, "B", "B", "C", "A", "D")),
    factor(c("W", NA, NA, "Z", "X", NA), levels=c("X", "Z", "W")))

  f <- mapping(1:4, 5:8, na=9)
  expect_equal(f(c(NA, 3, NA, 1, 4, 3, 2, 5)), c(9, 7, 9, 5, 8, 7, 6, NA))
})

test_that("inverse mapping works", {
  f <- mapping(c("A"="X", "B"="Y", "C"="Z"))
  expect_equal(
    inverse(f)(c(NA, "Y", "Y", "X", "W")),
    factor(c(NA, "B", "B", "A", NA), levels=c("A", "B", "C")))
})

test_that("domain and codomain work", {
  f <- mapping(c("A"="X", "B"="Y", "C"="Z"))
  expect_equal(domain(f), c("A", "B", "C"))
  expect_equal(codomain(f), c("X", "Y", "Z"))
})

test_that("text2mapping works", {
  f <- text2mapping("
    A | X
    B | Y
    C | Z
  ")
  expect_equal(
    f(c(NA, "B", "B", "C", "A", "D")),
    factor(c(NA, "Y", "Y", "Z", "X", NA), levels=c("X", "Y", "Z")))
  expect_equal(
    inverse(f)(c(NA, "Y", "Y", "X", "W")),
    factor(c(NA, "B", "B", "A", NA), levels=c("A", "B", "C")))
})

test_that("text2mapping works with extra separators", {

  f <- text2mapping("
    | A | X |
    | B | Y |
    | C | Z |
  ")
  expect_equal(
    f(c(NA, "B", "B", "C", "A", "D")),
    factor(c(NA, "Y", "Y", "Z", "X", NA), levels=c("X", "Y", "Z")))
  expect_equal(
    inverse(f)(c(NA, "Y", "Y", "X", "W")),
    factor(c(NA, "B", "B", "A", NA), levels=c("A", "B", "C")))
})

test_that("text2mapping works with different separator", {
  f <- text2mapping(sep="=", "
    A = X
    B = Y
    C = Z
  ")
  expect_equal(
    inverse(f)(c(NA, "Y", "Y", "X", "W")),
    factor(c(NA, "B", "B", "A", NA), levels=c("A", "B", "C")))
})

test_that("text2mapping works with NA when convert.na=FALSE", {
  f <- text2mapping(convert.na=FALSE, "
    A  | NA
    NA | Y
    C  | Z
  ")
  expect_equal(
    f(c(NA, "NA", "NA", "C", "A", "D")),
    factor(c(NA, "Y", "Y", "Z", "NA", NA), levels=c("NA", "Y", "Z")))
  expect_equal(
    inverse(f)(c(NA, "Y", "Y", "NA", "W")),
    factor(c(NA, "NA", "NA", "A", NA), levels=c("A", "NA", "C")))
})

test_that("text2mapping works with NA when convert.na=TRUE", {
  expect_warning(f <- text2mapping(convert.na=TRUE, "
    A  | NA
    NA | Y
    C  | Z
  "))

  expect_equal(
    f(c(NA, "B", "B", "C", "A", "D")),
    factor(c("Y", NA, NA, "Z", NA, NA), levels=c("Y", "Z")))
  expect_equal(
    inverse(f)(c(NA, "Y", "Y", "X", "W")),
    factor(c("A", NA, NA, NA, NA), levels=c("A", "C")))
})

test_that("text2mapping works with numeric values", {
  f <- text2mapping("
    1 | 3.4
    2 | 5.6
    3 | 7.8
  ")

  expect_equal(f(c(3, 3, 1, 3, 2)), c(7.8, 7.8, 3.4, 7.8, 5.6))

  f <- text2mapping(numericWherePossible=FALSE, "
    1 | 3.4
    2 | 5.6
    3 | 7.8
  ")

  expect_equal(f(c(3, 3, 1, 3, 2)),
    factor(c(7.8, 7.8, 3.4, 7.8, 5.6), levels=c(3.4, 5.6, 7.8)))
})

test_that("text2mapping passes ... to mapping", {
  f <- text2mapping(na="W", "
    A | X
    B | Y
    C | Z
  ")
  expect_equal(
    f(c(NA, "B", "B", "C", "A", "D")),
    factor(c("W", "Y", "Y", "Z", "X", NA), levels=c("X", "Y", "Z", "W")))

  f <- text2mapping(ch.as.fact=FALSE, "
    A | X
    B | Y
    C | Z
  ")
  expect_equal(f(c(NA, "B", "B", "C", "A", "D")), c(NA, "Y", "Y", "Z", "X", NA))
})

test_that("cut_mapping works", {
  x <- c(0, 10, 20, 30, Inf)
  m <- cut_mapping(x, right=FALSE,
      to=c("0 to <10", "10 to <20", "20 to <30", ">= 30"))
  expect_equal(
    m(c(5, 27, 3, 10, 99)),
    factor(c("0 to <10", "20 to <30", "0 to <10", "10 to <20", ">= 30"),
      levels=c("0 to <10", "10 to <20", "20 to <30", ">= 30")))
})

# vim: ts=2 sw=2 et
