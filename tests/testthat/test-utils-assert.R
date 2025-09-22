library(testthat)

## -------------------------------------------------------------------------- ##
## Checks, .assertScalar
## -------------------------------------------------------------------------- ##
test_that(".assertScalar works", {
    expect_error(.assertScalar(1L, type = TRUE))
    expect_error(.assertScalar(1L, type = 1L))
    expect_error(.assertScalar(1L, type = c("numeric", "character")))
    expect_error(.assertScalar(1L, type = "numeric", rngIncl = TRUE))
    expect_error(.assertScalar(1L, type = "numeric", rngIncl = "rng"))
    expect_error(.assertScalar(1L, type = "numeric", rngIncl = 1L))
    expect_error(.assertScalar(1L, type = "numeric", rngIncl = 1L:3L))
    expect_error(.assertScalar(1L, type = "numeric", rngExcl = TRUE))
    expect_error(.assertScalar(1L, type = "numeric", rngExcl = "rng"))
    expect_error(.assertScalar(1L, type = "numeric", rngExcl = 1L))
    expect_error(.assertScalar(1L, type = "numeric", rngExcl = 1L:3L))
    expect_error(.assertScalar(1L, type = "numeric", rngIncl = c(0L, 2L),
                               rngExcl = c(0L, 2L)))
    expect_error(.assertScalar(1L, type = "numeric", allowNULL = 1L))
    expect_error(.assertScalar(1L, type = "numeric", allowNULL = "rng"))
    expect_error(.assertScalar(1L, type = "numeric", allowNULL = NULL))
    expect_error(.assertScalar(1L, type = "numeric", allowNULL = c(TRUE, FALSE)))

    expect_true(.assertScalar(1L, type = "numeric", rngIncl = c(1L, 3L)))
    expect_error(.assertScalar(1L, type = "numeric", rngExcl = c(1L, 3L)))
    expect_true(.assertScalar(1L, type = "numeric", rngExcl = c(1L, 3L),
                              validValues = 1L))
    expect_true(.assertScalar(-1L, type = "numeric", rngIncl = c(1L, 3L),
                              validValues = c(-1L, 0L)))
    expect_error(.assertScalar(-1L, type = "numeric", rngIncl = c(1L, 3L),
                               validValues = 0L))
    expect_true(.assertScalar(-1L, type = "numeric", validValues = c(-1L, 0L)))
    expect_error(.assertScalar(-1L, type = "numeric", validValues = c(-2L, 0L)))
    expect_true(.assertScalar(NA_real_, type = "numeric", rngIncl = c(1L, 2L),
                              validValues = NA_real_))
    expect_error(.assertScalar(NA, type = "numeric", rngIncl = c(1L, 2L),
                               validValues = NA_real_))
    expect_true(.assertScalar(NA_real_, type = "numeric", rngIncl = c(1L, 2L),
                              validValues = NA))
    expect_true(.assertScalar(1L, type = "numeric", rngIncl = c(0L, 3L),
                              validValues = 3L))
    expect_true(.assertScalar(1L, rngIncl = c(0L, 3L), validValues = 3L))
    expect_true(.assertScalar(1L, type = "numeric", rngIncl = c(0L, 1L)))
    expect_error(.assertScalar(1L, type = "numeric", rngExcl = c(0L, 1L)))
    expect_true(.assertScalar(1L, type = "numeric", rngExcl = c(0L, 1L),
                              validValues = 1L))
    expect_error(.assertScalar(1L, type = "numeric", rngExcl = c(0L, 1L),
                               validValues = 3L:4L))
    expect_true(.assertScalar(NULL, type = "numeric", allowNULL = TRUE))
    expect_error(.assertScalar(NULL, type = "numeric", allowNULL = FALSE))
    expect_error(.assertScalar(1L, type = "character"))
    expect_error(.assertScalar("x", type = "numeric"))
    expect_error(.assertScalar(FALSE, type = "character"))
    expect_error(.assertScalar(c(1L, 2L), type = "numeric"))
    test <- "text"
    expect_error(.assertScalar(x = test, type = "numeric"),
                 "'test' must be of class 'numeric")
})

## -------------------------------------------------------------------------- ##
## Checks, .assertVector
## -------------------------------------------------------------------------- ##
test_that(".assertVector works", {
    expect_error(.assertVector(1L, type = TRUE))
    expect_error(.assertVector(1L, type = 1L))
    expect_error(.assertVector(1L, type = c("numeric", "character")))
    expect_error(.assertVector(1L, type = "numeric", rngIncl = TRUE))
    expect_error(.assertVector(1L, type = "numeric", rngIncl = "rng"))
    expect_error(.assertVector(1L, type = "numeric", rngIncl = 1L))
    expect_error(.assertVector(1L, type = "numeric", rngIncl = 1L:3L))
    expect_error(.assertVector(1L, type = "numeric", rngExcl = TRUE))
    expect_error(.assertVector(1L, type = "numeric", rngExcl = "rng"))
    expect_error(.assertVector(1L, type = "numeric", rngExcl = 1L))
    expect_error(.assertVector(1L, type = "numeric", rngExcl = 1L:3L))
    expect_error(.assertVector(1L, type = "numeric", rngIncl = c(0L, 2L),
                               rngExcl = c(0L, 2L)))
    expect_error(.assertVector(1L, type = "numeric", allowNULL = 1L))
    expect_error(.assertVector(1L, type = "numeric", allowNULL = "rng"))
    expect_error(.assertVector(1L, type = "numeric", allowNULL = NULL))
    expect_error(.assertVector(1L, type = "numeric", allowNULL = c(TRUE, FALSE)))
    expect_error(.assertVector(1L, type = "numeric", len = TRUE))
    expect_error(.assertVector(1L, type = "numeric", len = "rng"))
    expect_error(.assertVector(1L, type = "numeric", len = 1L:3L))
    expect_error(.assertVector(1L, type = "numeric", rngLen = TRUE))
    expect_error(.assertVector(1L, type = "numeric", rngLen = "rng"))
    expect_error(.assertVector(1L, type = "numeric", rngLen = 1L))
    expect_error(.assertVector(1L, type = "numeric", rngLen = 1L:3L))

    expect_true(.assertVector(c(1L, 2L), type = "numeric", rngIncl = c(1L, 3L)))
    expect_error(.assertVector(c(1L, 2L), type = "numeric", rngIncl = c(1.0, 1.5)))
    expect_error(.assertVector(c(1L, 2L), type = "numeric", rngExcl = c(1L, 3L)))
    expect_true(.assertVector(c(1L, 2L), type = "numeric", rngExcl = c(1L, 3L),
                              validValues = 1L))
    expect_error(.assertVector(c(1L, 2L), type = "numeric", validValues = c(1L, 3L)))
    expect_true(.assertVector(c(1L, 2L), type = "numeric", validValues = c(1L, 2L)))
    expect_error(.assertVector(c(1L, 2L), type = "numeric", len = 1L))
    expect_true(.assertVector(c(1L, 2L), type = "numeric", len = 2L))
    expect_error(.assertVector(c(1L, 2L), type = "numeric", rngLen = c(3L, 5L)))
    expect_true(.assertVector(c(1L, 2L), type = "numeric", rngLen = c(2L, 5L)))
    expect_true(.assertVector(c(1L, 2L), type = "numeric", rngLen = c(1L, 2L)))
    expect_error(.assertVector(c("a", "b"), type = "character",
                               validValues = c("A", "B")))
    expect_true(.assertVector(LETTERS[1L:2L], type = "character",
                              validValues = LETTERS))
    test <- "text"
    expect_error(.assertVector(x = test, type = "numeric"),
                 "'test' must be of class 'numeric")
})
