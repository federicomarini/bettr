test_that("transformVariable works", {

    expect_error(.getTransf("wrong"),
                 "Unknown transformation")

    ## Categorical variables --------------------------------------------------
    expect_identical(.transformCategoricalVariable(x = c("A", "B", "C"),
                                                   levels = c("B", "C", "A")),
                     c(3.0, 1.0, 2.0))
    expect_identical(.transformCategoricalVariable(x = c("A", "B", "C"),
                                                   levels = c("A", "D", "C", "B")),
                     c(1.0, 4.0, 3.0))
    expect_identical(.transformCategoricalVariable(x = c("A", "C", "B")),
                     c(1.0, 3.0, 2.0))

    ## Numerical variables ----------------------------------------------------
    x <- c(1.0, 5.0, -0.2, 3.0, -6.0, NA)

    ## -- Different transformations, applied to the data as-is
    expect_identical(.transformNumericVariable(x = x, flip = FALSE, offset = 0.0,
                                               transf = .getTransf("None"),
                                               bincuts = NULL),
                     x)

    expect_identical(.transformNumericVariable(x = x, flip = FALSE, offset = 0.0,
                                               transf = .getTransf("z-score"),
                                               bincuts = NULL),
                     scale(x, center = TRUE, scale = TRUE))

    t1 <- .transformNumericVariable(x = x, flip = FALSE, offset = 0.0,
                                    transf = .getTransf("[0,1]"),
                                    bincuts = NULL)
    expect_identical(t1, (x - min(x, na.rm = TRUE)) /
                         (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
    expect_identical(min(t1, na.rm = TRUE), 0.0)
    expect_identical(max(t1, na.rm = TRUE), 1.0)
    expect_length(t1, length(x))
    expect_identical(t1[5L], 0.0)
    expect_identical(t1[2L], 1.0)

    t1 <- .transformNumericVariable(x = x, flip = FALSE, offset = 0.0,
                                    transf = .getTransf("[-1,1]"),
                                    bincuts = NULL)
    expect_identical(t1, (2.0 * x - (min(x, na.rm = TRUE) + max(x, na.rm = TRUE))) /
                         (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
    expect_identical(min(t1, na.rm = TRUE), -1.0)
    expect_identical(max(t1, na.rm = TRUE), 1.0)
    expect_length(t1, length(x))
    expect_identical(t1[5L], -1.0)
    expect_identical(t1[2L], 1.0)

    t1 <- .transformNumericVariable(x = x, flip = FALSE, offset = 0.0,
                                    transf = .getTransf("Rank"),
                                    bincuts = NULL)
    expect_identical(t1, c(3.0, 5.0, 2.0, 4.0, 1.0, NA))

    t1 <- .transformNumericVariable(x = x, flip = FALSE, offset = 0.0,
                                    transf = .getTransf("Rank+[0,1]"),
                                    bincuts = NULL)
    expect_identical(t1, c(0.5, 1.0, 0.25, 0.75, 0.0, NA))

    t1 <- .transformNumericVariable(x = x, flip = FALSE, offset = 0.0,
                                    transf = .getTransf("z-score+[0,1]"),
                                    bincuts = NULL)
    y <- scale(x, center = TRUE, scale = TRUE)
    expect_identical(t1, (y - min(y, na.rm = TRUE)) / (max(y, na.rm = TRUE) - min(y, na.rm = TRUE)))

    ## -- Different transformations, applied to flipped data
    expect_identical(.transformNumericVariable(x = x, flip = TRUE, offset = 0.0,
                                               transf = .getTransf("None"),
                                               bincuts = NULL),
                     -x)

    expect_identical(.transformNumericVariable(x = x, flip = TRUE, offset = 0.0,
                                               transf = .getTransf("z-score"),
                                               bincuts = NULL),
                     scale(-x, center = TRUE, scale = TRUE))

    t1 <- .transformNumericVariable(x = x, flip = TRUE, offset = 0.0,
                                    transf = .getTransf("[0,1]"),
                                    bincuts = NULL)
    expect_identical(t1, ((-x) - min(-x, na.rm = TRUE)) /
                         (max(-x, na.rm = TRUE) - min(-x, na.rm = TRUE)))
    expect_identical(min(t1, na.rm = TRUE), 0.0)
    expect_identical(max(t1, na.rm = TRUE), 1.0)
    expect_length(t1, length(x))
    expect_identical(t1[5L], 1.0)
    expect_identical(t1[2L], 0.0)

    t1 <- .transformNumericVariable(x = x, flip = TRUE, offset = 0.0,
                                    transf = .getTransf("[-1,1]"),
                                    bincuts = NULL)
    expect_identical(t1, (2.0 * (-x) - (min(-x, na.rm = TRUE) + max(-x, na.rm = TRUE))) /
                         (max(-x, na.rm = TRUE) - min(-x, na.rm = TRUE)))
    expect_identical(min(t1, na.rm = TRUE), -1.0)
    expect_identical(max(t1, na.rm = TRUE), 1.0)
    expect_length(t1, length(x))
    expect_identical(t1[5L], 1.0)
    expect_identical(t1[2L], -1.0)

    t1 <- .transformNumericVariable(x = x, flip = TRUE, offset = 0.0,
                                    transf = .getTransf("Rank"),
                                    bincuts = NULL)
    expect_identical(t1, c(3.0, 1.0, 4.0, 2.0, 5.0, NA))

    t1 <- .transformNumericVariable(x = x, flip = TRUE, offset = 0.0,
                                    transf = .getTransf("Rank+[0,1]"),
                                    bincuts = NULL)
    expect_identical(t1, c(0.5, 0.0, 0.75, 0.25, 1.0, NA))

    t1 <- .transformNumericVariable(x = x, flip = TRUE, offset = 0.0,
                                    transf = .getTransf("z-score+[0,1]"),
                                    bincuts = NULL)
    y <- scale(-x, center = TRUE, scale = TRUE)
    expect_identical(t1, (y - min(y, na.rm = TRUE)) / (max(y, na.rm = TRUE) - min(y, na.rm = TRUE)))

    ## -- Different transformations, with offset
    expect_identical(.transformNumericVariable(x = x, flip = FALSE, offset = 1.0,
                                               transf = .getTransf("None"),
                                               bincuts = NULL),
                     x + 1.0)

    expect_identical(.transformNumericVariable(x = x, flip = TRUE, offset = 1.0,
                                               transf = .getTransf("z-score"),
                                               bincuts = NULL),
                     scale((-x + 1.0), center = TRUE, scale = TRUE))

    t1 <- .transformNumericVariable(x = x, flip = FALSE, offset = 1.0,
                                    transf = .getTransf("[0,1]"),
                                    bincuts = NULL)
    expect_identical(t1, (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
    expect_identical(min(t1, na.rm = TRUE), 0.0)
    expect_identical(max(t1, na.rm = TRUE), 1.0)
    expect_length(t1, length(x))
    expect_identical(t1[5L], 0.0)
    expect_identical(t1[2L], 1.0)

    t1 <- .transformNumericVariable(x = x, flip = FALSE, offset = 6.0,
                                    transf = .getTransf("[-1,1]"),
                                    bincuts = NULL)
    expect_identical(t1, (2.0 * x - (min(x, na.rm = TRUE) + max(x, na.rm = TRUE))) /
                         (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
    expect_identical(min(t1, na.rm = TRUE), -1.0)
    expect_identical(max(t1, na.rm = TRUE), 1.0)
    expect_length(t1, length(x))
    expect_identical(t1[5L], -1.0)
    expect_identical(t1[2L], 1.0)

    t1 <- .transformNumericVariable(x = x, flip = FALSE, offset = -7.0,
                                    transf = .getTransf("Rank"),
                                    bincuts = NULL)
    expect_identical(t1, c(3.0, 5.0, 2.0, 4.0, 1.0, NA))

    t1 <- .transformNumericVariable(x = x, flip = FALSE, offset = 3.0,
                                    transf = .getTransf("Rank+[0,1]"),
                                    bincuts = NULL)
    expect_identical(t1, c(0.5, 1.0, 0.25, 0.75, 0.0, NA))

    t1 <- .transformNumericVariable(x = x, flip = FALSE, offset = 2.0,
                                    transf = .getTransf("z-score+[0,1]"),
                                    bincuts = NULL)
    y <- scale(x, center = TRUE, scale = TRUE)
    expect_identical(t1, (y - min(y, na.rm = TRUE)) /
                         (max(y, na.rm = TRUE) - min(y, na.rm = TRUE)), ignore_attr = TRUE)

    ## -- Different transformations, after cutting
    expect_identical(.transformNumericVariable(x = x, flip = FALSE, offset = 0.0,
                                               transf = .getTransf("None"),
                                               bincuts = c(0.0, 2.0)),
                     c(2.0, 3.0, 1.0, 3.0, 1.0, NA))

    expect_identical(.transformNumericVariable(x = x, flip = FALSE, offset = 0.0,
                                               transf = .getTransf("z-score"),
                                               bincuts = c(0.0, 2.0)),
                     c(2.0, 2.0, 1.0, 2.0, 1.0, NA))

    t1 <- .transformNumericVariable(x = x, flip = FALSE, offset = 0.0,
                                    transf = .getTransf("[0,1]"),
                                    bincuts = c(0.0, 2.0))
    expect_identical(t1, c(1.0, 1.0, 1.0, 1.0, 1.0, NA))

    t1 <- .transformNumericVariable(x = x, flip = FALSE, offset = 0.0,
                                    transf = .getTransf("[-1,1]"),
                                    bincuts = c(0.0, 2.0))
    expect_identical(t1, c(2.0, 2.0, 2.0, 2.0, 1.0, NA))

    t1 <- .transformNumericVariable(x = x, flip = FALSE, offset = 0.0,
                                    transf = .getTransf("Rank"),
                                    bincuts = c(0.0, 2.0))
    expect_identical(t1, c(2.0, 2.0, 2.0, 2.0, 1.0, NA))
})
