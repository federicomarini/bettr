test_that("transformVariable works", {
    
    ## Categorical variables --------------------------------------------------
    expect_equal(.transformCategoricalVariable(x = c("A", "B", "C"),
                                               levels = c("B", "C", "A")), 
                 c(3, 1, 2))
    expect_equal(.transformCategoricalVariable(x = c("A", "B", "C"),
                                               levels = c("A", "D", "C", "B")),
                 c(1, 4, 3))
    expect_equal(.transformCategoricalVariable(x = c("A", "C", "B")),
                 c(1, 3, 2))
    
    ## Numerical variables ----------------------------------------------------
    x <- c(1, 5, -0.2, 3, -6, NA)
    
    ## -- Different transformations, applied to the data as-is
    expect_equal(.transformNumericVariable(x = x, flip = FALSE, offset = 0, 
                                           transf = .getTransf("None"), 
                                           bincuts = NULL),
                 x)
    
    expect_equal(.transformNumericVariable(x = x, flip = FALSE, offset = 0, 
                                           transf = .getTransf("z-score"), 
                                           bincuts = NULL),
                 scale(x, center = TRUE, scale = TRUE))
    
    t1 <- .transformNumericVariable(x = x, flip = FALSE, offset = 0, 
                                    transf = .getTransf("[0,1]"), 
                                    bincuts = NULL)
    expect_equal(t1, (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
    expect_true(min(t1, na.rm = TRUE) == 0)
    expect_true(max(t1, na.rm = TRUE) == 1)
    expect_length(t1, length(x))
    expect_equal(t1[5], 0)
    expect_equal(t1[2], 1)
    
    t1 <- .transformNumericVariable(x = x, flip = FALSE, offset = 0, 
                                    transf = .getTransf("[-1,1]"), 
                                    bincuts = NULL)
    expect_equal(t1, (2 * x - (min(x, na.rm = TRUE) + max(x, na.rm = TRUE))) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
    expect_true(min(t1, na.rm = TRUE) == -1)
    expect_true(max(t1, na.rm = TRUE) == 1)
    expect_length(t1, length(x))
    expect_equal(t1[5], -1)
    expect_equal(t1[2], 1)

    t1 <- .transformNumericVariable(x = x, flip = FALSE, offset = 0, 
                                    transf = .getTransf("Rank"), 
                                    bincuts = NULL)
    expect_equal(t1, c(3, 5, 2, 4, 1, NA))
    
    t1 <- .transformNumericVariable(x = x, flip = FALSE, offset = 0, 
                                    transf = .getTransf("Rank+[0,1]"), 
                                    bincuts = NULL)
    expect_equal(t1, c(0.5, 1, 0.25, 0.75, 0, NA))
    
    t1 <- .transformNumericVariable(x = x, flip = FALSE, offset = 0, 
                                    transf = .getTransf("z-score+[0,1]"), 
                                    bincuts = NULL)
    y <- scale(x, center = TRUE, scale = TRUE)
    expect_equal(t1, (y - min(y, na.rm = TRUE)) / (max(y, na.rm = TRUE) - min(y, na.rm = TRUE)))
    
    ## -- Different transformations, applied to flipped data
    expect_equal(.transformNumericVariable(x = x, flip = TRUE, offset = 0, 
                                           transf = .getTransf("None"), 
                                           bincuts = NULL),
                 -x)
    
    expect_equal(.transformNumericVariable(x = x, flip = TRUE, offset = 0, 
                                           transf = .getTransf("z-score"), 
                                           bincuts = NULL),
                 scale(-x, center = TRUE, scale = TRUE))
    
    t1 <- .transformNumericVariable(x = x, flip = TRUE, offset = 0, 
                                    transf = .getTransf("[0,1]"), 
                                    bincuts = NULL)
    expect_equal(t1, ((-x) - min(-x, na.rm = TRUE)) / (max(-x, na.rm = TRUE) - min(-x, na.rm = TRUE)))
    expect_true(min(t1, na.rm = TRUE) == 0)
    expect_true(max(t1, na.rm = TRUE) == 1)
    expect_length(t1, length(x))
    expect_equal(t1[5], 1)
    expect_equal(t1[2], 0)
    
    t1 <- .transformNumericVariable(x = x, flip = TRUE, offset = 0, 
                                    transf = .getTransf("[-1,1]"), 
                                    bincuts = NULL)
    expect_equal(t1, (2 * (-x) - (min(-x, na.rm = TRUE) + max(-x, na.rm = TRUE))) / (max(-x, na.rm = TRUE) - min(-x, na.rm = TRUE)))
    expect_true(min(t1, na.rm = TRUE) == -1)
    expect_true(max(t1, na.rm = TRUE) == 1)
    expect_length(t1, length(x))
    expect_equal(t1[5], 1)
    expect_equal(t1[2], -1)
    
    t1 <- .transformNumericVariable(x = x, flip = TRUE, offset = 0, 
                                    transf = .getTransf("Rank"), 
                                    bincuts = NULL)
    expect_equal(t1, c(3, 1, 4, 2, 5, NA))
    
    t1 <- .transformNumericVariable(x = x, flip = TRUE, offset = 0, 
                                    transf = .getTransf("Rank+[0,1]"), 
                                    bincuts = NULL)
    expect_equal(t1, c(0.5, 0, 0.75, 0.25, 1, NA))
    
    t1 <- .transformNumericVariable(x = x, flip = TRUE, offset = 0, 
                                    transf = .getTransf("z-score+[0,1]"), 
                                    bincuts = NULL)
    y <- scale(-x, center = TRUE, scale = TRUE)
    expect_equal(t1, (y - min(y, na.rm = TRUE)) / (max(y, na.rm = TRUE) - min(y, na.rm = TRUE)))
    
    ## -- Different transformations, with offset
    expect_equal(.transformNumericVariable(x = x, flip = FALSE, offset = 1, 
                                           transf = .getTransf("None"), 
                                           bincuts = NULL),
                 x + 1)
    
    expect_equal(.transformNumericVariable(x = x, flip = TRUE, offset = 1, 
                                           transf = .getTransf("z-score"), 
                                           bincuts = NULL),
                 scale((-x + 1), center = TRUE, scale = TRUE))
    
    t1 <- .transformNumericVariable(x = x, flip = FALSE, offset = 1, 
                                    transf = .getTransf("[0,1]"), 
                                    bincuts = NULL)
    expect_equal(t1, (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
    expect_true(min(t1, na.rm = TRUE) == 0)
    expect_true(max(t1, na.rm = TRUE) == 1)
    expect_length(t1, length(x))
    expect_equal(t1[5], 0)
    expect_equal(t1[2], 1)
    
    t1 <- .transformNumericVariable(x = x, flip = FALSE, offset = 6, 
                                    transf = .getTransf("[-1,1]"), 
                                    bincuts = NULL)
    expect_equal(t1, (2 * x - (min(x, na.rm = TRUE) + max(x, na.rm = TRUE))) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
    expect_true(min(t1, na.rm = TRUE) == -1)
    expect_true(max(t1, na.rm = TRUE) == 1)
    expect_length(t1, length(x))
    expect_equal(t1[5], -1)
    expect_equal(t1[2], 1)
    
    t1 <- .transformNumericVariable(x = x, flip = FALSE, offset = -7, 
                                    transf = .getTransf("Rank"), 
                                    bincuts = NULL)
    expect_equal(t1, c(3, 5, 2, 4, 1, NA))
    
    t1 <- .transformNumericVariable(x = x, flip = FALSE, offset = 3, 
                                    transf = .getTransf("Rank+[0,1]"), 
                                    bincuts = NULL)
    expect_equal(t1, c(0.5, 1, 0.25, 0.75, 0, NA))
    
    t1 <- .transformNumericVariable(x = x, flip = FALSE, offset = 2, 
                                    transf = .getTransf("z-score+[0,1]"), 
                                    bincuts = NULL)
    y <- scale(x, center = TRUE, scale = TRUE)
    expect_equal(t1, (y - min(y, na.rm = TRUE)) / (max(y, na.rm = TRUE) - min(y, na.rm = TRUE)), ignore_attr = TRUE)
    
    ## -- Different transformations, after cutting
    expect_equal(.transformNumericVariable(x = x, flip = FALSE, offset = 0, 
                                           transf = .getTransf("None"), 
                                           bincuts = c(0, 2)),
                 c(2, 3, 1, 3, 1, NA))
    
    expect_equal(.transformNumericVariable(x = x, flip = FALSE, offset = 0, 
                                           transf = .getTransf("z-score"), 
                                           bincuts = c(0, 2)),
                 c(2, 2, 1, 2, 1, NA))
    
    t1 <- .transformNumericVariable(x = x, flip = FALSE, offset = 0, 
                                    transf = .getTransf("[0,1]"), 
                                    bincuts = c(0, 2))
    expect_equal(t1, c(1, 1, 1, 1, 1, NA))

    t1 <- .transformNumericVariable(x = x, flip = FALSE, offset = 0, 
                                    transf = .getTransf("[-1,1]"), 
                                    bincuts = c(0, 2))
    expect_equal(t1, c(2, 2, 2, 2, 1, NA))
    
    t1 <- .transformNumericVariable(x = x, flip = FALSE, offset = 0, 
                                    transf = .getTransf("Rank"), 
                                    bincuts = c(0, 2))
    expect_equal(t1, c(2, 2, 2, 2, 1, NA))
})
