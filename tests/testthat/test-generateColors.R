test_that("generateColors works", {
    expect_equal(.gg_color_hue(3), 
                 c("#F8766D", "#00BA38", "#619CFF"))
    
    expect_null(.generateColors(df = NULL, inputColors = NULL))
    
    df <- data.frame(colA = LETTERS[1:4], 
                     colB = c("A", "A", "A", "B"),
                     colC = 1:4, 
                     colD = 5:8)
    
    expect_error(.generateColors(df = df, inputColors = list, 
                                 ggplot2Columns = "colC"),
                 "ggplot2 colors for continuous columns")
    
    set.seed(1)
    gc <- .generateColors(df = df, inputColors = list(), ggplot2Columns = c())
    expect_type(gc, "list")
    expect_length(gc, 4)
    expect_type(gc$colA, "character")
    expect_length(gc$colA, 4)
    expect_equal(gc$colA, c(A = "dodgerblue1", B = "orchid1", C = "mediumpurple4", D = "grey38"))
    expect_type(gc$colB, "character")
    expect_length(gc$colB, 2)
    expect_equal(gc$colB, c(A = "grey9", B = "gray34"))
    expect_type(gc$colC, "closure")
    expect_equal(gc$colC(3), "#A1A1A1FF")
    expect_type(gc$colD, "closure")
    expect_equal(gc$colD(2), "#FFFFFFFF")
    expect_equal(gc$colD(7), "#A08EDFFF")
    
    set.seed(1)
    gc <- .generateColors(df = df, inputColors = list(colB = c(A = "blue", B = "red")), ggplot2Columns = c())
    expect_type(gc, "list")
    expect_length(gc, 4)
    expect_type(gc$colA, "character")
    expect_length(gc$colA, 4)
    expect_equal(gc$colA, c(A = "dodgerblue1", B = "orchid1", C = "mediumpurple4", D = "grey38"))
    expect_type(gc$colB, "character")
    expect_length(gc$colB, 2)
    expect_equal(gc$colB, c(A = "blue", B = "red"))
    expect_type(gc$colC, "closure")
    expect_equal(gc$colC(3), "#5B5B5BFF")
    expect_type(gc$colD, "closure")
    expect_equal(gc$colD(2), "#FFFFFFFF")
    expect_equal(gc$colD(7), "#8B8B8BFF")
})
