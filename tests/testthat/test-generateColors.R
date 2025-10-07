test_that("generateColors works", {
    expect_identical(.ggColorHue(3L),
                     c("#F8766D", "#00BA38", "#619CFF"))

    expect_null(.generateColors(df = NULL, inputColors = NULL))

    df <- data.frame(colA = LETTERS[1L:4L],
                     colB = c("A", "A", "A", "B"),
                     colC = 1L:4L,
                     colD = 5L:8L)

    expect_error(.generateColors(df = df, inputColors = list,
                                 ggplot2Columns = "colC"),
                 "ggplot2 colors for continuous columns")

    set.seed(1L)
    gc <- .generateColors(df = df, inputColors = list(),
                          ggplot2Columns = character(0L))
    expect_type(gc, "list")
    expect_length(gc, 4L)
    expect_type(gc$colA, "character")
    expect_length(gc$colA, 4L)
    expect_identical(gc$colA, c(A = "dodgerblue1", B = "orchid1",
                                C = "mediumpurple4", D = "grey38"))
    expect_type(gc$colB, "character")
    expect_length(gc$colB, 2L)
    expect_identical(gc$colB, c(A = "grey9", B = "gray34"))
    expect_type(gc$colC, "closure")
    expect_identical(gc$colC(3L), "#A1A1A1FF")
    expect_type(gc$colD, "closure")
    expect_identical(gc$colD(2L), "#FFFFFFFF")
    expect_identical(gc$colD(7L), "#A08EDFFF")

    set.seed(1L)
    gc <- .generateColors(
        df = df,
        inputColors = list(colB = c(A = "blue", B = "red")),
        ggplot2Columns = character(0L)
    )
    expect_type(gc, "list")
    expect_length(gc, 4L)
    expect_type(gc$colA, "character")
    expect_length(gc$colA, 4L)
    expect_identical(gc$colA,
                     c(A = "dodgerblue1", B = "orchid1",
                       C = "mediumpurple4", D = "grey38"))
    expect_type(gc$colB, "character")
    expect_length(gc$colB, 2L)
    expect_identical(gc$colB, c(A = "blue", B = "red"))
    expect_type(gc$colC, "closure")
    expect_identical(gc$colC(3L), "#5B5B5BFF")
    expect_type(gc$colD, "closure")
    expect_identical(gc$colD(2L), "#FFFFFFFF")
    expect_identical(gc$colD(7L), "#8B8B8BFF")
})
