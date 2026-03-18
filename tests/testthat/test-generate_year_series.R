## Unit tests for `generate_year_series()`

# 2-year sliding windows
test_that("generate_year_series works for a simple overlapping case", {
    res <- generate_year_series(2014, 2016, 2, overlapping = TRUE, strict = TRUE)

    expect_equal(
        res,
        data.frame(
            from = c(2014L, 2015L),
            to   = c(2015L, 2016L),
            k    = c(2L, 2L)
        ),
        ignore_attr = TRUE
    )
})

# 3-year sliding windows
test_that("generate_year_series works for a simple overlapping case", {
    res <- generate_year_series(2018, 2025, 3, overlapping = TRUE, strict = TRUE)

    expect_equal(
        res,
        data.frame(
            from = c(2018L, 2019L, 2020L, 2021L, 2022L, 2023L),
            to   = c(2020L, 2021L, 2022L, 2023L, 2024L, 2025L),
            k    = rep(3L, 6)
        ),
        ignore_attr = TRUE
    )
})
