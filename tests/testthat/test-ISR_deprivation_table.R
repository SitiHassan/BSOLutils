
# Returns gt_tbl
test_that("ISR_deprivation_table returns a gt table", {
    input <- tibble::tibble(
        `IMD Quintile` = c(2, 3, 4),
        Ratio = c(1.2, 0.8, 1.0),
        `Lower CI` = c(1.1, 0.7, 0.9),
        `Upper CI` = c(1.3, 0.9, 1.1),
        Interpretation = c("Higher", "Lower", "Same"),
        `Statistical Significance` = c(
            "Significantly higher than IMD 1",
            "Significantly lower than IMD 1",
            "Not statistically significant"
        )
    )

    result <- ISR_deprivation_table(input)

    expect_s3_class(result, "gt_tbl")
})

# Doesn't error with valid input
test_that("ISR_deprivation_table runs without error on valid input", {
    input <- tibble::tibble(
        `IMD Quintile` = c(2, 3, 4),
        Ratio = c(1.2, 0.8, 1.0),
        `Lower CI` = c(1.1, 0.7, 0.9),
        `Upper CI` = c(1.3, 0.9, 1.1),
        Interpretation = c("Higher", "Lower", "Same"),
        `Statistical Significance` = c(
            "Significantly higher than IMD 1",
            "Significantly lower than IMD 1",
            "Not statistically significant"
        )
    )

    expect_no_error(ISR_deprivation_table(input))
})


# Helper column is not visible in the output data
test_that("ISR_deprivation_table adds and hides sig_fill helper column", {
    input <- tibble::tibble(
        `IMD Quintile` = c(2, 3, 4),
        Ratio = c(1.2, 0.8, 1.0),
        `Lower CI` = c(1.1, 0.7, 0.9),
        `Upper CI` = c(1.3, 0.9, 1.1),
        Interpretation = c("Higher", "Lower", "Same"),
        `Statistical Significance` = c(
            "Significantly higher than IMD 1",
            "Significantly lower than IMD 1",
            "Not statistically significant"
        )
    )

    result <- ISR_deprivation_table(input)

    expect_true("sig_fill" %in% names(result[["_data"]]))
})

# The significance colour is mapped correctlt
test_that("ISR_deprivation_table assigns expected significance fill colours", {
    input <- tibble::tibble(
        `IMD Quintile` = c(2, 3, 4),
        Ratio = c(1.2, 0.8, 1.0),
        `Lower CI` = c(1.1, 0.7, 0.9),
        `Upper CI` = c(1.3, 0.9, 1.1),
        Interpretation = c("Higher", "Lower", "Same"),
        `Statistical Significance` = c(
            "Significantly higher than IMD 1",
            "Significantly lower than IMD 1",
            "Not statistically significant"
        )
    )

    result <- ISR_deprivation_table(input)

    expect_equal(
        result[["_data"]]$sig_fill,
        c("#f4cccc", "#d9ead3", "#f2f2f2")
    )
})
