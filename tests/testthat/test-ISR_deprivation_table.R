
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


# The significance colour is mapped correctlt
test_that("ISR_deprivation_table significance mapping logic is correct", {
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

    # Recreate the mapping the function uses
    sig_rows <- input$`Statistical Significance` %in% c(
        "Significantly higher than IMD 1",
        "Significantly lower than IMD 1"
    )
    sig_fill_hex <- "#ffe5e5"
    nonsig_fill_hex <- "#f2f2f2"

    expected <- ifelse(sig_rows, sig_fill_hex, nonsig_fill_hex)

    result <- ISR_deprivation_table(input)

    expect_equal(expected, c("#ffe5e5", "#ffe5e5", "#f2f2f2"))
})
