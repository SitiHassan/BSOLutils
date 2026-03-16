
# Test output structure
test_that("create_ISR_deprivation_output returns expected columns", {
    input <- tibble::tibble(
        imd_quintile = 2,
        ratio = 1.234,
        lowerCI = 1.101,
        upperCI = 1.456
    )

    result <- create_ISR_deprivation_output(input)

    expect_named(
        result,
        c(
            "IMD Quintile",
            "Ratio",
            "Lower CI",
            "Upper CI",
            "Interpretation",
            "Statistical Significance"
        )
    )
})

# Test rounding
test_that("create_ISR_deprivation_output rounds numeric values to 2 decimals", {
    input <- tibble::tibble(
        imd_quintile = 2,
        ratio = 1.234,
        lowerCI = 1.101,
        upperCI = 1.456
    )

    result <- create_ISR_deprivation_output(input)

    expect_equal(result$Ratio, 1.23)
    expect_equal(result$`Lower CI`, 1.10)
    expect_equal(result$`Upper CI`, 1.46)
})

# Test significance classification

test_that("create_ISR_deprivation_output classifies statistical significance correctly", {
    input <- tibble::tibble(
        imd_quintile = c(2, 3, 4),
        ratio = c(1.2, 0.8, 1.0),
        lowerCI = c(1.05, 0.70, 0.95),
        upperCI = c(1.30, 0.90, 1.05)
    )

    result <- create_ISR_deprivation_output(input)

    expect_equal(
        result$`Statistical Significance`,
        c(
            "Significantly higher than IMD 1",
            "Significantly lower than IMD 1",
            "Not statistically significant"
        )
    )
})

# Test for when ratio == 1

test_that("create_ISR_deprivation_output handles ratio equal to 1", {
    input <- tibble::tibble(
        imd_quintile = 2,
        ratio = 1,
        lowerCI = 1,
        upperCI = 1
    )

    result <- create_ISR_deprivation_output(input)

    expect_equal(result$Interpretation, "Same as IMD 1 (95% CI: 0.0% to 0.0%)")
    expect_equal(result$`Statistical Significance`, "Not statistically significant")
})
