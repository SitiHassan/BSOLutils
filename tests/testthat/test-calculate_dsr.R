## Unit tests for `calculate_dsr()`

library(dplyr)

test_that("calculate_dsr works for basic grouped data", {
    df <- tibble::tibble(
        area = rep(c("North", "South"), each = 3),
        ageband = rep(c("0-19", "20-64", "65+"), times = 2),
        events = c(12, 25, 40, 10, 18, 35),
        population = c(5000, 12000, 3000, 4800, 11000, 3200),
        std_pop = c(5500, 12500, 3500, 5500, 12500, 3500)
    )

    result <- df %>%
        dplyr::group_by(area) %>%
        calculate_dsr(
            x = events,
            n = population,
            stdpop = std_pop
        )

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 2)
    expect_true(all(c("area", "total_count", "total_pop", "value", "lowercl", "uppercl",
                      "confidence", "statistic", "method") %in% names(result)))
})

test_that("calculate_dsr works with scalar multiplier", {
    df <- tibble::tibble(
        area = rep(c("North", "South"), each = 3),
        ageband = rep(c("0-19", "20-64", "65+"), times = 2),
        events = c(12, 25, 40, 10, 18, 35),
        population = c(5000, 12000, 3000, 4800, 11000, 3200),
        std_pop = c(5500, 12500, 3500, 5500, 12500, 3500)
    )

    result_1000 <- df %>%
        dplyr::group_by(area) %>%
        calculate_dsr(
            x = events,
            n = population,
            stdpop = std_pop,
            multiplier = 1000
        )

    result_100000 <- df %>%
        dplyr::group_by(area) %>%
        calculate_dsr(
            x = events,
            n = population,
            stdpop = std_pop,
            multiplier = 100000
        )

    expect_true(all(result_1000$statistic == "dsr per 1000"))
    expect_true(all(result_100000$statistic == "dsr per 100000"))
    expect_false(identical(result_1000$value, result_100000$value))
})

test_that("calculate_dsr works with multiplier supplied as a column", {
    df <- tibble::tibble(
        area = rep(c("North", "South"), each = 3),
        ageband = rep(c("0-19", "20-64", "65+"), times = 2),
        events = c(12, 25, 40, 10, 18, 35),
        population = c(5000, 12000, 3000, 4800, 11000, 3200),
        std_pop = c(5500, 12500, 3500, 5500, 12500, 3500),
        rate_multiplier = 100000
    )

    result <- df %>%
        dplyr::group_by(area) %>%
        calculate_dsr(
            x = events,
            n = population,
            stdpop = std_pop,
            multiplier = rate_multiplier
        )

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 2)
    expect_true(all(result$statistic == "dsr per 100000"))
})

test_that("calculate_dsr returns both 95% and 99.8% confidence intervals", {
    df <- tibble::tibble(
        area = rep(c("North", "South"), each = 3),
        ageband = rep(c("0-19", "20-64", "65+"), times = 2),
        events = c(12, 25, 40, 10, 18, 35),
        population = c(5000, 12000, 3000, 4800, 11000, 3200),
        std_pop = c(5500, 12500, 3500, 5500, 12500, 3500)
    )

    result <- df %>%
        dplyr::group_by(area) %>%
        calculate_dsr(
            x = events,
            n = population,
            stdpop = std_pop,
            confidence = c(0.95, 0.998)
        )

    expect_true(all(c("lower95_0cl", "upper95_0cl", "lower99_8cl", "upper99_8cl") %in% names(result)))
    expect_false(any(c("lowercl", "uppercl") %in% names(result)))
})

test_that("calculate_dsr returns expected columns for type = value", {
    df <- tibble::tibble(
        area = rep(c("North", "South"), each = 3),
        ageband = rep(c("0-19", "20-64", "65+"), times = 2),
        events = c(12, 25, 40, 10, 18, 35),
        population = c(5000, 12000, 3000, 4800, 11000, 3200),
        std_pop = c(5500, 12500, 3500, 5500, 12500, 3500)
    )

    result <- df %>%
        dplyr::group_by(area) %>%
        calculate_dsr(
            x = events,
            n = population,
            stdpop = std_pop,
            type = "value"
        )

    expect_true(all(c("area", "value") %in% names(result)))
    expect_false(any(c("lowercl", "uppercl", "confidence", "method", "statistic") %in% names(result)))
})

test_that("calculate_dsr errors when required columns are invalid", {
    df <- tibble::tibble(
        area = rep(c("North", "South"), each = 3),
        ageband = rep(c("0-19", "20-64", "65+"), times = 2),
        events = c(12, 25, 40, 10, 18, 35),
        population = c(5000, 12000, 3000, 4800, 11000, 3200),
        std_pop = c(5500, 12500, 3500, 5500, 12500, 3500)
    )

    expect_error(
        calculate_dsr(df, x = missing_col, n = population, stdpop = std_pop)
    )

    expect_error(
        calculate_dsr(df, x = events, n = population, stdpop = std_pop, type = "wrong"),
        "type must be one of value, lower, upper, standard or full"
    )

    expect_error(
        calculate_dsr(df, x = events, n = population, stdpop = std_pop, confidence = 0.5),
        "confidence level must be between 90 and 100 or between 0.9 and 1"
    )

    expect_error(
        calculate_dsr(df, x = events, n = population, stdpop = std_pop, multiplier = -1000),
        "multiplier must be greater than 0"
    )
})

test_that("calculate_dsr errors when multiplier column is invalid", {
    df_bad <- tibble::tibble(
        area = rep(c("North", "South"), each = 3),
        ageband = rep(c("0-19", "20-64", "65+"), times = 2),
        events = c(12, 25, 40, 10, 18, 35),
        population = c(5000, 12000, 3000, 4800, 11000, 3200),
        std_pop = c(5500, 12500, 3500, 5500, 12500, 3500),
        rate_multiplier = c(100000, 100000, NA, 100000, 100000, 100000)
    )

    expect_error(
        df_bad %>%
            dplyr::group_by(area) %>%
            calculate_dsr(
                x = events,
                n = population,
                stdpop = std_pop,
                multiplier = rate_multiplier
            ),
        "multiplier column cannot have missing values"
    )
})

test_that("calculate_dsr errors when independent_events is not logical", {
    df <- tibble::tibble(
        area = rep(c("North", "South"), each = 3),
        ageband = rep(c("0-19", "20-64", "65+"), times = 2),
        events = c(12, 25, 40, 10, 18, 35),
        population = c(5000, 12000, 3000, 4800, 11000, 3200),
        std_pop = c(5500, 12500, 3500, 5500, 12500, 3500)
    )

    expect_error(
        calculate_dsr(
            df,
            x = events,
            n = population,
            stdpop = std_pop,
            independent_events = "yes"
        ),
        "independent_events must be TRUE or FALSE"
    )
})

test_that("calculate_dsr works for non-independent events", {
    df_freq <- tibble::tibble(
        area = rep(c("North", "South"), each = 6),
        ageband = rep(c("0-19", "20-64", "65+"), times = 4),
        event_frequency = rep(c(1, 2), each = 3, times = 2),
        unique_people = c(10, 20, 15, 1, 2, 3, 8, 16, 12, 1, 1, 2),
        population = c(5000, 12000, 3000, 5000, 12000, 3000,
                       4800, 11000, 3200, 4800, 11000, 3200),
        std_pop = c(5500, 12500, 3500, 5500, 12500, 3500,
                    5500, 12500, 3500, 5500, 12500, 3500)
    )

    result <- df_freq %>%
        dplyr::group_by(area) %>%
        calculate_dsr(
            x = unique_people,
            n = population,
            stdpop = std_pop,
            independent_events = FALSE,
            eventfreq = event_frequency,
            ageband = ageband
        )

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 2)
    expect_true(all(c("value", "method") %in% names(result)))
    expect_true(all(grepl("non-independent events", result$method)))
})

test_that("calculate_dsr errors for missing non-independent event arguments", {
    df <- tibble::tibble(
        area = rep(c("North", "South"), each = 3),
        ageband = rep(c("0-19", "20-64", "65+"), times = 2),
        unique_people = c(10, 20, 15, 8, 16, 12),
        population = c(5000, 12000, 3000, 4800, 11000, 3200),
        std_pop = c(5500, 12500, 3500, 5500, 12500, 3500)
    )

    expect_error(
        calculate_dsr(
            df,
            x = unique_people,
            n = population,
            stdpop = std_pop,
            independent_events = FALSE
        ),
        "function calculate_dsr requires an eventfreq column"
    )
})

test_that("calculate_dsr does not suppress rates when total_count is less than 10", {
    df <- tibble::tibble(
        area = rep("North", 3),
        ageband = c("0-19", "20-64", "65+"),
        events = c(1, 2, 3),
        population = c(5000, 12000, 3000),
        std_pop = c(5500, 12500, 3500)
    )

    result <- df %>%
        dplyr::group_by(area) %>%
        calculate_dsr(
            x = events,
            n = population,
            stdpop = std_pop
        )

    expect_false(is.na(result$value))
})
