## Unit tests for `prop_ci()`

# basic behaviour: output structure and sensible bounds

test_that("`prop_ci()` basic behaviour and bounds", {
  res <- prop_ci(50, 100)
  expect_named(res, c("Rate", "LowerCI", "UpperCI"))
  expect_equal(res$Rate, 0.5)
  expect_equal(res$LowerCI, 0.403831530)
  expect_equal(res$UpperCI, 0.596168470)
  expect_true(res$LowerCI < res$Rate && res$Rate < res$UpperCI)

  res2 <- prop_ci(25, 50, ci = 0.9)
  expect_equal(res2$Rate, 0.5)
  expect_true(res2$LowerCI < res2$Rate && res2$Rate < res2$UpperCI)
})

# zero numerator / denominator behaviour

test_that("`prop_ci()` handles zero numerator or denominator", {
  expect_equal(prop_ci(0, 10)$Rate, 0)
  expect_warning(expect_warning(a <- prop_ci(1, 0)))
  expect_warning(expect_warning(b <- prop_ci(1, 0)))
  expect_true(is.nan(a$LowerCI))
  expect_true(is.infinite(b$Rate) )
})

# placeholder for a known value

test_that("`prop_ci()` known value placeholder", {
  # this result could be calculated externally
  expect_equal(prop_ci(1, 4)$Rate, 0.25)
})

prop_ci
sqrt
