
#' Resolve multiplier input for DSR calculations
#'
#' Internal helper used by [calculate_dsr()] to standardise the `multiplier`
#' argument. The function allows the multiplier to be supplied either as:
#'
#' * a positive numeric scalar (e.g. `100000`), or
#' * an unquoted column name in `data` containing multiplier values.
#'
#' The helper converts the input into a column named `multiplier` so that
#' downstream calculations can operate on a consistent structure.
#'
#' @param data A data frame containing the variables used for the DSR
#'   calculation.
#' @param multiplier Either a numeric scalar or an unquoted column name in
#'   `data` containing numeric multiplier values.
#'
#' @return
#' A data frame identical to `data` but containing a column named
#' `multiplier`. If `multiplier` was supplied as a scalar, a new column is
#' created. If supplied as a column name, that column is renamed to
#' `multiplier`.
#'
#' @details
#' The multiplier is used to scale the final directly age-standardised rate.
#' For example, a multiplier of `100000` produces rates per 100,000 population.
#'
#' When a multiplier column is supplied, the function checks that:
#'
#' * the column is numeric
#' * no values are missing
#' * all values are strictly greater than zero
#'
#' These checks ensure valid rate scaling during DSR calculation.
#'
#' @keywords internal
#' @noRd

# helper ------------------------------------------------------------------
resolve_multiplier <- function(data, multiplier) {
    mult_quo <- rlang::enquo(multiplier)

    # Case 1: bare column name
    if (rlang::quo_is_symbol(mult_quo)) {
        mult_name <- rlang::as_name(mult_quo)

        if (mult_name %in% names(data)) {
            data <- dplyr::rename(data, multiplier = !!mult_quo)

            if (!is.numeric(data$multiplier)) stop("multiplier column must be numeric")
            if (anyNA(data$multiplier)) stop("multiplier column cannot have missing values")
            if (any(data$multiplier <= 0)) stop("multiplier column values must be > 0")

            return(data)
        }
    }

    # Case 2: scalar numeric value
    mult_value <- rlang::eval_tidy(mult_quo)

    if (!is.numeric(mult_value)) stop("multiplier must be numeric or a numeric column")
    if (length(mult_value) != 1) stop("multiplier must be length 1 when not a column")
    if (is.na(mult_value)) stop("multiplier cannot be NA")
    if (mult_value <= 0) stop("multiplier must be greater than 0")

    dplyr::mutate(data, multiplier = mult_value)
}


#' Calculate directly age-standardised rates (DSRs)
#'
#' Calculate directly age-standardised rates (DSRs) and confidence intervals
#' from age-specific event counts, denominators, and a standard population.
#'
#' This function supports grouped or ungrouped data. When `data` is grouped,
#' one DSR is calculated for each grouping set.
#'
#' Confidence intervals are calculated using Byar's method with Dobson
#' adjustment. When events are not independent, an alternative variance
#' calculation can be used by supplying event frequency data.
#'
#' @param data A data frame or tibble containing observed events, population
#'   denominators, and standard population values.
#' @param x Unquoted column name in `data` containing the observed number of
#'   events for each age band (or other standardisation category).
#' @param n Unquoted column name in `data` containing the population
#'   denominators for each age band.
#' @param stdpop Unquoted column name in `data` containing the standard
#'   population for each age band.
#' @param type Character string controlling which columns are returned.
#'   Must be one of:
#'   \describe{
#'     \item{`"value"`}{Return only the DSR value column.}
#'     \item{`"lower"`}{Return only the lower confidence limit column(s).}
#'     \item{`"upper"`}{Return only the upper confidence limit column(s).}
#'     \item{`"standard"`}{Return DSR values and confidence limits, excluding
#'       metadata columns.}
#'     \item{`"full"`}{Return DSR values, confidence limits, and metadata
#'       columns.}
#'   }
#' @param confidence Numeric confidence level expressed either as a proportion
#'   between `0.9` and `1`, or as a percentage between `90` and `100`.
#'   Alternatively, use `c(0.95, 0.998)` to return both 95\% and 99.8\%
#'   confidence intervals.
#' @param multiplier Either:
#'   \itemize{
#'     \item a positive numeric scalar, such as `100000`, or
#'     \item an unquoted column name in `data` containing positive numeric
#'       multiplier values.
#'   }
#'   The multiplier is used to scale the final DSR, for example to express a
#'   rate per 1,000 or per 100,000 population. If supplied as a column, it must
#'   be constant within each group.
#' @param independent_events Logical. If `TRUE` (default), events are assumed
#'   to be independent. If `FALSE`, confidence intervals are adjusted using
#'   event frequency data supplied via `eventfreq` and `ageband`.
#' @param eventfreq Unquoted column name in `data` containing event frequency
#'   categories. Required only when `independent_events = FALSE`.
#' @param ageband Unquoted column name in `data` containing the age band (or
#'   other standardisation category). Required only when
#'   `independent_events = FALSE`.
#'
#' @details
#' The function first standardises the selected columns internally to `x`, `n`,
#' and `stdpop`, validates the inputs, and then calculates the directly
#' age-standardised rate.
#'
#' For independent events, the DSR and confidence intervals are calculated in a
#' single pass.
#'
#' For non-independent events, `x` should represent the number of unique
#' individuals within each event frequency category, rather than the total
#' number of events. In that case, the function first derives a custom variance
#' and then applies it in the final DSR calculation.
#'
#' This implementation returns DSRs and confidence intervals for all valid input
#' rows and does not suppress results when the total count is less than 10.
#' @return A tibble with one row per group, or one row overall if `data` is not
#'   grouped. Depending on the value of `type`, the output may include:
#'   \describe{
#'     \item{`total_count`}{Total event count across age bands.}
#'     \item{`total_pop`}{Total denominator across age bands.}
#'     \item{`value`}{Directly age-standardised rate.}
#'     \item{`lowercl`, `uppercl`}{Confidence limits when a single confidence
#'       level is requested.}
#'     \item{`lower95_0cl`, `upper95_0cl`, `lower99_8cl`, `upper99_8cl`}{
#'       Confidence limits when both 95\% and 99.8\% intervals are requested.}
#'     \item{`confidence`}{Confidence level used in the calculation.}
#'     \item{`statistic`}{Description of the rate scale, for example
#'       `"dsr per 100000"`.}
#'     \item{`method`}{Method used to calculate the confidence intervals.}
#'   }
#'
#' @section Source:
#' This function is adapted from the `phe_DSR.R` implementation in the
#' \pkg{PHEindicatormethods} package:
#' \url{https://github.com/dhsc-govuk/PHEindicatormethods/blob/master/R/phe_DSR.R}
#'
#' @references
#' Breslow NE, Day NE. \emph{Statistical methods in cancer research, volume II:
#' The design and analysis of cohort studies}. Lyon: International Agency for
#' Research on Cancer, World Health Organisation; 1987.
#'
#' Dobson A, Kuulasmaa K, Eberle E, Scherer J. Confidence intervals for
#' weighted sums of Poisson parameters. \emph{Statistics in Medicine}.
#' 1991;10:457-462.
#'
#' Fingertips Public Health Technical Guidance:
#' \url{https://fingertips.phe.org.uk/profile/guidance/supporting-information/PH-methods/}
#'
#' @examples
#' library(dplyr)
#'
#' dsr_data <- data.frame(
#'   area = rep(c("North", "South"), each = 3),
#'   ageband = rep(c("0-19", "20-64", "65+"), times = 2),
#'   events = c(12, 25, 40, 10, 18, 35),
#'   population = c(5000, 12000, 3000, 4800, 11000, 3200),
#'   std_pop = c(5500, 12500, 3500, 5500, 12500, 3500)
#' )
#'
#' # Basic grouped DSR
#' dsr_data %>%
#'   dplyr::group_by(area) %>%
#'   calculate_dsr(
#'     x = events,
#'     n = population,
#'     stdpop = std_pop
#'   )
#'
#' # Return both 95% and 99.8% confidence intervals
#' dsr_data %>%
#'   dplyr::group_by(area) %>%
#'   calculate_dsr(
#'     x = events,
#'     n = population,
#'     stdpop = std_pop,
#'     confidence = c(0.95, 0.998)
#'   )
#'
#' # Use a scalar multiplier
#' dsr_data %>%
#'   dplyr::group_by(area) %>%
#'   calculate_dsr(
#'     x = events,
#'     n = population,
#'     stdpop = std_pop,
#'     multiplier = 1000
#'   )
#'
#' # Use a multiplier column
#' dsr_data2 <- dsr_data %>%
#'   dplyr:mutate(rate_multiplier = 100000)
#'
#' dsr_data2 %>%
#'   dplyr::group_by(area) %>%
#'   calculate_dsr(
#'     x = events,
#'     n = population,
#'     stdpop = std_pop,
#'     multiplier = rate_multiplier
#'   )
#'
#' # Return a reduced output without metadata columns
#' dsr_data %>%
#'   dplyr::group_by(area) %>%
#'   calculate_dsr(
#'     x = events,
#'     n = population,
#'     stdpop = std_pop,
#'     type = "standard"
#'   )
#'
#' # Example for non-independent events
#' if (requireNamespace("tidyr", quietly = TRUE)) {
#'   library(tidyr)
#'
#'   dsr_freq_data <- dsr_data %>%
#'     dplyr:mutate(
#'       freq_1 = pmax(events - 2, 0),
#'       freq_2 = 1,
#'       freq_3 = 1
#'     ) %>%
#'     select(-events) %>%
#'     pivot_longer(
#'       cols = c(freq_1, freq_2, freq_3),
#'       names_to = "event_frequency",
#'       values_to = "unique_people"
#'     ) %>%
#'     dplyr:mutate(
#'       event_frequency = dplyr::case_when(
#'         event_frequency == "freq_1" ~ 1L,
#'         event_frequency == "freq_2" ~ 2L,
#'         TRUE ~ 3L
#'       )
#'     )
#'
#'   dsr_freq_data %>%
#'     dplyr::group_by(area) %>%
#'     calculate_dsr(
#'       x = unique_people,
#'       n = population,
#'       stdpop = std_pop,
#'       independent_events = FALSE,
#'       eventfreq = event_frequency,
#'       ageband = ageband
#'     )
#' }
#'
#' @importFrom rlang is_bool enquo quo_is_missing quo_is_symbol as_name eval_tidy
#' @importFrom dplyr rename mutate summarise select group_by ungroup filter
#' @importFrom dplyr left_join n_distinct case_when if_else pull group_vars
#' @importFrom dplyr starts_with across all_of pick group_cols first
#' @export
# main function -----------------------------------------------------------
calculate_dsr <- function(data,
                          x,
                          n,
                          stdpop,
                          type = "full",
                          confidence = 0.95,
                          multiplier = 1e5,
                          independent_events = TRUE,
                          eventfreq = NULL,
                          ageband = NULL) {

    # ---- basic checks ----
    if (missing(data) || missing(x) || missing(n) || missing(stdpop)) {
        stop("function calculate_dsr requires at least 4 arguments: data, x, n, stdpop")
    }

    if (!is.data.frame(data)) {
        stop("data must be a data frame object")
    }

    # ---- standardise main columns ----
    data <- data %>%
        dplyr::rename(
            x = {{ x }},
            n = {{ n }},
            stdpop = {{ stdpop }}
        )

    # ---- validate main columns ----
    if (!is.numeric(data$x)) stop("field x must be numeric")
    if (!is.numeric(data$n)) stop("field n must be numeric")
    if (!is.numeric(data$stdpop)) stop("field stdpop must be numeric")

    if (anyNA(data$n)) stop("field n cannot have missing values")
    if (anyNA(data$stdpop)) stop("field stdpop cannot have missing values")

    if (any(data$x < 0, na.rm = TRUE)) stop("numerators must all be greater than or equal to zero")
    if (any(data$n <= 0)) stop("denominators must all be greater than zero")
    if (any(data$stdpop < 0)) stop("stdpop must all be greater than or equal to zero")

    if (!(type %in% c("value", "lower", "upper", "standard", "full"))) {
        stop("type must be one of value, lower, upper, standard or full")
    }

    if (!is.numeric(confidence)) stop("confidence must be numeric")
    if (length(confidence) > 2) stop("a maximum of two confidence levels can be provided")

    if (length(confidence) == 2) {
        if (!(confidence[1] == 0.95 && confidence[2] == 0.998)) {
            stop("two confidence levels can only be produced if they are specified as 0.95 and 0.998")
        }
    } else if ((confidence < 0.9) || (confidence > 1 & confidence < 90) || (confidence > 100)) {
        stop("confidence level must be between 90 and 100 or between 0.9 and 1")
    }

    # ---- resolve multiplier (scalar or column) ----
    data <- resolve_multiplier(data, {{ multiplier }})

    # ---- validate independent_events ----
    if (!rlang::is_bool(independent_events)) {
        stop("independent_events must be TRUE or FALSE")
    }

    # ---- independent events path ----
    if (independent_events) {
        dsrs <- dsr_inner(
            data = data,
            type = type,
            confidence = confidence,
            rtn_nonindependent_vardsr = FALSE,
            use_nonindependent_vardsr = FALSE
        )
        return(dsrs)
    }

    # ---- non-independent events validation ----
    if (missing(eventfreq)) {
        stop("function calculate_dsr requires an eventfreq column to be specified when independent_events is FALSE")
    }

    if (missing(ageband)) {
        stop("function calculate_dsr requires an ageband column to be specified when independent_events is FALSE")
    }

    data <- data %>%
        dplyr::rename(
            eventfreq = {{ eventfreq }},
            ageband = {{ ageband }}
        )

    if (!is.numeric(data$eventfreq)) stop("eventfreq field must be numeric")
    if (anyNA(data$eventfreq)) stop("eventfreq field must not have any missing values")
    if (anyNA(data$ageband)) stop("ageband field must not have any missing values")

    data <- data %>% dplyr::group_by(eventfreq, .add = TRUE)

    grps <- dplyr::group_vars(data)[!dplyr::group_vars(data) %in% "eventfreq"]

    check_groups <- data %>%
        dplyr::group_by(pick(all_of(c(grps, "ageband")))) %>%
        dplyr::summarise(
            num_n = n_distinct(.data$n),
            num_stdpop = n_distinct(.data$stdpop),
            .groups = "drop"
        ) %>%
        filter(.data$num_n > 1 | .data$num_stdpop > 1)

    if (nrow(check_groups) > 0) {
        stop(paste0(
            "There are rows with the same grouping variables and ageband ",
            "but with different populations (n) or standard populations (stdpop)"
        ))
    }

    # variance calculation for non-independent events
    freq_var <- data %>%
        dsr_inner(
            type = type,
            confidence = confidence,
            rtn_nonindependent_vardsr = TRUE,
            use_nonindependent_vardsr = FALSE
        ) %>%
        dplyr::mutate(freqvars = .data$vardsr * .data$eventfreq^2) %>%
        dplyr::group_by(pick(all_of(grps))) %>%
        dplyr::summarise(custom_vardsr = sum(.data$freqvars), .groups = "drop")

    # collapse events; retain multiplier
    event_data <- data %>%
        dplyr::mutate(events = .data$eventfreq * .data$x) %>%
        dplyr::group_by(pick(all_of(c(grps, "ageband", "n", "stdpop")))) %>%
        dplyr::summarise(
            x = sum(.data$events, na.rm = TRUE),
            multiplier = first(.data$multiplier),
            .groups = "drop"
        )

    dsrs <- event_data %>%
        dplyr::left_join(freq_var, by = grps) %>%
        dplyr::group_by(pick(all_of(grps))) %>%
        dsr_inner(
            type = type,
            confidence = confidence,
            rtn_nonindependent_vardsr = FALSE,
            use_nonindependent_vardsr = TRUE
        )

    dsrs
}


# inner function ----------------------------------------------------------
#' Internal helper for calculate_dsr()
#'
#' Computes directly age-standardised rates after inputs have been standardised
#' and validated by `calculate_dsr()`.
#'
#' @keywords internal
#' @noRd

dsr_inner <- function(data,
                       type,
                       confidence,
                       rtn_nonindependent_vardsr = FALSE,
                       use_nonindependent_vardsr = FALSE) {

    if (isTRUE(rtn_nonindependent_vardsr) &&
        ("custom_vardsr" %in% names(data) || isTRUE(use_nonindependent_vardsr))) {
        stop("cannot get nonindependent vardsr and use nonindependent vardsr in the same execution")
    }

    confidence[confidence >= 90] <- confidence[confidence >= 90] / 100
    conf1 <- confidence[1]
    conf2 <- confidence[2]

    if (!"multiplier" %in% names(data)) {
        stop("internal error: multiplier column not found")
    }

    if (!is.numeric(data$multiplier)) stop("multiplier column must be numeric")
    if (anyNA(data$multiplier)) stop("multiplier column cannot have missing values")
    if (any(data$multiplier <= 0)) stop("multiplier column values must be > 0")

    grps <- dplyr::group_vars(data)
    if (length(grps) > 0) {
        mult_check <- data %>%
            dplyr::summarise(nuniq_mult = n_distinct(.data$multiplier), .groups = "drop")

        if (any(mult_check$nuniq_mult > 1)) {
            stop("Within-group multiplier must be constant.")
        }
    }

    if (!isTRUE(use_nonindependent_vardsr)) {
        method <- "Dobson"
        data <- dplyr::mutate(data, custom_vardsr = NA_real_)
    } else {
        method <- "Dobson, with confidence adjusted for non-independent events"
    }

    dsrs <- data %>%
        dplyr::mutate(
            wt_rate = PHEindicatormethods:::na.zero(.data$x) * .data$stdpop / .data$n,
            sq_rate = PHEindicatormethods:::na.zero(.data$x) * (.data$stdpop / .data$n)^2
        ) %>%
        dplyr::summarise(
            total_count = sum(.data$x, na.rm = TRUE),
            total_pop = sum(.data$n),
            base_value = sum(.data$wt_rate) / sum(.data$stdpop),
            vardsr = case_when(
                isTRUE(use_nonindependent_vardsr) ~ unique(.data$custom_vardsr),
                TRUE ~ 1 / sum(.data$stdpop)^2 * sum(.data$sq_rate)
            ),
            multiplier = first(.data$multiplier),
            .groups = "keep"
        )

    if (!isTRUE(rtn_nonindependent_vardsr)) {
        dsrs <- dsrs %>%
            ungroup() %>%
            dplyr::mutate(
                value = .data$base_value * .data$multiplier,
                lowercl = .data$value + sqrt(.data$vardsr / .data$total_count) *
                    (PHEindicatormethods:::byars_lower(.data$total_count, conf1) - .data$total_count) *
                    .data$multiplier,
                uppercl = .data$value + sqrt(.data$vardsr / .data$total_count) *
                    (PHEindicatormethods:::byars_upper(.data$total_count, conf1) - .data$total_count) *
                    .data$multiplier,
                lower99_8cl = .data$value + sqrt(.data$vardsr / .data$total_count) *
                    (PHEindicatormethods:::byars_lower(.data$total_count, 0.998) - .data$total_count) *
                    .data$multiplier,
                upper99_8cl = .data$value + sqrt(.data$vardsr / .data$total_count) *
                    (PHEindicatormethods:::byars_upper(.data$total_count, 0.998) - .data$total_count) *
                    .data$multiplier,
                confidence = paste0(confidence * 100, "%", collapse = ", "),
                statistic = paste("dsr per", format(.data$multiplier, scientific = FALSE)),
                method = method
            )

        if (!is.na(conf2)) {
            names(dsrs)[names(dsrs) == "lowercl"] <- "lower95_0cl"
            names(dsrs)[names(dsrs) == "uppercl"] <- "upper95_0cl"
        } else {
            dsrs <- dplyr::select(dsrs, -dplyr::any_of(c("lower99_8cl", "upper99_8cl"))) # remove 99.8% CI columns
        }
    }

    if (isTRUE(rtn_nonindependent_vardsr)) {
        dsrs <- dplyr::select(
            dsrs,
            dplyr::group_cols(),
            "vardsr"
            )
    } else if (type == "lower") {

        dsrs <- dplyr::select(
            dsrs,

            dplyr::any_of(c("total_count", "total_pop", "value",
                            "vardsr", "confidence", "statistic", "method")),
            -dplyr::starts_with("upper"))
    } else if (type == "upper") {

        dsrs <- dplyr::select(
            dsrs,
            -dplyr::any_of(c("total_count", "total_pop", "value",
                             "vardsr", "confidence", "statistic", "method")),
            -dplyr::starts_with("lower")
        )

    } else if (type == "value") {

        dsrs <- dplyr::select(
            dsrs,
            -dplyr::any_of(c("total_count", "total_pop",
                             "vardsr", "confidence", "statistic", "method")),
            -dplyr::starts_with("lower"),
            -dplyr::starts_with("upper")
        )

    } else if (type == "standard") {

        dsrs <- dplyr::select(
            dsrs,
            -dplyr::any_of(c("vardsr", "confidence", "statistic", "method"))
        )

    } else if (type == "full") {

        dsrs <- dplyr::select(dsrs, -dplyr::any_of("vardsr"))

    }


    dsrs
}
