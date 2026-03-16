#' Automatic ggplot2 ICB theme
#'
#' A theme designed to complement the ICB colour palette.
#' Uses a light, neutral background and minimalist styling.
#'
#' @param base_size Base font size.
#' @param base_family Base font family.
#'
#' @return A ggplot2 theme object.
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point(aes(colour = mpg)) +
#'   scale_colour_icb(discrete = FALSE) +
#'   theme_icb()
#'
#' @export
theme_icb <- function(base_size = 12, base_family = "Open Sans") {
    ggplot2::theme_classic(base_size = base_size, base_family = base_family) %+replace%
        ggplot2::theme(
            panel.background = element_blank()
            , panel.grid = element_blank()
            , plot.title = element_text(face = "bold", size = 16)
            , plot.subtitle = element_text(face = "italic",size = 10)
            , axis.line = element_line(colour = "#808080", linewidth = 0.75)
            , strip.background = element_rect(fill = "#b2b7b9")
            #, panel.grid.major = element_line(colour = "#b3b3b3", linewidth = 0.4)
            #, panel.grid.minor = element_line(colour = "#cfcfcf", linewidth = 0.4)
            # panel.grid.major = ggplot2::element_line(colour = "#DDDDDD"),
            # panel.grid.minor = ggplot2::element_blank(),
            # plot.title = ggplot2::element_text(face = "bold", colour = "#333333"),
            # axis.title = ggplot2::element_text(face = "bold", colour = "#444444"),
            # axis.text = ggplot2::element_text(colour = "#444444")
        )
}


#' ICB Theme Colours
#'
#' This function returns the colours that are used in the ICB theme as
#' a named vector of RGB values in hexadecimal form.
#'
#' If no arguments are passed to the function, then all of the colours are
#' returned. If only specific colours are required, then the names of the
#' colours that are required can be passed as strings to the function, and only
#' those colours will be returned.
#'
#' If a "palette" is selected then no arguments should be passed to ...
#'
#' Only one palette can be selected at a time.
#'
#' @param ... individual colours that you wish to get the values of
#' @param palette a name of a palette to select the colours of
#'
#' @return a named vector of RGB colours in hexadecimal form
#' @export
#'
#' @examples
#' # show all of the colours
#' icb_theme_cols()
#' # or, just show some colours
#' icb_theme_cols("green", "orange", "deep_navy", "nhs_blue")
#' # or, select a single palette
#' icb_theme_cols(palette = "ics_orange")
icb_theme_cols <- function(...,
                            palette = c(
                                NA,
                                "cluster",
                                "bsol",
                                "alternative",
                                "ics_blue",
                                "ics_orange",
                                "ics_green",
                                "ics_purple",
                                "ics_navy"
                            )) {
    # take the ... argument's and convert to a unique vector
    dots <- unique(c(...))
    # verify that the palette argument is valid, i.e. it is in the list of allowed
    # values (from the argument definition)
    pallete <- match.arg(palette)

    # if a palette has not been selected, then return the selected colours
    if (is.na(pallete)) {
        # if no colour's have been specified, then select all
        if (length(dots) < 1) {
            dots <- names(icb_theme_colours)
        }
        # immediately return the selected colours
        return(icb_theme_colours[dots])
    }

    # if we are selecting a palette then we shouldn't be specifying colours to
    # select
    if (length(dots) > 0) {
        stop("cannot specify colours and a palette to use")
    }

    # return the colours that are part of this palette
    # Taken from ICS branding guidelines in 'Colour Palette'
    colours <- switch(palette,
                      "cluster" = c("cluster_green1",
                                     "cluster_green2",
                                     "cluster_turquoise",
                                     "cluster_lightblue",
                                     "cluster_blue",
                                     "cluster_darkblue",
                                     "cluster_purple",
                                     "cluster_purplepink",
                                     "cluster_pinkorgange",
                                     "cluster_orange"),
                      "bsol" = c("green", "light_blue", "orange", "deep_navy", "purple"),
                      "alternative" = c("light_blue", "black_navy", "white"),

                      "ics_blue" = c("light_blue", "white", "green"),
                      "ics_orange" = c("orange", "white", "deep_navy"),
                      "ics_green" = c("green", "purple", "deep_navy"),
                      "ics_purple" = c("purple", "blue", "deep_navy"),
                      "ics_navy" = c("deep_navy", "white", "orange")
    )

    # return the colours from the selected palette
    return(icb_theme_colours[colours])
}


#' @rdname icb_theme_gradient_colour_scales
#' @name icb_theme_gradient_colour_scales
#' @title ICB Gradient colour Scale
#'
#' @description
#' Generates a colour gradient
#'
#' @param palette The name of palette to use
#' @param discrete Optional: boolean to indicate that this scale is a discrete
#'    scale. Defaults to TRUE
#' @param reverse Optional: boolean to reverse the direction of the scale.
#'    Defaults to FALSE
#' @param ... additional arguments passed to the ggplot functions
#'
#' @return a scale object to be used with a ggplot object
#'
NULL

#' @export
#' @importFrom ggplot2 discrete_scale scale_color_gradientn
#' @rdname icb_theme_gradient_colour_scales
scale_colour_icb <- function(palette = c(
    "cluster",
    "bsol",
    "alternative",
    "ics_blue",
    "ics_orange",
    "ics_green",
    "ics_purple",
    "ics_navy"
),
discrete = TRUE,
reverse = FALSE, ...) {
    palette <- match.arg(palette)

    pal <- icb_theme_pal(palette = palette, reverse = reverse)

    if (discrete) {
        discrete_scale("colour", paste0("icb_theme_", palette), palette = pal, ...)
    } else {
        scale_color_gradientn(colours = pal(256), ...)
    }
}
#' @export
#' @rdname icb_theme_gradient_colour_scales
scale_color_icb <- scale_colour_icb

#' @export
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn
#' @rdname icb_theme_gradient_colour_scales
scale_fill_icb <- function(palette = c(
    "cluster",
    "bsol",
    "alternative",
    "ics_blue",
    "ics_orange",
    "ics_green",
    "ics_purple",
    "ics_navy"
),
discrete = TRUE,
reverse = FALSE, ...) {
    palette <- match.arg(palette)

    pal <- icb_theme_pal(palette = palette, reverse = reverse)

    if (discrete) {
        discrete_scale("fill", paste0("icb_theme_", palette), palette = pal, ...)
    } else {
        scale_fill_gradientn(colours = pal(256), ...)
    }
}



