# Prepare colour gradients for ggplot

install.packages(c("magick", "imager"))


library(magick)

gradient_palette <- function(path, n = 100) {
    img <- image_read(path)

    # Convert to raster matrix of hex colours
    ras <- as.raster(img)

    h <- dim(ras)[1]
    w <- dim(ras)[2]

    # Horizontal gradient → sample columns
    xs <- seq(1, w, length.out = n)

    cols <- sapply(xs, function(x) {
        x <- round(x)
        rgb_col <- colMeans(col2rgb(ras[, x]))
        rgb(rgb_col[1]/255, rgb_col[2]/255, rgb_col[3]/255)
    })

    cols
}

palette <- gradient_palette("./dev/gradient.bmp", n = 10)




library(magick)

gradient_palette <- function(path, n = 100) {
    img <- image_read(path)

    # Convert to raster matrix of hex colours
    ras <- as.raster(img)

    h <- dim(ras)[1]
    w <- dim(ras)[2]

    # Horizontal gradient → sample columns
    xs <- seq(1, w, length.out = n)

    cols <- sapply(xs, function(x) {
        x <- round(x)
        rgb_col <- colMeans(col2rgb(ras[, x]))
        rgb(rgb_col[1]/255, rgb_col[2]/255, rgb_col[3]/255)
    })

    cols
}


palette <- gradient_palette("./dev/gradient.bmp", n = 10)
