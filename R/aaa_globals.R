# global variables are defined in this file. the file has been named such that
# it will be executed before any other file


# global variables are defined in this file. the file has been named such that
# it will be executed before any other file
.onLoad <- function(libname, pkgname){

    sysfonts::font_add_google("Open Sans", "Open Sans")
    #sysfonts::font_add("Arial", "arial.ttf", "arialbd.ttf", "ariali.ttf", "arialbi.ttf")
    #sysfonts::font_add("Calibri", "calibri.ttf", "calibrib.ttf", "calibrii.ttf", "calibriz.ttf")
    #sysfonts::font_add("Segoe UI", "segoeui.ttf", "segoeuib.ttf", "segoeuii.ttf", "segoeuiz.ttf")
    showtext::showtext_auto()


}



# taken from usethis: https://github.com/r-lib/usethis
#' @noRd
#' @import usethis fs
scoped_path_r <- function(scope = c("user", "project"), ..., envvar = NULL) {
    scope <- match.arg(scope)

    # Try environment variable in user scopes
    if (scope == "user" && !is.null(envvar)) {
        env <- Sys.getenv(envvar, unset = "")
        if (!identical(env, "")) {
            return(user_path_prep(env))
        }
    }

    root <- switch(scope,
                   user = fs::path_home_r(),
                   project = usethis::proj_get()
    )
    path(root, ...)
}

user_path_prep <- function(path) {
    ## usethis uses fs's notion of home directory
    ## this ensures we are consistent about that
    path_expand(path)
}

create_directory <- function(path) {
    if (dir_exists(path)) {
        return(invisible(FALSE))
    } else if (file_exists(path)) {
        ui_stop("{ui_path(path)} exists but is not a directory.")
    }

    dir_create(path, recurse = TRUE)
    ui_done("Creating {ui_path(path)}")
    invisible(TRUE)
}
