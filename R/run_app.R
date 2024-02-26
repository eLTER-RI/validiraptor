#' run the app locally
#' @export
run_app <- function() shiny::runApp(appDir = file.path(system.file(package = "validiraptor"), 'app'))

