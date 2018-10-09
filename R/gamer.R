
#' Run the gamer shiny app
#' @export
gamer <- function() {
  app <- system.file("shiny-examples", "gamer.R", package = "gamer")
  if (app == "") {
    stop("Could not find example directory. Try re-installing `gamer`.", call. = FALSE)
  }

  shiny::runApp(app, display.mode = "normal")
}