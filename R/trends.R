runApp <-
function() {
  appDir <- system.file("shiny", "trends", package = "trends")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing the `trends` package.",
         call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
