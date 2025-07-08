.onLoad <- function(libname, Snkmaker) {
  shiny::addResourcePath(
    prefix = "logo_resources",
    directoryPath = system.file("www", package = Snkmaker)
  )
}
