
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Enjoy plotting!")

  pdfFonts <- grDevices::pdfFonts
  extrafont::loadfonts("pdf" ,quiet = TRUE)
}
