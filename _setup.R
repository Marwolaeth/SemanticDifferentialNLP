if (!require(devtools)) {
  install.packages('devtools', type = 'source', INSTALL_opts = '--byte-compile')
}
devtools::install_github(
  'OscarKjell/text',
  build = TRUE
)

remotes::install_github('dreamRs/shinybusy')
