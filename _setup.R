if (!require(devtools)) {
  install.packages('devtools', type = 'source', INSTALL_opts = '--byte-compile')
}
devtools::install_github(
  'OscarKjell/text',
  build = TRUE
)

install.packages('SnowballC', type = 'source', INSTALL_opts = '--byte-compile')
