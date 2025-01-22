if (!require(devtools)) {
  install.packages('devtools', type = 'source', INSTALL_opts = '--byte-compile')
}
devtools::install_github(
  c(
    'OscarKjell/text',
    'tidyverse/ellmer',
    'rstudio/shinyvalidate',
    'daattali/shinycssloaders'
  ),
  build = TRUE
)

install.packages('SnowballC', type = 'source', INSTALL_opts = '--byte-compile')
