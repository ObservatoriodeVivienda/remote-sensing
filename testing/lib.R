# List of packages for session
.packages = c(
  'ggplot2',
  'plyr',
  'dplyr',
  'dtplyr',
  'data.table',
  'tidyr',
  'jpeg'
  )

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session 
lapply(.packages, require, character.only = T)

## Changing the ggplot default theme
theme_set(theme_bw())
