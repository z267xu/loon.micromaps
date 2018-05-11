
library(micromap)

data('USstates')
data('edPov')

devtools::use_data(USstates, overwrite = TRUE)
devtools::use_data(edPov, overwrite = TRUE)


us_lungmort <- read.csv('data-raw/state_lungmort_data.csv', stringsAsFactors = F)

devtools::use_data(us_lungmort, overwrite = TRUE)
