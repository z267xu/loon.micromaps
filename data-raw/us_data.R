

us_lungmort_2009_2011 <- read.csv('data-raw/state_lungmort_data.csv', stringsAsFactors = F)

devtools::use_data(us_lungmort_2009_2011, overwrite = TRUE)
