## Build something that takes inputs and converts to list of data.tables
library(data.table)
forklift <- function(country = 'England', tier = '1',
                   first_year = '1993', last_year = '1993') {
# can't decide whether to make yrs strings and coerce to numeric
# or vice versa
fld <- substr(first_year, 4,4)
lld <- substr(last_year, 4,4)
if(as.numeric(first_year) < 2000 && as.numeric(last_year) >= 2010) {
  # /^(199\d|200\d|2010)$/
  re <- paste('(199[', fld, '-9]|200[0-9]|201[0-', lld,
              '])_[[:digit:]]{4}.csv$', sep = '')
} else if(as.numeric(first_year) < 2000 && as.numeric(last_year) >= 2000 &&
          as.numeric(last_year) < 2010) {
  re <- paste('(199[', fld, '-9]|200[0-', lld,
              '])_[[:digit:]]{4}.csv$', sep = '')
} else if(as.numeric(first_year) < 2010 && as.numeric(first_year) >= 2000 &&
          as.numeric(last_year) >= 2010) {
  re <- paste('(200[', fld, '-]|201[0-', lld,
              '])_[[:digit:]]{4}.csv$', sep = '')
} else {
  re <- paste('(', substr(first_year,1,3), '[', fld, '-', lld,
              '])_[[:digit:]]{4}.csv$', sep = '')
}
patron <- paste(country, tier, re, sep = '_')
temp <- list.files(path = paste('data/',tolower(country),'/', sep = ''),
                   pattern= patron)
lapply(paste('data/',tolower(country),'/', temp, sep = ''), fread)
# list2env(
#   lapply(setNames(paste('data/',tolower(country),'/', temp, sep = ''),
#                   make.names(gsub("*.csv$", "", temp))),
#          read.csv), envir = environment())
# ^^ maybe useful (also .GlobalEnv), let's see if better keeping in list
}