## Build something that takes inputs and converts to list of data.tables
library(data.table)
forklift <- function(country = 'England', tier = '1', first_year = '1993',
                     last_year = '1993', source = '12xpert_matches',
                     processed = TRUE){
  fld <- substr(first_year, 4,4)
  lld <- substr(last_year, 4,4)
  if(as.numeric(first_year) < 2000 && as.numeric(last_year) >= 2010) {
    # /^(199\d|200\d|2010)$/
    re <- paste0('(199[', fld, '-9]|200[0-9]|201[0-', lld,
                '])_[[:digit:]]{4}.csv$')
  } else if(as.numeric(first_year) < 2000 && as.numeric(last_year) >= 2000 &&
            as.numeric(last_year) < 2010) {
    re <- paste0('(199[', fld, '-9]|200[0-', lld,
                '])_[[:digit:]]{4}.csv$')
  } else if(as.numeric(first_year) < 2010 && as.numeric(first_year) >= 2000 &&
            as.numeric(last_year) >= 2010) {
    re <- paste0('(200[', fld, '-9]|201[0-', lld,
                '])_[[:digit:]]{4}.csv$')
  } else {
    re <- paste0('(', substr(first_year,1,3), '[', fld, '-', lld,
                '])_[[:digit:]]{4}.csv$')
  }
  patron <- paste(country, tier, re, sep = '_')
  ## HERE WE CREATE path VARIABLE
  if(processed == TRUE) {
    camino <- paste0('data/', source, '/processed/', tolower(country),'/')
  } else {
    camino <- paste0('data/', source, '/raw/', tolower(country),'/')
  }
  temp <- list.files(path = camino,
                     pattern= patron)
  temp <- lapply(paste0(camino, temp), fread)#, fill = TRUE)
  if(source == 'espn_tables') {
    lapply(temp, function(x) x[!(is.na(x$GP))])
  } else{
    lapply(temp, function(x) x[!(is.na(x$FTHG))])
  }
}