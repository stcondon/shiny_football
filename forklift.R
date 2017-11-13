forklift <- function(country = 'England', tier = '1',
                     first_year = '1993', last_year = '1993') {
  # can't decide whether to make yrs strings and coerce to numeric
  # or vice versa
  fld <- substr(first_year, 4,4)
  lld <- substr(last_year, 4,4)
  if(as.numeric(first_year) < 2000 && as.numeric(last_year) >= 2010) {
    # /^(199\d|200\d|2010)$/
    re <- paste('(199[', fld, '-9]|200[[:digit::]]|201[0-', lld,
                '])_[[:digit:]]{4}.csv$', sep = '')
  } else if(as.numeric(first_year) < 2000 && as.numeric(last_year) < 2010) {
    re <- paste('(199[', fld, '-9]|200[0-', lld,
                '])_[[:digit:]]{4}.csv$', sep = '')
  } else if(as.numeric(first_year) < 2010 && as.numeric(last_year) >= 2010) {
    re <- paste('(200[', fld, '-]|201[0-', lld,
                '])_[[:digit:]]{4}.csv$', sep = '')
  }
  patron <- paste(country, tier, re, sep = '_')
  temp <- list.files(path = paste('data/',tolower(country),'/', sep = ''),
                     pattern= patron)
  # myfiles <- lapply(paste('data/',tolower(country),'/', temp, sep = ''),
                    # read.delim)
}