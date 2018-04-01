library(data.table)
scraped_vs_pulled <- function(scraped_dt, pulled_dt, country = 'England') {
  scraped_dt <- scraped_dt[,c(1,9,6,7)]
  names(scraped_dt) <- c('HomeTeam','p', 'scored', 'conceded')
  scraped_dt$p <- as.double(scraped_dt$p)
  tb <- match.fun(paste0(tolower(country), '_tb'))
  identical(scraped_dt, tb(pulled_dt))
}