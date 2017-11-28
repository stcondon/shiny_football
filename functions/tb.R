library(data.table)
tb <- function(dt, country = 'England') {
  ## Cut down table to just full time goals and results
  # dt <- dt[,c("HomeTeam","AwayTeam","FTR","FTHG","FTAG")]
  t <- rbindlist(list(dt[FTR == 'H', .(p = uniqueN(AwayTeam) * 3),
                         by = 'HomeTeam'],
                      dt[FTR == 'A', .(p = uniqueN(HomeTeam) * 3),
                         by = 'AwayTeam'],
                      dt[FTR == 'D', .(p = uniqueN(AwayTeam)),
                         by = 'HomeTeam'],
                      dt[FTR == 'D', .(p = uniqueN(HomeTeam)),
                         by = 'AwayTeam']))[,lapply(.SD,sum,na.rm=TRUE),
                                            by=HomeTeam]
  t <- merge(t,rbindlist(list(dt[, .(scored = sum(FTHG)), by = 'HomeTeam'],
                              dt[,sum(FTAG), by = 'AwayTeam']))
             [,lapply(.SD, sum, na.rm = TRUE), by=HomeTeam], by = 'HomeTeam',
             all = TRUE)
  t <- merge(t,rbindlist(list(dt[, .(conceded = sum(FTAG)), by = 'HomeTeam'],
                              dt[,sum(FTHG), by = 'AwayTeam']))
             [,lapply(.SD, sum, na.rm = TRUE), by=HomeTeam], by = 'HomeTeam',
             all = TRUE)
  if(country %in% c('Spain')) {
      ## h2h function}
  } else {
    t[order(p,scored - conceded, scored, decreasing = TRUE)]
  }
}
