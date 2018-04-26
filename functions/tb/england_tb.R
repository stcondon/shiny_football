library(data.table)
england_tb <- function(dt) {
  t <- rbindlist(list(dt[FTR == 'H', .(p = uniqueN(AwayTeam) * 3),
                         by = 'HomeTeam'],
                      dt[FTR == 'A', .(p = uniqueN(HomeTeam) * 3),
                         by = 'AwayTeam'],
                      dt[FTR == 'D', .(p = uniqueN(AwayTeam)),
                         by = 'HomeTeam'],
                      dt[FTR == 'D', .(p = uniqueN(HomeTeam)),
                         by = 'AwayTeam']))[,.(p = sum(p)), by = 'HomeTeam']
  t <- merge(t,rbindlist(list(dt[, .(scored = sum(FTHG)), by = 'HomeTeam'],
                              dt[,sum(FTAG), by = 'AwayTeam']))
             [,.(scored = sum(scored)), by = 'HomeTeam'], by = 'HomeTeam',
             all = TRUE)
  t <- merge(t,rbindlist(list(dt[, .(conceded = sum(FTAG)), by = 'HomeTeam'],
                              dt[,sum(FTHG), by = 'AwayTeam']))
             [,.(conceded = sum(conceded)), by = 'HomeTeam'], by = 'HomeTeam',
             all = TRUE)
  if(grepl('09$',dt$Date[1])) {
    t[HomeTeam == 'Portsmouth', p := p - 9]
  }
  if(nrow(dt) > length(unique(dt$HomeTeam)) * length(unique(dt$HomeTeam)) - 1) {
    ## HAVEN'T SEEN YET, DON'T KNOW WHAT DATA WOULD LOOK LIKE :(
    ## https://en.wikipedia.org/wiki/Premier_League#Competition_format
    temp <- rbindlist(list(dt[, .(games = uniqueN(Date)),
                              by = 'HomeTeam'],
                           dt[, .(games = uniqueN(Date)),
                              by = 'AwayTeam']))[,.(games = sum(games)),
                                                 by = 'HomeTeam']
  } else {
    t[order(p, scored - conceded, scored, decreasing = TRUE)]
  }
}