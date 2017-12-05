library(data.table)
tb <- function(dt, country = 'England') {
  ## Cut down table to just full time goals and results
  dt <- dt[,c("HomeTeam","AwayTeam","FTR","FTHG","FTAG")]
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
  if(country %in% c('Spain') & sum(duplicated(t$p)) > 0) {
      ## h2h function}
    t[,tb := 0]
    setkey(t, HomeTeam)
    temp <- t$p[duplicated(t$p)] ## duplicated doesn't return both, get p first
    teams <- t[p %in% temp, HomeTeam:p]
    setkey(teams, HomeTeam)
    for(tie in unique(teams[,p])) {
      ## limit to tied teams
      mini <- dt[(HomeTeam %in% teams[p == tie, HomeTeam]
                  & AwayTeam %in% teams[p == tie,HomeTeam])
                 |(HomeTeam %in% teams[p == tie, HomeTeam]
                   & AwayTeam %in% teams[p == tie, HomeTeam])]
      ## create mini table
      mini <- rbindlist(list(mini[FTR == 'H', .(p = uniqueN(AwayTeam) * 3),
                                by = 'HomeTeam'],
                             mini[FTR == 'A', .(p = uniqueN(HomeTeam) * 3),
                                by = 'AwayTeam'],
                             mini[FTR == 'D', .(p = uniqueN(AwayTeam)),
                                by = 'HomeTeam'],
                             mini[FTR == 'D', .(p = uniqueN(HomeTeam)),
                                by = 'AwayTeam']))[,.(p = sum(p)),
                                                   by = 'HomeTeam']
      if(nrow(mini) > 1) {
        t <- merge(t,rbindlist(list(dt[, .(scored = sum(FTHG)), by = 'HomeTeam'],
                                    dt[,sum(FTAG), by = 'AwayTeam']))
                   [,.(scored = sum(scored)), by = 'HomeTeam'], by = 'HomeTeam',
                   all = TRUE)
        t <- merge(t,rbindlist(list(dt[, .(conceded = sum(FTAG)), by = 'HomeTeam'],
                                    dt[,sum(FTHG), by = 'AwayTeam']))
                   [,.(conceded = sum(conceded)), by = 'HomeTeam'], by = 'HomeTeam',
                   all = TRUE)
      }
      mini[,tb := .I+1]
    }
  } else {
    t[order(p,scored - conceded, scored, decreasing = TRUE)]
  }
}
