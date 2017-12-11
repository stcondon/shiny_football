library(data.table)
tb <- function(dt) {
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
  t <- merge(t[,.(team = HomeTeam, p)],
             dt[, .(home_scored = sum(FTHG)), by = 'HomeTeam']
             [,.(team = HomeTeam, home_scored)], by = 'team', all = TRUE)
  if(country %in% c('Spain') & sum(duplicated(t$p)) > 0) {
    ## h2h function}
    t[,tb := as.numeric(0)]
    setkey(t, HomeTeam)
    temp <- t$p[duplicated(t$p)] ## duplicated doesn't return both, get p first
    teams <- t[p %in% temp, HomeTeam:p]
    # setkey(teams, HomeTeam)
    for(tie in unique(teams[,p])) {
      ## limit to tied teams
      temp <- dt[(HomeTeam %in% teams[p == tie, HomeTeam]
                  & AwayTeam %in% teams[p == tie,HomeTeam])
                 |(HomeTeam %in% teams[p == tie, HomeTeam]
                   & AwayTeam %in% teams[p == tie, HomeTeam])]
      ## create mini table
      mini <- rbindlist(list(temp[FTR == 'H', .(p = uniqueN(AwayTeam) * 3),
                                  by = 'HomeTeam'],
                             temp[FTR == 'A', .(p = uniqueN(HomeTeam) * 3),
                                  by = 'AwayTeam'],
                             temp[FTR == 'D', .(p = uniqueN(AwayTeam)),
                                  by = 'HomeTeam'],
                             temp[FTR == 'D', .(p = uniqueN(HomeTeam)),
                                  by = 'AwayTeam']))[,.(p = sum(p)),
                                                     by = 'HomeTeam']
      # if(nrow(mini) > 1 & sum(duplicated(mini$p)) > 0) {
      if(nrow(mini) > 1) {
        mini <- merge(mini,rbindlist(list(temp[, .(scored = sum(FTHG)),
                                               by = 'HomeTeam'],
                                          temp[,sum(FTAG),by = 'AwayTeam']))
                      [,.(scored = sum(scored)), by = 'HomeTeam'],
                      by = 'HomeTeam', all = TRUE)
        mini <- merge(mini,rbindlist(list(temp[, .(conceded = sum(FTAG)),
                                               by = 'HomeTeam'],
                                          temp[,sum(FTHG), by = 'AwayTeam']))
                      [,.(conceded = sum(conceded)), by = 'HomeTeam'],
                      by = 'HomeTeam', all = TRUE)[order(p, scored - conceded,
                                                         scored,
                                                         decreasing = TRUE)]
      }
      mini[, tb := 2 * nrow(mini) - .I]
      t[mini, tb := as.double(i.tb)] ## had to coerce to double to avoid warning
    }
    t <- t[order(p, tb, scored - conceded, scored, decreasing = TRUE)]
    t[,c(1:4)]
  } else {
    t[order(p, scored - conceded, scored, decreasing = TRUE)]
  }
}