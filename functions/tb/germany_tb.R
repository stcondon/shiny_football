library(data.table)
germany_tb <- function(dt) {
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
  if(sum(duplicated(t[,.(p,scored - conceded)])) > 0) {
    t <- merge(t, dt[,.(away_scored = sum(FTAG)), by = 'AwayTeam']
               [,.(HomeTeam = AwayTeam, away_scored)], by = 'HomeTeam',
               all = TRUE)
    t[,tb := 0]
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
      if(nrow(mini) > 1 & sum(duplicated(mini$p)) == 0) {
        mini <- mini[order(p, decreasing = TRUE)]
      }
      else if(nrow(mini) > 1) {
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
                                                         decreasing = TRUE)]
        if(sum(duplicated(mini[,.(p,scored-conceded)])) > 0) {
          setkey(mini, HomeTeam)
          mini[t, total_gd := i.scored - i.conceded]
          mini <- mini[order(p, scored - conceded, total_gd, decreasing = TRUE)]
        }
      }
      mini[, tb := 2 * nrow(mini) - .I]
      t[mini, tb := as.double(i.tb)] ## had to coerce to double to avoid warning
    }
    t <- t[order(p, tb, scored - conceded, scored, decreasing = TRUE)]
    t[,c(1:4)]
  }
  else {
    t[order(p, scored - conceded, decreasing = TRUE)]
  }
}