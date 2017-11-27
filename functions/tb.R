library(data.table)
tb <- function(dt, country = 'England') {
  rbindlist(list(dt[FTR == 'H',.(p = uniqueN(AwayTeam) * 3), by = 'HomeTeam'],
                 dt[FTR == 'A',.(p = uniqueN(HomeTeam) * 3), by = 'AwayTeam'],
                 dt[FTR == 'D',.(p = uniqueN(AwayTeam)), by = 'HomeTeam'],
                 dt[FTR == 'D',.(p = uniqueN(HomeTeam)), by = 'AwayTeam'])
            )[,lapply(.SD,sum,na.rm=TRUE),by=HomeTeam][order(p,decreasing=TRUE)]
}
