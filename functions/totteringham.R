library(data.table)
totteringham <- function(dt, team_1 = 'Arsenal', team_2 = 'Tottenham',
                         country = 'England') {
  tc <- length(unique(c(dt$HomeTeam, dt$AwayTeam)))
  dt <- dt[HomeTeam %in% c(team_1, team_2) | AwayTeam %in% c(team_1, team_2)]
  tb <- match.fun(paste0(tolower(country), '_tb'))
  t <- tb(dt)[HomeTeam %in% c(team_1, team_2)]
  t1_gl <- 2 * (tc - 1) - nrow(dt[HomeTeam == team_1 | AwayTeam == team_1])
  t2_gl <- 2 * (tc - 1) - nrow(dt[HomeTeam == team_2 | AwayTeam == team_2])
  if(t[HomeTeam == team_2,p] > t1_gl * 3 + t[HomeTeam == team_1,p]) {
    result <- "Sorry, blood, not this year"
  } else if(t[HomeTeam == team_1,p] > t2_gl * 3 + t[HomeTeam == team_2,p]) {
    dates <- dt$Date
    for(gameday in c(1:length(dates))) {
      dt <- head(dt, -1)
      t <- tb(dt)[HomeTeam %in% c(team_1, team_2)]
      t1_gl <- 2 * (tc - 1) - nrow(dt[HomeTeam == team_1 | AwayTeam == team_1])
      t2_gl <- 2 * (tc - 1) - nrow(dt[HomeTeam == team_2 | AwayTeam == team_2])
      if(t[HomeTeam == team_1,p] <= t2_gl * 3 + t[HomeTeam == team_2,p]) {
        result <- paste('Judgement Day: ', dates[nrow(dt) + 1])
        break
      }
    }
  } else {
    game <- 0
    while((t[HomeTeam==team_1,p+3*game] - t[HomeTeam==team_2,p]) / 3 < t1_gl) {
      game <- game + 1
      t1_gl <- t1_gl - 1
    }
    result <- paste('At LEAST', game, 'more games')
  }
  result
  ## looks good, problem years so far:
  ## 96/97: 05/04/97
  ## 04/05: 02/04/05
  ## 14/15: 04/05/15
  ## http://www.chiark.greenend.org.uk/~mikepitt/tothistory.html
}