library(data.table)
totteringham <- function(dt, team_1 = 'Arsenal', team_2 = 'Tottenham',
                         country = 'England') {
  tc <- length(unique(c(dt$HomeTeam, dt$AwayTeam)))
  dt <- dt[HomeTeam %in% c(team_1, team_2) | AwayTeam %in% c(team_1, team_2)]
  tb <- match.fun(paste0(tolower(country), '_tb'))
  t <- tb(dt)[HomeTeam %in% c(team_1, team_2)]
  t1_gl <- 2 * (tc - 1) - nrow(dt[HomeTeam == team_1 | AwayTeam == team_1])
  t2_gl <- 2 * (tc - 1) - nrow(dt[HomeTeam == team_2 | AwayTeam == team_2])
  # if()
}