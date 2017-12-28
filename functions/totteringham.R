library(data.table)
totteringham <- function(dt, team_1 = 'Arsenal', team_2 = 'Tottenham',
                         country = 'England') {
  t <- dt[HomeTeam %in% c(team_1, team_2) | AwayTeam %in% c(team_1, team_2)]
}