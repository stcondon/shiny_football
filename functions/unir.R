library(data.table)
unir <- function(xpert, fte, country = 'England') {
  team_names <- fread(paste0('data/team_names/', tolower(country),'_names.csv'))
  team_names <- merge(team_names, fte, by.x = 'fte', by.y = 'team1',
                      all.x = TRUE, all.y = FALSE)
  xpert$Date <- as.Date.character(xpert$Date, '%d/%m/%y')
  # team_names$date <- as.Date(team_names$date, '%Y-%m-%d')
  xpert <- merge(xpert, team_names, by.x = c('HomeTeam','Date'),
                 by.y = c('xpert', 'date'), all.x = TRUE, all.y = FALSE)
}

