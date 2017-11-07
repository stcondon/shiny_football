simpleTable <- function(df) {
       teams <- unique(c(df$HomeTeam, df$AwayTeam))
       for(n in 1:length(teams)) {
              ## wash, rinse
              season_mid <- list()
              season_mid$team<- teams[n]
              temp <- subset(df, HomeTeam == teams[n])
              #season_mid$games <- nrow(temp)
              season_mid$Wins <- sum(temp$FTR == "H")
              season_mid$Losses <- sum(temp$FTR == "A")
              season_mid$Draws <- sum(temp$FTR == "D")
              season_mid$Scored <- sum(temp$FTHG)
              season_mid$Conceded <- sum(temp$FTAG)

              ## and repeat
              temp <- subset(df, AwayTeam == teams[n])
              #season_mid$games <- season_mid$games + nrow(temp)
              season_mid$Wins <- season_mid$Wins + sum(temp$FTR == "A")
              season_mid$Losses <- season_mid$Losses + sum(temp$FTR == 'H')
              season_mid$Draws <- season_mid$Draws + sum(temp$FTR == "D")
              season_mid$Scored <- season_mid$Scored + sum(temp$FTAG)
              season_mid$Conceded <- season_mid$Conceded + sum(temp$FTHG)

              season_mid$points<-season_mid$Wins*3 + season_mid$Draws
              if(exists('mid_season') && nrow(mid_season) < length(teams)) {
                     mid_season <- rbind(mid_season, season_mid)
              } else {
                     mid_season <- data.frame(season_mid, stringsAsFactors = F)
              }
       }
       mid_season <- mid_season[order(mid_season$points,
                                      mid_season$Scored - mid_season$Conceded,
                                      decreasing = TRUE),]
       mid_season
}

progress <- function(country_frame, team, date) {
       df <- country_frame[country_frame$Date <= date,]
       Year <- df$year[nrow(df)]
       df <- df[df$year == df$year[nrow(df)],]
       df2 <- df[grep(team, df$HomeTeam),]
       df <- df[grep(team, df$AwayTeam),]
       df$FTR <- gsub("H", 0, df$FTR)
       df$FTR <- gsub("A", 3, df$FTR)
       df$FTR <- gsub("D", 1, df$FTR)
       df$HTR <- gsub("H", "L", df$HTR)
       df$HTR <- gsub("A", 'W', df$HTR)
       df2$FTR <- gsub("H", 3, df2$FTR)
       df2$FTR <- gsub("A", 0, df2$FTR)
       df2$FTR <- gsub("D", 1, df2$FTR)
       df2$HTR <- gsub("H", "W", df2$HTR)
       df2$HTR <- gsub("A", 'L', df2$HTR)
       df <- subset(df, select = -Div)
       df2 <- subset(df2, select = -Div)
       names(df) <-c("Year", "Date", "Opponent", "Team", "goalsConceded",
                     "goalsScored","Points", "HTgoalsConceded", "HTgoalsScored",
                     "HTR")
       names(df2) <-c('Year', "Date", "Team", "Opponent", "goalsScored",
                      "goalsConceded", "Points", "HTgoalsScored",
                      "HTgoalsConceded", "HTR")
       df <- df[,1:10]
       df2 <- df2[,1:10]
       df$HoA <- rep("Away", nrow(df))
       df2$HoA <- rep("Home", nrow(df2))
       df <- rbind(df2, df)
       df <- df[order(df$Date),]
       df$Points <- as.numeric(df$Points)
       df[,c(2:length(df), 1)]

}

rel_rank <- function(progress_frame, pre_table, post_table) {
       diffRankLastYear <- diffRankSeasonEnd <- vector(mode = "numeric")
       ourTeam <- progress_frame$Team[1]
       for(j in 1:nrow(progress_frame)) {
              theirTeam <- progress_frame$Opponent[j]
              antes <- pre_table$team
              despues <- post_table$team
              if(theirTeam %in% antes) {
                     diffRankLastYear<-c(diffRankLastYear,grep(theirTeam,antes)-
                                                grep(ourTeam,antes))
              } else {diffRankLastYear[j] <-length(antes)+1-grep(ourTeam,antes)}
              diffRankSeasonEnd <- c(diffRankSeasonEnd, grep(theirTeam,despues)-
                                            grep(ourTeam, despues))
       }
       df <- cbind(progress_frame, diffRankLastYear, diffRankSeasonEnd)
       df <- df[,c(1:3, 11:13, 6, 4, 5, 7:9, 10)]
       df
}

## add distinguish feature, distinguishes btwn home/away or summed up
zipUp <- function(table_frame, mean_sd = F) {
       if(length(table_frame) < 77){
              df <- data.frame(row.names = table_frame$team)
              df$Points <- table_frame$totalPts
              table_frame <- subset(table_frame, select = -c(11,36))
              for(n in 2:26) {
                     df[,n] <- table_frame[,n + 2] + table_frame[,n + 26]
              }
              colnames(df) <- c("Points", "Wins", "Losses", "Draws", "Scored",
                                "Conceded","half1Scored", "half1Conceded",
                                "Shutouts","Scoreless", "upAtHalf","downAtHalf",
                                "Comebacks", "letSlip", "salvagedPt",
                                "settled4Pt", "heldOn2Win", "cantComeback",
                                "lowScoring", "scorelessDraws", "squeakBy",
                                "Pipped", "bigWins", "bigLosses", "Goalfests")
       } else {
              df <- data.frame(row.names = table_frame$team)
              df$Points <- table_frame$totalPts
              table_frame <- subset(table_frame, select = -c(23,60))
              for(n in 2:38) {
                     df[,n] <- table_frame[,n + 2] + table_frame[,n + 38]
              }
              colnames(df) <- c("Points", "Wins", "Losses", "Draws", "Scored",
                                "Conceded","1stHalfScored", "1stHalfConceded",
                                "Shots", "onTarget", "shotsAllowed",
                                "onTargetAllowed","cornersTaken",
                                "cornersConceded", "Fouls", "foulsDrawn",
                                "Yellows", "yellowsDrawn", "Reds", "redsDrawn",
                                "Shutouts","Scoreless","upAtHalf","downAtHalf",
                                "Comebacks", "letSlip", "salvagedPt",
                                "settled4Pt", "heldOn2Win", "cantComeback",
                                "lowScoring", "scorelessDraws", "squeakBy",
                                "Pipped","bigWins","bigLosses", "Goalfests")
       }
       if(mean_sd == TRUE) {
              sd <- apply(df, 2, sd)
              df <- rbind(df, colMeans(df), sd)
              row.names(df) <- c(table_frame$team,"Mean", "SD")
              df <- df[,-length(df)]
              #df$year <- rep(table_frame$Year[1], nrow(df))
              #df
       } else {
              df <- df[,-length(df)]
              #df$year <- table_frame$Year
              #df
       }
       df
}

h2h <- function(team1, team2, inputFrame) {
       teams <- c(team1, team2)
       years <- unique(inputFrame$year)
       df <- subset(inputFrame, HomeTeam %in% teams)
       df <- subset(df, AwayTeam %in% teams)
       for(y in years) {
              temp <- df[df$year == y,]

       }
}
