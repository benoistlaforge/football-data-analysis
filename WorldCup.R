setwd("./db")

countries <- read.csv2("teams.csv", sep = ',')
games <- read.csv2("games.csv", sep=',')

# Displays human readable games
gamesId <- games$id
team1HumanReadable <- countries$title[games$team1_id]
team2HumanReadable <- countries$title[games$team2_id]

humanReadable <- data.frame(gamesId, team1HumanReadable, team2HumanReadable, games$score1, games$score2, games$winner)
humanReadable <- humanReadable[which(!is.na(humanReadable$games.winner)),]

# Gets number of goals scored for the specified country
getGoalsFor <- function(countryGames, country) {
    homeGames <- countryGames[countryGames$team1HumanReadable == country, ]
    homeGoalsFor <- sum(homeGames$games.score1, na.rm = TRUE)
    homeGoalsAgainst <- sum(homeGames$games.score2, na.rm = TRUE)
    awayGames <- countryGames[countryGames$team2HumanReadable == country, ]
    awayGoalsFor <- sum(awayGames$games.score2, na.rm = TRUE)
    awayGoalsAgainst <- sum(awayGames$games.score1, na.rm = TRUE)
    
    goalsFor <- awayGoalsFor + homeGoalsFor
    goalsAgainst <- awayGoalsAgainst + homeGoalsAgainst
    
    return(c(goalsFor, goalsAgainst))
}

# Gets a list of all games played for the specified country
getGamesFor <- function(country) {
    return(humanReadable[which(humanReadable$team1HumanReadable == country | humanReadable$team2HumanReadable == country),])
}

getNbGamesPlayed <- function(gamesPlayed) {
    return(length(gamesPlayed[,1]))
}

# A vector containing data names
statsNames <- c("Country", "Number of games played", "Goals for", "Goals against", "Goals for per game", "Goals against per game", "Wins", "Draws", "Losses")

# Returns stats for the specified country
getStats <- function(country) {
    # Gets specific France data
    gamesData <- getGamesFor(country)
    # Gets number of games played
    nbGamesPlayed <- getNbGamesPlayed(gamesData)
    # Gets games played as HOME team
    homeGames <- gamesData[which(gamesData$team1HumanReadable == country),]
    # Gets games played as AWAY team
    awayGames <- gamesData[which(gamesData$team2HumanReadable == country),]
    # Gets all the results
    homeResults <- table(homeGames$games.winner)
    awayResults <- table(awayGames$games.winner)
    wins <- length(which(homeGames$games.winner == 1)) + length(which(awayGames$games.winner == 2))
    losses <- length(which(homeGames$games.winner == 2)) + length(which(awayGames$games.winner == 1))
    draws <- length(which(homeGames$games.winner == 0)) + length(which(awayGames$games.winner == 0))
    # Gets goals scored in country's games
    goals <- getGoalsFor(gamesData, country)
    nbGamesPlayedFrance <- nbGamesPlayed
    goalsPerGame <- round(goals / nbGamesPlayed, 2)
    # Builds a data frame containing all stats
    stats <- data.frame(country, nbGamesPlayed, goals[1], goals[2], goalsPerGame[1], goalsPerGame[2], wins, draws, losses)
    names(stats) <- statsNames
    
    return(stats)
}

# Returns a list of all the country who ever participated to a World Cup tournament
getAllTimeParticipants <- function(games) {
    data <- sort(unique(c(as.character(games$team1HumanReadable), as.character(games$team2HumanReadable))))
    return(data)
}

allParticipants <- getAllTimeParticipants(humanReadable)

stats <- data.frame()

for (i in 1:length(allParticipants)) {
    stats <- rbind(stats, getStats(allParticipants[i]))
}
