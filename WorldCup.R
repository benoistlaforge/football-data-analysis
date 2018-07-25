# Returns only years from a vector of dates
getYearsFromDates <- function(dates) {
    return (substring(dates, 1, 4))
}

# Gets number of World Cup tournaments appearance from the specified set of games
getNbAttendances <- function (games) {
    return(length(unique(getYearsFromDates(games$games.play_at))))
}

# Gets number of goals scored for the specified country
getGoalsFor <- function(countryGames, country) {
    homeGames <- countryGames[countryGames$team1HumanReadable == country, ]
    homeGoalsFor <- sum(homeGames$games.score1, na.rm = TRUE)
    homeGoalsAgainst <- sum(homeGames$games.score2, na.rm = TRUE)
    homeGoalsEt1For <- sum(homeGames$games.score1et - homeGames$games.score1, na.rm = TRUE)
    homeGoalsEt1Against <- sum(homeGames$games.score2et - homeGames$games.score2, na.rm = TRUE)
    awayGames <- countryGames[countryGames$team2HumanReadable == country, ]
    awayGoalsFor <- sum(awayGames$games.score2, na.rm = TRUE)
    awayGoalsAgainst <- sum(awayGames$games.score1, na.rm = TRUE)
    awayGoalsEt1For <- sum(awayGames$games.score2et - awayGames$games.score2, na.rm = TRUE)
    awayGoalsEt1Against <- sum(awayGames$games.score1et - awayGames$games.score1, na.rm = TRUE)
    
    goalsFor <- awayGoalsFor + homeGoalsFor + awayGoalsEt1For + homeGoalsEt1For
    goalsAgainst <- awayGoalsAgainst + homeGoalsAgainst + awayGoalsEt1Against + homeGoalsEt1Against
    
    return(c(goalsFor, goalsAgainst))
}

# Gets a list of all games played for the specified country
getGamesFor <- function(humanReadable, country) {
    return(humanReadable[which(humanReadable$team1HumanReadable == country | humanReadable$team2HumanReadable == country),])
}

getNbGamesPlayed <- function(gamesPlayed) {
    return(length(gamesPlayed[,1]))
}

# Returns stats for the specified country
getStats <- function(humanReadable, country) {
    # Gets specific France data
    gamesData <- getGamesFor(humanReadable, country)
    # Gets appearances
    appearances <- getNbAttendances(gamesData)
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
    if (nbGamesPlayed > 0) {
        goalsPerGame <- round(goals / nbGamesPlayed, 2)
    } else {
        goalsPerGame <- c(0,0)
    }
    # Builds a data frame containing all stats
    stats <- data.frame(country, appearances, nbGamesPlayed, goals[1], goals[2], goalsPerGame[1], goalsPerGame[2], wins, draws, losses)
    # A vector containing data names
    statsNames <- c("Country", "Appearances", "Number of games played", "Goals for", "Goals against", "Goals for per game", "Goals against per game", "Wins", "Draws", "Losses")
    names(stats) <- statsNames
    
    return(stats)
}

# Returns a list of all the country who ever participated to a World Cup tournament
getAllTimeParticipants <- function(games) {
    data <- sort(unique(c(as.character(games$team1HumanReadable), as.character(games$team2HumanReadable))))
    return(data)
}

# Returns all games for a specific World Cup Edition
getGamesPerYear <- function(games, year) {
    return (games[which(games$games.year == year),])
}
