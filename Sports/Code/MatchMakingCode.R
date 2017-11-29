makeMatchDF <- function(fileName){
  # Parse the xml file
  #xmlTest = xmlParse(file = "../Data/Volleyball/2017/2017ULM.xml")
  xmlTest = xmlParse(file = fileName)
  
  # Convert the xml file to a lists of lists in R
  xmlDoc = xmlToList(xmlTest)
  
  # If I were to simply ask for the "team" tag, I would only get the FIRST instance of the tag.
  # I want to extract all "team" tags in the document
  playerInfo = xmlDoc[which(names(xmlDoc) == "team")]
  
  # Extract all player information for each team separately (using the same logic as in the previous step)
  playerInfo_1 = playerInfo[[1]][which(names(playerInfo[[1]]) == "player")]
  playerInfo_2 = playerInfo[[2]][which(names(playerInfo[[2]]) == "player")]
  
  #Player Table 1 (Away Team) ??Remove the Team Row in Table??
  # Attribute data to players for table
  stat_list = lapply(playerInfo_1, function(x) if(class(x) == "list"){return(data.frame(player = x$.attrs["name"], t(x$attack), t(x$set),  t(x$serve), t(x$defense), t(x$block), t(x$misc)))}else{return(NA)})
  
  # Only extract players who played (length of list elements for players who got in the game is 19)
  stat_final_list = stat_list[summary(stat_list)[,1] == 17]
  stat_final_1 = as.data.frame(data.table::rbindlist(stat_final_list))
  
  #Removing the "Team" row
  stat_final_1[-length(stat_final_1$player),]
  
  #Player Table 2 (USU)
  stat_list = lapply(playerInfo_2, function(x) if(class(x) == "list"){return(data.frame(player = x$.attrs["name"], t(x$attack), t(x$set),  t(x$serve), t(x$defense), t(x$block), t(x$misc)))}else{return(NA)})
  
  # Only extract players who played (length of list elements for players who got in the game is 19)
  stat_final_list = stat_list[summary(stat_list)[,1] == 17]
  stat_final_2 = as.data.frame(data.table::rbindlist(stat_final_list))
  
  # Remove files we no longer need
  remove(stat_list, stat_final_list, playerInfo_1, playerInfo_2, playerInfo)
  
  ### Make data table of the play by play ###
  plays <- xmlDoc$plays
  numGames <- length(plays)-1
  
  # Create the appropriate number of playDFs (based on number of games)
  for (i in 1:numGames){
    if (i == 1){
      playDF1 <- makePlayDf(plays[[i]], i)
    }
    if (i == 2){
      playDF2 <- makePlayDf(plays[[i]], i)
    }
    if (i == 3){
      playDF3 <- makePlayDf(plays[[i]], i)
    }
    if (i == 4){
      playDF4 <- makePlayDf(plays[[i]], i)
    }
    if (i == 5){
      playDF5 <- makePlayDf(plays[[i]], i)
    }
  }
  
  if (playDF1$serveteam[1] == "USU"){
    ifelse(playDF1$playingTeam == "Visitor",
           HomeVisitorIndicator <- "Visitor",
           HomeVisitorIndicator <- "Home")
  } else {
    ifelse(playDF1$playingTeam == "Visitor",
           HomeVisitorIndicator <- "Home",
           HomeVisitorIndicator <- "Visitor")
  }
  # Creating Home team sub dataframe
  for (i in 1:numGames){
    if (i == 1){
      game1df <- homeVisitorTeamDF(playDF1, HomeVisitorIndicator)
    }
    if (i == 2){
      game2df <- homeVisitorTeamDF(playDF2, HomeVisitorIndicator)
    }
    if (i == 3){
      game3df <- homeVisitorTeamDF(playDF3, HomeVisitorIndicator)
    }
    if (i == 4){
      game4df <- homeVisitorTeamDF(playDF4, HomeVisitorIndicator)
    }
    if (i == 5){
      game5df <- homeVisitorTeamDF(playDF5, HomeVisitorIndicator)
    }
  }
  
  # Binds the three games together into one match
  if(numGames==3){
    matchList <- list(game1df, game2df, game3df)
  } else if (numGames==4){
    matchList <- list(game1df, game2df, game3df, game4df)
  } else {
    matchList <- list(game1df, game2df, game3df, game4df, game5df)
  }
  
  # Finding the number of players
  
  Matchdf <- rbindlist(matchList, fill=TRUE)
  Matchdf[is.na(Matchdf)] <- 0
  
  # Making the player indicators not be factors... just for the group by.
  Matchdf[,12:length(Matchdf)] <- lapply(Matchdf[,12:length(Matchdf)], as.character)
  Matchdf[,12:length(Matchdf)] <- lapply(Matchdf[,12:length(Matchdf)], as.numeric)
  
  # Grouping by playKey and GameNumber to put volleys back into plays
  playsMatchdf <- Matchdf %>% select(playKey, gameNumber, 12:length(Matchdf)) %>% group_by(playKey, gameNumber) %>% summarize_all(sum)
  pointGroupedDF <- Matchdf %>%
    select(playKey, gameNumber, point) %>%
    group_by(playKey, gameNumber) %>%
    arrange(playKey, gameNumber) %>%
    slice(1)
  
  
  groupedMatchDF <- cbind(playsMatchdf, pointGroupedDF)
  groupedMatchDF[,3:(length(groupedMatchDF)-3)] <- lapply(groupedMatchDF[,3:(length(groupedMatchDF)-3)], as.factor)
  return(groupedMatchDF)
}

test <- makeMatchDF("2017BS1.XML")
test2 <- makeMatchDF("2017UV.XML")
test3 <- makeMatchDF("2017ULM.XML")
test4 <- makeMatchDF("2017PAC.XML")

test1RF <- randomForest(x = test[,3:(length(test)-3)],
                               y = as.factor(test$point),
                               importance = TRUE,
                               which.class = "USU",
                               ntree = 1000)
test1RF$confusion
varImpPlot(test1RF)

test2RF <- randomForest(x = test2[,3:(length(test2)-3)],
                        y = as.factor(test2$point),
                        importance = TRUE,
                        which.class = "USU",
                        ntree = 1000)
test2RF$confusion
varImpPlot(test2RF)

test3RF <- randomForest(x = test3[,3:(length(test3)-3)],
                        y = as.factor(test3$point),
                        importance = TRUE,
                        which.class = "USU",
                        ntree = 1000)
test3RF$confusion

test4RF <- randomForest(x = test4[,3:(length(test4)-3)],
                        y = as.factor(test4$point),
                        importance = TRUE,
                        which.class = "USU",
                        ntree = 1000)
test4RF$confusion
