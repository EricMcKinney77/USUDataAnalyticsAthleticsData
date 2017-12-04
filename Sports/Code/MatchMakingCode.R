# Load required packages
require(XML)
require(data.table)
require(magrittr)
require(tidyr)
library(dplyr)
library(randomForest)
library(sqldf)

# Functions --------------------------------------------------------------------
# Creating the volleyCounter function
volleycounter <- function(dfTokens) {
  volleys <- unlist(strsplit(as.vector(dfTokens), "(?<=(A:\\d\\s)|(A:\\d\\d\\s)|(SERVE:\\d\\s)|(SERVE:\\d\\d\\s)|(OVER:))", perl = TRUE))
  #if (sum(grep(",[[:upper:]]", dfTokens))) {
  # return(length(volleys)-1)}
  return(length(volleys))
}
# Beginning attempts to count the volleys
#playInfo <- as.vector(playDF_1$tokens)
#test <- unlist(strsplit(playInfo[28], "(?<=(A:\\d)|(SERVE:\\d)|(OVER:))", perl = TRUE))


#Creating the makePlayDf function
makePlayDf <- function(playData, gameNum){
  # Make a list of the length of each stat line, the game by game stats will all have between 7 and 11 entries
  playList = as.numeric(summary(playData)[,1])
  # This line needs to be updated. Plays are length 7, subs are lenghth 3
  playTable = playData[which(playList >= 7)]
  playTable = lapply(playTable, function(x) as.data.table(t(x), col.names = names(x)))
  
  # Create a dataframe and eliminate the missing values, the functions fill and replace_na (see tidyr package, dplyr package)
  # help us to do this very quickly. 
  playDF = as.data.frame(data.table::rbindlist(playTable,use.names=TRUE, fill=TRUE))
  playDF_1 = playDF %>% 
    mutate(hscore = as.numeric(hscore),
           vscore = as.numeric(vscore),
           number = as.numeric(number)
    )
  playDF_1$volleys <- rapply(as.list(playDF_1$tokens), volleycounter)
  playDF_1 <- splittingByVolley(playDF_1)
  playDF_1$gameNumber <- gameNum
  playDF_1 <- PlayingTeamIndicator(playDF_1)
  return(playDF_1)
}

# Creating function to split the dataframe into rows by each volley in a token
splittingByVolley <- function(df) {
  df$playKey <- 1:length(df$number)
  df1 <- df %>%
    mutate(tokens = strsplit(tokens, "(A:\\d\\s)|(A:\\d\\d\\s)|(SERVE:\\d\\s)|(SERVE:\\d\\d\\s)|(OVER:)|(CONT:)")) %>%
    unnest(tokens)
  df1$tokens <- unlist(strsplit(df$tokens, "(?<=(A:\\d\\s)|(A:\\d\\d\\s)|(SERVE:\\d\\s)|(SERVE:\\d\\d\\s)|(OVER:)|(CONT:))", perl = TRUE))
  return(df1)
}

# Creating a column in the dataframe to indicate which team played in that volley
PlayingTeamIndicator <- function(df){
  # Creating a column of NA's called playingTeam
  df$playingTeam <- NA
  
  # Filling the playingTeam indicator column with "Home" when Home serves
  df$playingTeam[grepl("TEAM:H SERVE:", df$tokens)] <- "Home"
  # Filling the playingTeam indicator column with "Visitor" when Visitor serves
  df$playingTeam[grepl("TEAM:V SERVE:", df$tokens)] <- "Visitor"
  
  # For loop, to cyclically change the playingTeam indicator column with "Home" 
  # or "Visitor", depending on what the previous row's indicator was.
  j <- 1
  for (i in df$playingTeam){
    if (is.na(df$playingTeam[j])){
      ifelse(df$playingTeam[j-1] == "Home",
             df$playingTeam[j] <- "Visitor",
             df$playingTeam[j] <- "Home")
    }
    j <- j+1
  }
  return(as.data.frame(df))
}

homeVisitorTeamDF <- function(df, homeVisitor){
  # Subsetting into just the home team dataframe
  playDF1Home <- df[df$playingTeam == homeVisitor,]
  
  # Removing rebound errors (RE), and creating a vector of the tokens
  playerNumbers <- gsub("RE:\\d+", "", playDF1Home$tokens)
  # Pulling out a vector of all the numbers of players in playerNumbers
  USUPlayers <- regmatches(playerNumbers, 
                           gregexpr("(\\d\\s)|(\\d\\d)|(\\d,)|(\\d)", 
                                    playerNumbers))
  # Creating vector of player combinations (31!)
  USUPlayerCombinations <- unique(USUPlayers)
  
  # Getting the unique player numbers, just the numbers of girls on the team that played
  USUPlayers <- unlist(USUPlayers)
  USUPlayers <- gsub(" ", "", USUPlayers)
  USUPlayers <- gsub(",", "", USUPlayers) %>%
    unique()
  
  # Creating Dummy Variable columns
  end <- 11+length(USUPlayers)
  for (i in 12:end){
    playDF1Home[,i] <- NA
  }
  # Naming the columns after the players
  columnNames <- lapply(USUPlayers, paste, "player", sep="")
  names(playDF1Home)[12:end] <- columnNames
  
  # Adding a space at the end so the next regular expression will work
  playDF1Home$tokens <- gsub("$", " ", playDF1Home$tokens)
  
  # Using regular expressions and a for loop to cycle through players, and create
  # indicator variables if that player played in that volley.
  j <- 12
  for (i in USUPlayers){
    test <- paste(":", i, sep="") %>%
      paste("\\s", sep="")
    playDF1Home[grepl(test, playDF1Home$tokens),j] <- 1
    playDF1Home[!grepl(test, playDF1Home$tokens),j] <- 0
    j <- j+1
  }
  playDF1Home[,12:end] <- lapply(playDF1Home[,12:end], as.factor)
  playDF1Home$point <- as.factor(playDF1Home$point)
  return(playDF1Home)
}

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

# Pulling in all matches
match1 <- makeMatchDF("2017BS1.XML")
match2 <- makeMatchDF("2017UV.XML")
match3 <- makeMatchDF("2017ULM.XML")
match4 <- makeMatchDF("2017PAC.XML")
match5 <- makeMatchDF("2017FS1.XML")

# Running Random Forests for each individual match
set.seed(333)
match1RF <- randomForest(x = match1[,3:(length(match1)-3)],
                               y = as.factor(match1$point),
                               importance = TRUE,
                               which.class = "USU",
                               ntree = 1000)
match1RF$confusion
varImpPlot(match1RF)

match2RF <- randomForest(x = match2[,3:(length(match2)-3)],
                        y = as.factor(match2$point),
                        importance = TRUE,
                        which.class = "USU",
                        ntree = 1000)
match2RF$confusion
varImpPlot(match2RF)

match3RF <- randomForest(x = match3[,3:(length(match3)-3)],
                        y = as.factor(match3$point),
                        importance = TRUE,
                        which.class = "USU",
                        ntree = 1000)
match3RF$confusion

match4RF <- randomForest(x = match4[,3:(length(match4)-3)],
                        y = as.factor(match4$point),
                        importance = TRUE,
                        which.class = "USU",
                        ntree = 1000)
match4RF$confusion

match5RF <- randomForest(x = match5[,3:(length(match5)-3)],
                         y = as.factor(match5$point),
                         importance = TRUE,
                         which.class = "USU",
                         ntree = 1000)
match5RF$confusion

# Creating one large dataframe of all the matches combined
USUMatchesList <- list(match1, match2, match3, match4, match5)
AllMatchesDF <- rbindlist(USUMatchesList, fill=TRUE)
AllMatchesDF[is.na(AllMatchesDF)] <- 0

# Subsetting to have only the "point" variable and all the players
RFprepDF <- AllMatchesDF[, c(14, 3:11, 15:27)]

RFprepDF$Response <- ifelse(RFprepDF$point=="USU", 1, 0)

USUvBallRF <- randomForest(x = RFprepDF[,2:(length(RFprepDF)-1)],
                         y = as.factor(RFprepDF$Response),
                         importance = TRUE,
                         which.class = 1,
                         ntree = 1000)
USUvBallRF$confusion
varImpPlot(USUvBallRF)

# Making the explanatory variables not factors
RFprepDF[]

# Rerun Random Forest, but with only factor levels of 1 or 0 for players
USUvBallDF_1_0 <- RFprepDF %>%
  mutate(`2player` = as.factor(ifelse(`2player`=="0", "0", "1")),
         `11player` = as.factor(ifelse(`11player`=="0", "0", "1")),
         `6player` = as.factor(ifelse(`6player`=="0", "0", "1")),
         `9player` = as.factor(ifelse(`9player`=="0", "0", "1")),
         `26player` = as.factor(ifelse(`26player`=="0", "0", "1")),
         `3player` = as.factor(ifelse(`3player`=="0", "0", "1")),
         `8player` = as.factor(ifelse(`8player`=="0", "0", "1")),
         `10player` = as.factor(ifelse(`10player`=="0", "0", "1")),
         `5player` = as.factor(ifelse(`5player`=="0", "0", "1")),
         `17player` = as.factor(ifelse(`17player`=="0", "0", "1")),
         `18player` = as.factor(ifelse(`18player`=="0", "0", "1")),
         `15player` = as.factor(ifelse(`15player`=="0", "0", "1")),
         `12player` = as.factor(ifelse(`12player`=="0", "0", "1")),
         `4player` = as.factor(ifelse(`4player`=="0", "0", "1")),
         `1player` = as.factor(ifelse(`1player`=="0", "0", "1")),
         `14player` = as.factor(ifelse(`14player`=="0", "0", "1")),
         `16player` = as.factor(ifelse(`16player`=="0", "0", "1")),
         `13player` = as.factor(ifelse(`13player`=="0", "0", "1")),
         `24player` = as.factor(ifelse(`24player`=="0", "0", "1")),
         `22player` = as.factor(ifelse(`22player`=="0", "0", "1")),
         `21player` = as.factor(ifelse(`21player`=="0", "0", "1")),
         `7player` = as.factor(ifelse(`7player`=="0", "0", "1")))

# Running the random forest
USUvBallDF_1_0RF <- randomForest(x = USUvBallDF_1_0[,2:(length(USUvBallDF_1_0)-1)],
                           y = as.factor(USUvBallDF_1_0$Response),
                           importance = TRUE,
                           which.class = 1,
                           ntree = 1000)
USUvBallDF_1_0RF$confusion
varImpPlot(USUvBallDF_1_0RF)


RFprepDF[,2:23] <- lapply(RFprepDF[,2:23], as.character)
RFprepDF[,2:23] <- lapply(RFprepDF[,2:23], as.numeric)

USUvBallRF_v2 <- randomForest(x = RFprepDF[,2:(length(RFprepDF)-1)],
                           y = as.factor(RFprepDF$Response),
                           importance = TRUE,
                           which.class = 1,
                           ntree = 1000)
USUvBallRF_v2$confusion
varImpPlot(USUvBallRF)

