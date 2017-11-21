# Code to learn how to use XML to extract sports Data
#install.packages("XML")
#install.packages("data.table") 

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
makePlayDf <- function(playData){
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
  playDF_1 <- PlayingTeamIndicator(playDF_1)
  return(playDF_1)
}

# Creating function to split the dataframe into rows by each volley in a token
splittingByVolley <- function(df) {
  df$primaryKey <- 1:length(df$number)
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
  end <- 10+length(USUPlayers)
  for (i in 11:end){
    playDF1Home[,i] <- NA
  }
  # Naming the columns after the players
  columnNames <- lapply(USUPlayers, paste, "player", sep="")
  names(playDF1Home)[11:end] <- columnNames
  
  # Adding a space at the end so the next regular expression will work
  playDF1Home$tokens <- gsub("$", " ", playDF1Home$tokens)
  
  # Using regular expressions and a for loop to cycle through players, and create
  # indicator variables if that player played in that volley.
  j <- 11
  for (i in USUPlayers){
    test <- paste(":", i, sep="") %>%
      paste("\\s", sep="")
    playDF1Home[grepl(test, playDF1Home$tokens),j] <- 1
    playDF1Home[!grepl(test, playDF1Home$tokens),j] <- 0
    j <- j+1
  }
  playDF1Home$point <- as.factor(playDF1Home$point)
  return(playDF1Home)
}

# Main Code ------------------------------------------------------------------------
# Parse the xml file
#xmlTest = xmlParse(file = "../Data/Volleyball/2017/2017ULM.xml")
xmlTest = xmlParse(file = "2017ULM.xml")

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
plays = xmlDoc$plays

# Create 1st, 2nd, and 3rd game dataframes 
playDF1 <- makePlayDf(plays[[1]])
playDF2 <- makePlayDf(plays[[2]])
playDF3 <- makePlayDf(plays[[3]])

# Creating Home team sub dataframe
game1df <- homeVisitorTeamDF(playDF1, "Home")
game2df <- homeVisitorTeamDF(playDF2, "Home")
game3df <- homeVisitorTeamDF(playDF3, "Home")

# Running a random forest predicting point based off of the players
set.seed(33)
game1RF <- randomForest(x = game1df[,11:18], 
                        y = game1df[,3], 
                        importance = TRUE,
                        which.class = "USU")
varImpPlot(game1RF)


game2RF <- randomForest(x = game2df[,11:18], 
                        y = game2df[,3], 
                        importance = TRUE,
                        which.class = "USU",
                        classwt = c(.5, .5),
                        ntree = 1000,
                        mtry = 4)
varImpPlot(game2RF)

game3RF <- randomForest(x = game3df[,11:18], 
                        y = game3df[,3], 
                        importance = TRUE,
                        which.class = "USU",
                        mtry = 4)
varImpPlot(game3RF)


game1RF$confusion
game2RF$confusion

# Finding how much each player played in the game
x <- c(sum(game1df$`8player`), sum(game1df$`6player`), sum(game1df$`3player`), sum(game1df$`2player`),
sum(game1df$`13player`), sum(game1df$`26player`), sum(game1df$`10player`), sum(game1df$`11player`))
hist(x)


# Binds the three games together.
test <- list(game1df, game2df, game3df)
test1 <- rbindlist(test, fill=TRUE) 
test1[is.na(test1)] <- 0

# Creating a Random Forest predicting point off of each player for the whole match
matchRF <- randomForest(x = test1[,11:21],
                        y = as.factor(test1$point),
                        importance = TRUE,
                        which.class = "USU",
                        classwt = c(.5,.5))
matchRF$confusion
varImpPlot(matchRF)

# TODO: Look at making the indicators for players into factor levels

test1.2 <- test1 %>% select(primaryKey,`8player`:`5player`) %>% group_by(primaryKey) %>% summarize_all(sum)
test1.3 <- left_join(test1.2, test1, by="primaryKey")
# TODO: Use join to get the "point" in the same df as the summarized 
# player indicators

# TODO: bind the games together before doing primary key etc.
# OR could number the games for each game, then the combination of primary key
# and game number, and group off of that ombination.