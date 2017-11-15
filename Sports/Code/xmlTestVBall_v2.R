# Code to learn how to use XML to extract sports Data
#install.packages("XML")
#install.packages("data.table") 

# Load required packages
require(XML)
require(data.table)
require(magrittr)
require(tidyr)
library(dplyr)

# Parse the xml file
xmlTest = xmlParse(file = "../Data/Volleyball/2017/2017ULM.xml")
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


makePlayDf <- function(playData){
  ## First game play table 
  # Make a list of the length of each stat line, the game by game stats will all have between 7 and 11 entries
  playList = as.numeric(summary(playData)[,1])
  # This line needs to be updated. Plays are length 7, subs are lenghth 3
  playTable = playData[which(playList >= 7)]
  playTable = lapply(playTable, function(x) as.data.table(t(x), col.names = names(x)))
  
  # Create a dataframe and eliminate the missing values, the functions fill and replace_na (see tidyr package)
  # help us to do this very quickly. 
  playDF = as.data.frame(data.table::rbindlist(playTable,use.names=TRUE, fill=TRUE))
  playDF_1 = playDF %>% 
    mutate(hscore = as.numeric(hscore),
           vscore = as.numeric(vscore),
           number = as.numeric(number)
    )
  return(playDF_1)
}
#Create 1st, 2nd, and 3rd game dataframes 
playDF_1 <- makePlayDf(plays[[1]])
playDF_2 <- makePlayDf(plays[[2]])
playDF_3 <- makePlayDf(plays[[3]])

# Beginning attempts to count the volleys
#playInfo <- as.vector(playDF_1$tokens)
#test <- unlist(strsplit(playInfo[28], "(?<=(A:\\d)|(SERVE:\\d)|(OVER:))", perl = TRUE))

# Creating the volleyCounter function
volleycounter <- function(dfTokens) {
  volleys <- unlist(strsplit(as.vector(dfTokens), "(?<=(A:\\d\\s)|(A:\\d\\d\\s)|(SERVE:\\d\\s)|(SERVE:\\d\\d\\s)|(OVER:))", perl = TRUE))
  #if (sum(grep(",[[:upper:]]", dfTokens))) {
   # return(length(volleys)-1)}
  return(length(volleys))
}

# Applying the volleyCounter function
playDF_1$volleys <- rapply(as.list(playDF_1$tokens), volleycounter)
playDF_2$volleys <- rapply(as.list(playDF_2$tokens), volleycounter)
playDF_3$volleys <- rapply(as.list(playDf_3$tokens), volleycounter)

# Splitting the dataframe into rows by each volley in a token
splittingByVolley <- function(df) {
df$primaryKey <- 1:length(df$number)
df1 <- df %>%
   mutate(tokens = strsplit(tokens, "(A:\\d\\s)|(A:\\d\\d\\s)|(SERVE:\\d\\s)|(SERVE:\\d\\d\\s)|(OVER:)")) %>%
   unnest(tokens)
df1$tokens <- unlist(strsplit(df$tokens, "(?<=(A:\\d\\s)|(A:\\d\\d\\s)|(SERVE:\\d\\s)|(SERVE:\\d\\d\\s)|(OVER:))", perl = TRUE))
return(df1)
}

# Applying the splittingByVolley function
playDF_1Split <- splittingByVolley(playDF_1)
playDF_2Split <- splittingByVolley(playDF_2)
playDF_3Split <- splittingByVolley(playDF_3)
