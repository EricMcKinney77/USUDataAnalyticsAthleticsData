# Code to learn how to use XML to extract sports Data
# install.packages("XML")

# Load required packages
require(XML)
require(data.table)
require(dplyr)
require(tidyr)
#require(zoo)

# Parse the xml file
xmlTest = xmlParse(file = "../Data/1617AF2.xml")

# Conver the xml file to a lists of lists in R
xmlDoc = xmlToList(xmlTest)


### Make a Table of all Players who entered the game ###

# If I were to simply ask for the "team" tag, I would only get the FIRST instance of the tag. 
# I want to extract all "team" tags in the document
playerInfo = xmlDoc[which(names(xmlDoc) == "team")]

# Extract all player information for each team separately (using the same logic as in the previous step)
playerInfo_1 = playerInfo[[1]][which(names(playerInfo[[1]]) == "player")]
playerInfo_2 = playerInfo[[2]][which(names(playerInfo[[2]]) == "player")]

## Player table 2=1 (Air Force)
# Players that did not enter the game are character vectors, players that played contain additional lists. I want to extract the information 
# for all players that played (i,e. all character lists of length 4)
stat_list = lapply(playerInfo_1, function(x) if(class(x) == "list"){return(data.frame(t(c(x$.attrs,x$stats))))}else{return(NA)})

# Only extract players who played (length of list elements for players who got in the game is 19)
stat_final_list = stat_list[summary(stat_list)[,1] > 13]  


stat_final = as.data.frame(data.table::rbindlist(stat_final_list, use.names=TRUE, fill=TRUE))

# In the event that a player's position is not specified, denote position as "u" for unknown
PlayerTable_1 = stat_final %>% select(-code, -oncourt) %>%  mutate_all(funs(as.character)) %>% replace_na(list(gs=0, pos="u"))


## Player table 2 (USU)
# Players that did not enter the game are character vectors, players that played contain additional lists. I want to extract the information 
# for all players that played (i,e. all character lists of length 4)
stat_list = lapply(playerInfo_2, function(x) if(class(x) == "list"){return(data.frame(t(c(x$.attrs,x$stats))))}else{return(NA)})

# Only extract players who played (length of list elements for players who got in the game is 19)
stat_final_list = stat_list[summary(stat_list)[,1] > 13]  


stat_final = as.data.frame(data.table::rbindlist(stat_final_list, use.names=TRUE, fill=TRUE))

# In the event that a player's position is not specified, denote position as "u" for unknown
PlayerTable_2 = stat_final %>% select(-code, -oncourt) %>%  mutate_all(funs(as.character)) %>% replace_na(list(gs=0, pos="u"))

# Remove files we no longer need
remove(stat_list, stat_final_list, stat_final, playerInfo_1, playerInfo_2, playerInfo)

### Make data table of the play by play ###

plays = xmlDoc$plays
plays_1st = plays[[1]]
plays_2nd = plays[[2]]

## First half play table 
# Make a list of the length of each stat line, the game by game stats will all have between 7 and 11 entries
playList = as.numeric(summary(plays_1st)[,1])
playTable = plays_1st[which(playList >= 7 & playList <= 12)]
playTable = lapply(playTable, function(x) as.data.table(t(x), col.names = names(x)))

# Create a dataframe and eliminate the missing values, the functions fill and replace_na (see tidyr package)
# help us to do this very quickly. 
playDF = as.data.frame(data.table::rbindlist(playTable,use.names=TRUE, fill=TRUE))
playDF_1 = playDF %>%  fill(vscore, hscore) %>% replace_na(list(paint="N",vscore = 0, hscore=0, fastb="N")) %>%
  mutate(time = as.POSIXct(time, format = "%M:%S"),
         hscore = as.numeric(hscore),
         vscore = as.numeric(vscore))

## Second half play table 
# Make a list of the length of each stat line, the game by game stats will all have between 7 and 11 entries
playList = as.numeric(summary(plays_2nd)[,1])
playTable = plays_2nd[which(playList >= 7 & playList <= 12)]
playTable = lapply(playTable, function(x) as.data.table(t(x), col.names = names(x)))

# Create a dataframe and eliminate the missing values, the functions fill and replace_na (see tidyr package)
# help us to do this very quickly. 
playDF = as.data.frame(data.table::rbindlist(playTable,use.names=TRUE, fill=TRUE))
playDF_2 = playDF %>%  fill(vscore, hscore) %>% fill(vscore, hscore, .direction = "up") %>%  
  replace_na(list(paint="N",vscore = 0, hscore=0, fastb="N")) %>%
  mutate(time = as.POSIXct(time, format = "%M:%S"),
         hscore = as.numeric(hscore),
         vscore = as.numeric(vscore))



# Remove copies we don't need
remove(playDF, plays, plays_1st, plays_2nd, playList, playTable)

plot((as.POSIXct("20:00", format = "%M:%S")-playDF_1$time)/60, playDF_1$hscore, type = "l", col="blue")
lines((as.POSIXct("20:00", format = "%M:%S")-playDF_1$time)/60, playDF_1$vscore, type = "l", col="red")

