# Code to learn how to use XML to extract sports Data
install.packages("XML")
install.packages("data.table")

# Load required packages
require(XML)
require(data.table)

# Parse the xml file
xmlTest = xmlParse(file = "../Data/Volleyball/2017/2017ULM.xml")

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


#Player Table 2 (USU)
stat_list = lapply(playerInfo_2, function(x) if(class(x) == "list"){return(data.frame(player = x$.attrs["name"], t(x$attack), t(x$set),  t(x$serve), t(x$defense), t(x$block), t(x$misc)))}else{return(NA)})

# Only extract players who played (length of list elements for players who got in the game is 19)
stat_final_list = stat_list[summary(stat_list)[,1] == 17]


stat_final_2 = as.data.frame(data.table::rbindlist(stat_final_list))

# Remove files we no longer need
remove(stat_list, stat_final_list, stat_final, playerInfo_1, playerInfo_2, playerInfo)

### Make data table of the play by play ###

plays = xmlDoc$plays
plays_1st = plays[[1]]
plays_2nd = plays[[2]]
plays_3rd = plays[[3]]

## First half play table 
# Make a list of the length of each stat line, the game by game stats will all have between 7 and 11 entries
playList = as.numeric(summary(plays_1st)[,1])
# This line needs to be updated. Plays are length 7, subs are lenghth 3
playTable = plays_1st[which(playList >= 7)]
playTable = lapply(playTable, function(x) as.data.table(t(x), col.names = names(x)))

#BROKEN HERE
# Create a dataframe and eliminate the missing values, the functions fill and replace_na (see tidyr package)
# help us to do this very quickly. 
playDF = as.data.frame(data.table::rbindlist(playTable,use.names=TRUE, fill=TRUE))
playDF_1 = playDF %>% 
  mutate(hscore = as.numeric(hscore),
         vscore = as.numeric(vscore),
         number = as.numeric(number)
         )

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

# Create Image for Running Score
plot((as.POSIXct("20:00", format = "%M:%S")-playDF_1$time)/60, playDF_1$hscore, type = "l", col="blue")
lines((as.POSIXct("20:00", format = "%M:%S")-playDF_1$time)/60, playDF_1$vscore, type = "l", col="red")