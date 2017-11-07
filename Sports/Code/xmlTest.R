# Code to learn how to use XML to extract sports Data
# install.packages("XML")

# Load required packages
require(XML)
require(data.table)
require(dplyr)
require(tidyr)
#require(zoo)

# Parse the xml file
xmlTest = xmlParse(file = "../Data/Basketball/1617/1617AF2.xml")
#xmlTest = xmlParse(file = "../Data/Basketball/1617/1617UV.xml")
#xmlTest = xmlParse(file = "../Data/Basketball/1617/1617NV1.xml")
#xmlTest = xmlParse(file = "../Data/Basketball/1617/1617LV1.xml")

# Conver the xml file to a lists of lists in R
xmlDoc = xmlToList(xmlTest)

# Check the structure
summary(xmlDoc)

### Make a Table of all Players who entered the game ###

# If I were to simply ask for the "team" tag, I would only get the FIRST instance of the tag. 
# I want to extract all "team" tags in the document
playerInfo = xmlDoc[which(names(xmlDoc) == "team")]

# Extract all player information for each team separately (using the same logic as in the previous step)
playerInfo_1 = playerInfo[[1]][which(names(playerInfo[[1]]) == "player")]
playerInfo_2 = playerInfo[[2]][which(names(playerInfo[[2]]) == "player")]

# Check the structure
summary(playerInfo_1)

## Player table 2=1 (Air Force)
# Players that did not enter the game are character vectors, players that played contain additional lists. I want to extract the information 
# for all players that played (i,e. all character lists of length 4)
stat_list = lapply(playerInfo_1, function(x) if(class(x) == "list"){return(data.frame(t(c(x$.attrs,x$stats))))}else{return(NA)})

# Only extract players who played (length of list elements for players who got in the game is 19)
stat_final_list = stat_list[summary(stat_list)[,1] > 13]  


stat_final = as.data.frame(data.table::rbindlist(stat_final_list, use.names=TRUE, fill=TRUE))

# In the event that a player's position is not specified, denote position as "u" for unknown
PlayerTable_1 = stat_final %>% select(-code, -oncourt) %>%  mutate_all(funs(as.character)) %>% 
  replace_na(list(gs=0, pos="u"))


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

## Organize the play by play of each half into data frames. 
# Make a list of the length of each stat line, the game by game stats will all have between 7 and 11 entries
playList = as.numeric(summary(plays_1st)[,1])
playTable = plays_1st[which(playList >= 6 & playList <= 12)]
playTable = lapply(playTable, function(x) as.data.table(t(x), col.names = names(x)))
playList2 = as.numeric(summary(plays_2nd)[,1])
playTable2 = plays_2nd[which(playList2 >= 6 & playList2 <= 12)]
playTable2 = lapply(playTable2, function(x) as.data.table(t(x), col.names = names(x)))
playDF1 = as.data.frame(data.table::rbindlist(playTable,use.names=TRUE, fill=TRUE)) %>%
  mutate(half=1)
playDF2 = as.data.frame(data.table::rbindlist(playTable2,use.names=TRUE, fill=TRUE)) %>%
  mutate(half=2)

playDF = bind_rows(playDF1, playDF2)

playDF = playDF %>% fill(vscore, hscore)



# Create a dataframe and eliminate the missing values, the functions fill and replace_na (see tidyr package)
# help us to do this very quickly. 
playDF = playDF %>%  fill(vscore, hscore, .direction = "down") %>% 
  replace_na(list(paint="N",vscore = 0, hscore=0, fastb="N", type = "NONE")) %>%
  mutate(time = as.POSIXct(time, format = "%M:%S"),
         hscore = as.numeric(hscore),
         vscore = as.numeric(vscore))

# Remove copies we don't need
remove(playDF1, playDF2, playList, playList2, plays_1st, plays_2nd, playTable, playTable2)


## Create a vector including the current 5 players on the floor for USU
# Determine the starting 5, create a vector combining the names
startV = PlayerTable_1 %>% mutate(gs = as.numeric(as.character(gs))) %>% 
  filter(gs > 0) %>% select(checkname)
startV = startV[order(startV),]

startH = PlayerTable_2 %>% mutate(gs = as.numeric(as.character(gs))) %>% 
  filter(gs > 0) %>% select(checkname)
startH = startH[order(startH),]

playerCombosV = combn(PlayerTable_1$checkname, 5) %>% apply(., 2, function (x) x[order(x)]) %>% t(.)
playerCombosH = combn(PlayerTable_2$checkname, 5) %>% apply(., 2, function (x) x[order(x)]) %>% t(.)

# Determine which combo consitutes the starting 5
lineupV = which(apply(playerCombosV, 1, identical, startV))
lineupH = which(apply(playerCombosH, 1, identical, startH))

# Default all linupes to NA
playDF$hLineUp = NA
playDF$vLineUp = NA

# Set possesions to "no change"
playDF$possess = 0

# Set timeouts to "none called"
playDF$timeout = 0

# Determine the starting lineup for each team, (this will be updated regularly)
hline = startH
vline = startV

# Assign linup index to each team 
playDF$hLineUp[1] <- lineupH
playDF$vLineUp[1] <- lineupV


# Loop through all game actions, change the lineups whenever a 
# substitution is made, and then update the  "on the floor" 
# lineup after ALL susbtitutions have been made. 
for(i in 2:nrow(playDF)){
  # Subs at half time are based on the starting lineups and not the team that completed the half. 
  # Therefore, for the first play of the second half, reset the lineups to starting
  if(playDF$half[i] == 2 && playDF$half[i-1] == 1){
    hline = startH
    vline = startV
    playDF$hLineUp[i] = which(apply(playerCombosH, 1, identical, hline))
    playDF$vLineUp[i] = which(apply(playerCombosV, 1, identical, vline))
  }
  
  ## PLAYER SUBSTITUTIONS AND LINEUP ADJUSTMENTS
  if(playDF$action[i] == "SUB"){
    if(playDF$vh[i] == "H"){
      if(playDF$type[i] == "IN"){
        hline = c(hline, playDF$checkname[i])
      }else{
        hline = hline[hline != playDF$checkname[i]]
      }
    }else{
      if(playDF$type[i] == "IN"){
        vline = c(vline, playDF$checkname[i])
      }else{
        vline = vline[vline != playDF$checkname[i]]
      }
    }
  }else{
    # Once all of the substitutions have been completed, the lineups should
    # be updated for the next lineup. 
    if(playDF$action[i-1] == "SUB"){
      hline = hline[order(hline)]
      vline = vline[order(vline)]
      playDF$hLineUp[i] = which(apply(playerCombosH, 1, identical, hline))
      playDF$vLineUp[i] = which(apply(playerCombosV, 1, identical, vline))
    }
    # For non-substitutions, we want to check for possession changes
  }
  
  # Lets keep track of timouts and remove them as well 
  # (timeouts will throw off possesion count if we keep them in)
  if(playDF$action[i] == "TIMEOUT"){
    playDF$timeout[i-1] = 1
  }
}

# Now fill in line-up information and remove substitutions from record
playDF = playDF %>% fill(hLineUp, vLineUp, .direction = "down") %>%
  filter(action != "SUB") %>%
  filter(action != "TIMEOUT")


## POSSESSION CHANGES
for(i in 1:(nrow(playDF)-1)){
  
  # A turnover necessarily implies a change in possesion 
  if(playDF$action[i] == "TURNOVER"){  # (level-1)
    playDF$possess[i] = 1
    
    # SOME rebounds imply a change of possession  
  }else if(playDF$action[i] == "REBOUND"){  # (level-1)
    # A defensive rebound also implies a change in possession.   
    if(playDF$type[i] == "DEF"){  # (level-2)
      playDF$possess[i] = 1
      
      # if the rebound was not defensive OR offensive, we simply need
      # to check that the rebounding team is different from the shooting
      # team
    }else if(playDF$type[i] != "OFF"){  # (level-2)
      # Check to see if the rebounding team is different than the 
      # shooting team. 
      if(playDF$vh[i] != playDF$vh[i-1]){  # (level-3)
        playDF$possess[i] = 1
      }
    }
    # Made shots imply a change of possesion, but we need to account for 
    # "and-1's" and other technicalities. 
  }else if(playDF$action[i] == "GOOD"){  # (level-1)
    # If not a free throw, make sure it is not an "and-1"
    if(playDF$type[i] != "FT"){  # (level-2)
      # If the following action is neither foul nor assist, change possesion
      if(playDF$action[i+1] != "FOUL" && playDF$action[i+1] != "ASSIST"){  # (level-3)
        playDF$possess[i] = 1
        # If the following action is assist, see if next action is foul
      }else if(playDF$action[i+1] == "ASSIST"){  # (level-3)
        # IF no, change posssesion
        if(i+2 <= nrow(playDF)){
          if(playDF$action[i+2] != "FOUL"){  # (level-4)
            playDF$possess[i] = 1
            # If yes, see if "and-one" free throw was attempted
          }else{  # (level-4)
            # If foul occured on shooting team, change possesion
            if(playDF$vh[i+2] == playDF$vh[i]){  # (level-5)
              playDF$possess[i] = 1
              
              # If foul occured on defending team, see if "and-1" free 
              # throw was shot. If not, change possesion. 
            }else{  # (level-5)
              if(i+3 <= nrow(playDF)){  
                if(playDF$type[i+3] != "FT"){ # (level-6)
                  playDF$possess[i] = 1
                }else{ # (level-6)
                  if(playDF$vh[i+3] != playDF$vh[i]){ # (level-7)
                    playDF$possess[i] = 1
                  } # end level-7
                } # end level-6
              } # end i+3 exists statement
            } # end level-5
          } # end level-4
        } # end i+2 exists statement
      }else if(playDF$action[i+1] == "FOUL"){ # Next action is a foul
        if(playDF$vh[i+1] == playDF$vh[i]){
          # If foul occured on the shooting team, change possesion. 
          playDF$possess[i] = 1
        }
      } # end level-3
    }else{  # (level-2) (i.e. shot is a free throw)
      # If shot is a free-throw, make sure it is the last free throw
      if(playDF$type[i+1] != "FT"){ # (level-3)
        playDF$possess[i] = 1
      }else{ # (level-3)
        # If it IS a free throw by the OTHER team, change possesion
        if(playDF$vh[i+1] != playDF$vh[i]){ # (level-4)
          playDF$possess[i] = 1
        } # end level-4
      } # end level-3
    } # end level-2
  } # end level-1
} # end for-loop


# Looks like the possesions are working OK. I am going to leave this script
# at this point until the presentation on Monday. 
tester = playDF %>% group_by(hLineUp) %>% summarize(count = sum(possess))














# CODE TO PLOT SCORE LINE (COMMENTED OUT)


# plot((as.POSIXct("20:00", format = "%M:%S")-playDF_1$time)/60, playDF_1$hscore, type = "l", col="blue")
# lines((as.POSIXct("20:00", format = "%M:%S")-playDF_1$time)/60, playDF_1$vscore, type = "l", col="red")
# 
# plot((as.POSIXct("20:00", format = "%M:%S")-playDF_2$time)/60, playDF_2$hscore, type = "l", col="blue",
#      ylim = c(min(playDF_2$hscore, playDF_2$vscore), max(playDF_2$hscore, playDF_2$vscore)))
# lines((as.POSIXct("20:00", format = "%M:%S")-playDF_2$time)/60, playDF_2$vscore, type = "l", col="red")
# 
# 
# 
# # Look at overtime game vs Utah valley
# xmlTest = xmlParse(file = "../Data/Basketball/1617/1617UV.xml")
# xmlDoc = xmlToList(xmlTest)
# 


# Tester change 2
