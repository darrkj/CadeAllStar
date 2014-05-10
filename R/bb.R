# Try to find players that are outliers, also player-game anomalies.

# Load the functions that pull game data.
source('game.R')

# Get all data for 2013
season <- seasonify(2013)


# Init list
basic <- list()
adv <- list()

# Number of games in season
len <- nrow(season)

# Loop over all games to get box score for a given game
for (i in 1:len) {
  print(i / len)
  # Pulls box score for the lk lookup string
  boxScore <- box_score(season[i, ]$lk)
  
  date <- season[i, 'date']
  guid <- season[i, 'guid']
  
  # Append the basic and advanced states for the home and away teams
  basic[[i]] <- rbind(data.frame(boxScore$awayBasic, date, guid),
                      data.frame(boxScore$homeBasic, date, guid))
  
  adv[[i]] <- rbind(data.frame(boxScore$awayAdv, date, guid),
                    data.frame(boxScore$homeAdv, date, guid))
}

rm(boxScore, date, guid, i, len,box_score, pull_day, seasonify, teams)


basic <- recurBind(basic)[[1]]

# Change the wierd namve Starters to player
names(basic)[1] <- 'player'

# Clear split lines for starters and reserves
basic <- basic[basic$player != 'Reserves', ]

# Add id field
basic$id <- 1:nrow(basic)


# Only two of three are needed FG FGA  FG.
basic$`FG.` <- NULL

# same {X3P X3PA X3P.} and  {FT FTA FT.}
basic$`X3P.` <- NULL
basic$`FT.` <- NULL

# Create seconds played from MP, character "43:39" min:sec
tmp <- strsplit(basic$MP, ':')

basic$SP <- as.numeric(lapply(tmp, '[[', 1)) * 60 + 
  as.numeric(lapply(tmp, '[[', 1))

# No longer need minutes played
basic$MP <- NULL

# Give fields more idiomatic names
basic <- rename(basic, replace = c('X...' = 'PM'))

# A very small amount of these were missing, '' in earlier data.
basic[basic$PM == '', ]$PM <- 0

#5172 10526
# score[5172, ]

# Remove fields that will mess with anomolies.
score <- basic[, !(names(basic) %in% c('player', 'date', 'guid'))]

# Transform all characters into integers 
for ( i in 1:ncol(score) ) {
  score[, i] <- as.integer(score[, i])
}

# Load Functions used in CADE
source('cade.R')

# Run cade on data, this takes a minute
basic$prob <- cade(score)$prob
score$prob <- basic$prob

score <- score[order(score$prob, decreasing = TRUE), ]


basic <- basic[order(basic$prob, decreasing = TRUE), ]

rev(sort(table(basic$player[1:30])))

# Aggregate the per game score up to just the player
rank <- ddply(basic, .(player), summarise, score = sum(prob))

rank <- rank[order(rank$score, decreasing = TRUE), ]
head(rank)

rank$rank <- 1:nrow(rank)

rank$player <- tolower(rank$player)


# This appears both ways so make everything lower case
# "Lebron James" "LeBron James"

# Load all star game data
al2013 <- 'http://www.allstarnba.es/editions/2013.htm'
al2013 <- readHTMLTable(al2013)

east <- al2013[[1]]$` EAST`
west <- al2013[[2]]$` WEST`

al2013 <- setdiff(c(east, west), 'TOTALS')

al2013 <- data.frame(player = tolower(al2013))

al2013 <- merge(al2013, rank)

al2013 <- al2013[order(al2013$rank), ]
al2013



#######


al2012 <- 'http://www.allstarnba.es/editions/2012.htm'
al2012 <- readHTMLTable(al2012)

east <- al2012[[1]]$` EAST`
west <- al2012[[2]]$` WEST`

al2012 <- setdiff(c(east, west), 'TOTALS')

al2012 <- data.frame(player = tolower(al2012))

al2012 <- merge(al2012, rank)

al2012 <- al2012[order(al2012$rank), ]
al2012

###############


# Does not work
if(F) {

# Remove fields that will mess with anomolies.
mm <- basic[, !(names(basic) %in% c('player', 'date', 'guid', 'prob', 'id'))]

for ( i in 1:ncol(mm) ) {
  mm[, i] <- as.integer(mm[, i])
}

#dd <- mm[, -c(9:15)]
for (i in 1:ncol(mm)) {
  mm[, i] <- normal(mm[, i])
}

mm <- mahalOutlier(mm)
}




