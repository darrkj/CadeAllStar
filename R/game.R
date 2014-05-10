library(XML)
library(lubridate)
library(plyr)
options(stringsAsFactors = FALSE)


# Pulls the boxscore for a game.
# expects something like 201211030NJN
box_score <- function(lk) {
  stopifnot(nchar(lk) == 12)
  site <- paste('http://www.basketball-reference.com/boxscores/', 
                lk, '.html', sep = '')
  # Read the box score information.
  boxScore <- readHTMLTable(site)
  
  n <- which(names(boxScore) == 'four_factors') - 1
  if (length(n) > 0) {
    boxScore <- boxScore[n:(n+6)][3:6]
    names(boxScore) <- c('awayBasic', 'awayAdv', 'homeBasic', 'homeAdv')
    boxScore
  } else if (length(boxScore) > 0) {
    list(score = boxScore[[which(unlist(lapply(boxScore, function(x) any(names(x) %in% 'Scoring'))))]][-1, ])
  }
}


# Pulls games for a given day.
pull_day <- function(day) {
  # oct 29 1947 is as far back as it goes
  # Date to collect data, ex: mdy('01-01-2004')
  site <- readHTMLTable(
    paste('http://www.basketball-reference.com/boxscores/index.cgi?month=', 
            month(day), '&day=', day(day), '&year=', year(day), sep = ''))
  
  if (length(site) > 0) {
    xx <- lapply(site, nrow)
    xx <- which(xx == 1)
    #xx <- as.vector(unlist(lapply(xx, function(x) if(x == 2) T else F)))
    gameList <- data.frame(date = day, away = 'dd', as = 5, home = 'ff', hs = 4)

    for (i in site[xx]) {
      xx <- data.frame(date = day, away = names(i)[1], as = names(i)[2], 
                       home = i[[1]], hs = i[[2]])
      gameList <- rbind(gameList, xx)
    }
    gameList <- gameList[-1, ]
    gameList$as <- as.integer(gameList$as)
    gameList$hs <- as.integer(gameList$hs)
    gameList$season <- year(gameList$date)
    gameList$season <- ifelse(month(gameList$date) > 9, 
                              gameList$season + 1, gameList$season)
    return(gameList)
  }
}

# This function pulls games for a given NBA season from Basketball Reference
seasonify <- function(y, test = FALSE) {
  teams <- teams()
  day <- mdy('10-05-2012')
  year(day) <- y - 1
  quit <- TRUE
  season <- pull_day(day)
  cnt <- 1
  
  while (quit) {
    season <- rbind(season, pull_day(day))
    day <- day + days(1)
    if (as.Date(day) == today() | month(day) == 7) quit <- FALSE
    if (test & !is.null(season)) if(nrow(season) > 25) quit <- FALSE
    
    if (cnt == 4) {cat('='); cnt <- 0}  else cnt <- cnt + 1
  }
  
  if (y < 1960) teams[teams$symbol == "BAL", ]$symbol = "BLB"
  
  th <- teams[, c('symbol', 'ref')]
  names(th) <- c('hsym', 'home')
  
  ta <- teams[, c('symbol', 'ref')]
  names(ta) <- c('asym', 'away')
  
  season <- merge(season, th, by = 'home', all.x = TRUE)
  season <- merge(season, ta, by = 'away', all.x = TRUE)
  
  season$guid <- paste(season$hsym, season$asym, substr(season$date, 1, 4), 
                       substr(season$date, 6, 7), substr(season$date, 9, 10), sep = '')
  
  season$lk <- paste(substr(season$date, 1, 4), substr(season$date, 6, 7), 
                     substr(season$date, 9, 10), 0, season$hsym, sep = '')
  
  # Reorder by date games were played and return
  season[order(season$date), ]
}

# Just a lookup table wrapped in afunction.
teams <- function() {
  x <- c('WSC', 'Washington Capitols', 'Washington', 'Capitols',
    'BAL', 'Baltimore Bullets', 'Baltimore', 'Bullets',
    'CHS', 'Chicago Stags', 'Chicago', 'Stags',
    'NYK', 'New York Knicks', 'New York', 'Knicks',
    'PHW', 'Philadelphia Warriors', 'Philadelphia', 'Warriors',
    'BOS', 'Boston Celtics', 'Boston', 'Celtics',
    'PRO', 'Providence Steam Rollers', 'Providence', 'Steam Rollers',
    'STB', 'St. Louis Bombers', 'St. Louis', 'Bombers',
    'INJ', 'Indianapolis Jets', 'Indianapolis', 'Jets',
    'FTW', 'Fort Wayne Pistons', 'Fort Wayne', 'Pistons',
    'ROC', 'Rochester Royals', 'Rochester', 'Royals',
    'MNL', 'Minneapolis Lakers', 'Minneapolis', 'Lakers',
    'INO', 'Indianapolis Olympians', 'Indianapolis', 'Olympians',
    'WAT', 'Waterloo Hawks', 'Waterloo', 'Hawks',
    'AND', 'Anderson Packers', 'Anderson', 'Packers',
    'SHE', 'Sheboygan Red Skins', 'Sheboygan', 'Red Skins',
    'SYR', 'Syracuse Nationals', 'Syracuse', 'Nationals',
    'DEN', 'Denver Nuggets', 'Denver', 'Nuggets',
    'TRI', 'Tri-Cities Blackhawks', 'Tri-Cities', 'Blackhawks',
    'MLH', 'Milwaukee Hawks', 'Milwaukee', 'Hawks',
    'STL', 'St. Louis Hawks', 'St. Louis', 'Hawks',
    'CIN', 'Cincinnati Royals', 'Cincinnati', 'Royals',
    'DET', 'Detroit Pistons', 'Detroit', 'Pistons',
    'LAL', 'Los Angeles Lakers', 'Los Angeles', 'Lakers',
    'CHP', 'Chicago Packers', 'Chicago', 'Packers',
    'CHZ', 'Chicago Zephyrs', 'Chicago', 'Zephyrs',
    'SFW', 'San Francisco Warriors', 'San Francisco', 'Warriors',
    'PHI', 'Philadelphia 76ers', 'Philadelphia', '76ers',
    'CHI', 'Chicago Bulls', 'Chicago', 'Bulls',
    'SDR', 'San Diego Rockets', 'San Diego', 'Rockets',
    'SEA', 'Seattle SuperSonics', 'Seattle', 'SuperSonics',
    'ATL', 'Atlanta Hawks', 'Atlanta', 'Hawks',
    'MIL', 'Milwaukee Bucks', 'Milwaukee', 'Bucks',
    'PHO', 'Phoenix Suns', 'Phoenix', 'Suns',
    'CLE', 'Cleveland Cavaliers', 'Cleveland', 'Cavaliers',
    'POR', 'Portland Trail Blazers', 'Portland Trail', 'Blazers',
    'BUF', 'Buffalo Braves', 'Buffalo', 'Braves',
    'GSW', 'Golden State Warriors', 'Golden State', 'Warriors',
    'HOU', 'Houston Rockets', 'Houston', 'Rockets',
    'KCO', 'Kansas City-Omaha Kings', 'Kansas City-Omaha', 'Kings',
    'CAP', 'Capital Bullets', 'Capital', 'Bullets',
    'WSB', 'Washington Bullets', 'Washington', 'Bullets',
    'NOJ', 'New Orleans Jazz', 'New Orleans', 'Jazz',
    'KCK', 'Kansas City Kings', 'Kansas City', 'Kings',
    'NYN', 'New York Nets', 'New York', 'Nets',
    'IND', 'Indiana Pacers', 'Indiana', 'Pacers',
    'SAS', 'San Antonio Spurs', 'San Antonio', 'Spurs',
    'NJN', 'New Jersey Nets', 'New Jersey', 'Nets',
    'SDC', 'San Diego Clippers', 'San Diego', 'Clippers',
    'UTA', 'Utah Jazz', 'Utah', 'Jazz',
    'DAL', 'Dallas Mavericks', 'Dallas', 'Mavericks',
    'LAC', 'Los Angeles Clippers', 'Los Angeles', 'Clippers',
    'SAC', 'Sacramento Kings', 'Sacramento', 'Kings',
    'CHH', 'Charlotte Hornets', 'Charlotte', 'Hornets',
    'MIA', 'Miami Heat', 'Miami', 'Heat',
    'ORL', 'Orlando Magic', 'Orlando', 'Magic',
    'MIN', 'Minnesota Timberwolves', 'Minnesota', 'Timberwolves',
    'TOR', 'Toronto Raptors', 'Toronto', 'Raptors',
    'VAN', 'Vancouver Grizzlies', 'Vancouver', 'Grizzlies',
    'WAS', 'Washington Wizards', 'Washington', 'Wizards',
    'MEM', 'Memphis Grizzlies', 'Memphis', 'Grizzlies',
    'NOH', 'New Orleans Hornets', 'New Orleans', 'Hornets',
    'CHA', 'Charlotte Bobcats', 'Charlotte', 'Bobcats',
    'NOK', 'New Orleans/Oklahoma City Hornets', 'New Orleans/Oklahoma City', 'Hornets',
    'OKC', 'Oklahoma City Thunder', 'Oklahoma City', 'Thunder',
    'BRK', 'Brooklyn Nets', 'Brooklyn', 'Nets',
    'NOP', 'New Orleans Pelicans', 'New Orleans', 'Pelicans')
  xx <- matrix(x, ncol = 4, nrow = 67, byrow = T)
  xx <- as.data.frame(xx)
  names(xx) <- c('symbol', 'ref', 'loc', 'name')
  xx
}


# Useful function for appending data frames.
recurBind <- function(dList) {
  len <- length(dList) / 2
  # Preallocate list for small improvement.
  data <- vector("list", len)
  j <- 1
  for (i in seq(len)) {
    # Merge each set of two sequential data sets together.
    data[[j]] <- rbind(dList[[(i * 2) - 1]], dList[[i * 2]])
    j <- j + 1
  }
  # In case length was odd, just add last set to the end.
  if (floor(len) != len) {
    data[[j]] <- dList[[len * 2]]
  }
  # Less data to store on the stack, tail call optimization would be nice here.
  rm(dList, len, j)
  # Recursive call.
  if (length(data) > 1) {
    data <- recurBind(data)
  }
  return(data)
}


