## Initialize Environment 
renv::restore()


## Dictionary ----------------------------------------------------------------------------------------------------------

# INPUT_FILE : the input file in (.csv) format ex. 'data/NFL_Template.csv'


# SPORT : the corresponding sport ex. NBA, AFL_5, AFL_8, NFL, GOLF


# METHOD : the method that modifies the point projection each run
  # standard - uses provided projections with no modification
  # uniform - uses provided floor / ceiling projections to draw projections from uniform distribution 
  # normal - uses provided projections & SD to draw projections from normal distribution 
  # deviation - uses provided  min / max deviation to draw from a uniform distribution than modify provided projections


# POSITIONS_SAME_TEAM_MAX : limit the number of positions from same team 
  # Examples Explained 
    # list("RB" = 1, "WR" = 1)  - allow only 1 running back and only 1 wide receiver from the same team


# POSITIONS_STACK : stack players by position from same team or by their opponent
  # Examples Explained
    # list("QB" = list('stack' = c('WR', "TE"), 'n' = 2, 'same_team' = TRUE)) - stack QB with at least 2 players either WR or TE from same team
    # list("QB" = list('stack' = c('WR'), 'n' = 1, 'same_team' = FALSE)) - stack QB with 1 WR from opposite team
    # list("QB" = list('stack' = c('WR', "TE"), 'n' = 2, 'same_team' = TRUE), 
    #      "QB" = list('stack' = c('WR'), 'n' = 1, 'same_team' = FALSE)) - also can combine both exmaples above


# TEAM_MIN : set minimum players from a certain team
  # Examples Explained 
    # list('MIA' = 1) - at least 1 players from Miami Dolphins 


# TEAM_MAX : set minimum players from a certain team
  # Examples Explained 
    # list('MIA' = 2) - at most 2 players from Miami Dolphins 


# N : number of lineups ex. 100
  

# SAVE : boolean if you want to save selected lineups


# SAVE_PATH : file location of saved lineups only if SAVE <- TRUE


# Note on EXPOSURE settings. The input file columns (Min.Exposure / Max.Exposure) control this
  # AfterEachExposureStrategy - is the exposure strategy being used. For more details see: https://pydfs-lineup-optimizer.readthedocs.io/en/latest/usage.html?highlight=AfterEachExposureStrategy#player-pool



## Edit Selections -----------------------------------------------------------------------------------------------------

# 1) Sport:
INPUT_FILE <-  'data/NFL_Template.csv'
SPORT <- 'NFL' 

# 2) Projection Method: 
METHOD = 'deviation' 

# 3) Stacking:
POSITIONS_SAME_TEAM_MAX <- list("RB" = 2, "WR" = 2) 

POSITIONS_STACK <- list("QB" = list('stack' = c('WR', "TE"), 'n' = 2, 'same_team' = TRUE), 
                        "QB" = list('stack' = c('WR'), 'n' = 1, 'same_team' = FALSE)
                        )

# 4) Team: 
TEAM_MIN = list()
TEAM_MAX = list() 

# 5) Number of Lineups:
N = 10 

# 6) Save ?
SAVE <- TRUE
SAVE_PATH <- paste('output/', SPORT, Sys.time(), '.csv', sep = "_")


## Run to Create Lineups -----------------------------------------------------------------------------------------------
source('src/main.R')





