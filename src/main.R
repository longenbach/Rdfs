## Initialize
renv::restore()

library(lpSolve)
library(glue)
sessionInfo()

source('src/1_setup.R')


## Select INPUT_FILE & SPORT ------------------------------------------------------------------------------------------
INPUT_FILE <- 'data/NBA_Template1.csv' 
SPORT <- 'NBA'

# ---------------------------------------------------------------------------------------------------------------------

input_df <- format_input_fx(df = read.csv(INPUT_FILE))
get_sport <- get_sport_fx(sport = SPORT)
setup_list <- setup_lp_fx(get_sport = get_sport, df = input_df)


sol <- lp(direction = "max", 
          objective.in = setup_list$DF$Projection, 
          const.mat = do.call("rbind", lapply(setup_list$LP, get, x="con")),
          const.dir = unlist(lapply(setup_list$LP, get, x="dir")),
          const.rhs = unlist(lapply(setup_list$LP, get, x="rhs")),
          all.bin = TRUE
)
setup_list$DF[sol$solution==1, ]


### Check List:

# 1) Ability to use for multiple sports and salary/position constraints. Status: DONE (needs testing)

# 2) Ability to add randomness to the projections the optimizer uses for each player. Status: TO-DO

# 3) Ability to stack players by position from same team and also someone from their opponent. Status: TO-DO

# 4) Ability to set max or min players from a specific team. Status: TO-DO

# 5) Set exposure settings for each player via the CSV that the optimizer obides too. Status: TO-DO

# 6) Print line ups in format that can be uploaded to our site. Status: TO-DO


