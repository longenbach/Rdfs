library(lpSolve)
library(glue)
sessionInfo()


source('src/1_setup.R') # ----------------------------------------------------------------------------------------------
INPUT_df <- format_input_fx(df = read.csv(INPUT_FILE))
SPORT_list = get_sport_fx(sport = SPORT)
SETUP_list <- setup_lp_fx(get_sport = SPORT_list, 
                          df = INPUT_df)
# print(SETUP_list$DF)
# print(SETUP_list$LP)


source('src/2_projection.R') # -----------------------------------------------------------------------------------------


source('src/3_stack.R') # ----------------------------------------------------------------------------------------------
SETUP_list$LP[['POSITIONS_SAME_TEAM_MAX']] <- create_pos_same_team_max_lp_fx(setup_df = SETUP_list$DF,
                                                positions_same_team_max = POSITIONS_SAME_TEAM_MAX)$POSITIONS_SAME_TEAM_MAX

SETUP_list$LP[['POSITIONS_STACK']] <- create_positions_stack_lp_fx(setup_df = SETUP_list$DF,
                                                                   positions_stack = POSITIONS_STACK)$POSITIONS_STACK
# print(SETUP_list$LP)


source('src/4_team.R') # -----------------------------------------------------------------------------------------------
team_LPs <- create_team_lp_fx(setup_df = SETUP_list$DF, 
                              team_max_list = TEAM_MAX, 
                              team_min_list = TEAM_MIN, 
                              exclude_positions = SPORT_list[[1]]$TEAM_CAP$exclude)
SETUP_list$LP[['TEAM_MAX']] <- team_LPs$TEAM_MAX
SETUP_list$LP[['TEAM_MIN']] <- team_LPs$TEAM_MIN
# print(SETUP_list$LP)


source('src/5_exposure.R') # -------------------------------------------------------------------------------------------
EXPOSURE_list <- init_exposure_list_fx(setup_df = SETUP_list$DF, n_tot = N)
# print(EXPOSURE_list)


# ----------------------------------------------------------------------------------------------------------------------

# CREATE LINEUPS: 
SAVE_df <- data.frame()
TRACKER_df <- data.frame()
for (RUN in 1:N){
  print(glue('{RUN} ) -----------------------------------------------------------------------------------------------'))
  
  # modify points projection
  SETUP_list$DF <- append_projections_fx(setup_df = SETUP_list$DF, 
                                         proj_df = modify_projections_fx(setup_df = SETUP_list$DF, method = METHOD))
  
  # solve
  sol <- lp(direction = "max", 
            objective.in = SETUP_list$DF$Projection_modify, 
            const.mat = do.call("rbind", lapply(SETUP_list$LP, get, x="con")),
            const.dir = unlist(lapply(SETUP_list$LP, get, x="dir")),
            const.rhs = unlist(lapply(SETUP_list$LP, get, x="rhs")),
            all.bin = TRUE
  )
  
  # get solution
  lineup_i <- SETUP_list$DF[sol$solution==1, ]
  lineup_i$line_up <- RUN
  
  # append 
  SAVE_df <- rbind(SAVE_df, lineup_i)
  TRACKER_df <- rbind(TRACKER_df, sort(lineup_i$ID))
  print(lineup_i)
  
  # update exposure list 
  EXPOSURE_list <- update_exposure_list_fx(select_ids = lineup_i$ID, run = RUN, exposure_list = EXPOSURE_list)
  
  # create / recreate exposure constraints 
  expo_LPs <- create_exposure_lp_fx(setup_df = SETUP_list$DF, exposure_list = EXPOSURE_list)
  
  # append exposure constraints 
  SETUP_list$LP[['EXPO_MAX']] <- expo_LPs$EXPO_MAX
  SETUP_list$LP[['EXPO_MIN']] <- expo_LPs$EXPO_MIN

}
names(TRACKER_df) <- sub("^","ID", 1:ncol(TRACKER_df))
print(glue('Number of Duplicates: {sum(duplicated(TRACKER_df))}'))

SAVE_df$IsDuplicate <- rep(duplicated(TRACKER_df), each = ncol(TRACKER_df))


if (isTRUE(SAVE)) {
  write.csv(SAVE_df, SAVE_PATH, row.names = FALSE)
  print(glue('SAVED LINEUPS: {SAVE_PATH}'))
}

