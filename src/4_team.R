library(glue)
sessionInfo()


create_team_lp_fx <- function(setup_df, team_max_list, team_min_list, exclude_positions) {
  LP_list <- list('TEAM_MAX' = NULL, 
                  'TEAM_MIN' = NULL)
  team_names <- unique(setup_df$Team)    
  
  # Check: nonexistent Teams Max
  team_max_nonexistent <- setdiff(names(team_max_list), team_names)
  if ( length(team_max_nonexistent) != 0 ) {
    stop(glue('🛑 the teams in TEAM_MAX are nonexistent: {paste(team_max_nonexistent, collapse = ", ")}'))
  }
  
  # Check: nonexistent Teams Min
  team_min_nonexistent <- setdiff(names(team_min_list), team_names)
  if ( length(team_min_nonexistent) != 0 ) {
    stop(glue('🛑 the teams in TEAM_MIN are nonexistent: {paste(team_min_nonexistent, collapse = ", ")}'))
  }
  
  # Check: all exclude_positions are legit
  exclude_positions_nonlegit <- setdiff(exclude_positions, setup_df$Position)
  if ( length(exclude_positions_nonlegit) != 0 ) {
    stop(glue('🛑 the positions in exclude_positions are nonexistent: {paste(exclude_positions_nonlegit, collapse = ", ")}'))
  }
  
  
  team_names_max <- names(team_max_list)
  if (length(team_names_max) != 0) {
    LP_list$TEAM_MAX <- list('con' = t(sapply(team_names_max, 
                                                         function(team) as.numeric(setup_df$Team == team & !(setup_df$Position %in% exclude_positions)))),
                                        'dir' = rep('<=', times=length(team_names_max)),
                                        'rhs' = unlist(team_max_list)
    )
  }
  
  team_names_min <- names(team_min_list)
  if (length(team_names_min) != 0) {
    LP_list$TEAM_MIN <- list('con' = t(sapply(team_names_min, 
                                              function(team) as.numeric(setup_df$Team == team & !(setup_df$Position %in% exclude_positions)))),
                             'dir' = rep('>=', times=length(team_names_min)),
                             'rhs' = unlist(team_min_list)
    )
  }

  return(LP_list)
}




