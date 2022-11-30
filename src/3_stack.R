library(glue)
sessionInfo()


create_pos_same_team_max_lp_fx <- function(setup_df, positions_same_team_max) {
  
  # Setup:
  LP_list <- list('POSITIONS_SAME_TEAM_MAX' = NULL)
  position_names_selected <- names(positions_same_team_max)
  if ( length(position_names_selected) == 0 ) {
    return(LP_list)
  }
  
  # Check: nonexistent positions
  position_names <- unique(setup_df$Position)
  positions_nonexistent <- setdiff(position_names_selected, position_names)
  if ( length(positions_nonexistent) != 0 ) {
    stop(glue('🛑 the positions in POSITIONS_SAME_TEAM_MAX are nonexistent: {paste(positions_nonexistent, collapse = ", ")}'))
  }
  

  # Constraint: 
  team_names <- unique(setup_df$Team)  
  con_matrix <- matrix(nrow = 0, ncol = nrow(setup_df))
  for ( pos in position_names_selected ) {
    con_tmp <- t(sapply(team_names, function(team) as.numeric(setup_df$Team == team & setup_df$Position == pos)))
    row.names(con_tmp) <- sub("^", glue('{pos}_'), row.names(con_tmp))    
    con_matrix <- rbind(con_matrix, con_tmp)
  }
  
  LP_list$POSITIONS_SAME_TEAM_MAX <- list('con' = con_matrix,
                                          'dir' = rep('<=', times=nrow(con_matrix)),
                                          'rhs' = rep(unlist(positions_same_team_max), each = length(team_names))
                                         )
  return(LP_list)
}