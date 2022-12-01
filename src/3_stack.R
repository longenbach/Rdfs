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



create_positions_stack_lp_fx <- function(setup_df, positions_stack) {
  
  # Setup:
  LP_list <- list('POSITIONS_STACK' = NULL)
  position_stack_names <- names(positions_stack)
  if ( length(position_stack_names) == 0 ) {
    return(LP_list)
  }
  
  # Check: NEED TO ADD CHECKS 
  
  
  # Functions: 
  which_team_fx <- function(is_same, row_df) {
    if (isTRUE(is_same)) {
      team <- row_df$Team
    }
    if (isFALSE(is_same)) {
      team <- row_df$Opponent
    }
    return(team)
  }
  
  
  # Constraint: 
  con_matrix <- matrix(nrow = 0, ncol = nrow(setup_df))
  for ( j in 1:length(position_stack_names) ) {
    pos <- position_stack_names[[j]]
    stack <- positions_stack[[j]]$stack
    n <- positions_stack[[j]]$n
    same_team <- positions_stack[[j]]$same_team
    
    pos_IDs <- setup_df$ID[setup_df$Position == pos]
    
    con_tmp <- t(sapply(pos_IDs, function(id) {
      id_TF <- setup_df$ID == id & setup_df$Position == pos
      # Check: NEED TO CHECK length(id_TF) == 1
      team <- which_team_fx(same_team, setup_df[id_TF, ])
      as.numeric(setup_df$ID != id & setup_df$Position %in% stack & setup_df$Team == team) + ifelse(id_TF, -n, 0)
    }))
    row.names(con_tmp) <- sub("^", glue('{pos}_{n}_{paste(stack, collapse = "-")}_{same_team}_'), row.names(con_tmp))  
    con_matrix <- rbind(con_matrix, con_tmp)
  }
  
  LP_list$POSITIONS_STACK <- list('con' = con_matrix,
                                          'dir' = rep('>=', times=nrow(con_matrix)),
                                          'rhs' = rep(0, each = nrow(con_matrix))
  )
  return(LP_list)
}









