library(glue)
sessionInfo()


# Hard-code required INPUT_COLUMNS, OPTIMIZER options, & CONSTRAINTS for each sport
INPUT_COLUMNS <- c("ID", "Name", "Position", "Position2", "Price", "Team", "Opponent", "Projection", "SD", "Min.deviation", "Max.deviation", "Min.Exposure", "Max.Exposure", "Floor", "Ceiling")
OPTIMIZER_OPTIONS <- c('multi_pos', 'flex_pos', 'standard')
CONSTRAINTS <- list(
  'NBA' = list('PRICE_CAP' = 100000,
               'POSITIONS' = list('PG' = 2, 'SG' = 2, 'SF' = 2, 'PF' = 2, 'C' = 1), 
               'TEAM_CAP' = list('n' = 5),
               'optimizer' = 'multi_pos',
               'notes' = '🏀 NBA'
               ),
  'AFL_5' = list('PRICE_CAP' = 100000,
                 'POSITIONS' = list('FWD' = 2, 'MID' = 4, 'DEF' = 2, 'RK' = 1),
                 'TEAM_CAP' = list('n' = 5),
                 'optimizer' = 'multi_pos',
                 'notes' = '🏉 AFL multi game version'
                 ),
  'AFL_8' = list('PRICE_CAP' = 100000,
                 'POSITIONS' = list('FWD' = 2, 'MID' = 4, 'DEF' = 2, 'RK' = 1),
                 'TEAM_CAP' = list('n' = 8),
                 'optimizer' = 'multi_pos',
                 'notes' = '🏉 AFL single game version'
                 ),
  'NFL' = list('PRICE_CAP' = 100000,
               'POSITIONS' = list('QB' = 1, 'RB' = c(2,3), 'WR' = c(3,4), 'TE' = c(1,2), 'DST' = 1),
               'TEAM_CAP' =  list('n' = 4, 'exclude' = c('DST')),
               'SIZE_CAP' = list('n' = 9),
               'optimizer' = 'flex_pos',
               'notes' = '🏈 NFL'
               ),
  'GOLF' = list('PRICE_CAP' = 100000,
                'SIZE_CAP' = list('n' = 6),
                'optimizer' = 'standard',
                'notes' = '🐅 Golf'
                )
)


format_input_fx <- function(df, input_columns = INPUT_COLUMNS) {
  
  # Check: Missing Columns
  missing_cols <- setdiff(input_columns, names(df))
  if ( length( missing_cols ) != 0 ) {
    stop(glue('🛑 input .csv file is missing the following columns: {paste(missing_cols, collapse = ", ")}'))
  }
  
  # Check: Duplicate IDs
  dup_ids <- df$ID[duplicated(df$ID)] 
  if ( length(dup_ids) != 0 ) {
    stop(glue('🛑 ID column has duplicates for the following IDs: {paste(dup_ids, collapse = ", ")}'))
  }
  
  # Format: 
  df$ID <- as.character(df$ID)
  
  return(df)
}


get_sport_fx <- function(sport, sport_options = names(CONSTRAINTS), constraints = CONSTRAINTS) {
  
  # Check: Sport Option is defined
  if ( !(sport %in% sport_options) ) {
    stop(glue('🛑 only the following sport options are available: {paste(sport_options, collapse = ", ")}'))
  }
  
  return(CONSTRAINTS[sport])
}


setup_lp_fx <- function(get_sport, df, optimizer_options = OPTIMIZER_OPTIONS) {
  
  # Initialize: 
  LP_dict <- list() 
  constr <- get_sport[[1]]
  
  # Check: Optimizer Option is available 
  if ( !(constr$optimizer %in% optimizer_options) ) {
    stop(glue('🛑 the sport: {names(get_sport)} needs one of the following optimizers: {paste(optimizer_options, collapse = ", ")}'))
  }
  
  # Check: all Positions constraints are defined 
  missing_positions <- setdiff(df$Position, names(constr$POSITIONS))
  if ( length(missing_positions) != 0 ) {
    stop(glue('🛑 Position column has undefined constraints for the following Positions: {paste(missing_positions, collapse = ", ")}'))
  }
  
  ## Multiple Position Optimizer 
  if (constr$optimizer == 'multi_pos') {
    multi_df <- df[df$Position2 %in% names(constr$POSITIONS),]
    multi_df$Position <- multi_df$Position2
    opt_df <- rbind(df, multi_df)
    row.names(opt_df) <- NULL # reset the row names
    
    LP_dict[['PRICE_CAP']] <- list('con' = opt_df$Price, 
                                   'dir' = '<=', 
                                   'rhs' = constr$PRICE_CAP
                                  )
    
    
    position_names <- names(constr$POSITIONS) # unique(opt_df$Position) 
    LP_dict[['POSITIONS']] <- list('con' = t(sapply(position_names, 
                                                    function(pos) as.numeric(opt_df$Position == pos))),
                                   'dir' = rep('==', times=length(position_names)),
                                   'rhs' = sapply(position_names, function(pos) constr$POSITIONS[[pos]])
                                  )
    
    id_names <- opt_df$ID[duplicated(opt_df$ID)]
    LP_dict[['DUALS']] <- list('con' = t(sapply(id_names, 
                                                function(id) as.numeric(opt_df$ID == id))),
                               'dir' = rep('<=', times=length(id_names)),
                               'rhs' = rep(1, times=length(id_names))
                              )
    
    
    team_names <- unique(opt_df$Team)                               
    LP_dict[['TEAM_CAP']] <- list('con' = t(sapply(team_names, 
                                                   function(team) as.numeric(opt_df$Team == team))),
                                  'dir' = rep('<=', times=length(team_names)),
                                  'rhs' = rep(constr$TEAM_CAP$n, times=length(team_names))
                                  )
  }
  
  ## Flex Position Optimizer 
  if (constr$optimizer == 'flex_pos') {
    opt_df <- df
    
    LP_dict[['PRICE_CAP']] <- list('con' = opt_df$Price, 
                                   'dir' = '<=', 
                                   'rhs' = constr$PRICE_CAP
    )
    
    assign_dir_fx <- function(pos_vec) {
      if(length(pos_vec) == 1) { return( c('==') ) }
      if(length(pos_vec) == 2) { return( c('>=', '<=') ) }
      stop('🛑 each position needs a single limit ( format: limit ) or mulitiple limit ( format: c(min, max) )')
    }
    
    position_names_rep <- rep(names(constr$POSITIONS), sapply(constr$POSITIONS, length))
    LP_dict[['POSITIONS']] <- list('con' = t(sapply(position_names_rep, 
                                                    function(pos) as.numeric(opt_df$Position == pos))),
                                   'dir' = unlist(sapply(constr$POSITIONS, assign_dir_fx)),
                                   'rhs' = unlist(sapply(constr$POSITIONS, sort))
                                  )
    
    LP_dict[['SIZE_CAP']] <- list('con' = rep(1, each = length(opt_df$ID)), 
                                  'dir' = '==', 
                                  'rhs' = constr$SIZE_CAP
                                  )
    
    team_names <- unique(opt_df$Team)                               
    LP_dict[['TEAM_CAP']] <- list('con' = t(sapply(team_names, 
                                                   function(team) as.numeric(opt_df$Team == team & !(opt_df$Position %in% constr$TEAM_CAP$exclude)))),
                                  'dir' = rep('<=', times=length(team_names)),
                                  'rhs' = rep(constr$TEAM_CAP$n, times=length(team_names))
                                  )
  }
  
  ## Simple Standard Optimizer
  if (constr$optimizer == 'standard') {
    opt_df <- df
    
    LP_dict[['PRICE_CAP']] <- list('con' = opt_df$Price, 
                                   'dir' = '<=', 
                                   'rhs' = constr$PRICE_CAP
                                  )
    
    LP_dict[['SIZE_CAP']] <- list('con' = rep(1, each = length(opt_df$ID)), 
                                  'dir' = '==', 
                                  'rhs' = constr$SIZE_CAP
                                  )
  }
  
  SETUP_list <- list('DF' = opt_df, 'LP' = LP_dict)
  return(SETUP_list)
}



