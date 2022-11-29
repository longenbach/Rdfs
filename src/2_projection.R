library(glue)
sessionInfo()


PROJECTION_METHODS <- c('standard', 'uniform', 'normal', 'deviation')
modify_projections_fx <- function(setup_df, method, projection_methods = PROJECTION_METHODS) {
  # Initialize: 
  dedup_df <- setup_df[!duplicated(setup_df$ID),] 
  proj_df <- data.frame('ID' = dedup_df$ID, 
                        'Projection_modify' = NA, 
                        'Projection_method' = rep(method, times = nrow(dedup_df))
  )
  
  # Check: Method is available 
  if ( !(method %in% projection_methods) ) {
    stop(glue('🛑 the projection method: {method} needs one of the following methods: {paste(projection_methods, collapse = ", ")}'))
  }
  
  # Method: standard
  if (method == 'standard') {
    proj_df$Projection_modify <- dedup_df$Projection
    return(proj_df)
  }
  
  # Define: Projection Functions 
  proj_uniform_fx <- function(floor, ceiling) {
    return( runif(1, min = floor, max = ceiling) )
  }
  
  proj_normal_fx <- function(projection, standard_dev) {
    return( rnorm(1, mean = projection, sd = standard_dev) )
  }
  
  proj_deviation_fx <- function(projection, min_deviation, max_deviation) {
    multiplier <- runif(1, min = min_deviation, max = max_deviation)
    neg1_or_pos1 <- 2*rbinom(n = 1, size = 1, prob = 0.5) - 1
    multiplier_rand <- 1 + neg1_or_pos1 * multiplier
    return(multiplier_rand * projection)
  }
  
  # Method: uniform
  if (method == 'uniform') {
    proj_df$Projection_modify <- mapply(proj_uniform_fx, 
                                        floor = dedup_df$Floor, 
                                        dedup_df$Ceiling)
  }
  
  # Method: normal
  if (method == 'normal') {
    proj_df$Projection_modify <- mapply(proj_normal_fx, 
                                        projection = dedup_df$Projection, 
                                        standard_dev = dedup_df$SD)
  }
  
  # Method: deviation
  if (method == 'deviation') {
    proj_df$Projection_modify <- mapply(proj_deviation_fx, projection = dedup_df$Projection, 
                                        min_deviation = dedup_df$Min.deviation,
                                        max_deviation = dedup_df$Max.deviation)
  }
  
  return(proj_df)
}


append_projections_fx <- function(setup_df, proj_df) {
  # Define: order before merge
  setup_df$keep_order <- 1:nrow(setup_df)
  
  # Remove: proj_df columns from setup_df if present
  proj_non_id_cols <- setdiff(names(proj_df), 'ID') 
  for ( col in proj_non_id_cols){
    setup_df[col] <- NULL
  }
  
  # Merge: setup_df w/ projections
  setup_df_NEW <- merge(x = setup_df, y = proj_df, by = 'ID', all.x = TRUE, sort = FALSE)
  
  # Reorder: by keep_order column
  setup_df_NEW <- setup_df_NEW[order(setup_df_NEW$keep_order), ]
  row.names(setup_df_NEW) <- NULL
  return( setup_df_NEW )
}


