library(glue)
sessionInfo()


format_lineup_fx <- function(lineup_df, get_sport) {
  constr <- get_sport[[1]]
  
  # Check: NEED TO ADD CHECKS 
  
  # Append: site_order based on original position ordering
  positions_ord <- constr$POSITIONS
  temp_df <- data.frame('Position' = names(positions_ord), 'site_order' = 1:length(positions_ord))
  lineup_df_so <- merge(lineup_df, temp_df,  on = 'Position', all.x = TRUE)
  

  # Only for 'flex_pos' optimizer
  if (SPORT_list[[1]]$optimizer == 'flex_pos'){
    
    # Determine min(ID) of the FLEX position
    pos_lineup_min <- unlist(lapply(positions_ord, min)) 
    pos_lineup_select <- sapply(names(positions_ord), function(pos) sum(lineup_i$Position == pos))
    pos_flex <- names(pos_lineup_select[pos_lineup_select != pos_lineup_min])
    flex_min_id <- min(lineup_df_so[lineup_df_so$Position == pos_flex, ]$ID)
    
    
    # Subset: 
    rest_df <- lineup_df_so[lineup_df_so$ID != flex_min_id & lineup_df_so$Position != 'DST', ]
    flex_df <- lineup_df_so[lineup_df_so$ID == flex_min_id, ]
    dst_df <- lineup_df_so[lineup_df_so$Position == 'DST', ] # TO-DO: Remove 'DST' hardcode 

    # Assign: site_order & reassign position to flex
    flex_df$site_order <- max(temp_df$site_order)
    flex_df$Position <- 'FLEX'
    dst_df$site_order <- dst_df$site_order + 1
    
    # Concat: 
    lineup_df_so <- rbind(rest_df, flex_df, dst_df) 
  }
  

  lineup_df_order <- lineup_df_so[order(lineup_df_so$site_order),]
  header <- lineup_df_order$Position
  
  name_id <- mapply(FUN = function(name, id) {glue('{name} ({id})')}, lineup_df_order$Name, lineup_df_order$ID)
  output_df <- rbind(data.frame(matrix(nrow = 0, ncol = length(header))), name_id)
  names(output_df) <- header
  output_df
  
  return(output_df)
  
}

