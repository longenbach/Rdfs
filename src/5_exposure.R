library(glue)
sessionInfo()


init_exposure_list_fx <- function(setup_df, N) {
  # Initialize: main list
  exposure_list = list('ID' = list(), 
                       'N' = N, 
                       'exclude_maxIDs' = NULL, 
                       'include_minIDs' = NULL
                      )
  # Initialize: each sub list
  ids <- unique(setup_df$ID)
  for (i in ids) {
    exposure_list$ID[[i]] = list('expo_n' = 0, # n = how many lineups
                              'expo_rp' = 0, # rp = running % of lineups
                              'expo_tp' = 0, # tp = total (N) % of lineups
                              'Min.Exposure' = setup_df$Min.Exposure[setup_df$ID == i][1], # input .csv file defined Min.Exposure
                              'Max.Exposure' = setup_df$Max.Exposure[setup_df$ID == i][1] # input .csv file defined Max.Exposure
                              ) 
  }
  
  return(exposure_list)
}


update_exposure_list_fx <- function(select_ids, run_iter, exposure_list = EXPOSURE_list) {
  
  # Update: how many lineups
  for (i in select_ids) {
    exposure_list$ID[[i]]$expo_n <- exposure_list$ID[[i]]$expo_n + 1
  }
  
  # Update: running % of lineups / total (N) % of lineups
  exposure_list$run_iter <- run_iter
  for (jj in names(exposure_list$ID)) {
    exposure_list$ID[[jj]]$expo_rp <- exposure_list$ID[[jj]]$expo_n / exposure_list$run_iter
    exposure_list$ID[[jj]]$expo_tp <- exposure_list$ID[[jj]]$expo_n / exposure_list$N
  }

  # Determine: which IDs are over/under the Max.Exposure/Min.Exposure limits
  # AfterEachExposureStrategy ------------------------------------------------------------------------------------------
  running_exposures <- unlist(lapply(exposure_list$ID, get, x="expo_rp")) 
  rp_greaterthan_maxexpo <- running_exposures > unlist(lapply(exposure_list$ID, get, x="Max.Exposure"))
  rp_lessthan_minexpo <- running_exposures < unlist(lapply(exposure_list$ID, get, x="Min.Exposure"))
  
  exclude_maxexpo_ids <- names(rp_greaterthan_maxexpo[rp_greaterthan_maxexpo])
  include_minexpo_ids <- names(rp_lessthan_minexpo[rp_lessthan_minexpo])
  # --------------------------------------------------------------------------------------------------------------------
  
  # Update: which IDs to exclude / include
  exposure_list$exclude_maxIDs <- exclude_maxexpo_ids
  exposure_list$include_minIDs <- include_minexpo_ids
  
    
  return(exposure_list)
}


create_exposure_lp_fx <- function(setup_df, exposure_list = EXPOSURE_list) {
  LP_list <- list('EXPO_MAX' = NULL, 
                  'EXPO_MIN' = NULL)
  
  emi <- exposure_list$exclude_maxIDs
  if (length(emi) != 0) {
    LP_list$EXPO_MAX <- list('con' = t(sapply(emi, 
                                              function(id) as.numeric(setup_df$ID == id))),
                             'dir' = rep('==', times=length(emi)),
                             'rhs' = rep(0, times=length(emi))
    )
  }
  
  imi <- exposure_list$include_minIDs
  if (length(imi) != 0) {
    LP_list$EXPO_MIN <- list('con' = t(sapply(imi, 
                                              function(id) as.numeric(setup_df$ID == id))),
                             'dir' = rep('==', times=length(imi)),
                             'rhs' = rep(1, times=length(imi))
    )
  }
  
  return(LP_list)
}


