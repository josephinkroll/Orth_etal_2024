# By: Jasper Denissen
# 2023-07-23
# Compute LAI masks per decade
mode <- 'all'
print(paste0('mode == ', mode))
if(mode == 'all'){
  load("/Net/Groups/BGI/people/jdenis/scripts/Data/202306_cmip6_Nature_Perspective_yr_LAI_trends_acccmip6_all.RData")
}else if(mode == 'aligned'){
  load("/Net/Groups/BGI/people/jdenis/scripts/Data/202306_cmip6_Nature_Perspective_yr_LAI_trends_acccmip6_aligned.RData")
}

# ########################################################################################################################
# ##########################################                                  ############################################
# ##########################################        new model list (w/ hurs)  ############################################
# ##########################################                                  ############################################
# ########################################################################################################################

lon <- seq(-179,179,2)
lat <- seq(-89,89,2)

# load table containing source_id and member_id of the regridded CMIP6
path_cmip6 <- "/Net/Groups/BGI/scratch/jdenis/Data/cmip6_202312_climex_ESD_acccmip6/"
path_cmip6_2.0x2.0 <- "/Net/Groups/BGI/scratch/jdenis/Data/cmip6_202312_climex_ESD_acccmip6_2.0x2.0/"
list_all <- list.files(path_cmip6)
list_hurs <- list_all[grep("hurs_*",list_all)]
list_mrsol <- list_all[grep("mrsol_*",list_all)]

source_id <- member_id <- c()
for(i in 1:length(list_hurs)){
  source_id[i] <- strsplit(list_hurs[i],"_")[[1]][3]
  member_id[i] <- strsplit(list_hurs[i],"_")[[1]][5]
}

source_id_mrsol <- member_id_mrsol <- c()
for(i in 1:length(list_mrsol)){
  source_id_mrsol[i] <- strsplit(list_mrsol[i],"_")[[1]][3]
}

cmip6_data.df <- setNames(data.frame(matrix(ncol = 2, nrow = 0)),
                          c("source_id","member_id"))
for(source in unique(source_id)){
  if(source %in% unique(source_id_mrsol)){
    print(source)
    cmip6_data.df <- rbind(cmip6_data.df,
                           data.frame("source_id" = source,
                                      "member_id" = member_id[which(source_id == source)[1]]))
  }
}
cmip6_data.df$SM <- 'mrsol'

# decadal values
if(mode == 'all'){
  av_lai.array <-
    array(NaN,c(180,90,length(cmip6_data.df$source_id)*12))
  count_all <- 1
  for(i in 1:length(cmip6_data.df$source_id)){ 
    av_lai.array[,,count_all:(count_all+11)] <- av_lai.list[[i]]
    count_all <- count_all + 12
    print(paste(i, " is done...",sep=''))
  }
  # Rearrange the grid cells, because they are shifted 180 degrees in longitude
  test <- array(NaN,c(180,90,length(cmip6_data.df$source_id)*12)); test[1:90,,] <- av_lai.array[91:180,,]; test[91:180,,] <- av_lai.array[1:90,,]; av_lai.array <- test
  mask_lai <- array(NaN,c(180,90,length(cmip6_data.df$source_id)*12))
  mask_lai[which(av_lai.array > 0.5)] <- 1
  # already apply the lai mask! 
  
  # grid cells for the regression analysis should only retain values if the respective model has all values in the time series.
  mask_obs <- array(NaN,c(180,90,length(cmip6_data.df$source_id)*12))
}else if(mode == 'aligned'){
  av_lai.array <-
    array(NaN,c(180,90,length(cmip6_data.df$source_id)*6))
  count_all <- 1
  for(i in 1:length(cmip6_data.df$source_id)){ 
    av_lai.array[,,count_all:(count_all+5)] <- av_lai.list[[i]]
    count_all <- count_all + 6
    print(paste(i, " is done...",sep=''))
  }
  # Rearrange the grid cells, because they are shifted 180 degrees in longitude
  test <- array(NaN,c(180,90,length(cmip6_data.df$source_id)*6)); test[1:90,,] <- av_lai.array[91:180,,]; test[91:180,,] <- av_lai.array[1:90,,]; av_lai.array <- test
  mask_lai <- array(NaN,c(180,90,length(cmip6_data.df$source_id)*6))
  mask_lai[which(av_lai.array > 0.5)] <- 1
  # already apply the lai mask! 
  
  # grid cells for the regression analysis should only retain values if the respective model has all values in the time series.
  mask_obs <- array(NaN,c(180,90,length(cmip6_data.df$source_id)*6))
}

ct_dec <- 1
ct_yr <- 1
mode <- 'all'
print(paste0('mode == ', mode))
if(mode == 'all'){
  mask_obs_yr <- array(NaN,c(180,90,length(cmip6_data.df$source_id)*120))
  for(i in 1:length(cmip6_data.df$source_id)){ 
    for(x in 1:180){
      for(y in 1:90){
        if(sum(!is.na(mask_lai[x,y,(ct_dec:(ct_dec+11))])) == 12){ # check if all the decades have a value there
          mask_obs[x,y,(ct_dec:(ct_dec+11))] <- 1
          if(mode == 'all'){
            mask_obs_yr[x,y,(ct_yr:(ct_yr+119))] <- 1
          }else if(mode == 'aligned'){
            mask_obs_yr[x,y,(ct_yr:(ct_yr+119))] <- 1
          }
          
        }
      }
    }
    ct_dec <- ct_dec + 12
    ct_yr <- ct_yr + 120
  }
}else if(mode == 'aligned'){
  mask_obs_yr <- array(NaN,c(180,90,length(cmip6_data.df$source_id)*60))
  for(i in 1:length(cmip6_data.df$source_id)){ 
    for(x in 1:180){
      for(y in 1:90){
        if(sum(!is.na(mask_lai[x,y,(ct_dec:(ct_dec+5))])) == 6){ # check if all the decades have a value there
          mask_obs[x,y,(ct_dec:(ct_dec+5))] <- 1
          if(mode == 'all'){
            mask_obs_yr[x,y,(ct_yr:(ct_yr+119))] <- 1
          }else if(mode == 'aligned'){
            mask_obs_yr[x,y,(ct_yr:(ct_yr+59))] <- 1
          }
          
        }
      }
    }
    ct_dec <- ct_dec + 6
    ct_yr <- ct_yr + 60
  }
}



if(mode == 'all'){
  file_to_save <- "/Net/Groups/BGI/people/jdenis/scripts/Data/202307_mask_lai_all.RData"
}else if(mode == 'aligned'){
  file_to_save <- "/Net/Groups/BGI/people/jdenis/scripts/Data/202307_mask_lai_aligned.RData"
}
save(
  mask_obs,
  mask_obs_yr,
  
  file = file_to_save)
