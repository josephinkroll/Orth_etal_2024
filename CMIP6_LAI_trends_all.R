# By: Jasper Denissen
# 2023-06-25
# scripts to compute CMIP6 LAI & other trends from 1982 - 2001, 2002 - 2021 and 2081 - 2100
mode <- 'all'
print(paste0('mode == ', mode))
if(mode == 'all'){
  load("/Net/Groups/BGI/people/jdenis/scripts/Data/202306_cmip6_Nature_Perspective_yr_LAI_trends_acccmip6_all.RData")
  load("/Net/Groups/BGI/people/jdenis/scripts/Data/total_land_area_all.RData")
  load("/Net/Groups/BGI/people/jdenis/scripts/Data/202307_mask_lai_all.RData")
}else if(mode == 'aligned'){
  load("/Net/Groups/BGI/people/jdenis/scripts/Data/202306_cmip6_Nature_Perspective_yr_LAI_trends_acccmip6_aligned.RData")
  load("/Net/Groups/BGI/people/jdenis/scripts/Data/total_land_area_aligned.RData")
  load("/Net/Groups/BGI/people/jdenis/scripts/Data/202307_mask_lai_aligned.RData")
}
# load("/Net/Groups/BGI/people/jdenis/scripts/Data/202306_cmip6_Nature_Perspective_ELI_10yr.RData")
# load("/Net/Groups/BGI/people/jdenis/scripts/Data/202306_cmip6_Nature_Perspective_ELI_10yr_aligned.RData")


source('/Net/Groups/BGI/people/jdenis/scripts/functions/plot_discrete_cbar.R')

# ########################################################################################################################
# ##########################################                                  ############################################
# ##########################################        new model list (w/ hurs)  ############################################
# ##########################################                                  ############################################
# ########################################################################################################################

lon <- seq(-179,179,2)
lat <- seq(-89,89,2)

# maybe now per surface area instead of % of grid cells?
r <- raster()  # by default 1 by 1 degree
res(r) <- 2 # so change the resolution
a <- raster::area(r) # calculate the area of a 2x2 degree grid from N - S, as area varies only by latitude, not longitude
area <- a[,1]
area.array <- array(NaN,c(180,90))
for(x in 1:180){
  area.array[x,] <- area
}

# function to compute mean of randomly sampled 10 and of the other 10 (yearly) values.
# returns the difference between those two means.
random_trend <- function(x, min=20){
  if(sum(!is.na(x)) == min){
    # if(sum(!is.na(x)) == min & sum(x != 0) == min){
    mean1 <- c()
    mean2 <- c()
    trend <- c()
    sample1 <- sample(1:min, size=10, replace=F)
    mean1 <- mean(x[sample1])
    mean2 <- mean(x[setdiff(1:min, sample1)])
    trend <- mean2 - mean1
  } else{
    trend <- NaN
  }
  if(!is.na(trend) & trend != 0){
    return(trend)
  } else{
    return(NaN)
  }
}

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
  # member_id_mrsol[i] <- strsplit(list_mrsol[i],"_")[[1]][5]
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
# cmip6_data.df$SM[c(3,10:13)] <- 'mrso'

# # now the strongest average energy proxy
# dcorr_max.list <- list()
# corr_rgy_veg_max.list <- corr_rgy_veg_rsds.list
# tas_or_rsds.array <- array(NaN,c(180,90,length(cmip6_data.df$source_id)))
# tas_cum.array <- array(0,c(180,90))
# for(i in 1:length(cmip6_data.df$source_id)){
#   for(x in 1:180){
#     for(y in 1:90){
#       if(sum(!is.na(corr_rgy_veg.list[[i]][x,y,])) > 0){
#         if(mean(corr_rgy_veg.list[[i]][x,y,], na.rm = T) > mean(corr_rgy_veg_rsds.list[[i]][x,y,], na.rm = T)){
#           corr_rgy_veg_max.list[[i]][x,y,] <- corr_rgy_veg.list[[i]][x,y,]
#           tas_or_rsds.array[x,y,i] <- 1 # temperature
#           tas_cum.array[x,y] <- tas_cum.array[x,y] + 1
#         }else{
#           tas_or_rsds.array[x,y,i] <- 2 # shortwave incoming radiation
#         }
#       }
#     }
#   }
#   dcorr_max.list[[i]] <- corr_wtr_veg.list[[i]] - corr_rgy_veg_max.list[[i]]
# }

# decadal values
# dcorr_max.array <-
av_lai.array <-
  av_mrsol.array <-
  av_vpd.array <-
  av_pr.array <-
  av_AI.array <- 
  array(NaN,c(180,90,length(cmip6_data.df$source_id)*12))
count_all <- 1
for(i in 1:length(cmip6_data.df$source_id)){ 
  # dcorr_max.array[,,count_all:(count_all+11)] <- dcorr_max.list[[i]]
  av_lai.array[,,count_all:(count_all+11)] <- av_lai.list[[i]]
  av_mrsol.array[,,count_all:(count_all+11)] <- av_mrsol.list[[i]]
  av_vpd.array[,,count_all:(count_all+11)] <- av_vpd.list[[i]]
  av_pr.array[,,count_all:(count_all+11)] <- av_pr.list[[i]]
  av_AI.array[,,count_all:(count_all+11)] <- av_netrad.list[[i]]/av_pr.list[[i]]
  count_all <- count_all + 12
  print(paste(i, " is done...",sep=''))
}
# Rearrange the grid cells, because they are shifted 180 degrees in longitude
test <- array(NaN,c(180,90,length(cmip6_data.df$source_id)*12)); test[1:90,,] <- av_lai.array[91:180,,]; test[91:180,,] <- av_lai.array[1:90,,]; av_lai.array <- test
# mask_lai <- array(NaN,c(180,90,length(cmip6_data.df$source_id)*12))
# mask_lai[which(av_lai.array > 0.5)] <- 1
# # already apply the lai mask! 
# 
# # grid cells for the regression analysis should only retain values if the respective model has all values in the time series.
# mask_obs <- array(NaN,c(180,90,length(cmip6_data.df$source_id)*12))
# mask_obs_yr <- array(NaN,c(180,90,length(cmip6_data.df$source_id)*120))
# ct_dec <- 1
# ct_yr <- 1
# for(i in 1:length(cmip6_data.df$source_id)){ 
#   for(x in 1:180){
#     for(y in 1:90){
#       if(sum(!is.na(mask_lai[x,y,(ct_dec:(ct_dec+11))])) == 12){ # check if all the models have a value there
#         mask_obs[x,y,(ct_dec:(ct_dec+11))] <- 1
#         mask_obs_yr[x,y,(ct_yr:(ct_yr+119))] <- 1
#       }
#     }
#   }
#   ct_dec <- ct_dec + 6
#   ct_yr <- ct_yr + 60
# }

av_lai.array <- av_lai.array * mask_obs
# test <- array(NaN,c(180,90,length(cmip6_data.df$source_id)*12)); test[1:90,,] <- dcorr_max.array[91:180,,]; test[91:180,,] <- dcorr_max.array[1:90,,]; dcorr_max.array <- test * mask_obs
test <- array(NaN,c(180,90,length(cmip6_data.df$source_id)*12)); test[1:90,,] <- av_mrsol.array[91:180,,]; test[91:180,,] <- av_mrsol.array[1:90,,]; av_mrsol.array <- test * mask_obs
test <- array(NaN,c(180,90,length(cmip6_data.df$source_id)*12)); test[1:90,,] <- av_vpd.array[91:180,,]; test[91:180,,] <- av_vpd.array[1:90,,]; av_vpd.array <- test * mask_obs
test <- array(NaN,c(180,90,length(cmip6_data.df$source_id)*12)); test[1:90,,] <- av_pr.array[91:180,,]; test[91:180,,] <- av_pr.array[1:90,,]; av_pr.array <- test * mask_obs
test <- array(NaN,c(180,90,length(cmip6_data.df$source_id)*12)); test[1:90,,] <- av_AI.array[91:180,,]; test[91:180,,] <- av_AI.array[1:90,,]; av_AI.array <- test * mask_obs

# yearly values
av_lai_yr.array <-
  av_mrsol_yr.array <-
  av_vpd_yr.array <-
  av_pr_yr.array <-
  av_netrad_yr.array <-
  array(NaN,c(180,90,length(cmip6_data.df$source_id)*120))
count_all <- 1
for(i in 1:length(cmip6_data.df$source_id)){ 
  av_lai_yr.array[,,count_all:(count_all+119)] <- av_lai_yr.list[[i]]
  av_mrsol_yr.array[,,count_all:(count_all+119)] <- av_mrsol_yr.list[[i]]
  av_vpd_yr.array[,,count_all:(count_all+119)] <- av_vpd_yr.list[[i]]
  av_pr_yr.array[,,count_all:(count_all+119)] <- av_pr_yr.list[[i]]
  av_netrad_yr.array[,,count_all:(count_all+119)] <- av_netrad_yr.list[[i]]
  count_all <- count_all + 120
  print(paste(i, " is done...",sep=''))
}
# Rearrange the grid cells, because they are shifted 180 degrees in longitude
test <- array(NaN,c(180,90,length(cmip6_data.df$source_id)*120)); test[1:90,,] <- av_lai_yr.array[91:180,,]; test[91:180,,] <- av_lai_yr.array[1:90,,]; av_lai_yr.array <- test * mask_obs_yr
test <- array(NaN,c(180,90,length(cmip6_data.df$source_id)*120)); test[1:90,,] <- av_mrsol_yr.array[91:180,,]; test[91:180,,] <- av_mrsol_yr.array[1:90,,]; av_mrsol_yr.array <- test * mask_obs_yr
test <- array(NaN,c(180,90,length(cmip6_data.df$source_id)*120)); test[1:90,,] <- av_vpd_yr.array[91:180,,]; test[91:180,,] <- av_vpd_yr.array[1:90,,]; av_vpd_yr.array <- test * mask_obs_yr
test <- array(NaN,c(180,90,length(cmip6_data.df$source_id)*120)); test[1:90,,] <- av_pr_yr.array[91:180,,]; test[91:180,,] <- av_pr_yr.array[1:90,,]; av_pr_yr.array <- test * mask_obs_yr
test <- array(NaN,c(180,90,length(cmip6_data.df$source_id)*120)); test[1:90,,] <- av_netrad_yr.array[91:180,,]; test[91:180,,] <- av_netrad_yr.array[1:90,,]; av_netrad_yr.array <- test * mask_obs_yr

# for(t in 1:60){
#   print(min(c(av_lai_yr.array[,,t]),na.rm=T))
# }

diff_LAI <- 
  diff_mrsol <- 
  diff_vpd <- 
  diff_pr <- 
  diff_AI <- 
  array(NaN, c(180,90,length(cmip6_data.df$source_id),12))
dec <- 1
for(i in 1:length(cmip6_data.df$source_id)){
  for(ct_6 in seq(1,12,2)){
    diff_LAI[,,i,ct_6] <- av_lai.array[,,dec+1] - av_lai.array[,,dec]
    diff_mrsol[,,i,ct_6] <- av_mrsol.array[,,dec+1] - av_mrsol.array[,,dec]
    diff_vpd[,,i,ct_6] <- av_vpd.array[,,dec+1] - av_vpd.array[,,dec]
    diff_pr[,,i,ct_6] <- av_pr.array[,,dec+1] - av_pr.array[,,dec]
    diff_AI[,,i,ct_6] <- av_AI.array[,,dec+1] - av_AI.array[,,dec]
    dec <- dec + 2
  }
}

trend_LAI <- 
  trend_mrsol <- 
  trend_vpd <- 
  trend_pr <- 
  trend_AI <- 
  array(NaN, c(180,90,length(cmip6_data.df$source_id),12))
dec <- 1
for(i in 1:length(cmip6_data.df$source_id)){ 
  for(ct_6 in seq(1,12,2)){
    # for(yr20 in 1:6){
    for(x in 1:180){
      for(y in 1:90){
        if(!is.na(diff_LAI[x,y,i,ct_6]) & av_lai.array[x,y,dec] > 0.5 & av_lai.array[x,y,(dec+1)] > 0.5){
          if(diff_LAI[x,y,i,ct_6] < 0){
            trend_LAI[x,y,i,ct_6] <- 1 #decreasing
          }else{
            trend_LAI[x,y,i,ct_6] <- 2 #increasing
          }
          if(diff_mrsol[x,y,i,ct_6] < 0){
            trend_mrsol[x,y,i,ct_6] <- 1 #decreasing
          }else{
            trend_mrsol[x,y,i,ct_6] <- 2 #increasing
          }
          if(diff_vpd[x,y,i,ct_6] < 0){
            trend_vpd[x,y,i,ct_6] <- 1 #decreasing
          }else{
            trend_vpd[x,y,i,ct_6] <- 2 #increasing
          }
          if(diff_pr[x,y,i,ct_6] < 0){
            trend_pr[x,y,i,ct_6] <- 1 #decreasing
          }else{
            trend_pr[x,y,i,ct_6] <- 2 #increasing
          }
          if(diff_AI[x,y,i,ct_6] < 0){
            trend_AI[x,y,i,ct_6] <- 1 #decreasing
          }else{
            trend_AI[x,y,i,ct_6] <- 2 #increasing
          }
        }
      }
    }
    dec <- dec + 2
  }
}
trend_LAI <- trend_LAI[,,,seq(1,12,2)]
diff_LAI <- diff_LAI[,,,seq(1,12,2)]
trend_mrsol <- trend_mrsol[,,,seq(1,12,2)]
diff_mrsol <- diff_mrsol[,,,seq(1,12,2)]
trend_vpd <- trend_vpd[,,,seq(1,12,2)]
diff_vpd <- diff_vpd[,,,seq(1,12,2)]
trend_pr <- trend_pr[,,,seq(1,12,2)]
diff_pr <- diff_pr[,,,seq(1,12,2)]
trend_AI <- trend_AI[,,,seq(1,12,2)]
diff_AI <- diff_AI[,,,seq(1,12,2)]

x=93;y=70 # Germany
x=88;y=66 # Spain
# x=86;y=66 # Spain but more west
x=68;y=38 # Brazil
x=163;y=29 # AUS
x=40;y=65 # test dom_water_var

ct_20 <- 1
sign_LAI <- 
  sign_mrsol <- 
  sign_vpd <- 
  sign_pr <- 
  array(NaN,c(180,90,length(cmip6_data.df$source_id),6))
for(i in 1:length(cmip6_data.df$source_id)){ 
  ct_3 <- 1
  for(yr20 in 1:6){
    for(x in 1:180){
      for(y in 1:90){
        if(sum(!is.na(diff_LAI[x,y,i,])) == 6 & length(which(diff_LAI[x,y,i,] != 0)) == 6 & sum(!is.na(av_lai_yr.array[x,y,(ct_20:(ct_20+19))])) == 20){
          if(yr20 != 2){
            t_LAI <- unname(quantile(replicate(300, random_trend(av_lai_yr.array[x,y,(ct_20:(ct_20+19))])), probs=c(0.05, 0.95), na.rm=T))
          }else{
            t_LAI <- unname(quantile(replicate(300, random_trend(av_lai_yr.array[x,y,(ct_20:(ct_20+18))], min=19)), probs=c(0.05, 0.95), na.rm=T))
          }
          
          if(diff_LAI[x,y,i,ct_3] >= t_LAI[2] | diff_LAI[x,y,i,ct_3] < t_LAI[1]){
            sign_LAI[x,y,i,ct_3] <- 1
          } else{
            sign_LAI[x,y,i,ct_3] <- 2
          }
          if(yr20 != 2){
            t_mrsol <- unname(quantile(replicate(300, random_trend(av_mrsol_yr.array[x,y,(ct_20:(ct_20+19))])), probs=c(0.05, 0.95), na.rm=T))
          }else{
            t_mrsol <- unname(quantile(replicate(300, random_trend(av_mrsol_yr.array[x,y,(ct_20:(ct_20+18))], min=19)), probs=c(0.05, 0.95), na.rm=T))
          }
          
          if(diff_mrsol[x,y,i,ct_3] >= t_mrsol[2] | diff_mrsol[x,y,i,ct_3] < t_mrsol[1]){
            sign_mrsol[x,y,i,ct_3] <- 1
          } else{
            sign_mrsol[x,y,i,ct_3] <- 2
          }
          if(yr20 != 2){
            t_vpd <- unname(quantile(replicate(300, random_trend(av_vpd_yr.array[x,y,(ct_20:(ct_20+19))])), probs=c(0.05, 0.95), na.rm=T))
          }else{
            t_vpd <- unname(quantile(replicate(300, random_trend(av_vpd_yr.array[x,y,(ct_20:(ct_20+18))], min=19)), probs=c(0.05, 0.95), na.rm=T))
          }
          
          if(diff_vpd[x,y,i,ct_3] >= t_vpd[2] | diff_vpd[x,y,i,ct_3] < t_vpd[1]){
            sign_vpd[x,y,i,ct_3] <- 1
          } else{
            sign_vpd[x,y,i,ct_3] <- 2
          }
          if(yr20 != 2){
            t_pr <- unname(quantile(replicate(300, random_trend(av_pr_yr.array[x,y,(ct_20:(ct_20+19))])), probs=c(0.05, 0.95), na.rm=T))
          }else{
            t_pr <- unname(quantile(replicate(300, random_trend(av_pr_yr.array[x,y,(ct_20:(ct_20+18))], min=19)), probs=c(0.05, 0.95), na.rm=T))
          }
          
          if(diff_pr[x,y,i,ct_3] >= t_pr[2] | diff_pr[x,y,i,ct_3] < t_pr[1]){
            sign_pr[x,y,i,ct_3] <- 1
          } else{
            sign_pr[x,y,i,ct_3] <- 2
          }
        }
      }
      print(paste0(cmip6_data.df$source_id[i], " ", ct_3, "/6 ", round(x/(180)*100,2),"%"))
    }
    ct_3 <- ct_3 + 1
    ct_20 <- ct_20 + 20
    if(yr20 == 2){
      ct_20 <- ct_20 - 1
    }
  }
}

valid_indices <- array(NaN, c(180,90,length(cmip6_data.df$source_id),6))
ct_20 <- 1
for(i in 1:length(cmip6_data.df$source_id)){ 
  for(yr20 in 1:6){
    for(x in 1:180){
      for(y in 1:90){
        if(yr20 != 2){
          if(sum(!is.na(av_pr_yr.array[x,y,(ct_20:(ct_20+19))])) == 20 & sum(!is.na(av_netrad_yr.array[x,y,(ct_20:(ct_20+19))])) == 20){
            valid_indices[x,y,i,yr20] <- 1
          }
        }else{
          if(sum(!is.na(av_pr_yr.array[x,y,(ct_20:(ct_20+18))])) == 19 & sum(!is.na(av_netrad_yr.array[x,y,(ct_20:(ct_20+18))])) == 19){
            valid_indices[x,y,i,yr20] <- 1
          }}
      }
      # print(x)
    }
    ct_20 <- ct_20 + 20
    if(yr20 == 2){
      ct_20 <- ct_20 - 1
    }
  }
}



# y_MSWEPtp_1st <- abind(y_MSWEPtp_80*lambda, y_MSWEPtp_90*lambda)
# y_MSWEPtp_2nd <- abind(y_MSWEPtp_00*lambda, y_MSWEPtp_10*lambda)
# 
# y_snr_1st <- abind(y_snr_80, y_snr_90)
# y_snr_2nd <- abind(y_snr_00, y_snr_10)
# 
# year_idx_1st <- as.numeric(abind(y_80_idx, y_90_idx))
# year_idx_2nd <- as.numeric(abind(y_00_idx, y_10_idx))
# 
# rndm_years_1st <- replicate(300,sample(year_idx_1st, 10, replace=F))
# rndm_years_2nd <- replicate(300,sample(year_idx_2nd, 10, replace=F))


# Number of cores to use
num_cores <- 8  # Adjust according to your system

# Register parallel backend
registerDoParallel(cores = num_cores)

# year_idx <- seq(1982,2001,1)
rndm_years <- replicate(300,sample(1:20, 10, replace=F))
rndm_years19 <- replicate(300,sample(1:19, 10, replace=F))

ct_20 <- 1
years <- array(NaN,c(length(cmip6_data.df$source_id),5,20))
years19 <- array(NaN,c(length(cmip6_data.df$source_id),1,19))
for(i in 1:length(cmip6_data.df$source_id)){
  for(yr20 in 1:6){
    if(yr20 != 2){
      years[i,yr20,] <- ct_20:(ct_20+19)
    }else{
      years19[i,1,] <- ct_20:(ct_20+18)
    }
    ct_20 <- ct_20 + 20
    if(yr20 == 2){
      ct_20 <- ct_20 - 1
    }
  }
}

# i <- 1
# # Compute correlations for valid grid cells across multiple years in parallel
# AIidx_rndm <- foreach(yr20 = 1:6, .combine = "c") %:%
#   foreach(x = 1:180, .combine = "c") %:%
#   foreach(y = 1:90, .combine = "c") %dopar% {
#     if (!is.na(valid_indices[x,y,i,yr20])) {
#       AI_idx <- rep(NaN, 300)
#       for (s in 1:600) {
#         AI_idx[s] <- mean(av_netrad_yr.array[x,y,years[i,yr20,rndm_years[,s]]])/mean(av_pr_yr.array[x,y,years[i,yr20,rndm_years[,s]]])
#       }
#       AI_idx
#     } else {
#       rep(NaN, 300)
#     }
#   }

# Compute correlations for valid grid cells across multiple years in parallel
AIidx_rndm <- foreach(i = 1:length(cmip6_data.df$source_id), .combine = "c") %:%
  foreach(yr20 = 1:6, .combine = "c") %:%
  foreach(x = 1:180, .combine = "c") %:%
  foreach(y = 1:90, .combine = "c") %dopar% {
    if(!is.na(valid_indices[x,y,i,yr20])) {
      AI_idx <- rep(NaN, 300)
      for(s in 1:300) {
        if(yr20 != 2){
          yrs_i <- years[i,yr20,rndm_years[,s]]
        }else{
          yrs_i <- years19[i,1,rndm_years19[,s]]
        }
        
        AI_idx[s] <- mean(av_netrad_yr.array[x,y,yrs_i])/mean(av_pr_yr.array[x,y,yrs_i])
      }
      AI_idx
    }else {
      rep(NaN, 300)
    }
  }

AIidx_rndm_nonsel <- foreach(i = 1:length(cmip6_data.df$source_id), .combine = "c") %:%
  foreach(yr20 = 1:6, .combine = "c") %:%
  foreach(x = 1:180, .combine = "c") %:%
  foreach(y = 1:90, .combine = "c") %dopar% {
    if(!is.na(valid_indices[x,y,i,yr20])) {
      AI_idx <- rep(NaN, 300)
      for(s in 1:300) {
        if(yr20 != 2){
          yrs_i <- setdiff(years[i,yr20,],years[i,yr20,rndm_years[,s]])
        }else{
          yrs_i <- setdiff(years19[i,1,],years19[i,1,rndm_years19[,s]])
        }
        
        AI_idx[s] <- mean(av_netrad_yr.array[x,y,yrs_i])/mean(av_pr_yr.array[x,y,yrs_i])
      }
      AI_idx
    } else {
      rep(NaN, 300)
    }
  }

#Deregister parallel backend
stopImplicitCluster()

#Then reshaping the array
# AI_rndm_ar <- aperm(array(AIidx_rndm, c(300, 90, 180, 3)), c(3,2,4,1))
AI_rndm_ar <- aperm(array(AIidx_rndm, c(300, 90, 180, 6, length(cmip6_data.df$source_id))), c(3,2,5,4,1))
AI_rndm_ar_nonsel <- aperm(array(AIidx_rndm_nonsel, c(300, 90, 180, 6, length(cmip6_data.df$source_id))), c(3,2,5,4,1))

AI_trend <- AI_rndm_ar_nonsel - AI_rndm_ar

ct_20 <- 1
sign_AI <- 
  array(NaN,c(180,90,length(cmip6_data.df$source_id),6))
for(i in 1:length(cmip6_data.df$source_id)){ 
  ct_3 <- 1
  for(yr20 in 1:6){
    for(x in 1:180){
      for(y in 1:90){
        if(sum(!is.na(diff_LAI[x,y,i,])) == 6 & length(which(diff_LAI[x,y,i,] != 0)) == 6 & sum(!is.na(av_lai_yr.array[x,y,(ct_20:(ct_20+19))])) == 20){
          t_AI <- unname(quantile(AI_trend[x,y,i,yr20,], probs=c(0.05, 0.95), na.rm=T))
          if(diff_AI[x,y,i,ct_3] >= t_AI[2] | diff_AI[x,y,i,ct_3] < t_AI[1]){
            sign_AI[x,y,i,ct_3] <- 1
          } else{
            sign_AI[x,y,i,ct_3] <- 2
          }
        }
      }
      print(paste0(cmip6_data.df$source_id[i], " ", ct_3, "/3 ", round(x/(180)*100,2),"%"))
    }
    ct_3 <- ct_3 + 1
    ct_20 <- ct_20 + 20
    if(yr20 == 2){
      ct_20 <- ct_20 - 1
    }
  }
}

# # save.image("/Net/Groups/BGI/people/jdenis/scripts/Data/CMIP6_LAI_trends.RData")
# save.image("/Net/Groups/BGI/people/jdenis/scripts/Data/CMIP6_LAI_trends_wwatvar_all.RData")

# image.plot(sign_LAI[,,1,3], breaks = c(0, 1.1, 2.1, 2.4), col = c("green4", "lightgreen", "grey"))

sign_trend_LAI <- array(NaN, c(180,90,length(cmip6_data.df$source_id),6))
ct_6 <- 1
for(h in 1:length(cmip6_data.df$source_id)){ 
  for(i in 1:6){
    for(x in 1:180){
      for(y in 1:90){
        if(sum(!is.na(mask_obs[x,y,(ct_6:(ct_6+11))])) == 12){
          if(!is.na(trend_LAI[x,y,h,i]) & !is.na(sign_LAI[x,y,h,i])){
            if(trend_LAI[x,y,h,i] == 1 & sign_LAI[x,y,h,i] == 1){
              sign_trend_LAI[x,y,h,i] <- 1 #decreasing & significant
            }else if(trend_LAI[x,y,h,i] == 1 & sign_LAI[x,y,h,i] == 2){
              sign_trend_LAI[x,y,h,i] <- 2 #decreasing & insignificant
            }else if(trend_LAI[x,y,h,i] == 2 & sign_LAI[x,y,h,i] == 1){
              sign_trend_LAI[x,y,h,i] <- 4 #increasing & significant
            }else if(trend_LAI[x,y,h,i] == 2 & sign_LAI[x,y,h,i] == 2){
              sign_trend_LAI[x,y,h,i] <- 3 #increasing & insignificant
            }
          }else{
            sign_trend_LAI[x,y,h,i] <- 5 # significance or trend is missing
          }
          # }else{
          #   sign_trend_LAI[x,y,h,i] <- 5 # not part of the study area
        }
      }
    }
  }
  ct_6 <- ct_6 + 6
}

sign_trend_mrsol <- array(NaN, c(180,90,length(cmip6_data.df$source_id),6))
ct_6 <- 1
for(h in 1:length(cmip6_data.df$source_id)){ 
  for(i in 1:6){
    for(x in 1:180){
      for(y in 1:90){
        if(sum(!is.na(mask_obs[x,y,(ct_6:(ct_6+11))])) == 12){
          if(!is.na(trend_mrsol[x,y,h,i]) & !is.na(sign_mrsol[x,y,h,i])){
            if(trend_mrsol[x,y,h,i] == 1 & sign_mrsol[x,y,h,i] == 1){
              sign_trend_mrsol[x,y,h,i] <- 1 #decreasing & significant
            }else if(trend_mrsol[x,y,h,i] == 1 & sign_mrsol[x,y,h,i] == 2){
              sign_trend_mrsol[x,y,h,i] <- 2 #decreasing & insignificant
            }else if(trend_mrsol[x,y,h,i] == 2 & sign_mrsol[x,y,h,i] == 1){
              sign_trend_mrsol[x,y,h,i] <- 4 #increasing & significant
            }else if(trend_mrsol[x,y,h,i] == 2 & sign_mrsol[x,y,h,i] == 2){
              sign_trend_mrsol[x,y,h,i] <- 3 #increasing & insignificant
            }
          }else{
            sign_trend_mrsol[x,y,h,i] <- 5 # significance or trend is missing
          }
          # }else{
          #   sign_trend_mrsol[x,y,h,i] <- 5 # not part of the study area
        }
      }
    }
  }
  ct_6 <- ct_6 + 6
}

sign_trend_vpd <- array(NaN, c(180,90,length(cmip6_data.df$source_id),6))
ct_6 <- 1
for(h in 1:length(cmip6_data.df$source_id)){ 
  for(i in 1:6){
    for(x in 1:180){
      for(y in 1:90){
        if(sum(!is.na(mask_obs[x,y,(ct_6:(ct_6+11))])) == 12){
          if(!is.na(trend_vpd[x,y,h,i]) & !is.na(sign_vpd[x,y,h,i])){
            if(trend_vpd[x,y,h,i] == 1 & sign_vpd[x,y,h,i] == 1){
              sign_trend_vpd[x,y,h,i] <- 1 #decreasing & significant
            }else if(trend_vpd[x,y,h,i] == 1 & sign_vpd[x,y,h,i] == 2){
              sign_trend_vpd[x,y,h,i] <- 2 #decreasing & insignificant
            }else if(trend_vpd[x,y,h,i] == 2 & sign_vpd[x,y,h,i] == 1){
              sign_trend_vpd[x,y,h,i] <- 4 #increasing & significant
            }else if(trend_vpd[x,y,h,i] == 2 & sign_vpd[x,y,h,i] == 2){
              sign_trend_vpd[x,y,h,i] <- 3 #increasing & insignificant
            }
          }else{
            sign_trend_vpd[x,y,h,i] <- 5 # significance or trend is missing
          }
          # }else{
          #   sign_trend_vpd[x,y,h,i] <- 5 # not part of the study area
        }
      }
    }
  }
  ct_6 <- ct_6 + 6
}

sign_trend_pr <- array(NaN, c(180,90,length(cmip6_data.df$source_id),6))
ct_6 <- 1
for(h in 1:length(cmip6_data.df$source_id)){ 
  for(i in 1:6){
    for(x in 1:180){
      for(y in 1:90){
        if(sum(!is.na(mask_obs[x,y,(ct_6:(ct_6+11))])) == 12){
          if(!is.na(trend_pr[x,y,h,i]) & !is.na(sign_pr[x,y,h,i])){
            if(trend_pr[x,y,h,i] == 1 & sign_pr[x,y,h,i] == 1){
              sign_trend_pr[x,y,h,i] <- 1 #decreasing & significant
            }else if(trend_pr[x,y,h,i] == 1 & sign_pr[x,y,h,i] == 2){
              sign_trend_pr[x,y,h,i] <- 2 #decreasing & insignificant
            }else if(trend_pr[x,y,h,i] == 2 & sign_pr[x,y,h,i] == 1){
              sign_trend_pr[x,y,h,i] <- 4 #increasing & significant
            }else if(trend_pr[x,y,h,i] == 2 & sign_pr[x,y,h,i] == 2){
              sign_trend_pr[x,y,h,i] <- 3 #increasing & insignificant
            }
          }else{
            sign_trend_pr[x,y,h,i] <- 5 # significance or trend is missing
          }
          # }else{
          #   sign_trend_pr[x,y,h,i] <- 5 # not part of the study area
        }
      }
    }
  }
  ct_6 <- ct_6 + 6
}

sign_trend_AI <- array(NaN, c(180,90,length(cmip6_data.df$source_id),6))
ct_6 <- 1
for(h in 1:length(cmip6_data.df$source_id)){ 
  for(i in 1:6){
    for(x in 1:180){
      for(y in 1:90){
        if(sum(!is.na(mask_obs[x,y,(ct_6:(ct_6+11))])) == 12){
          if(!is.na(trend_AI[x,y,h,i]) & !is.na(sign_AI[x,y,h,i])){
            if(trend_AI[x,y,h,i] == 1 & sign_AI[x,y,h,i] == 1){
              sign_trend_AI[x,y,h,i] <- 1 #decreasing & significant
            }else if(trend_AI[x,y,h,i] == 1 & sign_AI[x,y,h,i] == 2){
              sign_trend_AI[x,y,h,i] <- 2 #decreasing & insignificant
            }else if(trend_AI[x,y,h,i] == 2 & sign_AI[x,y,h,i] == 1){
              sign_trend_AI[x,y,h,i] <- 4 #increasing & significant
            }else if(trend_AI[x,y,h,i] == 2 & sign_AI[x,y,h,i] == 2){
              sign_trend_AI[x,y,h,i] <- 3 #increasing & insignificant
            }
          }else{
            sign_trend_AI[x,y,h,i] <- 5 # significance or trend is missing
          }
          # }else{
          #   sign_trend_AI[x,y,h,i] <- 5 # not part of the study area
        }
      }
    }
  }
  ct_6 <- ct_6 + 6
}

cols_trends_LAI <- c("peru", "moccasin", "lightgreen", "green4", "lightgrey", "white")
cols_trends_wtr <- c("peru", "moccasin", "deepskyblue", "dodgerblue", "lightgrey", "white")
cols_trends_vpd <- c("dodgerblue", "deepskyblue", "moccasin", "peru", "lightgrey", "white")
cuts_classes <- c(0,1.01,2.01,3.01,4.01, 5.01)
labels_trends_LAI <- c("- LAI*", "- LAI", "+ LAI", "+ LAI*", "Missing trend/sign.", "NA")
labels_trends_mrsol <- c("- SM*", "- SM", "+ SM", "+ SM*", "Missing trend/sign.", "NA")
labels_trends_vpd <- c("- VPD*", "- VPD", "+ VPD", "+ VPD*", "Missing trend/sign.", "NA")
labels_trends_pr <- c("- precip.*", "- precip.", "+ precip.", "+ precip.*", "Missing trend/sign.", "NA")
labels_trends_AI <- c("- Dryness*", "- Dryness", "+ Dryness", "+ Dryness*", "Missing trend/sign.", "NA")
periods_classes <- c("1982 - 2001", "2002 - 2020", "2021 - 2040", "2041 - 2060", "2061 - 2080", "2081 - 2100")

x=93;y=70 # Germany
x=88;y=66 # Spain
# x=86;y=66 # Spain but more west
x=68;y=38 # Brazil
x=163;y=29 # AUS
x=40;y=65 # test dom_water_var

# Multi-model means
################################################################################################################
##########################################               #######################################################
##########################################      LAI      #######################################################
##########################################               #######################################################
################################################################################################################
mean_sign_trend_LAI <- array(NaN,c(180,90,6))
mean_sign_trend_LAI_tie <- 
  array(NaN,c(180,90,6,3))
for(i in 1:6){
  for(x in 1:180){
    for(y in 1:90){
      if(sum(!is.na(sign_trend_LAI[x,y,,i])) > 5 & length(which(sign_trend_LAI[x,y,,i] < 5)) > 5){
        # if(sum(!is.na(sign_trend_LAI[x,y,,i])) > 5){
        count_sign_trend_LAI <- c()
        for(v in 1:4){ # loop over all possible trends
          count_sign_trend_LAI[v] <- length(which(sign_trend_LAI[x,y,,i] == v))
        }
        if(max(count_sign_trend_LAI, na.rm = T) > 2){
          if(length(which(count_sign_trend_LAI == max(count_sign_trend_LAI, na.rm = T))) == 1){ # if one water variable is the dominant one across all models
            mean_sign_trend_LAI[x,y,i] <- which(count_sign_trend_LAI == max(count_sign_trend_LAI, na.rm = T))
          }else{ # 5 if it's a tie
            mean_sign_trend_LAI[x,y,i] <- 5
            if(length(which(count_sign_trend_LAI == max(count_sign_trend_LAI, na.rm = T))) == 2){ # 2way tie
              mean_sign_trend_LAI_tie[x,y,i,c(1,2)] <- which(count_sign_trend_LAI == max(count_sign_trend_LAI, na.rm = T))
            }else{ #3way tie
              mean_sign_trend_LAI_tie[x,y,i,] <- which(count_sign_trend_LAI == max(count_sign_trend_LAI, na.rm = T))
            }
          }
        }else{ # When there are 2 or less CMIP6 models agreeing on dominant variable
          mean_sign_trend_LAI[x,y,i] <- 6
        }
      }else if(sum(!is.na(sign_trend_LAI[x,y,,i])) > 0){ # 10 < 6/9 Models with values
        mean_sign_trend_LAI[x,y,i] <- 7
      }
    }
  }
}


sign_trend_LAI.df <- setNames(data.frame(matrix(ncol = 5, nrow = 0)),
                              c("lon","lat","LAI","source_id","period"))


for(h in 1:length(cmip6_data.df$source_id)){ 
  for(i in 1:6){
    for(x in 1:180){
      for(y in 1:90){
        if(!is.na(sign_trend_LAI[x,y,h,i])){
          sign_trend_LAI.df <- rbind(sign_trend_LAI.df,
                                     data.frame("lon" = lon[x],
                                                "lat" = lat[y],
                                                "LAI" = sign_trend_LAI[x,y,h,i],
                                                "source_id" = cmip6_data.df$source_id[h],
                                                "period" = periods_classes[i]))
        }
      }
    }
  }
}

sign_trend_LAI.df$cuts_LAI <- cut(sign_trend_LAI.df$LAI, cuts_classes, include.lowest = T)

################################################################################################################
##########################################               #######################################################
##########################################      mrsol    #######################################################
##########################################               #######################################################
################################################################################################################
mean_sign_trend_mrsol <- array(NaN,c(180,90,6))
mean_sign_trend_mrsol_tie <- 
  array(NaN,c(180,90,6,3))
for(i in 1:6){
  for(x in 1:180){
    for(y in 1:90){
      if(sum(!is.na(sign_trend_mrsol[x,y,,i])) > 5 & length(which(sign_trend_mrsol[x,y,,i] < 5)) > 5){
        count_sign_trend_mrsol <- c()
        for(v in 1:4){ # loop over all possible trends
          count_sign_trend_mrsol[v] <- length(which(sign_trend_mrsol[x,y,,i] == v))
        }
        if(max(count_sign_trend_mrsol, na.rm = T) > 2){
          if(length(which(count_sign_trend_mrsol == max(count_sign_trend_mrsol, na.rm = T))) == 1){ # if one water variable is the dominant one across all models
            mean_sign_trend_mrsol[x,y,i] <- which(count_sign_trend_mrsol == max(count_sign_trend_mrsol, na.rm = T))
          }else{ # 5 if it's a tie
            mean_sign_trend_mrsol[x,y,i] <- 5
            if(length(which(count_sign_trend_mrsol == max(count_sign_trend_mrsol, na.rm = T))) == 2){ # 2way tie
              mean_sign_trend_mrsol_tie[x,y,i,c(1,2)] <- which(count_sign_trend_mrsol == max(count_sign_trend_mrsol, na.rm = T))
            }else{ #3way tie
              mean_sign_trend_mrsol_tie[x,y,i,] <- which(count_sign_trend_mrsol == max(count_sign_trend_mrsol, na.rm = T))
            }
          }
        }else{ # When there are 2 or less CMIP6 models agreeing on dominant variable
          mean_sign_trend_mrsol[x,y,i] <- 6
        }
      }else if(sum(!is.na(sign_trend_mrsol[x,y,,i])) > 0){ # 10 < 6/9 Models with values
        mean_sign_trend_mrsol[x,y,i] <- 7
      }
    }
  }
}


sign_trend_mrsol.df <- setNames(data.frame(matrix(ncol = 5, nrow = 0)),
                                c("lon","lat","mrsol","source_id","period"))


for(h in 1:length(cmip6_data.df$source_id)){ 
  for(i in 1:6){
    for(x in 1:180){
      for(y in 1:90){
        if(!is.na(sign_trend_mrsol[x,y,h,i])){
          sign_trend_mrsol.df <- rbind(sign_trend_mrsol.df,
                                       data.frame("lon" = lon[x],
                                                  "lat" = lat[y],
                                                  "mrsol" = sign_trend_mrsol[x,y,h,i],
                                                  "source_id" = cmip6_data.df$source_id[h],
                                                  "period" = periods_classes[i]))
        }
      }
    }
  }
}

sign_trend_mrsol.df$cuts_mrsol <- cut(sign_trend_mrsol.df$mrsol, cuts_classes, include.lowest = T)

################################################################################################################
##########################################               #######################################################
##########################################      vpd      #######################################################
##########################################               #######################################################
################################################################################################################
mean_sign_trend_vpd <- array(NaN,c(180,90,6))
mean_sign_trend_vpd_tie <- 
  array(NaN,c(180,90,6,3))
for(i in 1:6){
  for(x in 1:180){
    for(y in 1:90){
      if(sum(!is.na(sign_trend_vpd[x,y,,i])) > 5 & length(which(sign_trend_vpd[x,y,,i] < 5)) > 5){
        count_sign_trend_vpd <- c()
        for(v in 1:4){ # loop over all possible trends
          count_sign_trend_vpd[v] <- length(which(sign_trend_vpd[x,y,,i] == v))
        }
        if(max(count_sign_trend_vpd, na.rm = T) > 2){
          if(length(which(count_sign_trend_vpd == max(count_sign_trend_vpd, na.rm = T))) == 1){ # if one water variable is the dominant one across all models
            mean_sign_trend_vpd[x,y,i] <- which(count_sign_trend_vpd == max(count_sign_trend_vpd, na.rm = T))
          }else{ # 5 if it's a tie
            mean_sign_trend_vpd[x,y,i] <- 5
            if(length(which(count_sign_trend_vpd == max(count_sign_trend_vpd, na.rm = T))) == 2){ # 2way tie
              mean_sign_trend_vpd_tie[x,y,i,c(1,2)] <- which(count_sign_trend_vpd == max(count_sign_trend_vpd, na.rm = T))
            }else{ #3way tie
              mean_sign_trend_vpd_tie[x,y,i,] <- which(count_sign_trend_vpd == max(count_sign_trend_vpd, na.rm = T))
            }
          }
        }else{ # When there are 2 or less CMIP6 models agreeing on dominant variable
          mean_sign_trend_vpd[x,y,i] <- 6
        }
      }else if(sum(!is.na(sign_trend_vpd[x,y,,i])) > 0){ # 10 < 6/9 Models with values
        mean_sign_trend_vpd[x,y,i] <- 7
      }
    }
  }
}


sign_trend_vpd.df <- setNames(data.frame(matrix(ncol = 5, nrow = 0)),
                              c("lon","lat","vpd","source_id","period"))


for(h in 1:length(cmip6_data.df$source_id)){ 
  for(i in 1:6){
    for(x in 1:180){
      for(y in 1:90){
        if(!is.na(sign_trend_vpd[x,y,h,i])){
          sign_trend_vpd.df <- rbind(sign_trend_vpd.df,
                                     data.frame("lon" = lon[x],
                                                "lat" = lat[y],
                                                "vpd" = sign_trend_vpd[x,y,h,i],
                                                "source_id" = cmip6_data.df$source_id[h],
                                                "period" = periods_classes[i]))
        }
      }
    }
  }
}

sign_trend_vpd.df$cuts_vpd <- cut(sign_trend_vpd.df$vpd, cuts_classes, include.lowest = T)

################################################################################################################
##########################################               #######################################################
##########################################      pr       #######################################################
##########################################               #######################################################
################################################################################################################
mean_sign_trend_pr <- array(NaN,c(180,90,6))
mean_sign_trend_pr_tie <- 
  array(NaN,c(180,90,6,3))
for(i in 1:6){
  for(x in 1:180){
    for(y in 1:90){
      if(sum(!is.na(sign_trend_pr[x,y,,i])) > 5 & length(which(sign_trend_pr[x,y,,i] < 5)) > 5){
        count_sign_trend_pr <- c()
        for(v in 1:4){ # loop over all possible trends
          count_sign_trend_pr[v] <- length(which(sign_trend_pr[x,y,,i] == v))
        }
        if(max(count_sign_trend_pr, na.rm = T) > 2){
          if(length(which(count_sign_trend_pr == max(count_sign_trend_pr, na.rm = T))) == 1){ # if one water variable is the dominant one across all models
            mean_sign_trend_pr[x,y,i] <- which(count_sign_trend_pr == max(count_sign_trend_pr, na.rm = T))
          }else{ # 5 if it's a tie
            mean_sign_trend_pr[x,y,i] <- 5
            if(length(which(count_sign_trend_pr == max(count_sign_trend_pr, na.rm = T))) == 2){ # 2way tie
              mean_sign_trend_pr_tie[x,y,i,c(1,2)] <- which(count_sign_trend_pr == max(count_sign_trend_pr, na.rm = T))
            }else{ #3way tie
              mean_sign_trend_pr_tie[x,y,i,] <- which(count_sign_trend_pr == max(count_sign_trend_pr, na.rm = T))
            }
          }
        }else{ # When there are 2 or less CMIP6 models agreeing on dominant variable
          mean_sign_trend_pr[x,y,i] <- 6
        }
      }else if(sum(!is.na(sign_trend_pr[x,y,,i])) > 0){ # 10 < 6/9 Models with values
        mean_sign_trend_pr[x,y,i] <- 7
      }
    }
  }
}


sign_trend_pr.df <- setNames(data.frame(matrix(ncol = 5, nrow = 0)),
                             c("lon","lat","pr","source_id","period"))


for(h in 1:length(cmip6_data.df$source_id)){ 
  for(i in 1:6){
    for(x in 1:180){
      for(y in 1:90){
        if(!is.na(sign_trend_pr[x,y,h,i])){
          sign_trend_pr.df <- rbind(sign_trend_pr.df,
                                    data.frame("lon" = lon[x],
                                               "lat" = lat[y],
                                               "pr" = sign_trend_pr[x,y,h,i],
                                               "source_id" = cmip6_data.df$source_id[h],
                                               "period" = periods_classes[i]))
        }
      }
    }
  }
}

sign_trend_pr.df$cuts_pr <- cut(sign_trend_pr.df$pr, cuts_classes, include.lowest = T)

################################################################################################################
##########################################               #######################################################
##########################################      AI       #######################################################
##########################################               #######################################################
################################################################################################################
mean_sign_trend_AI <- array(NaN,c(180,90,6))
mean_sign_trend_AI_tie <- 
  array(NaN,c(180,90,6,3))
for(i in 1:6){
  for(x in 1:180){
    for(y in 1:90){
      if(sum(!is.na(sign_trend_AI[x,y,,i])) > 5 & length(which(sign_trend_AI[x,y,,i] < 5)) > 5){
        count_sign_trend_AI <- c()
        for(v in 1:4){ # loop over all possible trends
          count_sign_trend_AI[v] <- length(which(sign_trend_AI[x,y,,i] == v))
        }
        if(max(count_sign_trend_AI, na.rm = T) > 2){
          if(length(which(count_sign_trend_AI == max(count_sign_trend_AI, na.rm = T))) == 1){ # if one water variable is the dominant one across all models
            mean_sign_trend_AI[x,y,i] <- which(count_sign_trend_AI == max(count_sign_trend_AI, na.rm = T))
          }else{ # 5 if it's a tie
            mean_sign_trend_AI[x,y,i] <- 5
            if(length(which(count_sign_trend_AI == max(count_sign_trend_AI, na.rm = T))) == 2){ # 2way tie
              mean_sign_trend_AI_tie[x,y,i,c(1,2)] <- which(count_sign_trend_AI == max(count_sign_trend_AI, na.rm = T))
            }else{ #3way tie
              mean_sign_trend_AI_tie[x,y,i,] <- which(count_sign_trend_AI == max(count_sign_trend_AI, na.rm = T))
            }
          }
        }else{ # When there are 2 or less CMIP6 models agreeing on dominant variable
          mean_sign_trend_AI[x,y,i] <- 6
        }
      }else if(sum(!is.na(sign_trend_AI[x,y,,i])) > 0){ # 10 < 6/9 Models with values
        mean_sign_trend_AI[x,y,i] <- 7
      }
    }
  }
}


sign_trend_AI.df <- setNames(data.frame(matrix(ncol = 5, nrow = 0)),
                             c("lon","lat","AI","source_id","period"))


for(h in 1:length(cmip6_data.df$source_id)){ 
  for(i in 1:6){
    for(x in 1:180){
      for(y in 1:90){
        if(!is.na(sign_trend_AI[x,y,h,i])){
          sign_trend_AI.df <- rbind(sign_trend_AI.df,
                                    data.frame("lon" = lon[x],
                                               "lat" = lat[y],
                                               "AI" = sign_trend_AI[x,y,h,i],
                                               "source_id" = cmip6_data.df$source_id[h],
                                               "period" = periods_classes[i]))
        }
      }
    }
  }
}

sign_trend_AI.df$cuts_AI <- cut(sign_trend_AI.df$AI, cuts_classes, include.lowest = T)

# save.image("/Net/Groups/BGI/people/jdenis/scripts/Data/CMIP6_LAI_trends.RData")
save.image("/Net/Groups/BGI/people/jdenis/scripts/Data/CMIP6_LAI_trends_wwatvar_all.RData")

# This is a data set from the maptools package
data(wrld_simpl)

# Create a data.frame object for ggplot. ggplot requires a data frame.
mymap <- fortify(wrld_simpl)

area_sign_trend_LAI_wo_mask.df <- 
  area_sign_trend_mrsol_wo_mask.df <- 
  area_sign_trend_vpd_wo_mask.df <- 
  area_sign_trend_pr_wo_mask.df <- 
  area_sign_trend_AI_wo_mask.df <- 
  # area_sign_trend_LAI_mask.df <- 
  setNames(data.frame(matrix(ncol = 5, nrow = 0)),
           c("area","sign","var","source_id","period"))
for(h in 1:length(cmip6_data.df$source_id)){ 
  for(i in 1:6){
    for(c in 1:5){
      area_sign_trend_LAI_wo_mask.df <- rbind(area_sign_trend_LAI_wo_mask.df, 
                                              data.frame("area" = (sum(area.array[which(sign_trend_LAI[,,h,i] == c)], na.rm = T)/total_land_area)*100,
                                                         "sign" = c,
                                                         "var" = labels_trends_LAI[c],
                                                         "source_id" = cmip6_data.df$source_id[h],
                                                         "period" = periods_classes[i]))
      
      area_sign_trend_mrsol_wo_mask.df <- rbind(area_sign_trend_mrsol_wo_mask.df, 
                                                data.frame("area" = (sum(area.array[which(sign_trend_mrsol[,,h,i] == c)], na.rm = T)/total_land_area)*100,
                                                           "sign" = c,
                                                           "var" = labels_trends_mrsol[c],
                                                           "source_id" = cmip6_data.df$source_id[h],
                                                           "period" = periods_classes[i]))
      
      area_sign_trend_vpd_wo_mask.df <- rbind(area_sign_trend_vpd_wo_mask.df, 
                                              data.frame("area" = (sum(area.array[which(sign_trend_vpd[,,h,i] == c)], na.rm = T)/total_land_area)*100,
                                                         "sign" = c,
                                                         "var" = labels_trends_vpd[c],
                                                         "source_id" = cmip6_data.df$source_id[h],
                                                         "period" = periods_classes[i]))
      
      area_sign_trend_pr_wo_mask.df <- rbind(area_sign_trend_pr_wo_mask.df, 
                                             data.frame("area" = (sum(area.array[which(sign_trend_pr[,,h,i] == c)], na.rm = T)/total_land_area)*100,
                                                        "sign" = c,
                                                        "var" = labels_trends_pr[c],
                                                        "source_id" = cmip6_data.df$source_id[h],
                                                        "period" = periods_classes[i]))
      
      area_sign_trend_AI_wo_mask.df <- rbind(area_sign_trend_AI_wo_mask.df, 
                                             data.frame("area" = (sum(area.array[which(sign_trend_AI[,,h,i] == c)], na.rm = T)/total_land_area)*100,
                                                        "sign" = c,
                                                        "var" = labels_trends_AI[c],
                                                        "source_id" = cmip6_data.df$source_id[h],
                                                        "period" = periods_classes[i]))
    }
  }
}

# save(
#   sign_trend_LAI.df,
#   sign_trend_mrsol.df,
#   sign_trend_vpd.df,
#   sign_trend_pr.df,
#   sign_trend_AI.df,
#   area_sign_trend_LAI_wo_mask.df,
#   area_sign_trend_mrsol_wo_mask.df,
#   area_sign_trend_vpd_wo_mask.df,
#   area_sign_trend_pr_wo_mask.df,
#   area_sign_trend_AI_wo_mask.df,
#   file = "/Net/Groups/BGI/people/jdenis/scripts/scripts_Nature_Perspective/sign_trend_wtr_var_all.RData"
# )

for(i in 1:6){
  # c <- ggplot(filter(sign_trend_LAI.df, source_id == cmip6_data.df$source_id[1] & period == periods_classes[1]), aes(x=lon,y=lat,fill=cuts_LAI)) +
  c <- ggplot(filter(sign_trend_LAI.df, period == periods_classes[i]), aes(x=lon,y=lat,fill=cuts_LAI)) +
    geom_tile() +
    geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
             inherit.aes = F, fill = NA, color = "black", size = .1) +
    facet_wrap(~source_id) +
    scale_fill_manual("LAI trend",
                      values = cols_trends_LAI,
                      labels = labels_trends_LAI) +
    scale_x_continuous("",
                       limits=c(-180,180),
                       expand=c(0,0)) +
    scale_y_continuous("",
                       limits=c(-60,75),
                       expand=c(0,0)) +
    ggtitle(periods_classes[i]) +
    guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, nrow = 1)) +
    theme(legend.position = "bottom",
          # theme(legend.position = c(.75,.15),
          legend.text = element_text(size=12),
          legend.title = element_text(size=20),
          strip.background = element_blank(),
          axis.line.x = element_line(colour = "black"),
          axis.line.y = element_line(colour = "black"),
          strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(color = 'black', fill = NA, size = 1),
          panel.background = element_blank(),
          axis.text = element_text(size=18),
          # axis.title = element_text(size=20),
          axis.title = element_blank(),
          plot.title = element_text(size=24, hjust = 0.5),
          plot.tag.position = c(.55,0.03)
    )
  # c
  ggsave(paste0("/Net/Groups/BGI/people/jdenis/scripts/scripts_Nature_Perspective/sign_trend_LAI_",substr(periods_classes[i], start = 1, stop = 4),"_",substr(periods_classes[i], start = 8, stop = 11),".png"), plot = c, width = 9*1.5, height = 7*1.5, units = "in")
  
  c <- ggplot(filter(sign_trend_mrsol.df, period == periods_classes[i]), aes(x=lon,y=lat,fill=cuts_mrsol)) +
    geom_tile() +
    geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
             inherit.aes = F, fill = NA, color = "black", size = .1) +
    facet_wrap(~source_id) +
    scale_fill_manual("soil moisture trend",
                      values = cols_trends_wtr,
                      labels = labels_trends_mrsol) +
    scale_x_continuous("",
                       limits=c(-180,180),
                       expand=c(0,0)) +
    scale_y_continuous("",
                       limits=c(-60,75),
                       expand=c(0,0)) +
    ggtitle(periods_classes[i]) +
    guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, nrow = 1)) +
    theme(legend.position = "bottom",
          # theme(legend.position = c(.75,.15),
          legend.text = element_text(size=12),
          legend.title = element_text(size=20),
          strip.background = element_blank(),
          axis.line.x = element_line(colour = "black"),
          axis.line.y = element_line(colour = "black"),
          strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(color = 'black', fill = NA, size = 1),
          panel.background = element_blank(),
          axis.text = element_text(size=18),
          # axis.title = element_text(size=20),
          axis.title = element_blank(),
          plot.title = element_text(size=24, hjust = 0.5),
          plot.tag.position = c(.55,0.03)
    )
  # c
  ggsave(paste0("/Net/Groups/BGI/people/jdenis/scripts/scripts_Nature_Perspective/sign_trend_mrsol_",substr(periods_classes[i], start = 1, stop = 4),"_",substr(periods_classes[i], start = 8, stop = 11),".png"), plot = c, width = 9*1.5, height = 7*1.5, units = "in")
  
  c <- ggplot(filter(sign_trend_vpd.df, period == periods_classes[i]), aes(x=lon,y=lat,fill=cuts_vpd)) +
    geom_tile() +
    geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
             inherit.aes = F, fill = NA, color = "black", size = .1) +
    facet_wrap(~source_id) +
    scale_fill_manual("soil moisture trend",
                      values = cols_trends_vpd,
                      labels = labels_trends_vpd) +
    scale_x_continuous("",
                       limits=c(-180,180),
                       expand=c(0,0)) +
    scale_y_continuous("",
                       limits=c(-60,75),
                       expand=c(0,0)) +
    ggtitle(periods_classes[i]) +
    guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, nrow = 1)) +
    theme(legend.position = "bottom",
          # theme(legend.position = c(.75,.15),
          legend.text = element_text(size=12),
          legend.title = element_text(size=20),
          strip.background = element_blank(),
          axis.line.x = element_line(colour = "black"),
          axis.line.y = element_line(colour = "black"),
          strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(color = 'black', fill = NA, size = 1),
          panel.background = element_blank(),
          axis.text = element_text(size=18),
          # axis.title = element_text(size=20),
          axis.title = element_blank(),
          plot.title = element_text(size=24, hjust = 0.5),
          plot.tag.position = c(.55,0.03)
    )
  # c
  ggsave(paste0("/Net/Groups/BGI/people/jdenis/scripts/scripts_Nature_Perspective/sign_trend_vpd_",substr(periods_classes[i], start = 1, stop = 4),"_",substr(periods_classes[i], start = 8, stop = 11),".png"), plot = c, width = 9*1.5, height = 7*1.5, units = "in")
  
  c <- ggplot(filter(sign_trend_pr.df, period == periods_classes[i]), aes(x=lon,y=lat,fill=cuts_pr)) +
    geom_tile() +
    geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
             inherit.aes = F, fill = NA, color = "black", size = .1) +
    facet_wrap(~source_id) +
    scale_fill_manual("soil moisture trend",
                      values = cols_trends_wtr,
                      labels = labels_trends_pr) +
    scale_x_continuous("",
                       limits=c(-180,180),
                       expand=c(0,0)) +
    scale_y_continuous("",
                       limits=c(-60,75),
                       expand=c(0,0)) +
    ggtitle(periods_classes[i]) +
    guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, nrow = 1)) +
    theme(legend.position = "bottom",
          # theme(legend.position = c(.75,.15),
          legend.text = element_text(size=12),
          legend.title = element_text(size=20),
          strip.background = element_blank(),
          axis.line.x = element_line(colour = "black"),
          axis.line.y = element_line(colour = "black"),
          strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(color = 'black', fill = NA, size = 1),
          panel.background = element_blank(),
          axis.text = element_text(size=18),
          # axis.title = element_text(size=20),
          axis.title = element_blank(),
          plot.title = element_text(size=24, hjust = 0.5),
          plot.tag.position = c(.55,0.03)
    )
  # c
  ggsave(paste0("/Net/Groups/BGI/people/jdenis/scripts/scripts_Nature_Perspective/sign_trend_pr_",substr(periods_classes[i], start = 1, stop = 4),"_",substr(periods_classes[i], start = 8, stop = 11),".png"), plot = c, width = 9*1.5, height = 7*1.5, units = "in")
  
  c <- ggplot(filter(sign_trend_AI.df, period == periods_classes[i]), aes(x=lon,y=lat,fill=cuts_AI)) +
    geom_tile() +
    geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
             inherit.aes = F, fill = NA, color = "black", size = .1) +
    facet_wrap(~source_id) +
    scale_fill_manual("soil moisture trend",
                      values = cols_trends_vpd,
                      labels = labels_trends_AI) +
    scale_x_continuous("",
                       limits=c(-180,180),
                       expand=c(0,0)) +
    scale_y_continuous("",
                       limits=c(-60,75),
                       expand=c(0,0)) +
    ggtitle(periods_classes[i]) +
    guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, nrow = 1)) +
    theme(legend.position = "bottom",
          # theme(legend.position = c(.75,.15),
          legend.text = element_text(size=12),
          legend.title = element_text(size=20),
          strip.background = element_blank(),
          axis.line.x = element_line(colour = "black"),
          axis.line.y = element_line(colour = "black"),
          strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(color = 'black', fill = NA, size = 1),
          panel.background = element_blank(),
          axis.text = element_text(size=18),
          # axis.title = element_text(size=20),
          axis.title = element_blank(),
          plot.title = element_text(size=24, hjust = 0.5),
          plot.tag.position = c(.55,0.03)
    )
  # c
  ggsave(paste0("/Net/Groups/BGI/people/jdenis/scripts/scripts_Nature_Perspective/sign_trend_AI_",substr(periods_classes[i], start = 1, stop = 4),"_",substr(periods_classes[i], start = 8, stop = 11),".png"), plot = c, width = 9*1.5, height = 7*1.5, units = "in")
}

# sign_trend_LAI_1982_2001.png (FigS3) & sign_trend_LAI_2002_2020.png (FigS4)


cols_mean_trends_LAI <- c("peru", "moccasin", "lightgreen", "green4", "black",'grey40',"snow2")
# cols_mean_trends_LAI <- c("green4", "lightgreen", "moccasin", "peru", "black",'grey40',"snow2")
cols_mean_trends_wtr <- c("peru", "moccasin", "deepskyblue", "dodgerblue", "black",'grey40',"snow2")
cols_mean_trends_vpd <- c("dodgerblue", "deepskyblue", "moccasin", "peru", "black",'grey40',"snow2")
cuts_classes_bars <- c(0,1.01,2.01,3.01,4.01, 5.01, 6.01, 7.01)

################################################################################################################
##########################################               #######################################################
##########################################      LAI      #######################################################
##########################################               #######################################################
################################################################################################################
mean_sign_trend_LAI.df <- setNames(data.frame(matrix(ncol = 4, nrow = 0)),
                                   c("lon","lat","LAI","period"))
for(i in 1:6){
  for(x in 1:180){
    for(y in 1:90){
      if(!is.na(mean_sign_trend_LAI[x,y,i])){
        mean_sign_trend_LAI.df <- rbind(mean_sign_trend_LAI.df,
                                        data.frame("lon" = lon[x],
                                                   "lat" = lat[y],
                                                   "LAI" = mean_sign_trend_LAI[x,y,i],
                                                   "period" = periods_classes[i]))
      }
    }
  }
}

labels_mean_trends_LAI <- c("- LAI*", "- LAI", "+ LAI", "+ LAI*",'Tie', '< 3/9 Models agree', '< 6/9 Models with values')
# labels_mean_trends_LAI <- c("+ LAI*","+ LAI", "- LAI", "- LAI*", 'Tie', '< 3/9 Models agree', '< 6/9 Models with values')
area_mean_sign_trend_LAI.df <- setNames(data.frame(matrix(ncol = 3, nrow = 0)),
                                        c("area","var","period"))
for(i in 1:6){
  for(v in 1:4){ # loop over all water variables
    area_mean_sign_trend_LAI.df <- rbind(area_mean_sign_trend_LAI.df, 
                                         data.frame("area" = (sum(area.array[which(mean_sign_trend_LAI[,,i] == v)], na.rm = T)/total_land_area)*100,
                                                    "var" = labels_mean_trends_LAI[v],
                                                    "period" = periods_classes[i]))
  }
}

# x = 120; y = 72
count_i <- 0
for(i in 1:6){
  for(x in 1:180){
    for(y in 1:90){
      if(!is.na(mean_sign_trend_LAI[x,y,i])){
        if(mean_sign_trend_LAI[x,y,i] == 5){
          # convert array into vector for one grid cell
          mean_sign_trend_LAI_tie_c <- c(mean_sign_trend_LAI_tie[x,y,i,][which(!is.na(mean_sign_trend_LAI_tie[x,y,i,]))])
          # area_mean_sign_trend_LAI_tie$area[mean_sign_trend_LAI_tie_c] <- area_mean_sign_trend_LAI_tie$area[mean_sign_trend_LAI_tie_c] + ((area.array[x,y]/total_land_area)*100)/length(mean_sign_trend_LAI_tie_c)
          area_mean_sign_trend_LAI.df$area[mean_sign_trend_LAI_tie_c + count_i] <- area_mean_sign_trend_LAI.df$area[mean_sign_trend_LAI_tie_c + count_i] + ((area.array[x,y]/total_land_area)*100)/length(mean_sign_trend_LAI_tie_c)
        }
      }
    }
  }
  count_i <- count_i + 4
}

area_mean_sign_trend_LAI.df$var_fac <- factor(area_mean_sign_trend_LAI.df$var, levels = labels_mean_trends_LAI)
mean_sign_trend_LAI.df$cuts_LAI <- cut(mean_sign_trend_LAI.df$LAI, cuts_classes_bars, include.lowest = T)

################################################################################################################
##########################################               #######################################################
##########################################      mrsol    #######################################################
##########################################               #######################################################
################################################################################################################
mean_sign_trend_mrsol.df <- setNames(data.frame(matrix(ncol = 4, nrow = 0)),
                                     c("lon","lat","mrsol","period"))
for(i in 1:6){
  for(x in 1:180){
    for(y in 1:90){
      if(!is.na(mean_sign_trend_mrsol[x,y,i])){
        mean_sign_trend_mrsol.df <- rbind(mean_sign_trend_mrsol.df,
                                          data.frame("lon" = lon[x],
                                                     "lat" = lat[y],
                                                     "mrsol" = mean_sign_trend_mrsol[x,y,i],
                                                     "period" = periods_classes[i]))
      }
    }
  }
}

labels_mean_trends_mrsol <- c("- SM*", "- SM", "+ SM", "+ SM*",'Tie', '< 3/9 Models agree', '< 6/9 Models with values')
area_mean_sign_trend_mrsol.df <- setNames(data.frame(matrix(ncol = 3, nrow = 0)),
                                          c("area","var","period"))
for(i in 1:6){
  for(v in 1:4){ # loop over all water variables
    area_mean_sign_trend_mrsol.df <- rbind(area_mean_sign_trend_mrsol.df, 
                                           data.frame("area" = (sum(area.array[which(mean_sign_trend_mrsol[,,i] == v)], na.rm = T)/total_land_area)*100,
                                                      "var" = labels_mean_trends_mrsol[v],
                                                      "period" = periods_classes[i]))
  }
}

# x = 120; y = 72
count_i <- 0
for(i in 1:6){
  for(x in 1:180){
    for(y in 1:90){
      if(!is.na(mean_sign_trend_mrsol[x,y,i])){
        if(mean_sign_trend_mrsol[x,y,i] == 5){
          # convert array into vector for one grid cell
          mean_sign_trend_mrsol_tie_c <- c(mean_sign_trend_mrsol_tie[x,y,i,][which(!is.na(mean_sign_trend_mrsol_tie[x,y,i,]))])
          # area_mean_sign_trend_mrsol_tie$area[mean_sign_trend_mrsol_tie_c] <- area_mean_sign_trend_mrsol_tie$area[mean_sign_trend_mrsol_tie_c] + ((area.array[x,y]/total_land_area)*100)/length(mean_sign_trend_mrsol_tie_c)
          area_mean_sign_trend_mrsol.df$area[mean_sign_trend_mrsol_tie_c + count_i] <- area_mean_sign_trend_mrsol.df$area[mean_sign_trend_mrsol_tie_c + count_i] + ((area.array[x,y]/total_land_area)*100)/length(mean_sign_trend_mrsol_tie_c)
        }
      }
    }
  }
  count_i <- count_i + 4
}

area_mean_sign_trend_mrsol.df$var_fac <- factor(area_mean_sign_trend_mrsol.df$var, levels = labels_mean_trends_mrsol)
mean_sign_trend_mrsol.df$cuts_mrsol <- cut(mean_sign_trend_mrsol.df$mrsol, cuts_classes_bars, include.lowest = T)

################################################################################################################
##########################################               #######################################################
##########################################      vpd      #######################################################
##########################################               #######################################################
################################################################################################################
mean_sign_trend_vpd.df <- setNames(data.frame(matrix(ncol = 4, nrow = 0)),
                                   c("lon","lat","vpd","period"))
for(i in 1:6){
  for(x in 1:180){
    for(y in 1:90){
      if(!is.na(mean_sign_trend_vpd[x,y,i])){
        mean_sign_trend_vpd.df <- rbind(mean_sign_trend_vpd.df,
                                        data.frame("lon" = lon[x],
                                                   "lat" = lat[y],
                                                   "vpd" = mean_sign_trend_vpd[x,y,i],
                                                   "period" = periods_classes[i]))
      }
    }
  }
}

labels_mean_trends_vpd <- c("- VPD*", "- VPD", "+ VPD", "+ VPD*",'Tie', '< 3/9 Models agree', '< 6/9 Models with values')
area_mean_sign_trend_vpd.df <- setNames(data.frame(matrix(ncol = 3, nrow = 0)),
                                        c("area","var","period"))
for(i in 1:6){
  for(v in 1:4){ # loop over all water variables
    area_mean_sign_trend_vpd.df <- rbind(area_mean_sign_trend_vpd.df, 
                                         data.frame("area" = (sum(area.array[which(mean_sign_trend_vpd[,,i] == v)], na.rm = T)/total_land_area)*100,
                                                    "var" = labels_mean_trends_vpd[v],
                                                    "period" = periods_classes[i]))
  }
}

# x = 120; y = 72
count_i <- 0
for(i in 1:6){
  for(x in 1:180){
    for(y in 1:90){
      if(!is.na(mean_sign_trend_vpd[x,y,i])){
        if(mean_sign_trend_vpd[x,y,i] == 5){
          # convert array into vector for one grid cell
          mean_sign_trend_vpd_tie_c <- c(mean_sign_trend_vpd_tie[x,y,i,][which(!is.na(mean_sign_trend_vpd_tie[x,y,i,]))])
          # area_mean_sign_trend_vpd_tie$area[mean_sign_trend_vpd_tie_c] <- area_mean_sign_trend_vpd_tie$area[mean_sign_trend_vpd_tie_c] + ((area.array[x,y]/total_land_area)*100)/length(mean_sign_trend_vpd_tie_c)
          area_mean_sign_trend_vpd.df$area[mean_sign_trend_vpd_tie_c + count_i] <- area_mean_sign_trend_vpd.df$area[mean_sign_trend_vpd_tie_c + count_i] + ((area.array[x,y]/total_land_area)*100)/length(mean_sign_trend_vpd_tie_c)
        }
      }
    }
  }
  count_i <- count_i + 4
}

area_mean_sign_trend_vpd.df$var_fac <- factor(area_mean_sign_trend_vpd.df$var, levels = labels_mean_trends_vpd)
mean_sign_trend_vpd.df$cuts_vpd <- cut(mean_sign_trend_vpd.df$vpd, cuts_classes_bars, include.lowest = T)

################################################################################################################
##########################################               #######################################################
##########################################      pr       #######################################################
##########################################               #######################################################
################################################################################################################
mean_sign_trend_pr.df <- setNames(data.frame(matrix(ncol = 4, nrow = 0)),
                                  c("lon","lat","pr","period"))
for(i in 1:6){
  for(x in 1:180){
    for(y in 1:90){
      if(!is.na(mean_sign_trend_pr[x,y,i])){
        mean_sign_trend_pr.df <- rbind(mean_sign_trend_pr.df,
                                       data.frame("lon" = lon[x],
                                                  "lat" = lat[y],
                                                  "pr" = mean_sign_trend_pr[x,y,i],
                                                  "period" = periods_classes[i]))
      }
    }
  }
}

labels_mean_trends_pr <- c("- precip.*", "- precip.", "+ precip.", "+ precip.*",'Tie', '< 3/9 Models agree', '< 6/9 Models with values')
area_mean_sign_trend_pr.df <- setNames(data.frame(matrix(ncol = 3, nrow = 0)),
                                       c("area","var","period"))
for(i in 1:6){
  for(v in 1:4){ # loop over all water variables
    area_mean_sign_trend_pr.df <- rbind(area_mean_sign_trend_pr.df, 
                                        data.frame("area" = (sum(area.array[which(mean_sign_trend_pr[,,i] == v)], na.rm = T)/total_land_area)*100,
                                                   "var" = labels_mean_trends_pr[v],
                                                   "period" = periods_classes[i]))
  }
}

# x = 120; y = 72
count_i <- 0
for(i in 1:6){
  for(x in 1:180){
    for(y in 1:90){
      if(!is.na(mean_sign_trend_pr[x,y,i])){
        if(mean_sign_trend_pr[x,y,i] == 5){
          # convert array into vector for one grid cell
          mean_sign_trend_pr_tie_c <- c(mean_sign_trend_pr_tie[x,y,i,][which(!is.na(mean_sign_trend_pr_tie[x,y,i,]))])
          # area_mean_sign_trend_pr_tie$area[mean_sign_trend_pr_tie_c] <- area_mean_sign_trend_pr_tie$area[mean_sign_trend_pr_tie_c] + ((area.array[x,y]/total_land_area)*100)/length(mean_sign_trend_pr_tie_c)
          area_mean_sign_trend_pr.df$area[mean_sign_trend_pr_tie_c + count_i] <- area_mean_sign_trend_pr.df$area[mean_sign_trend_pr_tie_c + count_i] + ((area.array[x,y]/total_land_area)*100)/length(mean_sign_trend_pr_tie_c)
        }
      }
    }
  }
  count_i <- count_i + 4
}

area_mean_sign_trend_pr.df$var_fac <- factor(area_mean_sign_trend_pr.df$var, levels = labels_mean_trends_pr)
mean_sign_trend_pr.df$cuts_pr <- cut(mean_sign_trend_pr.df$pr, cuts_classes_bars, include.lowest = T)

################################################################################################################
##########################################               #######################################################
##########################################      AI       #######################################################
##########################################               #######################################################
################################################################################################################
mean_sign_trend_AI.df <- setNames(data.frame(matrix(ncol = 4, nrow = 0)),
                                  c("lon","lat","AI","period"))
for(i in 1:6){
  for(x in 1:180){
    for(y in 1:90){
      if(!is.na(mean_sign_trend_AI[x,y,i])){
        mean_sign_trend_AI.df <- rbind(mean_sign_trend_AI.df,
                                       data.frame("lon" = lon[x],
                                                  "lat" = lat[y],
                                                  "AI" = mean_sign_trend_AI[x,y,i],
                                                  "period" = periods_classes[i]))
      }
    }
  }
}

labels_mean_trends_AI <- c("- Dryness*", "- Dryness", "+ Dryness", "+ Dryness*",'Tie', '< 3/9 Models agree', '< 6/9 Models with values')
# labels_mean_trends_AI <- c("+ Dryness*", "+ Dryness", "- Dryness", "- Dryness*", 'Tie', '< 3/9 Models agree', '< 6/9 Models with values')
area_mean_sign_trend_AI.df <- setNames(data.frame(matrix(ncol = 3, nrow = 0)),
                                       c("area","var","period"))
for(i in 1:6){
  for(v in 1:4){ # loop over all water variables
    area_mean_sign_trend_AI.df <- rbind(area_mean_sign_trend_AI.df, 
                                        data.frame("area" = (sum(area.array[which(mean_sign_trend_AI[,,i] == v)], na.rm = T)/total_land_area)*100,
                                                   "var" = labels_mean_trends_AI[v],
                                                   "period" = periods_classes[i]))
  }
}

# x = 120; y = 72
count_i <- 0
for(i in 1:6){
  for(x in 1:180){
    for(y in 1:90){
      if(!is.na(mean_sign_trend_AI[x,y,i])){
        if(mean_sign_trend_AI[x,y,i] == 5){
          # convert array into vector for one grid cell
          mean_sign_trend_AI_tie_c <- c(mean_sign_trend_AI_tie[x,y,i,][which(!is.na(mean_sign_trend_AI_tie[x,y,i,]))])
          # area_mean_sign_trend_AI_tie$area[mean_sign_trend_AI_tie_c] <- area_mean_sign_trend_AI_tie$area[mean_sign_trend_AI_tie_c] + ((area.array[x,y]/total_land_area)*100)/length(mean_sign_trend_AI_tie_c)
          area_mean_sign_trend_AI.df$area[mean_sign_trend_AI_tie_c + count_i] <- area_mean_sign_trend_AI.df$area[mean_sign_trend_AI_tie_c + count_i] + ((area.array[x,y]/total_land_area)*100)/length(mean_sign_trend_AI_tie_c)
        }
      }
    }
  }
  count_i <- count_i + 4
}

area_mean_sign_trend_AI.df$var_fac <- factor(area_mean_sign_trend_AI.df$var, levels = labels_mean_trends_AI)
mean_sign_trend_AI.df$cuts_AI <- cut(mean_sign_trend_AI.df$AI, cuts_classes_bars, include.lowest = T)


for(i in 1:6){
  bb <- ggplot(filter(mean_sign_trend_LAI.df, period == periods_classes[i]), aes(x=lon,y=lat,fill=cuts_LAI)) +
    # bb <- ggplot(mean_sign_trend_LAI.df, aes(x=lon,y=lat,fill=cuts_LAI)) +
    geom_tile() +
    geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
             inherit.aes = F, fill = NA, color = "black", size = .1) +
    scale_fill_manual("Mean LAI trend",
                      values = cols_mean_trends_LAI,
                      labels = labels_mean_trends_LAI,
                      drop = F) +
    scale_x_continuous("longitude",
                       limits=c(-180,180),
                       expand=c(0,0)) +
    scale_y_continuous("latitude",
                       limits=c(-60,75),
                       expand=c(0,0)) +
    # facet_wrap(~period, ncol = 1) +
    ggtitle(periods_classes[i]) +
    guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, nrow = 2)) +
    theme(legend.position = "bottom",
          legend.text = element_text(size=12),
          legend.title = element_text(size=20),
          strip.background = element_blank(),
          axis.line.x = element_line(colour = "black"),
          axis.line.y = element_line(colour = "black"),
          strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(color = 'black', fill = NA, size = 1),
          panel.background = element_blank(),
          axis.text = element_text(size=18),
          axis.title = element_blank(),
          plot.title = element_text(size=24, hjust = 0.5),
          plot.tag.position = c(.55,0.03)
    )
  # bb
  bb<- ggplotGrob(bb)
  
  
  bar_mean_sign_trend_LAI <- ggplot(filter(area_mean_sign_trend_LAI.df, period == periods_classes[i]), aes(x = var_fac, y = area, fill = var_fac)) +
    geom_bar(stat='identity',col = 'black', width = 1, position = "stack") +
    geom_text(inherit.aes = F, data = filter(area_mean_sign_trend_LAI.df, period == periods_classes[i] & area<1), aes(x=var,y=area+2.5,label=round(area,1)), size = 2.5) +
    geom_text(inherit.aes = F, data = filter(area_mean_sign_trend_LAI.df, period == periods_classes[i] & area>1), aes(x=var,y=area+2.5,label=round(area,0)), size = 2.5) +
    scale_x_discrete("") +
    scale_y_continuous(expression(paste("land area-%")), expand = c(0,0), limits = c(0, (max(area_mean_sign_trend_LAI.df$area)) + 5), position = "right") +
    scale_fill_manual(values = c("- LAI*" = cols_mean_trends_LAI[1],
                                 "- LAI" = cols_mean_trends_LAI[2],
                                 "+ LAI" = cols_mean_trends_LAI[3],
                                 "+ LAI*" = cols_mean_trends_LAI[4]),
                      drop = F) +
    theme(legend.position = "none",
          legend.text = element_text(size=12),
          legend.title = element_text(size=20),
          strip.background = element_blank(),
          axis.line.x = element_line(colour = "black"),
          axis.line.y = element_line(colour = "black"),
          strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(color = 'black', fill = NA, size = 1),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          axis.text = element_text(size=9),
          axis.text.x = element_blank(),
          axis.ticks.x=element_blank(),
          axis.title = element_text(size=9),
          plot.title = element_text(size=10)
    )
  # bar_mean_sign_trend_LAI
  bar_mean_sign_trend_LAI <- ggplotGrob(bar_mean_sign_trend_LAI)
  
  gt1 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
  gt1 <- gtable_add_grob(gt1, bb, t=1, b=32, l=1, r=32)
  gt1 <- gtable_add_grob(gt1, bar_mean_sign_trend_LAI, t = 25, l = 3, b = 17, r = 10)
  grid.draw(gt1)
  
  ggsave(paste0("/Net/Groups/BGI/people/jdenis/scripts/scripts_Nature_Perspective/mean_sign_trend_LAI_",substr(periods_classes[i], start = 1, stop = 4),"_",substr(periods_classes[i], start = 8, stop = 11),".png"), plot = gt1, width = 9, height = 7, units = "in")
  
  bb <- ggplot(filter(mean_sign_trend_mrsol.df, period == periods_classes[i]), aes(x=lon,y=lat,fill=cuts_mrsol)) +
    # bb <- ggplot(mean_sign_trend_mrsol.df, aes(x=lon,y=lat,fill=cuts_mrsol)) +
    geom_tile() +
    geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
             inherit.aes = F, fill = NA, color = "black", size = .1) +
    scale_fill_manual("Mean soil moisture trend",
                      values = cols_mean_trends_wtr,
                      labels = labels_mean_trends_mrsol,
                      drop = F) +
    scale_x_continuous("longitude",
                       limits=c(-180,180),
                       expand=c(0,0)) +
    scale_y_continuous("latitude",
                       limits=c(-60,75),
                       expand=c(0,0)) +
    # facet_wrap(~period, ncol = 1) +
    ggtitle(periods_classes[i]) +
    guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, nrow = 2)) +
    theme(legend.position = "bottom",
          legend.text = element_text(size=12),
          legend.title = element_text(size=20),
          strip.background = element_blank(),
          axis.line.x = element_line(colour = "black"),
          axis.line.y = element_line(colour = "black"),
          strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(color = 'black', fill = NA, size = 1),
          panel.background = element_blank(),
          axis.text = element_text(size=18),
          axis.title = element_blank(),
          plot.title = element_text(size=24, hjust = 0.5),
          plot.tag.position = c(.55,0.03)
    )
  # bb
  bb<- ggplotGrob(bb)
  
  
  bar_mean_sign_trend_mrsol <- ggplot(filter(area_mean_sign_trend_mrsol.df, period == periods_classes[i]), aes(x = var_fac, y = area, fill = var_fac)) +
    geom_bar(stat='identity',col = 'black', width = 1, position = "stack") +
    geom_text(inherit.aes = F, data = filter(area_mean_sign_trend_mrsol.df, period == periods_classes[i] & area<1), aes(x=var,y=area+2.5,label=round(area,1)), size = 2.5) +
    geom_text(inherit.aes = F, data = filter(area_mean_sign_trend_mrsol.df, period == periods_classes[i] & area>1), aes(x=var,y=area+2.5,label=round(area,0)), size = 2.5) +
    scale_x_discrete("") +
    scale_y_continuous(expression(paste("land area-%")), expand = c(0,0), limits = c(0, (max(area_mean_sign_trend_mrsol.df$area)) + 5), position = "right") +
    scale_fill_manual(values = c("- SM*" = cols_mean_trends_wtr[1],
                                 "- SM" = cols_mean_trends_wtr[2],
                                 "+ SM" = cols_mean_trends_wtr[3],
                                 "+ SM*" = cols_mean_trends_wtr[4]),
                      drop = F) +
    theme(legend.position = "none",
          legend.text = element_text(size=12),
          legend.title = element_text(size=20),
          strip.background = element_blank(),
          axis.line.x = element_line(colour = "black"),
          axis.line.y = element_line(colour = "black"),
          strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(color = 'black', fill = NA, size = 1),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          axis.text = element_text(size=9),
          axis.text.x = element_blank(),
          axis.ticks.x=element_blank(),
          axis.title = element_text(size=9),
          plot.title = element_text(size=10)
    )
  # bar_mean_sign_trend_mrsol
  bar_mean_sign_trend_mrsol <- ggplotGrob(bar_mean_sign_trend_mrsol)
  
  gt1 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
  gt1 <- gtable_add_grob(gt1, bb, t=1, b=32, l=1, r=32)
  gt1 <- gtable_add_grob(gt1, bar_mean_sign_trend_mrsol, t = 25, l = 3, b = 17, r = 10)
  grid.draw(gt1)
  
  ggsave(paste0("/Net/Groups/BGI/people/jdenis/scripts/scripts_Nature_Perspective/mean_sign_trend_mrsol_",substr(periods_classes[i], start = 1, stop = 4),"_",substr(periods_classes[i], start = 8, stop = 11),".png"), plot = gt1, width = 9, height = 7, units = "in")
  
  bb <- ggplot(filter(mean_sign_trend_vpd.df, period == periods_classes[i]), aes(x=lon,y=lat,fill=cuts_vpd)) +
    # bb <- ggplot(mean_sign_trend_vpd.df, aes(x=lon,y=lat,fill=cuts_vpd)) +
    geom_tile() +
    geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
             inherit.aes = F, fill = NA, color = "black", size = .1) +
    scale_fill_manual("Mean VPD trend",
                      values = cols_mean_trends_vpd,
                      labels = labels_mean_trends_vpd,
                      drop = F) +
    scale_x_continuous("longitude",
                       limits=c(-180,180),
                       expand=c(0,0)) +
    scale_y_continuous("latitude",
                       limits=c(-60,75),
                       expand=c(0,0)) +
    # facet_wrap(~period, ncol = 1) +
    ggtitle(periods_classes[i]) +
    guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, nrow = 2)) +
    theme(legend.position = "bottom",
          legend.text = element_text(size=12),
          legend.title = element_text(size=20),
          strip.background = element_blank(),
          axis.line.x = element_line(colour = "black"),
          axis.line.y = element_line(colour = "black"),
          strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(color = 'black', fill = NA, size = 1),
          panel.background = element_blank(),
          axis.text = element_text(size=18),
          axis.title = element_blank(),
          plot.title = element_text(size=24, hjust = 0.5),
          plot.tag.position = c(.55,0.03)
    )
  # bb
  bb<- ggplotGrob(bb)
  
  
  bar_mean_sign_trend_vpd <- ggplot(filter(area_mean_sign_trend_vpd.df, period == periods_classes[i]), aes(x = var_fac, y = area, fill = var_fac)) +
    geom_bar(stat='identity',col = 'black', width = 1, position = "stack") +
    geom_text(inherit.aes = F, data = filter(area_mean_sign_trend_vpd.df, period == periods_classes[i] & area<1), aes(x=var,y=area+2.5,label=round(area,1)), size = 2.5) +
    geom_text(inherit.aes = F, data = filter(area_mean_sign_trend_vpd.df, period == periods_classes[i] & area>1), aes(x=var,y=area+2.5,label=round(area,0)), size = 2.5) +
    scale_x_discrete("") +
    scale_y_continuous(expression(paste("land area-%")), expand = c(0,0), limits = c(0, (max(area_mean_sign_trend_vpd.df$area)) + 5), position = "right") +
    scale_fill_manual(values = c("- VPD*" = cols_mean_trends_vpd[1],
                                 "- VPD" = cols_mean_trends_vpd[2],
                                 "+ VPD" = cols_mean_trends_vpd[3],
                                 "+ VPD*" = cols_mean_trends_vpd[4]),
                      drop = F) +
    theme(legend.position = "none",
          legend.text = element_text(size=12),
          legend.title = element_text(size=20),
          strip.background = element_blank(),
          axis.line.x = element_line(colour = "black"),
          axis.line.y = element_line(colour = "black"),
          strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(color = 'black', fill = NA, size = 1),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          axis.text = element_text(size=9),
          axis.text.x = element_blank(),
          axis.ticks.x=element_blank(),
          axis.title = element_text(size=9),
          plot.title = element_text(size=10)
    )
  # bar_mean_sign_trend_vpd
  bar_mean_sign_trend_vpd <- ggplotGrob(bar_mean_sign_trend_vpd)
  
  gt1 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
  gt1 <- gtable_add_grob(gt1, bb, t=1, b=32, l=1, r=32)
  gt1 <- gtable_add_grob(gt1, bar_mean_sign_trend_vpd, t = 25, l = 3, b = 17, r = 10)
  grid.draw(gt1)
  
  ggsave(paste0("/Net/Groups/BGI/people/jdenis/scripts/scripts_Nature_Perspective/mean_sign_trend_vpd_",substr(periods_classes[i], start = 1, stop = 4),"_",substr(periods_classes[i], start = 8, stop = 11),".png"), plot = gt1, width = 9, height = 7, units = "in")
  
  bb <- ggplot(filter(mean_sign_trend_pr.df, period == periods_classes[i]), aes(x=lon,y=lat,fill=cuts_pr)) +
    # bb <- ggplot(mean_sign_trend_pr.df, aes(x=lon,y=lat,fill=cuts_pr)) +
    geom_tile() +
    geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
             inherit.aes = F, fill = NA, color = "black", size = .1) +
    scale_fill_manual("Mean precipitation trend",
                      values = cols_mean_trends_wtr,
                      labels = labels_mean_trends_pr,
                      drop = F) +
    scale_x_continuous("longitude",
                       limits=c(-180,180),
                       expand=c(0,0)) +
    scale_y_continuous("latitude",
                       limits=c(-60,75),
                       expand=c(0,0)) +
    # facet_wrap(~period, ncol = 1) +
    ggtitle(periods_classes[i]) +
    guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, nrow = 2)) +
    theme(legend.position = "bottom",
          legend.text = element_text(size=12),
          legend.title = element_text(size=20),
          strip.background = element_blank(),
          axis.line.x = element_line(colour = "black"),
          axis.line.y = element_line(colour = "black"),
          strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(color = 'black', fill = NA, size = 1),
          panel.background = element_blank(),
          axis.text = element_text(size=18),
          axis.title = element_blank(),
          plot.title = element_text(size=24, hjust = 0.5),
          plot.tag.position = c(.55,0.03)
    )
  # bb
  bb<- ggplotGrob(bb)
  
  
  bar_mean_sign_trend_pr <- ggplot(filter(area_mean_sign_trend_pr.df, period == periods_classes[i]), aes(x = var_fac, y = area, fill = var_fac)) +
    geom_bar(stat='identity',col = 'black', width = 1, position = "stack") +
    geom_text(inherit.aes = F, data = filter(area_mean_sign_trend_pr.df, period == periods_classes[i] & area<1), aes(x=var,y=area+2.5,label=round(area,1)), size = 2.5) +
    geom_text(inherit.aes = F, data = filter(area_mean_sign_trend_pr.df, period == periods_classes[i] & area>1), aes(x=var,y=area+2.5,label=round(area,0)), size = 2.5) +
    scale_x_discrete("") +
    scale_y_continuous(expression(paste("land area-%")), expand = c(0,0), limits = c(0, (max(area_mean_sign_trend_pr.df$area)) + 5), position = "right") +
    scale_fill_manual(values = c("- precip.*" = cols_mean_trends_wtr[1],
                                 "- precip." = cols_mean_trends_wtr[2],
                                 "+ precip." = cols_mean_trends_wtr[3],
                                 "+ precip.*" = cols_mean_trends_wtr[4]),
                      drop = F) +
    theme(legend.position = "none",
          legend.text = element_text(size=12),
          legend.title = element_text(size=20),
          strip.background = element_blank(),
          axis.line.x = element_line(colour = "black"),
          axis.line.y = element_line(colour = "black"),
          strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(color = 'black', fill = NA, size = 1),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          axis.text = element_text(size=9),
          axis.text.x = element_blank(),
          axis.ticks.x=element_blank(),
          axis.title = element_text(size=9),
          plot.title = element_text(size=10)
    )
  # bar_mean_sign_trend_pr
  bar_mean_sign_trend_pr <- ggplotGrob(bar_mean_sign_trend_pr)
  
  gt1 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
  gt1 <- gtable_add_grob(gt1, bb, t=1, b=32, l=1, r=32)
  gt1 <- gtable_add_grob(gt1, bar_mean_sign_trend_pr, t = 25, l = 3, b = 17, r = 10)
  grid.draw(gt1)
  
  ggsave(paste0("/Net/Groups/BGI/people/jdenis/scripts/scripts_Nature_Perspective/mean_sign_trend_pr_",substr(periods_classes[i], start = 1, stop = 4),"_",substr(periods_classes[i], start = 8, stop = 11),".png"), plot = gt1, width = 9, height = 7, units = "in")
  
  bb <- ggplot(filter(mean_sign_trend_AI.df, period == periods_classes[i]), aes(x=lon,y=lat,fill=cuts_AI)) +
    # bb <- ggplot(mean_sign_trend_AI.df, aes(x=lon,y=lat,fill=cuts_AI)) +
    geom_tile() +
    geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
             inherit.aes = F, fill = NA, color = "black", size = .1) +
    scale_fill_manual("Mean dryness trend",
                      values = cols_mean_trends_wtr,
                      labels = labels_mean_trends_AI,
                      drop = F) +
    scale_x_continuous("longitude",
                       limits=c(-180,180),
                       expand=c(0,0)) +
    scale_y_continuous("latitude",
                       limits=c(-60,75),
                       expand=c(0,0)) +
    # facet_wrap(~period, ncol = 1) +
    ggtitle(periods_classes[i]) +
    guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, nrow = 2)) +
    theme(legend.position = "bottom",
          legend.text = element_text(size=12),
          legend.title = element_text(size=20),
          strip.background = element_blank(),
          axis.line.x = element_line(colour = "black"),
          axis.line.y = element_line(colour = "black"),
          strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(color = 'black', fill = NA, size = 1),
          panel.background = element_blank(),
          axis.text = element_text(size=18),
          axis.title = element_blank(),
          plot.title = element_text(size=24, hjust = 0.5),
          plot.tag.position = c(.55,0.03)
    )
  # bb
  bb<- ggplotGrob(bb)
  
  
  bar_mean_sign_trend_AI <- ggplot(filter(area_mean_sign_trend_AI.df, period == periods_classes[i]), aes(x = var_fac, y = area, fill = var_fac)) +
    geom_bar(stat='identity',col = 'black', width = 1, position = "stack") +
    geom_text(inherit.aes = F, data = filter(area_mean_sign_trend_AI.df, period == periods_classes[i] & area<1), aes(x=var,y=area+2.5,label=round(area,1)), size = 2.5) +
    geom_text(inherit.aes = F, data = filter(area_mean_sign_trend_AI.df, period == periods_classes[i] & area>1), aes(x=var,y=area+2.5,label=round(area,0)), size = 2.5) +
    scale_x_discrete("") +
    scale_y_continuous(expression(paste("land area-%")), expand = c(0,0), limits = c(0, (max(area_mean_sign_trend_AI.df$area)) + 5), position = "right") +
    scale_fill_manual(values = c("- Dryness*" = cols_mean_trends_vpd[1],
                                 "- Dryness" = cols_mean_trends_vpd[2],
                                 "+ Dryness" = cols_mean_trends_vpd[3],
                                 "+ Dryness*" = cols_mean_trends_vpd[4]),
                      drop = F) +
    theme(legend.position = "none",
          legend.text = element_text(size=12),
          legend.title = element_text(size=20),
          strip.background = element_blank(),
          axis.line.x = element_line(colour = "black"),
          axis.line.y = element_line(colour = "black"),
          strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(color = 'black', fill = NA, size = 1),
          panel.background = element_rect(fill = "transparent",colour = NA),
          plot.background = element_rect(fill = "transparent",colour = NA),
          axis.text = element_text(size=9),
          axis.text.x = element_blank(),
          axis.ticks.x=element_blank(),
          axis.title = element_text(size=9),
          plot.title = element_text(size=10)
    )
  # bar_mean_sign_trend_AI
  bar_mean_sign_trend_AI <- ggplotGrob(bar_mean_sign_trend_AI)
  
  gt1 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
  gt1 <- gtable_add_grob(gt1, bb, t=1, b=32, l=1, r=32)
  gt1 <- gtable_add_grob(gt1, bar_mean_sign_trend_AI, t = 25, l = 3, b = 17, r = 10)
  grid.draw(gt1)
  
  ggsave(paste0("/Net/Groups/BGI/people/jdenis/scripts/scripts_Nature_Perspective/mean_sign_trend_AI_",substr(periods_classes[i], start = 1, stop = 4),"_",substr(periods_classes[i], start = 8, stop = 11),".png"), plot = gt1, width = 9, height = 7, units = "in")
}

#### Fig2 ####
bb <- ggplot(filter(mean_sign_trend_LAI.df, period == periods_classes[1]), aes(x=lon,y=lat,fill=cuts_LAI)) +
  # bb <- ggplot(mean_sign_trend_LAI.df, aes(x=lon,y=lat,fill=cuts_LAI)) +
  geom_tile() +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  scale_fill_manual("Mean LAI trend",
                    values = cols_mean_trends_LAI,
                    labels = labels_mean_trends_LAI,
                    drop = F) +
  scale_x_continuous("longitude",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("latitude",
                     limits=c(-60,75),
                     expand=c(0,0)) +
  # facet_wrap(~period, ncol = 1) +
  ggtitle(paste0(periods_classes[1])) +
  # ggtitle(paste0("a)")) +
  guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, nrow = 2)) +
  # theme(legend.position = "bottom",
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_blank(),
        plot.title = element_text(size=24, hjust = 0.5),
        plot.tag.position = c(.55,0.03)
  )
bb
bb<- ggplotGrob(bb)


bar_mean_sign_trend_LAI <- ggplot(filter(area_mean_sign_trend_LAI.df, period == periods_classes[1]), aes(x = var_fac, y = area, fill = var_fac)) +
  geom_bar(stat='identity',col = 'black', width = 1, position = "stack") +
  geom_text(inherit.aes = F, data = filter(area_mean_sign_trend_LAI.df, period == periods_classes[1] & area<1), aes(x=var,y=area+2.5,label=round(area,1)), size = 5) +
  geom_text(inherit.aes = F, data = filter(area_mean_sign_trend_LAI.df, period == periods_classes[1] & area>1), aes(x=var,y=area+2.5,label=round(area,0)), size = 5) +
  scale_x_discrete("",limits=rev) +
  scale_y_continuous("", expand = c(0,0), limits = c(0, (max(area_mean_sign_trend_LAI.df$area)) + 5), position = "left") +
  scale_fill_manual(values = c("- LAI*" = cols_mean_trends_LAI[1],
                               "- LAI" = cols_mean_trends_LAI[2],
                               "+ LAI" = cols_mean_trends_LAI[3],
                               "+ LAI*" = cols_mean_trends_LAI[4]),
                    drop = F) +
  ggtitle(expression(paste("% of land area"))) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=16),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.title = element_text(size=9),
        plot.title = element_text(size=16, hjust = 0.5)
  )
bar_mean_sign_trend_LAI
bar_mean_sign_trend_LAI <- ggplotGrob(bar_mean_sign_trend_LAI)

gt1_a <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1_a <- gtable_add_grob(gt1_a, bb, t=1, b=32, l=1, r=32)
gt1_a <- gtable_add_grob(gt1_a, bar_mean_sign_trend_LAI, t = 29, l = 3, b = 19, r = 10)
# gt1 <- gtable_add_grob(gt1, grobTree(textGrob("a)", x=0, hjust = 0),
#                                      textGrob(periods_classes[1], x=1, hjust = 0.5)),
#                        t = 1, l=4)
grid.draw(gt1_a)

bb <- ggplot(filter(mean_sign_trend_LAI.df, period == periods_classes[2]), aes(x=lon,y=lat,fill=cuts_LAI)) +
  # bb <- ggplot(mean_sign_trend_LAI.df, aes(x=lon,y=lat,fill=cuts_LAI)) +
  geom_tile() +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  scale_fill_manual("Mean LAI trend",
                    values = cols_mean_trends_LAI,
                    labels = labels_mean_trends_LAI,
                    drop = F) +
  scale_x_continuous("longitude",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("latitude",
                     limits=c(-60,75),
                     expand=c(0,0)) +
  # facet_wrap(~period, ncol = 1) +
  ggtitle(paste0(periods_classes[2])) +
  # ggtitle(paste0("b)")) +
  guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, nrow = 2)) +
  # theme(legend.position = "bottom",
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_blank(),
        plot.title = element_text(size=24, hjust = 0.5),
        plot.tag.position = c(.55,0.03)
  )
bb
bb<- ggplotGrob(bb)


bar_mean_sign_trend_LAI <- ggplot(filter(area_mean_sign_trend_LAI.df, period == periods_classes[2]), aes(x = var_fac, y = area, fill = var_fac)) +
  geom_bar(stat='identity',col = 'black', width = 1, position = "stack") +
  geom_text(inherit.aes = F, data = filter(area_mean_sign_trend_LAI.df, period == periods_classes[2] & area<1), aes(x=var,y=area+2.5,label=round(area,1)), size = 5) +
  geom_text(inherit.aes = F, data = filter(area_mean_sign_trend_LAI.df, period == periods_classes[2] & area>1), aes(x=var,y=area+2.5,label=round(area,0)), size = 5) +
  scale_x_discrete("",limits=rev) +
  scale_y_continuous("", expand = c(0,0), limits = c(0, (max(area_mean_sign_trend_LAI.df$area)) + 5), position = "left") +
  scale_fill_manual(values = c("- LAI*" = cols_mean_trends_LAI[1],
                               "- LAI" = cols_mean_trends_LAI[2],
                               "+ LAI" = cols_mean_trends_LAI[3],
                               "+ LAI*" = cols_mean_trends_LAI[4]),
                    drop = F) +
  ggtitle(expression(paste("% of land area"))) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=16),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.title = element_text(size=9),
        plot.title = element_text(size=16, hjust = 0.5)
  )
bar_mean_sign_trend_LAI
bar_mean_sign_trend_LAI <- ggplotGrob(bar_mean_sign_trend_LAI)

gt1_b <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1_b <- gtable_add_grob(gt1_b, bb, t=1, b=32, l=1, r=32)
gt1_b <- gtable_add_grob(gt1_b, bar_mean_sign_trend_LAI, t = 29, l = 3, b = 19, r = 10)
# gt1 <- gtable_add_grob(gt1, grobTree(textGrob("a)", x=0, hjust = 0),
#                                      textGrob(periods_classes[2], x=1, hjust = 0.5)),
#                        t = 1, l=4)
grid.draw(gt1_b)

for_legend <- ggplot(filter(mean_sign_trend_LAI.df, period == periods_classes[2]), aes(x=lon,y=lat,fill=cuts_LAI)) +
  # bb <- ggplot(mean_sign_trend_LAI.df, aes(x=lon,y=lat,fill=cuts_LAI)) +
  geom_tile() +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  scale_fill_manual("Mean LAI trend",
                    values = cols_mean_trends_LAI,
                    labels = labels_mean_trends_LAI,
                    drop = F) +
  scale_x_continuous("longitude",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("latitude",
                     limits=c(-60,75),
                     expand=c(0,0)) +
  # facet_wrap(~period, ncol = 1) +
  ggtitle(paste0(periods_classes[2])) +
  # ggtitle(paste0("b)")) +
  guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, nrow = 2)) +
  theme(legend.position = "bottom",
  # theme(legend.position = "none",
        legend.text = element_text(size=18),
        legend.title = element_text(size=26),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_blank(),
        plot.title = element_text(size=24, hjust = 0.5),
        plot.tag.position = c(.55,0.03)
  )
bb_legend <- cowplot::get_legend(for_legend)


##### Water trend agreement #####
##### Per model #####
ct_6 <- 1
agreement_per_model <- array(NaN,c(180,90,length(cmip6_data.df$source_id),6))
for(i in 1:length(cmip6_data.df$source_id)){
  for(yr20 in 1:6){
    for(x in 1:180){
      for(y in 1:90){
        if(sum(!is.na(mask_obs[x,y,(ct_6:(ct_6+11))])) == 12){
          if(sum(c(sign_trend_mrsol[x,y,i,yr20] == 1, sign_trend_vpd[x,y,i,yr20] == 4, sign_trend_pr[x,y,i,yr20] == 1, sign_trend_AI[x,y,i,yr20] == 4), na.rm = T) > 2){
            agreement_per_model[x,y,i,yr20] <- 1 # agreement on significant drying
          }else{
            agreement_per_model[x,y,i,yr20] <- 2 # alternative
          }
        }
      }
    }
  }
  ct_6 <- ct_6 + 6
}

cols_agreement <- c("peachpuff4",'navajowhite2')
cuts_agreement <- cuts_classes_bars <- c(0,1.01,2.01)
labels_agreement <- c("Significant drying", "Insignificant or no drying")

agreement_per_model.df <- setNames(data.frame(matrix(ncol = 5, nrow = 0)),
                                   c("lon","lat","agreement","source_id","period"))


for(h in 1:length(cmip6_data.df$source_id)){ 
  for(i in 1:6){
    for(x in 1:180){
      for(y in 1:90){
        if(!is.na(sign_trend_LAI[x,y,h,i])){
          agreement_per_model.df <- rbind(agreement_per_model.df,
                                          data.frame("lon" = lon[x],
                                                     "lat" = lat[y],
                                                     "agreement" = agreement_per_model[x,y,h,i],
                                                     "source_id" = cmip6_data.df$source_id[h],
                                                     "period" = periods_classes[i]))
        }
      }
    }
  }
  print(paste0(cmip6_data.df$source_id[h], " is done..."))
}
agreement_per_model.df$cuts_agreement <- cut(agreement_per_model.df$agreement, cuts_agreement, include.lowest = T)

area_sign_trend_agreement_wo_mask.df <- 
  setNames(data.frame(matrix(ncol = 5, nrow = 0)),
           c("area","sign","var","source_id","period"))
for(h in 1:length(cmip6_data.df$source_id)){ 
  for(i in 1:6){
    for(c in 1:2){
      area_sign_trend_agreement_wo_mask.df <- rbind(area_sign_trend_agreement_wo_mask.df, 
                                                    data.frame("area" = (sum(area.array[which(agreement_per_model[,,h,i] == c)], na.rm = T)/total_land_area)*100,
                                                               "sign" = c,
                                                               "var" = labels_agreement[c],
                                                               "source_id" = cmip6_data.df$source_id[h],
                                                               "period" = periods_classes[i]))
    }
  }
  print(paste0(cmip6_data.df$source_id[h], " is done..."))
}

save(
  sign_trend_LAI.df,
  agreement_per_model.df,
  sign_trend_mrsol.df,
  sign_trend_vpd.df,
  sign_trend_pr.df,
  sign_trend_AI.df,
  area_sign_trend_LAI_wo_mask.df,
  area_sign_trend_agreement_wo_mask.df,
  area_sign_trend_mrsol_wo_mask.df,
  area_sign_trend_vpd_wo_mask.df,
  area_sign_trend_pr_wo_mask.df,
  area_sign_trend_AI_wo_mask.df,
  file = "/Net/Groups/BGI/people/jdenis/scripts/scripts_Nature_Perspective/sign_trend_wtr_var_all.RData"
)

for(i in 1:6){
  bb <- ggplot(filter(agreement_per_model.df, period == periods_classes[i]), aes(x=lon,y=lat,fill=cuts_agreement)) +
    # bb <- ggplot(filter(agreement_per_model.df, period == periods_classes[i] & source_id == cmip6_data.df$source_id[h]), aes(x=lon,y=lat,fill=cuts_agreement)) +
    # bb <- ggplot(mean_sign_trend_LAI.df, aes(x=lon,y=lat,fill=cuts_LAI)) +
    geom_tile() +
    geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
             inherit.aes = F, fill = NA, color = "black", size = .1) +
    scale_fill_manual("",
                      values = cols_agreement,
                      labels = labels_agreement,
                      drop = F) +
    scale_x_continuous("longitude",
                       limits=c(-180,180),
                       expand=c(0,0)) +
    scale_y_continuous("latitude",
                       limits=c(-60,75),
                       expand=c(0,0)) +
    facet_wrap(~source_id, ncol = 3) +
    ggtitle(periods_classes[i]) +
    guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, nrow = 1)) +
    theme(legend.position = "bottom",
          legend.text = element_text(size=12),
          legend.title = element_text(size=20),
          strip.background = element_blank(),
          axis.line.x = element_line(colour = "black"),
          axis.line.y = element_line(colour = "black"),
          strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(color = 'black', fill = NA, size = 1),
          panel.background = element_blank(),
          axis.text = element_text(size=18),
          axis.title = element_blank(),
          plot.title = element_text(size=24, hjust = 0.5),
          plot.tag.position = c(.55,0.03)
    )
  # bb
  ggsave(paste0("/Net/Groups/BGI/people/jdenis/scripts/scripts_Nature_Perspective/agreement_wtr_",substr(periods_classes[i], start = 1, stop = 4),"_",substr(periods_classes[i], start = 8, stop = 11),".png"), plot = bb, width = 9*1.5, height = 7*1.5, units = "in")
}

##### Water trend agreement #####
##### Between models #####
agreement <- 
  agreement_sum <- array(NaN,c(180,90,6))
for(yr20 in 1:6){
  for(x in 1:180){
    for(y in 1:90){
      if(sum(!is.na(agreement_per_model[x,y,,yr20])) > 0){
        agreement_sum[x,y,yr20] <- sum(agreement_per_model[x,y,,yr20] == 1, na.rm = T)
        if(sum(agreement_per_model[x,y,,yr20] == 1, na.rm = T) > 5){
          agreement[x,y,yr20] <- 1 # multi-model agreement on significant drying
        }else{
          agreement[x,y,yr20] <- 2 # alternative
        }
      }
    }
  }
}

# how many models have full LAI time series with enough variability
models_sum <- array(NaN,c(180,90))
mask_obs_copy <- mask_obs
mask_obs_copy[which(is.na(mask_obs_copy))] <- 0
models_sum <- mask_obs_copy[,,1] + mask_obs_copy[,,7] + mask_obs_copy[,,13] + 
  mask_obs_copy[,,19] + mask_obs_copy[,,25] + mask_obs_copy[,,31] + 
  mask_obs_copy[,,37] + mask_obs_copy[,,43] + mask_obs_copy[,,49]

cols_agreement_sum <- brewer.pal(5, "Oranges")
# pie(rep(1, length(cols_agreement_perc)), col = cols_agreement_perc, main = "")
cuts_agreement_sum <- cuts_classes_bars <- c(-.01,.01,1.01,2.01,3.01,4.01)
labels_agreement_sum <- 0:4
# cols_agreement_perc <- colorRampPalette(brewer.pal(9, "Oranges"))(10)
# cuts_agreement_perc <- cuts_classes_bars <- c(-.01,.01,1.01,2.01,3.01,4.01,5.01,6.01,7.01,8.01,9.01,10.01)*10
# labels_agreement_perc <- (0:10)*10
# cols_agreement_perc <- colorRampPalette(brewer.pal(5, "Oranges"))(5)
# cuts_agreement_perc <- cuts_classes_bars <- c(-.01,.01,1.01,2.01,3.01,4.01,5.01)*10
# labels_agreement_perc <- (0:5)*10
# cols_agreement_perc <- colorRampPalette(brewer.pal(4, "Oranges"))(4)
# cuts_agreement_perc <- cuts_classes_bars <- c(-.01,.01,1.51,3.01,4.51,6.01)*10
# labels_agreement_perc <- (0:4)*15
cols_agreement_perc <- colorRampPalette(brewer.pal(7, "Oranges"))(7)
cuts_agreement_perc <- cuts_classes_bars <- c(-.01,10.01,15.01,20.01,25.01,30.01,35.01,40.01,45.01)
labels_agreement_perc <- seq(10,45,5)

# plot the discrete colorbar for median dcorr
# dbar <- plot_discrete_cbar(legend_title = expression("% of models with significant drying"),
#                            breaks = c(0, 0.01, labels_agreement_perc[2:length(labels_agreement_perc)]),
#                            colors = c('lightgrey', cols_agreement_perc[9], cols_agreement_perc[10], cols_agreement_perc[1],
#                                       cols_agreement_perc[2:8]),
#                            # breaks = labels_agreement_perc,
#                            # colors = c(cols_agreement_perc[1], cols_agreement_perc[10],
#                            #            cols_agreement_perc[2:9]),
#                            # colors = c(cols_agreement_perc[0],cols_agreement_perc[10],cols_agreement_perc[1],
#                            #            cols_agreement_perc[2],cols_agreement_perc[3],cols_agreement_perc[4],
#                            #            cols_agreement_perc[5],cols_agreement_perc[6],cols_agreement_perc[7],
#                            #            cols_agreement_perc[8],cols_agreement_perc[9]),
#                            spacing = "constant",
#                            font_size = 6,
#                            spacing_scaling = 4,
#                            width = .2,
#                            triangle_size = .175,
#                            legend_direction = "horizontal")
# plot the discrete colorbar for median dcorr
dbar <- plot_discrete_cbar(legend_title = expression("% of models with significant drying"),
                           breaks = c(labels_agreement_perc),
                           colors = c(cols_agreement_perc),
                           # breaks = labels_agreement_perc,
                           # colors = c(cols_agreement_perc[1], cols_agreement_perc[10],
                           #            cols_agreement_perc[2:9]),
                           # colors = c(cols_agreement_perc[0],cols_agreement_perc[10],cols_agreement_perc[1],
                           #            cols_agreement_perc[2],cols_agreement_perc[3],cols_agreement_perc[4],
                           #            cols_agreement_perc[5],cols_agreement_perc[6],cols_agreement_perc[7],
                           #            cols_agreement_perc[8],cols_agreement_perc[9]),
                           spacing = "constant",
                           font_size = 6,
                           spacing_scaling = 4,
                           width = .2,
                           triangle_size = .175,
                           legend_direction = "horizontal")
# dbar

agreement_sum.df <- setNames(data.frame(matrix(ncol = 4, nrow = 0)),
                             c("lon","lat","agreement","period"))
agreement_perc.df <- setNames(data.frame(matrix(ncol = 5, nrow = 0)),
                              c("lon","lat","area","agreement","period"))

for(i in 1:6){
  for(x in 1:180){
    for(y in 1:90){
      if(!is.na(agreement_sum[x,y,i])){
        agreement_sum.df <- rbind(agreement_sum.df,
                                  data.frame("lon" = lon[x],
                                             "lat" = lat[y],
                                             "agreement_sum" = agreement_sum[x,y,i],
                                             "period" = periods_classes[i]))
      }
    }
  }
}
agreement_sum.df$cuts_agreement_sum <- cut(agreement_sum.df$agreement_sum, cuts_agreement_sum, include.lowest = T)

for(i in 1:6){
  for(x in 1:180){
    for(y in 1:90){
      if(!is.na(agreement_sum[x,y,i]) & models_sum[x,y] > 4){
        agreement_perc.df <- rbind(agreement_perc.df,
                                   data.frame("lon" = lon[x],
                                              "lat" = lat[y],
                                              "area" = area.array[x,y],
                                              "agreement_perc" = (agreement_sum[x,y,i]/models_sum[x,y])*100,
                                              "period" = periods_classes[i]))
      }
    }
  }
}

area_agreement_perc.df <- setNames(data.frame(matrix(ncol = 3, nrow = 0)),
                                   c("area","var","period"))
labels_agreement_perc_plot <- c("0",
                                "< 15",
                                "< 20",
                                "< 25",
                                "< 30",
                                "< 35",
                                "< 40",
                                "< 45")
# data.frame("area" = (sum(area.array[which((agreement_sum[,,i]/models_sum)*100 > cuts_agreement_perc[v] & (agreement_sum[,,i]/models_sum)*100 <= cuts_agreement_perc[v+1])], na.rm = T)/total_land_area)*100,
# data.frame("area" = sum(filter(agreement_perc.df, period == periods_classes[i] & agreement_perc > cuts_agreement_perc[v] & agreement_perc <= cuts_agreement_perc[v+1])$area)/total_land_area*100,
for(i in 1:6){
  for(v in 1:8){ # loop over all water variables
    area_agreement_perc.df <- rbind(area_agreement_perc.df, 
                                    data.frame("area" = sum(filter(agreement_perc.df, period == periods_classes[i] & agreement_perc > cuts_agreement_perc[v] & agreement_perc <= cuts_agreement_perc[v+1])$area)/total_land_area*100,
                                               "var" = labels_agreement_perc_plot[v],
                                               "period" = periods_classes[i]))
  }
}
agreement_perc.df$cuts_agreement_perc <- cut(agreement_perc.df$agreement_perc, cuts_agreement_perc, include.lowest = T)
area_agreement_perc.df$var_fac <- factor(area_agreement_perc.df$var, levels = labels_agreement_perc_plot)


for(i in 1:6){
  bb <- ggplot(filter(agreement_sum.df, period == periods_classes[i]), aes(x=lon,y=lat,fill=cuts_agreement_sum)) +
    # bb <- ggplot(filter(agreement_per_model.df, period == periods_classes[i] & source_id == cmip6_data.df$source_id[h]), aes(x=lon,y=lat,fill=cuts_agreement)) +
    # bb <- ggplot(mean_sign_trend_LAI.df, aes(x=lon,y=lat,fill=cuts_LAI)) +
    geom_tile() +
    geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
             inherit.aes = F, fill = NA, color = "black", size = .1) +
    scale_fill_manual("Sum of models with significant drying",
                      values = cols_agreement_sum,
                      labels = labels_agreement_sum,
                      drop = F) +
    scale_x_continuous("longitude",
                       limits=c(-180,180),
                       expand=c(0,0)) +
    scale_y_continuous("latitude",
                       limits=c(-60,75),
                       expand=c(0,0)) +
    # facet_wrap(~source_id, ncol = 3) +
    ggtitle(periods_classes[i]) +
    guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, nrow = 1)) +
    theme(legend.position = "bottom",
          legend.text = element_text(size=12),
          legend.title = element_text(size=20),
          strip.background = element_blank(),
          axis.line.x = element_line(colour = "black"),
          axis.line.y = element_line(colour = "black"),
          strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(color = 'black', fill = NA, size = 1),
          panel.background = element_blank(),
          axis.text = element_text(size=18),
          axis.title = element_blank(),
          plot.title = element_text(size=24, hjust = 0.5),
          plot.tag.position = c(.55,0.03)
    )
  # bb
  ggsave(paste0("/Net/Groups/BGI/people/jdenis/scripts/scripts_Nature_Perspective/agreement_wtr_sum_",substr(periods_classes[i], start = 1, stop = 4),"_",substr(periods_classes[i], start = 8, stop = 11),".png"), plot = bb, width = 9*1.5, height = 7*1.5, units = "in")
  
  bb <- ggplot(filter(agreement_perc.df, period == periods_classes[i]), aes(x=lon,y=lat,fill=cuts_agreement_perc)) +
    # bb <- ggplot(filter(agreement_per_model.df, period == periods_classes[i] & source_id == cmip6_data.df$source_id[h]), aes(x=lon,y=lat,fill=cuts_agreement)) +
    # bb <- ggplot(mean_sign_trend_LAI.df, aes(x=lon,y=lat,fill=cuts_LAI)) +
    geom_tile() +
    geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
             inherit.aes = F, fill = NA, color = "black", size = .1) +
    scale_fill_manual("% of models with significant drying",
                      values = c('lightgrey',cols_agreement_perc,cols_agreement_perc),
                      labels = c(labels_agreement_perc,labels_agreement_perc[1:5]),
                      drop = F) +
    scale_x_continuous("longitude",
                       limits=c(-180,180),
                       expand=c(0,0)) +
    scale_y_continuous("latitude",
                       limits=c(-60,75),
                       expand=c(0,0)) +
    # facet_wrap(~source_id, ncol = 3) +
    ggtitle(periods_classes[i]) +
    guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, nrow = 1)) +
    theme(legend.position = "none",
          legend.text = element_text(size=12),
          legend.title = element_text(size=20),
          strip.background = element_blank(),
          axis.line.x = element_line(colour = "black"),
          axis.line.y = element_line(colour = "black"),
          strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(color = 'black', fill = NA, size = 1),
          panel.background = element_blank(),
          axis.text = element_text(size=18),
          axis.title = element_blank(),
          plot.title = element_text(size=24, hjust = 0.5),
          plot.tag.position = c(.55,0.03)
    )
  bb
  
  # reduce top and bottom margins
  empty <- ggplot() + theme_void()
  dbar <- dbar + theme(plot.margin = unit(c(-35, 10, -30, 10), "pt"))
  dbar_smaller <- grid.arrange(empty, dbar, empty , ncol=3, widths = c(1,4,1))
  
  plot <- grid.arrange(bb,dbar_smaller, nrow = 2, heights = c(.8,.2))
  
  ggsave(paste0("/Net/Groups/BGI/people/jdenis/scripts/scripts_Nature_Perspective/agreement_wtr_perc_",substr(periods_classes[i], start = 1, stop = 4),"_",substr(periods_classes[i], start = 8, stop = 11),".png"), plot = plot, width = 9*1.5, height = 7*1.5, units = "in")
  
}

##### Fig 2 #####

bb_c <- ggplot(filter(agreement_perc.df, period == periods_classes[1]), aes(x=lon,y=lat,fill=cuts_agreement_perc)) +
  # bb <- ggplot(filter(agreement_per_model.df, period == periods_classes[1] & source_id == cmip6_data.df$source_id[h]), aes(x=lon,y=lat,fill=cuts_agreement)) +
  # bb <- ggplot(mean_sign_trend_LAI.df, aes(x=lon,y=lat,fill=cuts_LAI)) +
  geom_tile() +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  scale_fill_manual("% of models with significant drying",
                    values = c('lightgrey',cols_agreement_perc,cols_agreement_perc),
                    labels = c(labels_agreement_perc,labels_agreement_perc[1:5]),
                    drop = F) +
  scale_x_continuous("longitude",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("latitude",
                     limits=c(-60,75),
                     expand=c(0,0)) +
  # facet_wrap(~source_id, ncol = 3) +
  ggtitle("") +
  guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, nrow = 1)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_blank(),
        plot.title = element_text(size=24, hjust = 0),
        plot.tag.position = c(.55,0.03)
  )
bb_c
bb_c <- ggplotGrob(bb_c)


bar_area_agreement_perc_c <- ggplot(filter(area_agreement_perc.df, period == periods_classes[1]), aes(x = var_fac, y = area, fill = var_fac)) +
  geom_bar(stat='identity',col = 'black', width = 1, position = "stack") +
  geom_text(inherit.aes = F, data = filter(area_agreement_perc.df, period == periods_classes[1] & area<1), aes(x=var_fac,y=area+5,label=round(area,1)), size = 3) +
  geom_text(inherit.aes = F, data = filter(area_agreement_perc.df, period == periods_classes[1] & area>1), aes(x=var_fac,y=area+5,label=round(area,0)), size = 3) +
  scale_x_discrete("") +
  scale_y_continuous("", expand = c(0,0), limits = c(0, (max(area_agreement_perc.df$area)) + 10), position = "left") +
  scale_fill_manual(values = c("0" = 'lightgrey',
                               "< 15" = cols_agreement_perc[1],
                               "< 20" = cols_agreement_perc[2],
                               "< 25" = cols_agreement_perc[3],
                               "< 30" = cols_agreement_perc[4],
                               "< 35" = cols_agreement_perc[5],
                               "< 40" = cols_agreement_perc[6],
                               "< 45" = cols_agreement_perc[7]),
                    drop = F) +
  ggtitle(expression(paste("% of land area"))) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=16),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.title = element_text(size=9),
        plot.title = element_text(size=16, hjust = 0.5)
  )
bar_area_agreement_perc_c
bar_area_agreement_perc_c <- ggplotGrob(bar_area_agreement_perc_c)

gt1_c <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1_c <- gtable_add_grob(gt1_c, bb_c, t=1, b=32, l=1, r=32)
# gt1_c <- gtable_add_grob(gt1_c, bar_area_agreement_perc, t = 25, l = 3, b = 17, r = 10)
gt1_c <- gtable_add_grob(gt1_c, bar_area_agreement_perc_c, t = 29, l = 3, b = 19, r = 10)
grid.draw(gt1_c)
# plot_c <- grid.arrange(gt1_c,dbar_smaller, nrow = 2, heights = c(.8,.2))

bb_d <- ggplot(filter(agreement_perc.df, period == periods_classes[2]), aes(x=lon,y=lat,fill=cuts_agreement_perc)) +
  # bb <- ggplot(filter(agreement_per_model.df, period == periods_classes[2] & source_id == cmip6_data.df$source_id[h]), aes(x=lon,y=lat,fill=cuts_agreement)) +
  # bb <- ggplot(mean_sign_trend_LAI.df, aes(x=lon,y=lat,fill=cuts_LAI)) +
  geom_tile() +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  scale_fill_manual("% of models with significant drying",
                    values = c('lightgrey',cols_agreement_perc,cols_agreement_perc),
                    labels = c(labels_agreement_perc,labels_agreement_perc[1:5]),
                    drop = F) +
  scale_x_continuous("longitude",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("latitude",
                     limits=c(-60,75),
                     expand=c(0,0)) +
  # facet_wrap(~source_id, ncol = 3) +
  ggtitle("") +
  guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, nrow = 1)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=18),
        axis.title = element_blank(),
        plot.title = element_text(size=24, hjust = 0),
        plot.tag.position = c(.55,0.03)
  )
bb_d
bb_d <- ggplotGrob(bb_d)

bar_area_agreement_perc_d <- ggplot(filter(area_agreement_perc.df, period == periods_classes[2]), aes(x = var_fac, y = area, fill = var_fac)) +
  geom_bar(stat='identity',col = 'black', width = 1, position = "stack") +
  geom_text(inherit.aes = F, data = filter(area_agreement_perc.df, period == periods_classes[2] & area<1), aes(x=var_fac,y=area+5,label=round(area,1)), size = 3) +
  geom_text(inherit.aes = F, data = filter(area_agreement_perc.df, period == periods_classes[2] & area>1), aes(x=var_fac,y=area+5,label=round(area,0)), size = 3) +
  scale_x_discrete("") +
  scale_y_continuous("", expand = c(0,0), limits = c(0, (max(area_agreement_perc.df$area)) + 10), position = "left") +
  scale_fill_manual(values = c("0" = 'lightgrey',
                               "< 15" = cols_agreement_perc[1],
                               "< 20" = cols_agreement_perc[2],
                               "< 25" = cols_agreement_perc[3],
                               "< 30" = cols_agreement_perc[4],
                               "< 35" = cols_agreement_perc[5],
                               "< 40" = cols_agreement_perc[6],
                               "< 45" = cols_agreement_perc[7]),
                    drop = F) +
  ggtitle(expression(paste("% of land area"))) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=16),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.title = element_text(size=9),
        plot.title = element_text(size=16, hjust = 0.5)
  )
bar_area_agreement_perc_d
bar_area_agreement_perc_d <- ggplotGrob(bar_area_agreement_perc_d)

gt1_d <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1_d <- gtable_add_grob(gt1_d, bb_d, t=1, b=32, l=1, r=32)
# gt1_d <- gtable_add_grob(gt1_d, bar_area_agreement_perc, t = 25, l = 3, b = 17, r = 10)
gt1_d <- gtable_add_grob(gt1_d, bar_area_agreement_perc_d, t = 29, l = 3, b = 19, r = 10)
grid.draw(gt1_d)

# reduce top and bottom margins
empty <- ggplot() + theme_void()
dbar <- dbar + theme(plot.margin = unit(c(-35, 10, -30, 10), "pt"))
dbar_smaller <- grid.arrange(empty, dbar, empty , ncol=3, widths = c(1,4,1))

# make grey box titled 'insignificant'
rect_grey <- data.frame(x = c(1),
                        colors = c("lightgrey"))

dbar_grey <- ggplot(rect_grey, aes(x, y = 0, fill = colors)) +
  geom_tile(width = .9, height = .45) + # make square tiles
  scale_fill_identity(guide = "none") + # color the tiles with the colors in the data frame
  coord_fixed() + # make sure tiles are square
  ggtitle("No agreement") + 
  theme_void() + # remove any axis markings
  theme(plot.title = element_text(size=18, hjust = 0.5, margin = margin(0, 0, .3, 0, "cm")),
        plot.margin = unit(c(.6,0,1.7,0),"cm"))


# plot <- grid.arrange(grid.arrange(gt1_a,gt1_b,gt1_c,gt1_d, ncol = 2, nrow = 2),
plot_ab <- grid.arrange(plot_grid(gt1_a,gt1_b,labels = c("a)","b)"), ncol = 2, label_size = 20),
                        grid.arrange(empty,bb_legend,empty, ncol = 3, widths = c(.15,.7,.15)),
                        nrow = 2, heights = c(1,.4))
plot_cd <- grid.arrange(plot_grid(gt1_c,gt1_d,labels = c("c)","d)"), ncol = 2, label_size = 20),
                        grid.arrange(empty, dbar_grey, dbar_smaller, empty, ncol=4, widths = c(.2,.03,.57,.2)),
                        nrow = 2, heights = c(1,.4))

plot <- grid.arrange(plot_ab, plot_cd,
                     nrow = 2)



# ggsave(paste0("/Net/Groups/BGI/people/jdenis/scripts/scripts_Nature_Perspective/Fig2.png"), plot = plot, width = 18*1.25, height = 14*1.25, units = "in")
ggsave(paste0("/Net/Groups/BGI/people/jdenis/scripts/scripts_Nature_Perspective/Fig2.png"), plot = plot, width = 18, height = 14, units = "in")

# plot_d <- grid.arrange(gt1_d,dbar_smaller, nrow = 2, heights = c(.8,.2))
