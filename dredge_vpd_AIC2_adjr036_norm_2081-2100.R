# script to do a regression analysis, explaining LAI variability with several water-availability, water-demand and energy-availability variables
# 2081-2100
# 2024-05-17
# by: Jasper Denissen
# copied from: dredge_vpd_LAI005_5yr_AIC.R

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
source('/Net/Groups/BGI/people/jdenis/scripts/functions/plot_discrete_cbar.R')

lon <- seq(-179,179,2)
lat <- seq(-89,89,2)

# ########################################################################################################################
# ##########################################                                  ############################################
# ##########################################        new model list (w/ hurs)  ############################################
# ##########################################                                  ############################################
# ########################################################################################################################
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

source_id_mrsol <- c()
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

print("Take care to remove CNRM-CM6-1 (or [3] in if length(cmip6_data.df$source_id) == 11)")

# av_lai.array <- array(NaN,c(180,90,length(cmip6_data.df$source_id)))
# 
# for(i in 1:(length(cmip6_data.df$source_id))){
#   for(x in 1:180){
#     for(y in 1:90){
#       av_lai.array[x,y,i] <- mean(av_lai_yr.list[[i]][x,y,c(101:120)])
#     }
#   }
# }
# 
# # av_lai.array <-
# #   array(NaN,c(180,90,length(cmip6_data.df$source_id)*12))
# # count_all <- 1
# # for(i in 1:(length(cmip6_data.df$source_id))){
# #   av_lai.array[,,count_all:(count_all+11)] <- av_lai.list[[i]]
# #   count_all <- count_all + 12
# #   print(paste(i, " is done...",sep=''))
# # }
# # Rearrange the grid cells, because they are shifted 180 degrees in longitude
# test <- array(NaN,c(180,90,length(cmip6_data.df$source_id))); test[1:90,,] <- av_lai.array[91:180,,]; test[91:180,,] <- av_lai.array[1:90,,]; av_lai.array <- test
# # mask from 5yr LAI values > 0.5
# mask_lai <- array(NaN,c(180,90,length(cmip6_data.df$source_id)))
# mask_lai[which(av_lai.array > 0.5)] <- 1
# # already apply the lai mask! 
# 
# # grid cells for the regression analysis should only retain values if the respective model has all values in the time series.
# mask_obs <- array(NaN,c(180,90,length(cmip6_data.df$source_id)))
# mask_obs_yr <- array(NaN,c(180,90,length(cmip6_data.df$source_id)*120))
# ct_yr <- 1
# for(i in 1:length(cmip6_data.df$source_id)){ 
#   for(x in 1:180){
#     for(y in 1:90){
#       if(!is.na(mask_lai[x,y,i])){ # check if all the models have a value there
#         # if(sum(!is.na(mask_lai[x,y,(ct_dec:(ct_dec+11))])) == 12){ # check if all the models have a value there
#         mask_obs[x,y,i] <- 1
#         mask_obs_yr[x,y,(ct_yr:(ct_yr+119))] <- 1
#       }
#     }
#   }
#   ct_yr <- ct_yr + 120
# }
# 
# av_lai.array <- av_lai.array * mask_obs

# yearly values
av_lai_yr.array <-
  av_mrsol_yr.array <-
  max_tasmax_yr.array <-
  min_mrsol_yr.array <-
  av_vpd_yr.array <-
  av_pr_yr.array <-
  av_netrad_yr.array <-
  # av_AI_yr.array <-
  av_rsds_yr.array <-
  av_tas_yr.array <-
  array(NaN,c(180,90,length(cmip6_data.df$source_id)*120))
count_all <- 1
for(i in 1:length(cmip6_data.df$source_id)){ 
  av_lai_yr.array[,,count_all:(count_all+119)] <- av_lai_yr.list[[i]]
  av_mrsol_yr.array[,,count_all:(count_all+119)] <- av_mrsol_yr.list[[i]]
  max_tasmax_yr.array[,,count_all:(count_all+119)] <- max_tasmax_yr.list[[i]]
  min_mrsol_yr.array[,,count_all:(count_all+119)] <- min_mrsol_yr.list[[i]]
  av_vpd_yr.array[,,count_all:(count_all+119)] <- av_vpd_yr.list[[i]]
  av_pr_yr.array[,,count_all:(count_all+119)] <- av_pr_yr.list[[i]]
  av_netrad_yr.array[,,count_all:(count_all+119)] <- av_netrad_yr.list[[i]]
  # av_AI_yr.array[,,count_all:(count_all+119)] <- av_AI_yr.list[[i]]
  av_rsds_yr.array[,,count_all:(count_all+119)] <- av_rsds_yr.list[[i]]
  av_tas_yr.array[,,count_all:(count_all+119)] <- av_tas_yr.list[[i]]
  count_all <- count_all + 120
  print(paste(i, " is done...",sep=''))
}
# Rearrange the grid cells, because they are shifted 180 degrees in longitude
test <- array(NaN,c(180,90,length(cmip6_data.df$source_id)*120)); test[1:90,,] <- av_lai_yr.array[91:180,,]; test[91:180,,] <- av_lai_yr.array[1:90,,]; av_lai_yr.array <- test
test <- array(NaN,c(180,90,length(cmip6_data.df$source_id)*120)); test[1:90,,] <- av_mrsol_yr.array[91:180,,]; test[91:180,,] <- av_mrsol_yr.array[1:90,,]; av_mrsol_yr.array <- test
test <- array(NaN,c(180,90,length(cmip6_data.df$source_id)*120)); test[1:90,,] <- max_tasmax_yr.array[91:180,,]; test[91:180,,] <- max_tasmax_yr.array[1:90,,]; max_tasmax_yr.array <- test
test <- array(NaN,c(180,90,length(cmip6_data.df$source_id)*120)); test[1:90,,] <- min_mrsol_yr.array[91:180,,]; test[91:180,,] <- min_mrsol_yr.array[1:90,,]; min_mrsol_yr.array <- test
test <- array(NaN,c(180,90,length(cmip6_data.df$source_id)*120)); test[1:90,,] <- av_vpd_yr.array[91:180,,]; test[91:180,,] <- av_vpd_yr.array[1:90,,]; av_vpd_yr.array <- test
test <- array(NaN,c(180,90,length(cmip6_data.df$source_id)*120)); test[1:90,,] <- av_pr_yr.array[91:180,,]; test[91:180,,] <- av_pr_yr.array[1:90,,]; av_pr_yr.array <- test
test <- array(NaN,c(180,90,length(cmip6_data.df$source_id)*120)); test[1:90,,] <- av_netrad_yr.array[91:180,,]; test[91:180,,] <- av_netrad_yr.array[1:90,,]; av_netrad_yr.array <- test
test <- array(NaN,c(180,90,length(cmip6_data.df$source_id)*120)); test[1:90,,] <- av_rsds_yr.array[91:180,,]; test[91:180,,] <- av_rsds_yr.array[1:90,,]; av_rsds_yr.array <- test
test <- array(NaN,c(180,90,length(cmip6_data.df$source_id)*120)); test[1:90,,] <- av_tas_yr.array[91:180,,]; test[91:180,,] <- av_tas_yr.array[1:90,,]; av_tas_yr.array <- test

ct_120 <- 101
if(mode == 'all'){
  ct_60 <- ct_120
}else if(mode == 'aligned'){
  ct_60 <- 41
}

for(h in 1:length(cmip6_data.df$source_id)){
  av_lai_yr.array[,,(ct_120:(ct_120+19))] <- av_lai_yr.array[,,(ct_120:(ct_120+19))] * mask_obs_yr[,,(ct_60:(ct_60+19))]
  av_mrsol_yr.array[,,(ct_120:(ct_120+19))] <- av_mrsol_yr.array[,,(ct_120:(ct_120+19))] * mask_obs_yr[,,(ct_60:(ct_60+19))]
  max_tasmax_yr.array[,,(ct_120:(ct_120+19))] <- max_tasmax_yr.array[,,(ct_120:(ct_120+19))] * mask_obs_yr[,,(ct_60:(ct_60+19))]
  min_mrsol_yr.array[,,(ct_120:(ct_120+19))] <- min_mrsol_yr.array[,,(ct_120:(ct_120+19))] * mask_obs_yr[,,(ct_60:(ct_60+19))]
  av_vpd_yr.array[,,(ct_120:(ct_120+19))] <- av_vpd_yr.array[,,(ct_120:(ct_120+19))] * mask_obs_yr[,,(ct_60:(ct_60+19))]
  av_pr_yr.array[,,(ct_120:(ct_120+19))] <- av_pr_yr.array[,,(ct_120:(ct_120+19))] * mask_obs_yr[,,(ct_60:(ct_60+19))]
  av_netrad_yr.array[,,(ct_120:(ct_120+19))] <- av_netrad_yr.array[,,(ct_120:(ct_120+19))] * mask_obs_yr[,,(ct_60:(ct_60+19))]
  av_rsds_yr.array[,,(ct_120:(ct_120+19))] <- av_rsds_yr.array[,,(ct_120:(ct_120+19))] * mask_obs_yr[,,(ct_60:(ct_60+19))]
  av_tas_yr.array[,,(ct_120:(ct_120+19))] <- av_tas_yr.array[,,(ct_120:(ct_120+19))] * mask_obs_yr[,,(ct_60:(ct_60+19))]
  ct_120 <- ct_120 + 120
  ct_60 <- ct_60 + 60
}

# maybe now per surface area instead of % of grid cells?
r <- raster()  # by default 1 by 1 degree
res(r) <- 2 # so change the resolution
a <- raster::area(r) # calculate the area of a 2x2 degree grid from N - S, as area varies only by latitude, not longitude
area <- a[,1]
area.array <- array(NaN,c(180,90))
for(x in 1:180){
  area.array[x,] <- area
}

# with neighboring grid cells
# vars <- c("mrsol","pr","vpd","AI","tas","rsds","netrad","adj. R2 < 0.36") # 7 variables
# var_type <- c("water","water","demand","demand","energy","energy","energy")
vars <- c("mrsol","min_mrsol","pr","vpd","tas","max_tasmax","rsds","netrad","adj. R2 < 0.36") # 9 variables
var_type <- c("water","water","water","demand","energy","energy","energy","energy")

options(na.action = "na.fail")
dom_var_dredge_relimp_ng <- array(NaN,c(180,90,length(cmip6_data.df$source_id),2))
dom_var_dredge_relimp_ng_all <- array(0,c(180,90,length(cmip6_data.df$source_id),(length(vars)-1)))
adj_R2_dredge_relimp_ng <- array(NaN,c(180,90,length(cmip6_data.df$source_id)))
dom_water_var_ng <- array(NaN,c(180,90,length(cmip6_data.df$source_id),2))
rank_dom_water_var_ng <- array(NaN,c(180,90,length(cmip6_data.df$source_id)))
lin_mod_ng.array <- array(NaN,c(180,90,length(cmip6_data.df$source_id)))
ct_120 <- 101
x=93;y=70 # Germany
x=88;y=66 # Spain
# x=86;y=66 # Spain but more west
x=68;y=38 # Brazil
x=163;y=29 # AUS
x=40;y=65 # test dom_water_var

for(h in 1:length(cmip6_data.df$source_id)){
  for(x in 1:180){
    for(y in 1:90){
      if(sum(!is.na(av_lai_yr.array[x,y,(ct_120:(ct_120+19))])) == 20 & round(sd(av_lai_yr.array[x,y,(ct_120:(ct_120+19))]),4) != 0 & round(sd(av_mrsol_yr.array[x,y,(ct_120:(ct_120+19))]),4) != 0){
        relimp_variables.list <- list()
        rsq.vec <- c()
        # linear model with normalized predictors
        predictors.df <- data.frame("lai" = (av_lai_yr.array[x,y,(ct_120:(ct_120+19))] - min(av_lai_yr.array[x,y,(ct_120:(ct_120+19))], na.rm = T))/(max(av_lai_yr.array[x,y,(ct_120:(ct_120+19))], na.rm = T) - min(av_lai_yr.array[x,y,(ct_120:(ct_120+19))], na.rm = T)),
                                    "mrsol" = (av_mrsol_yr.array[x,y,(ct_120:(ct_120+19))] - min(av_mrsol_yr.array[x,y,(ct_120:(ct_120+19))], na.rm = T))/(max(av_mrsol_yr.array[x,y,(ct_120:(ct_120+19))], na.rm = T) - min(av_mrsol_yr.array[x,y,(ct_120:(ct_120+19))], na.rm = T)),
                                    "min_mrsol" = (min_mrsol_yr.array[x,y,(ct_120:(ct_120+19))] - min(min_mrsol_yr.array[x,y,(ct_120:(ct_120+19))], na.rm = T))/(max(min_mrsol_yr.array[x,y,(ct_120:(ct_120+19))], na.rm = T) - min(min_mrsol_yr.array[x,y,(ct_120:(ct_120+19))], na.rm = T)),
                                    "pr" = (av_pr_yr.array[x,y,(ct_120:(ct_120+19))] - min(av_pr_yr.array[x,y,(ct_120:(ct_120+19))], na.rm = T))/(max(av_pr_yr.array[x,y,(ct_120:(ct_120+19))], na.rm = T) - min(av_pr_yr.array[x,y,(ct_120:(ct_120+19))], na.rm = T)),
                                    "vpd" = (av_vpd_yr.array[x,y,(ct_120:(ct_120+19))] - min(av_vpd_yr.array[x,y,(ct_120:(ct_120+19))], na.rm = T))/(max(av_vpd_yr.array[x,y,(ct_120:(ct_120+19))], na.rm = T) - min(av_vpd_yr.array[x,y,(ct_120:(ct_120+19))], na.rm = T)),
                                    "tas" = (av_tas_yr.array[x,y,(ct_120:(ct_120+19))] - min(av_tas_yr.array[x,y,(ct_120:(ct_120+19))], na.rm = T))/(max(av_tas_yr.array[x,y,(ct_120:(ct_120+19))], na.rm = T) - min(av_tas_yr.array[x,y,(ct_120:(ct_120+19))], na.rm = T)),
                                    "max_tasmax" = (max_tasmax_yr.array[x,y,(ct_120:(ct_120+19))] - min(max_tasmax_yr.array[x,y,(ct_120:(ct_120+19))], na.rm = T))/(max(max_tasmax_yr.array[x,y,(ct_120:(ct_120+19))], na.rm = T) - min(max_tasmax_yr.array[x,y,(ct_120:(ct_120+19))], na.rm = T)),
                                    "rsds" = (av_rsds_yr.array[x,y,(ct_120:(ct_120+19))] - min(av_rsds_yr.array[x,y,(ct_120:(ct_120+19))], na.rm = T))/(max(av_rsds_yr.array[x,y,(ct_120:(ct_120+19))], na.rm = T) - min(av_rsds_yr.array[x,y,(ct_120:(ct_120+19))], na.rm = T)),
                                    "netrad" = (av_netrad_yr.array[x,y,(ct_120:(ct_120+19))] - min(av_netrad_yr.array[x,y,(ct_120:(ct_120+19))], na.rm = T))/(max(av_netrad_yr.array[x,y,(ct_120:(ct_120+19))], na.rm = T) - min(av_netrad_yr.array[x,y,(ct_120:(ct_120+19))], na.rm = T)))
        
        # lm_dcorr_rsds.df <- lm(formula = lai ~ mrsol + pr + vpd + AI + tas + rsds + netrad, data = predictors.df)
        lm_dcorr_rsds.df <- lm(formula = lai ~ mrsol + min_mrsol + pr + vpd + tas + max_tasmax + rsds + netrad, data = predictors.df)
        dd <- dredge(lm_dcorr_rsds.df, extra = list(AIC))
        
        # IMPORTANCE: pick a subset of best models (all have same performance/complexity in terms of AIC)
        dd_subset <- subset(dd, delta < 2)
        relimp_var_lmg.df <- setNames(data.frame(matrix(ncol = 3, nrow = 0)),
                                      c("var","lmg","weight"))
        total_var_expl.c <- c()
        for(lin_mod in 1:dim(dd_subset)[1]){ # loop over all models of good enough quality
          rsq.vec[lin_mod] <- summary(get.models(dd, lin_mod)[[1]])$r.squared
          # p.vec[lin_mod] <- summary(get.models(dd, lin_mod)[[1]])$coefficients
          if(rsq.vec[lin_mod] > 0.36){ # double check whether their adjusted r squared are half decent
            print(lin_mod)
            if(length(names(summary(get.models(dd_subset, lin_mod)[[1]])$aliased)) - 1 > 1){ # check if model has multiple variables
              relimp <- calc.relimp(get.models(dd_subset, lin_mod)[[1]])
              for(i in 1:(length(vars)-1)){ # amount of variables
                if(sum(grepl(vars[i], names(relimp@lmg))) == 1){ # extract all variables and explained variance per variable
                  relimp_var_lmg.df <- rbind(relimp_var_lmg.df,
                                             data.frame("var" = i, # which variable
                                                        "lmg" = unname(relimp$lmg[which(grepl(vars[i], names(relimp@lmg)) == T)]), # explained variance per variable
                                                        "weight" = dd_subset$weight[lin_mod])) # Akaike weight per model
                  dom_var_dredge_relimp_ng_all[x,y,h,i] <- dom_var_dredge_relimp_ng_all[x,y,h,i] + 1
                }
              }
            }else{ # if the model has only one variable
              expl_dcorr_rsds_dredge.df <- summary(get.models(dd_subset, lin_mod)[[1]])
              for(i in 1:(length(vars)-1)){
                if(grepl(vars[i],names(expl_dcorr_rsds_dredge.df$aliased)[2])){ # if there are only two explanatory variables left in the model, the first one is the intercept and the second one is the variable
                  relimp_var_lmg.df <- rbind(relimp_var_lmg.df,
                                             data.frame("var" = i, # which variable
                                                        "lmg" = expl_dcorr_rsds_dredge.df$r.squared, # explained variance for dominant variable
                                                        "weight" = dd_subset$weight[lin_mod])) # Akaike weight per model
                  dom_var_dredge_relimp_ng_all[x,y,h,i] <- dom_var_dredge_relimp_ng_all[x,y,h,i] + 1
                  next
                }
              }
            }
          }
          if(lin_mod == dim(dd_subset)[1]){ # on the last model, to summarize all linear models for this grid cell
            lin_mod_ng.array[x,y,h] <- sum(rsq.vec > 0.36) # number of linear models with sufficient explanatory power
            adj_R2_dredge_relimp_ng[x,y,h] <- mean(rsq.vec, na.rm = T)
            for(j in 1:(length(vars)-1)){
              total_var_expl.c[j] <- weighted.mean(x = relimp_var_lmg.df$lmg[which(relimp_var_lmg.df$var == j)], w = relimp_var_lmg.df$weight[which(relimp_var_lmg.df$var == j)], na.rm = T)
            }
            if(sum(is.na(total_var_expl.c)) == length(vars)-1){
              dom_var_dredge_relimp_ng[x,y,h,1] <- length(vars) # this will substitute the value length(vars) when there are linear models, but they are not of sufficient explanatory power. 
            }else if(length(which(total_var_expl.c == max(total_var_expl.c, na.rm = T))) == 1){ # if there is one dominant variable
              dom_var_dredge_relimp_ng[x,y,h,1] <- which(total_var_expl.c == max(total_var_expl.c, na.rm = T))
            }else if(length(which(total_var_expl.c == max(total_var_expl.c, na.rm = T))) > 1){ # if there is a two way tie
              dom_var_dredge_relimp_ng[x,y,h,] <- which(total_var_expl.c == max(total_var_expl.c, na.rm = T))
            }
            # # ranks across models
            if(dom_var_dredge_relimp_ng[x,y,h,1] < length(vars)){ # go into this loop only if there are any explanatory linear models
              total_var_expl.c[which(is.na(total_var_expl.c))] <- 0 # Replace NaN with 0 in total_var_expl.c such that variables not appearing in explanatory models come last in the ranking
              total_ranks.c <- rank(total_var_expl.c, ties.method = 'random', na.last = T) # rank the total explained variance per variable
              # total_ranks.c[which(is.na(total_var_expl.c))] <- NaN # Enter NaN if the variable appears in no model at all
              total_ranks.c <- (length(vars)) - total_ranks.c # revert the ranks so that 1 is the highest rank!
              
              if(length(which(!is.na(total_var_expl.c[1:length(which(var_type != 'energy'))]))) > 0){ # Only compute when water variables have any explanatory power.
                if(length(which(total_var_expl.c[1:length(which(var_type != 'energy'))] == max(total_var_expl.c[1:length(which(var_type != 'energy'))], na.rm = T))) == 1){
                  dom_water_var_ng[x,y,h,1] <- which(total_var_expl.c[1:length(which(var_type != 'energy'))] == max(total_var_expl.c[1:length(which(var_type != 'energy'))], na.rm = T)) # what is the most important water availability variable?
                }else if(length(which(total_var_expl.c[1:length(which(var_type != 'energy'))] == max(total_var_expl.c[1:length(which(var_type != 'energy'))], na.rm = T))) > 1 & sum(total_var_expl.c[1:length(which(var_type != 'energy'))]) > 0){
                  dom_water_var_ng[x,y,h,] <- which(total_var_expl.c[1:length(which(var_type != 'energy'))] == max(total_var_expl.c[1:length(which(var_type != 'energy'))], na.rm = T)) # what is the most important water availability variable?
                }
                rank_dom_water_var_ng[x,y,h] <- min(total_ranks.c[1:length(which(var_type != 'energy'))], na.rm = T)
              }
            }
          }
        }
      }
    }
    print(paste(round(x/(180)*100,2),"%"))
  }
  ct_120 <- ct_120 + 120
}

# save.image("/Net/Groups/BGI/people/jdenis/scripts/Data/dredge_vpd_LAI005_acccmip6_EF_yr_AIC005_2081_2100.RData")
save.image("/Net/Groups/BGI/people/jdenis/scripts/Data/dredge_AIC2_adjr036_norm_2081_2100.RData")

# There are ties in the underlying data too...
# make an array that holds the ties
dom_var_dredge_relimp_ng_tie <- array(NaN,c(180,90,length(cmip6_data.df$source_id),2))
for(h in 1:length(cmip6_data.df$source_id)){
  for(x in 1:180){
    for(y in 1:90){
      if(!is.na(dom_var_dredge_relimp_ng[x,y,h,2])){
        dom_var_dredge_relimp_ng_tie[x,y,h,] <- dom_var_dredge_relimp_ng[x,y,h,]
      }
    }
  }
}

how_many_ties <- array(NaN,c(180,90,length(cmip6_data.df$source_id)))
for(h in 1:length(cmip6_data.df$source_id)){
  for(x in 1:180){
    for(y in 1:90){
      if(!is.na(dom_var_dredge_relimp_ng_tie[x,y,h,1])){
        how_many_ties[x,y,h] <- 1
      }
    }
  }
}

# how_many_vars_relimp_ng <- array(NaN,c(180,90,length(cmip6_data.df$source_id)))
# for(h in 1:length(cmip6_data.df$source_id)){
#   for(x in 1:180){
#     for(y in 1:90){
#       how_many_vars_relimp_ng[x,y,h] <- length(which(dom_var_dredge_relimp_ng_all[x,y,h,] != 0))
#     }
#   }
# }
# how_many_vars_relimp_ng[which(how_many_vars_relimp_ng == 0)] <- NaN

dom_var_ts.df <- setNames(data.frame(matrix(ncol = 7, nrow = 0)),
                          c("dom_var_dredge_relimp_ng","dom_water_var_ng","rank_dom_water_var_ng","lon","lat","ties","source_id"))
for(h in 1:length(cmip6_data.df$source_id)){
  for(x in 1:180){
    for(y in 1:90){
      if(!is.na(dom_var_dredge_relimp_ng[x,y,h,1])){ # every grid cell that has linear models with values. If there is no water variable in any of the regression models of that grid cell, dom_water_var_ng[x,y,h] will be NaN and in the plot the last value of cols_dom_water_var will be assigned ('no water variable' = 'grey40')
        dom_var_ts.df <- rbind(dom_var_ts.df,
                               data.frame("dom_var_dredge_relimp_ng" = dom_var_dredge_relimp_ng[x,y,h,1],
                                          "dom_water_var_ng" = dom_water_var_ng[x,y,h,1],
                                          "rank_dom_water_var_ng" = rank_dom_water_var_ng[x,y,h],
                                          "lon" = lon[x],
                                          "lat" = lat[y],
                                          "ties" = how_many_ties[x,y,h],
                                          "source_id" = cmip6_data.df$source_id[h]
                               ))
      }
    }
  }
}
# now cluster energy and water variables together in a new variable
dom_var_ts.df$dom_var_dredge_relimp_ng_cluster <- NaN
dom_var_ts.df$dom_var_dredge_relimp_ng_cluster[which(dom_var_ts.df$dom_var_dredge_relimp_ng < 5)] <- 1
dom_var_ts.df$dom_var_dredge_relimp_ng_cluster[which(dom_var_ts.df$dom_var_dredge_relimp_ng > 4)] <- 2
dom_var_ts.df$dom_var_dredge_relimp_ng_cluster[which(dom_var_ts.df$dom_var_dredge_relimp_ng == 9)] <- 3
# enter in rank_dom_water_var_ng the lowest rank + 1, which indicates that there are linear models
dom_var_ts.df$dom_water_var_ng[which(dom_var_ts.df$dom_var_dredge_relimp_ng == length(vars) & is.na(dom_var_ts.df$dom_water_var_ng))] <- max(unique(dom_var_ts.df$dom_water_var_ng), na.rm = T) + 1
dom_var_ts.df$rank_dom_water_var_ng[which(dom_var_ts.df$dom_var_dredge_relimp_ng == length(vars) & is.na(dom_var_ts.df$rank_dom_water_var_ng))] <- max(unique(dom_var_ts.df$rank_dom_water_var_ng), na.rm = T) + 1

# labels_dom_var <- c("Soil moisture","Precipitation","Vapour Pressure Deficit","AI","Air Temperature","Incoming shortwave radiation","Net radiation",expression("adjusted R"^2*" < 0.36"))
# cols_dom_var <- c(brewer.pal(11,"RdYlBu")[9], 'turquoise2', 'lila', 'sienna',brewer.pal(11,"RdYlBu")[3], 'red','yellow', 'snow2')
labels_dom_var <- c("Soil moisture","Minimum soil moisture","Precipitation","Vapour Pressure Deficit","Air Temperature","Maximum daily air temperature","Incoming shortwave radiation","Net radiation",expression("adjusted R"^2*" < 0.36"))
labels_dom_var_cluster <- c('Water','Energy',expression("adjusted R"^2*" < 0.36"))
# cols_dom_var <- c(brewer.pal(11,"RdYlBu")[9], 'sienna', 'turquoise2', 'orange', brewer.pal(11,"RdYlBu")[3], 'red4', 'red','yellow', 'snow2')
cols_dom_var <- c(brewer.pal(11,"RdYlBu")[9], 'darkblue', 'darkturquoise', 'sienna', brewer.pal(11,"RdYlBu")[3], 'red4', 'mediumorchid','gold1', 'snow2')
cols_dom_var_cluster <- c('darkturquoise', brewer.pal(11,"RdYlBu")[3], 'snow2')
cols_dom_water_var <- c(cols_dom_var[1:length(which(var_type != 'energy'))],'snow2','grey40')
labels_dom_water_var <- c(labels_dom_var[1:length(which(var_type != 'energy'))],expression("adjusted R"^2*" < 0.36"),'No water variable')
# cols_dom_water_var <- c(cols_dom_var[1:length(which(var_type != 'energy'))],'snow2')
# labels_dom_water_var <- c(labels_dom_var[1:length(which(var_type != 'energy'))],expression("adjusted R"^2*" < 0.36"))
myvalues_dom_var <- seq(0.5,length(vars)+0.5,1)

dom_var_ts.df$cuts_dom_var_dredge_relimp_ng <- cut(dom_var_ts.df$dom_var_dredge_relimp_ng, myvalues_dom_var, include.lowest = T)
dom_var_ts.df$cuts_dom_var_dredge_relimp_ng_cluster <- cut(dom_var_ts.df$dom_var_dredge_relimp_ng_cluster, myvalues_dom_var[1:4], include.lowest = T)
dom_var_ts.df$cuts_dom_water_var_ng <- cut(dom_var_ts.df$dom_water_var_ng, myvalues_dom_var[1:(length(which(var_type != 'energy'))+2)], include.lowest = T)
dom_var_ts.df$cuts_rank_dom_water_var_ng <- cut(dom_var_ts.df$rank_dom_water_var_ng, myvalues_dom_var, include.lowest = T)

ties.df <- dom_var_ts.df[which(dom_var_ts.df$ties == 1),]

# This is a data set from the maptools package
data(wrld_simpl)

# Create a data.frame object for ggplot. ggplot requires a data frame.
mymap <- fortify(wrld_simpl)

a <- ggplot(dom_var_ts.df, aes(x=lon,y=lat,fill=cuts_dom_var_dredge_relimp_ng)) +
  geom_tile() +
  geom_point(inherit.aes = F, data = ties.df, aes(x=lon,y=lat), size = .1) +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  facet_wrap(~source_id) +
  # geom_segment(data = hotspot_regs.df, inherit.aes = F, aes(x = x, y = y, xend = xend, yend = yend), lty = 'dashed') +
  scale_fill_manual("Dominant variable",
                    values = cols_dom_var,
                    labels = labels_dom_var,
                    drop = F) +
  scale_x_continuous("longitude",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("latitude",
                     limits=c(-60,75),
                     expand=c(0,0)) +
  guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, nrow = 2)) +
  ggtitle("2081 - 2100") +
  theme(legend.position = "bottom",
        # theme(legend.position = c(.75,.15),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20, hjust = 0.5),
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
a

a_cluster <- ggplot(dom_var_ts.df, aes(x=lon,y=lat,fill=cuts_dom_var_dredge_relimp_ng_cluster)) +
  geom_tile() +
  # geom_point(inherit.aes = F, data = ties.df, aes(x=lon,y=lat), size = .1) +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  facet_wrap(~source_id) +
  # geom_segment(data = hotspot_regs.df, inherit.aes = F, aes(x = x, y = y, xend = xend, yend = yend), lty = 'dashed') +
  scale_fill_manual("Dominant variable",
                    values = cols_dom_var_cluster,
                    labels = labels_dom_var_cluster,
                    drop = F) +
  scale_x_continuous("longitude",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("latitude",
                     limits=c(-60,75),
                     expand=c(0,0)) +
  guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, nrow = 1)) +
  ggtitle("2081 - 2100") +
  theme(legend.position = "bottom",
        # theme(legend.position = c(.75,.15),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20, hjust = 0.5),
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
a_cluster

b <- ggplot(dom_var_ts.df, aes(x=lon,y=lat,fill=cuts_dom_water_var_ng)) +
  geom_tile() +
  # geom_point(inherit.aes = F, data = single_var.df, aes(x=lon,y=lat), size = .5) +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  facet_wrap(~source_id) +
  # geom_segment(data = hotspot_regs.df, inherit.aes = F, aes(x = x, y = y, xend = xend, yend = yend), lty = 'dashed') +
  scale_fill_manual("Highest ranking water variable",
                    values = cols_dom_water_var,
                    labels = labels_dom_water_var,
                    drop = F) +
  scale_x_continuous("longitude",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("latitude",
                     limits=c(-60,75),
                     expand=c(0,0)) +
  guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, nrow = 2)) +
  ggtitle("2081 - 2100") +
  theme(legend.position = "bottom",
        # theme(legend.position = c(0.75,0.15),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20, hjust = 0.5),
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
b

lev_count <- 1
lev_c <- c()
for(lev in levels(dom_var_ts.df$cuts_rank_dom_water_var_ng)){
  print(lev)
  if(lev %in% unique(dom_var_ts.df$cuts_rank_dom_water_var_ng)){
    print("bingo")
    lev_c <- c(lev_c, lev_count)
  }
  lev_count <- lev_count + 1
}

labels_ranks <- c()
for(i in 1:(length(vars)-1)){
  labels_ranks <- c(labels_ranks, paste0("Rank ", i))
}

c <- ggplot(dom_var_ts.df, aes(x=lon,y=lat,fill=cuts_rank_dom_water_var_ng)) +
  geom_tile() +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  facet_wrap(~source_id) +
  # scale_fill_manual("Rank dominant \n water variable",
  scale_fill_manual("Rank of highest ranking water variable",
                    # values = c(rev(brewer.pal(length(lev_c)-1, "RdYlBu")),'snow2'),
                    values = c(rev(brewer.pal(length(lev_c)+2, "RdYlBu"))[lev_c[1:(length(lev_c)-1)]],'snow2'),
                    labels = c(labels_ranks[lev_c][1:(length(lev_c)-1)],expression("adjusted R"^2*" < 0.36"))) +
  scale_x_continuous("",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("",
                     limits=c(-60,75),
                     expand=c(0,0)) +
  guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, nrow = 2)) +
  theme(legend.position = "bottom",
        # theme(legend.position = c(.75,.15),
        legend.text = element_text(size=12),
        legend.title = element_text(size=20, hjust = 0.5),
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
c

draft_plot <- plot_grid(a_cluster, c, ncol = 1)

# a <- ggplotGrob(a)
#
# gt1 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
# gt1 <- gtable_add_grob(gt1, a, t=1, b=32, l=1, r=32)
# gt1 <- gtable_add_grob(gt1, plot_bar_dom_var_dredge_relimp_ng_rsds_per_reg, t = 26, l = 3, b = 18, r = 8)
# gt1 <- gtable_add_grob(gt1, plot_bar_dom_var_dredge_relimp_ng, t = 16, l = 3, b = 10, r = 8)
# grid.draw(gt1)

# how much % actually has a good prediction?
# sum(area.array[which(!is.na(dom_var_dredge_relimp_ng[,,1]) & dom_var_dredge_relimp_ng[,,1] != 9)], na.rm = T) / total_land_area

ggsave("/Net/Groups/BGI/people/jdenis/scripts/scripts_Nature_Perspective/dom_water_var_AIC2_adjr036_2081_2100.png", plot = b, width = 9*1.5, height = 7*1.5, units = "in")
ggsave("/Net/Groups/BGI/people/jdenis/scripts/scripts_Nature_Perspective/dom_var_AIC2_adjr036_2081_2100.png", plot = a, width = 9*1.5, height = 7*1.5, units = "in")
ggsave("/Net/Groups/BGI/people/jdenis/scripts/scripts_Nature_Perspective/dom_var_rank_cluster_AIC2_adjr036_2081_2100.png", plot = draft_plot, width = 9*1.5, height = 14*1.5, units = "in")


# Multi-model means
mean_rank <-
  mean_dom_water_var <- array(NaN,c(180,90))
for(x in 1:180){
  for(y in 1:90){
    if(sum(!is.na(rank_dom_water_var_ng[x,y,])) > 5){
      mean_rank[x,y] <- mean(rank_dom_water_var_ng[x,y,], na.rm = T)
      count_dom_water_var <- c()
      for(v in 1:length(which(var_type != 'energy'))){ # loop over all water variables
        count_dom_water_var[v] <- length(which(dom_water_var_ng[x,y,,1] == v))
      }
      if(length(which(count_dom_water_var == max(count_dom_water_var, na.rm = T))) == 1){ # if one water variable is the dominant one across all models
        mean_dom_water_var[x,y] <- which(count_dom_water_var == max(count_dom_water_var, na.rm = T))
      }else{ # 5 if it's a tie
        mean_dom_water_var[x,y] <- 5
      }
    }else if(sum(!is.na(rank_dom_water_var_ng[x,y,])) > 0){ # 6 Not enough/insignificant models
      mean_dom_water_var[x,y] <- 6
    }
  }
}
# hist(c(mean_dom_water_var))
# unique(c(mean_dom_water_var))

# test how often tas and mrsol extremes have some importance as their average counterparts
tas_tie <-
  mrsol_tie <- 
  c()
for(h in 1:length(cmip6_data.df$source_id)){
  mrsol_tie <- c(mrsol_tie, (sum(area.array[which(dom_var_dredge_relimp_ng[,,h,1] == 1 & dom_var_dredge_relimp_ng[,,h,2] == 2)]) / sum(area.array[which(dom_var_dredge_relimp_ng[,,h,1] == 1 | dom_var_dredge_relimp_ng[,,h,1] == 2)])*100))
  tas_tie <- c(tas_tie, (sum(area.array[which(dom_var_dredge_relimp_ng[,,h,1] == 5 & dom_var_dredge_relimp_ng[,,h,2] == 6)]) / sum(area.array[which(dom_var_dredge_relimp_ng[,,h,1] == 5 | dom_var_dredge_relimp_ng[,,h,1] == 6)])*100))
}

mean_dom_var <- 
  array(NaN,c(180,90))
mean_dom_var_tie <- 
  array(NaN,c(180,90,3))
for(x in 1:180){
  for(y in 1:90){
    if(sum(!is.na(dom_var_dredge_relimp_ng[x,y,,1][which(dom_var_dredge_relimp_ng[x,y,,1] < 9)])) > 5){
      # if(sum(!is.na(dom_var_dredge_relimp_ng[x,y,,1])) > 5){
      count_dom_var <- c()
      for(v in 1:(length(vars) - 1)){ # loop over all water variables
        count_dom_var[v] <- length(which(dom_var_dredge_relimp_ng[x,y,,1] == v))
      }
      if(max(count_dom_var, na.rm = T) > 2){
        if(length(which(count_dom_var == max(count_dom_var, na.rm = T))) == 1){ # if one water variable is the dominant one across all models
          mean_dom_var[x,y] <- which(count_dom_var == max(count_dom_var, na.rm = T))
        }else{ # 9 if it's a tie
          mean_dom_var[x,y] <- 9
          if(length(which(count_dom_var == max(count_dom_var, na.rm = T))) == 2){ # 2way tie
            mean_dom_var_tie[x,y,c(1,2)] <- which(count_dom_var == max(count_dom_var, na.rm = T))
          }else{ #3way tie
            mean_dom_var_tie[x,y,] <- which(count_dom_var == max(count_dom_var, na.rm = T))
          }
        }
      }else{ # When there are 2 or less CMIP6 models agreeing on dominant variable
        mean_dom_var[x,y] <- 10
      }
    }else if(sum(!is.na(dom_var_dredge_relimp_ng[x,y,,1])) > 0){ # 10 Not enough/insignificant models
      mean_dom_var[x,y] <- 11
    }
  }
}

mean_dom_water_var_ts.df <- setNames(data.frame(matrix(ncol = 4, nrow = 0)),
                                     c("mean_rank","mean_dom_water_var","lon","lat"))
for(x in 1:180){
  for(y in 1:90){
    if(!is.na(mean_dom_water_var[x,y])){
      mean_dom_water_var_ts.df <- rbind(mean_dom_water_var_ts.df,
                                        data.frame("mean_rank" = mean_rank[x,y],
                                                   "mean_dom_water_var" = mean_dom_water_var[x,y],
                                                   "lon" = lon[x],
                                                   "lat" = lat[y]
                                        ))
    }
  }
}

vars_water_bar <- c("mrsol","min_mrsol","pr","vpd","tie")
area_mean_dom_water_var <- setNames(data.frame(matrix(ncol = 2, nrow = 0)),
                                    c("area","var"))
for(v in 1:5){ # loop over all water variables
  area_mean_dom_water_var <- rbind(area_mean_dom_water_var,
                                   data.frame("area" = (sum(area.array[which(mean_dom_water_var == v)], na.rm = T)/total_land_area)*100,
                                              "var" = vars_water_bar[v]))
}
area_mean_dom_water_var$var_fac <- factor(area_mean_dom_water_var$var, levels = vars_water_bar)

cols_mean_dom_water_var <- c(cols_dom_var[1:length(which(var_type != 'energy'))],'black','grey40')
labels_mean_dom_water_var <- c(labels_dom_var[1:length(which(var_type != 'energy'))],'Tie','Not enough/insignificant models')
myvalues_dom_var <- seq(0.5,length(vars)+0.5,1)
myvalues_rank_var <- seq(1,length(vars),.5)

mean_dom_water_var_ts.df$cuts_mean_dom_water_var <- cut(mean_dom_water_var_ts.df$mean_dom_water_var, myvalues_dom_var[1:(length(which(var_type != 'energy'))+3)], include.lowest = T)
mean_dom_water_var_ts.df$cuts_mean_rank <- cut(mean_dom_water_var_ts.df$mean_rank, myvalues_rank_var, include.lowest = T)

mean_dom_var_ts.df <- setNames(data.frame(matrix(ncol = 3, nrow = 0)),
                               c("mean_dom_var","lon","lat"))
for(x in 1:180){
  for(y in 1:90){
    if(!is.na(mean_dom_var[x,y])){
      mean_dom_var_ts.df <- rbind(mean_dom_var_ts.df,
                                  data.frame("mean_dom_var" = mean_dom_var[x,y],
                                             "lon" = lon[x],
                                             "lat" = lat[y]
                                  ))
    }
  }
}
# now cluster energy and water variables together in a new variable
mean_dom_var_ts.df$mean_dom_var_cluster <- NaN
mean_dom_var_ts.df$mean_dom_var_cluster[which(mean_dom_var_ts.df$mean_dom_var < 5)] <- 1
mean_dom_var_ts.df$mean_dom_var_cluster[which(mean_dom_var_ts.df$mean_dom_var > 4)] <- 2
mean_dom_var_ts.df$mean_dom_var_cluster[which(mean_dom_var_ts.df$mean_dom_var == 9)] <- 3
mean_dom_var_ts.df$mean_dom_var_cluster[which(mean_dom_var_ts.df$mean_dom_var == 10)] <- 4
mean_dom_var_ts.df$mean_dom_var_cluster[which(mean_dom_var_ts.df$mean_dom_var == 11)] <- 5

vars_bar <- c(vars[1:8])
vars_bar_cluster <- c('Water','Energy')
area_mean_dom_var <- setNames(data.frame(matrix(ncol = 2, nrow = 0)),
                              c("area","var"))
for(v in 1:(length(vars)-1)){ # loop over all variables
  area_mean_dom_var <- rbind(area_mean_dom_var,
                             data.frame("area" = (sum(area.array[which(mean_dom_var == v)], na.rm = T)/total_land_area)*100,
                                        "var" = vars_bar[v]))
}
area_mean_dom_var_cluster <- data.frame("area" = (sum(area.array[which(mean_dom_var < 5)], na.rm = T)/total_land_area)*100,
                                        "var" = "Water")
area_mean_dom_var_cluster <- rbind(area_mean_dom_var_cluster,
                                   data.frame("area" = (sum(area.array[which(mean_dom_var > 4 & mean_dom_var < 9)], na.rm = T)/total_land_area)*100,
                                              "var" = "Energy"))


# area_mean_dom_var_tie <- area_mean_dom_var
# x = 13; y = 78
for(x in 1:180){
  for(y in 1:90){
    if(!is.na(mean_dom_var[x,y])){
      if(mean_dom_var[x,y] == 9){
        # convert array into vector for one grid cell
        mean_dom_var_tie_c <- c(mean_dom_var_tie[x,y,][which(!is.na(mean_dom_var_tie[x,y,]))])
        mean_dom_var_tie_c_cluster <- c()
        # now cluster energy and water variables together in a new variable
        for(tie in mean_dom_var_tie_c){
          if(tie < 5){
            mean_dom_var_tie_c_cluster <- c(mean_dom_var_tie_c_cluster, 1)
          }else{
            mean_dom_var_tie_c_cluster <- c(mean_dom_var_tie_c_cluster, 2)
          }
        }
        
        # area_mean_dom_var_tie$area[mean_dom_var_tie_c] <- area_mean_dom_var_tie$area[mean_dom_var_tie_c] + ((area.array[x,y]/total_land_area)*100)/length(mean_dom_var_tie_c)
        area_mean_dom_var$area[mean_dom_var_tie_c] <- area_mean_dom_var$area[mean_dom_var_tie_c] + ((area.array[x,y]/total_land_area)*100)/length(mean_dom_var_tie_c)
        
        area_mean_dom_var_cluster$area[mean_dom_var_tie_c_cluster] <- area_mean_dom_var_cluster$area[mean_dom_var_tie_c_cluster] + ((area.array[x,y]/total_land_area)*100)/length(mean_dom_var_tie_c_cluster)
      }
    }
  }
}
area_mean_dom_var$var_fac <- factor(area_mean_dom_var$var, levels = vars_bar)
area_mean_dom_var_cluster$var_fac <- factor(area_mean_dom_var_cluster$var, levels = vars_bar_cluster)

# cols_mean_dom_var <- c(cols_dom_var[1:(length(vars) - 1)],'black','grey40',cols_dom_var[length(vars)])
cols_mean_dom_var <- c(cols_dom_var[1:(length(vars) - 1)],'black','grey40',cols_dom_var[length(vars)])
cols_mean_dom_var_cluster <- c(cols_dom_var[3],cols_dom_var[5],'black','grey40',cols_dom_var[length(vars)])
labels_mean_dom_var <- c(labels_dom_var[1:(length(vars)-1)], 'Tie', '< 3/9 Models agree', 'Not enough/insignificant models')
labels_mean_dom_var_cluster <- c('Water','Energy', 'Tie', '< 3/9 Models agree', 'Not enough/insignificant models')
myvalues_dom_var <- seq(0.5,length(vars)+2.5,1)

mean_dom_var_ts.df$cuts_mean_dom_var <- cut(mean_dom_var_ts.df$mean_dom_var, myvalues_dom_var, include.lowest = T)
mean_dom_var_ts.df$cuts_mean_dom_var_cluster <- cut(mean_dom_var_ts.df$mean_dom_var_cluster, myvalues_dom_var[1:6], include.lowest = T)

# bb <- ggplot(mean_dom_water_var_ts.df, aes(x=lon,y=lat,fill=cuts_mean_dom_water_var)) +
#   geom_tile() +
#   geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
#            inherit.aes = F, fill = NA, color = "black", size = .1) +
#   scale_fill_manual("Mean dominant water variable",
#                     values = cols_mean_dom_water_var,
#                     labels = labels_mean_dom_water_var,
#                     drop = F) +
#   scale_x_continuous("longitude",
#                      limits=c(-180,180),
#                      expand=c(0,0)) +
#   scale_y_continuous("latitude",
#                      limits=c(-60,75),
#                      expand=c(0,0)) +
#   guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, nrow = 2)) +
#   theme(legend.position = "bottom",
#         legend.text = element_text(size=12),
#         legend.title = element_text(size=20, hjust = 0.5),
#         strip.background = element_blank(),
#         axis.line.x = element_line(colour = "black"),
#         axis.line.y = element_line(colour = "black"),
#         strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_rect(color = 'black', fill = NA, size = 1),
#         panel.background = element_blank(),
#         axis.text = element_text(size=18),
#         axis.title = element_blank(),
#         plot.title = element_text(size=24, hjust = 0.5),
#         plot.tag.position = c(.55,0.03)
#   )
# bb
# bb<- ggplotGrob(bb)

bb <- ggplot(mean_dom_var_ts.df, aes(x=lon,y=lat,fill=cuts_mean_dom_var)) +
  geom_tile() +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  scale_fill_manual("Mean dominant variable",
                    values = cols_mean_dom_var,
                    labels = labels_mean_dom_var,
                    drop = F) +
  scale_x_continuous("longitude",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("latitude",
                     limits=c(-60,75),
                     expand=c(0,0)) +
  guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, nrow = 3)) +
  ggtitle("2081 - 2100") +
  theme(legend.position = "bottom",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20, hjust = 0.5),
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

bb_cluster <- ggplot(mean_dom_var_ts.df, aes(x=lon,y=lat,fill=cuts_mean_dom_var_cluster)) +
  geom_tile() +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  scale_fill_manual("Mean dominant variable",
                    values = cols_mean_dom_var_cluster,
                    labels = labels_mean_dom_var_cluster,
                    drop = F) +
  scale_x_continuous("longitude",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("latitude",
                     limits=c(-60,75),
                     expand=c(0,0)) +
  guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, nrow = 2)) +
  ggtitle("2081 - 2100") +
  theme(legend.position = "bottom",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20, hjust = 0.5),
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
bb_cluster
bb_cluster<- ggplotGrob(bb_cluster)

# bar_mean_dom_water_var <- ggplot(area_mean_dom_water_var, aes(x = var_fac, y = area, fill = var_fac)) +
#   geom_bar(stat='identity',col = 'black', width = 1, position = "stack") +
#   geom_text(inherit.aes = F, data = area_mean_dom_water_var, aes(x=var,y=area+2.5,label=round(area,2)), size = 2.5) +
#   scale_x_discrete("") +
#   scale_y_continuous(expression(paste("land area-%")), expand = c(0,0), limits = c(0, (max(area_mean_dom_water_var$area)) + 5), position = "right") +
#   scale_fill_manual(values = c("mrsol" = cols_mean_dom_water_var[1],
#                                "min_mrsol" = cols_mean_dom_water_var[2],
#                                "pr" = cols_mean_dom_water_var[3],
#                                "vpd" = cols_mean_dom_water_var[4],
#                                "tie" = cols_mean_dom_water_var[5]),
#                     drop = F) +
#   theme(legend.position = "none",
#         legend.text = element_text(size=12),
#         legend.title = element_text(size=20, hjust = 0.5),
#         strip.background = element_blank(),
#         axis.line.x = element_line(colour = "black"),
#         axis.line.y = element_line(colour = "black"),
#         strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.border = element_rect(color = 'black', fill = NA, size = 1),
#         panel.background = element_rect(fill = "transparent",colour = NA),
#         plot.background = element_rect(fill = "transparent",colour = NA),
#         axis.text = element_text(size=9),
#         axis.text.x = element_blank(),
#         axis.ticks.x=element_blank(),
#         axis.title = element_text(size=9),
#         plot.title = element_text(size=10)
#   )
# bar_mean_dom_water_var
# bar_mean_dom_water_var <- ggplotGrob(bar_mean_dom_water_var)

bar_mean_dom_var <- ggplot(area_mean_dom_var, aes(x = var_fac, y = area, fill = var_fac)) +
  geom_bar(stat='identity',col = 'black', width = 1, position = "stack") +
  geom_text(inherit.aes = F, data = subset(area_mean_dom_var, area<1), aes(x=var,y=area+1,label=round(area,1)), size = 2.5) +
  geom_text(inherit.aes = F, data = subset(area_mean_dom_var, area>1), aes(x=var,y=area+1,label=round(area,0)), size = 2.5) +
  scale_x_discrete("") +
  scale_y_continuous(expression(paste("land area-%")), expand = c(0,0), limits = c(0, 30), position = "right") +
  # scale_y_continuous(expression(paste("land area-%")), expand = c(0,0), limits = c(0, (max(area_mean_dom_var$area)) + 5), position = "right") +
  scale_fill_manual(values = c("mrsol" = cols_mean_dom_var[1],
                               "min_mrsol" = cols_mean_dom_var[2],
                               "pr" = cols_mean_dom_var[3],
                               "vpd" = cols_mean_dom_var[4],
                               "tas" = cols_mean_dom_var[5],
                               "max_tasmax" = cols_mean_dom_var[6],
                               "rsds" = cols_mean_dom_var[7],
                               "netrad" = cols_mean_dom_var[8]),
                    drop = F) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20, hjust = 0.5),
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
bar_mean_dom_var
bar_mean_dom_var <- ggplotGrob(bar_mean_dom_var)

gt1 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1 <- gtable_add_grob(gt1, bb, t=1, b=32, l=1, r=32)
gt1 <- gtable_add_grob(gt1, bar_mean_dom_var, t = 25, l = 3, b = 17, r = 10)
grid.draw(gt1)

bar_mean_dom_var_cluster <- ggplot(area_mean_dom_var_cluster, aes(x = var_fac, y = area, fill = var_fac)) +
  geom_bar(stat='identity',col = 'black', width = .5, position = "stack") +
  geom_text(inherit.aes = F, data = subset(area_mean_dom_var_cluster, area<1), aes(x=var,y=area+1,label=round(area,1)), size = 2.5) +
  geom_text(inherit.aes = F, data = subset(area_mean_dom_var_cluster, area>1), aes(x=var,y=area+1,label=round(area,0)), size = 2.5) +
  scale_x_discrete("") +
  scale_y_continuous(expression(paste("land area-%")), expand = c(0,0), limits = c(0, 30), position = "right") +
  # scale_y_continuous(expression(paste("land area-%")), expand = c(0,0), limits = c(0, (max(area_mean_dom_var$area)) + 5), position = "right") +
  scale_fill_manual(values = c("Water" = cols_mean_dom_var[3],
                               "Energy" = cols_mean_dom_var[5]),
                    drop = F) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20, hjust = 0.5),
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
bar_mean_dom_var_cluster
bar_mean_dom_var_cluster <- ggplotGrob(bar_mean_dom_var_cluster)

gt1_cluster <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1_cluster <- gtable_add_grob(gt1_cluster, bb_cluster, t=1, b=32, l=1, r=32)
gt1_cluster <- gtable_add_grob(gt1_cluster, bar_mean_dom_var_cluster, t = 25, l = 3, b = 17, r = 7)
grid.draw(gt1_cluster)

# total_area_mean_rank <- sum(area.array[which(!is.na(mean_rank))], na.rm = T)
area_mean_rank <- setNames(data.frame(matrix(ncol = 2, nrow = 0)),
                           c("area","rank"))
for(r in 1:6){ # loop over all water variables
  area_mean_rank <- rbind(area_mean_rank,
                          data.frame("area" = (sum(area.array[which(mean_rank >= myvalues_rank_var[c(1:7)][r] & mean_rank < myvalues_rank_var[c(1:7)][r+1])], na.rm = T)/total_land_area)*100,
                                     "rank" = paste0('Rank', r)))
}



# plot the discrete colorbar for median dcorr
dbar <- plot_discrete_cbar(breaks = c(myvalues_rank_var[c(1:7)]),
                           colors = c(rev(brewer.pal(10, "RdYlBu")))[1:6],
                           legend_title = expression("Mean rank of highest ranking water variable"),
                           spacing = "constant",
                           font_size = 6,
                           spacing_scaling = 4,
                           width = .2,
                           triangle_size = .175,
                           legend_direction = "horizontal")

cc <- ggplot(subset(mean_dom_water_var_ts.df, !is.na(mean_rank)), aes(x=lon,y=lat,fill=cuts_mean_rank)) +
  # cc <- ggplot(mean_dom_water_var_ts.df[which(!is.na(mean_dom_water_var_ts.df$mean_rank)),], aes(x=lon,y=lat,fill=cuts_mean_rank)) +
  geom_tile() +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  scale_fill_manual("Mean rank highest ranking water variable",
                    # values = c(rev(brewer.pal(6, "RdYlBu")))) +
                    values = c(rev(brewer.pal(10, "RdYlBu")))[1:6]) +
  scale_x_continuous("",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("",
                     limits=c(-60,75),
                     expand=c(0,0)) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20, hjust = 0.5),
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
cc
cc <- ggplotGrob(cc)

bar_mean_rank <- ggplot(area_mean_rank, aes(x = rank, y = area, fill = rank)) +
  geom_bar(stat='identity',col = 'black', width = 1, position = "stack") +
  geom_text(inherit.aes = F, data = subset(area_mean_rank, area<1), aes(x=rank,y=area+1,label=round(area,1)), size = 2.5) +
  geom_text(inherit.aes = F, data = subset(area_mean_rank, area>1), aes(x=rank,y=area+1,label=round(area,0)), size = 2.5) +
  scale_x_discrete("") +
  scale_y_continuous(expression(paste("land area-%")), expand = c(0,0), limits = c(0, 30), position = "right") +
  scale_fill_manual(values = c("Rank1" = c(rev(brewer.pal(10, "RdYlBu")))[1:6][1],
                               "Rank2" = c(rev(brewer.pal(10, "RdYlBu")))[1:6][2],
                               "Rank3" = c(rev(brewer.pal(10, "RdYlBu")))[1:6][3],
                               "Rank4" = c(rev(brewer.pal(10, "RdYlBu")))[1:6][4],
                               "Rank5" = c(rev(brewer.pal(10, "RdYlBu")))[1:6][5],
                               "Rank6" = c(rev(brewer.pal(10, "RdYlBu")))[1:6][6]),
                    drop = F) +
  theme(legend.position = "none",
        legend.text = element_text(size=12),
        legend.title = element_text(size=20, hjust = 0.5),
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
        # axis.text.x = element_text(angle = 0),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.title = element_text(size=9),
        plot.title = element_text(size=10)
  )
bar_mean_rank
bar_mean_rank <- ggplotGrob(bar_mean_rank)

gt2 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt2 <- gtable_add_grob(gt2, cc, t=1, b=32, l=1, r=32)
gt2 <- gtable_add_grob(gt2, bar_mean_rank, t = 30, l = 3, b = 19, r = 10)
grid.draw(gt2)

# reduce top and bottom margins
empty <- ggplot() + theme_void()
dbar <- dbar + theme(plot.margin = unit(c(-35, 10, -30, 10), "pt"))
dbar_smaller <- grid.arrange(empty, dbar, empty , ncol=3, widths = c(1,4,1))

plot <- grid.arrange(gt2,dbar_smaller, nrow = 2, heights = c(.8,.2))

# ddraft_plot <- grid.arrange(gt1, plot, ncol = 1)

ddraft_plot_cluster <- grid.arrange(gt1_cluster, plot, ncol = 1)

# ggsave("/Net/Groups/BGI/people/jdenis/scripts/scripts_Nature_Perspective/test_mean_dom_water_var_yr_filteryrLAI_2081_2100_AIC005.png", plot = ddraft_plot, width = 9, height = 14, units = "in")
# ggsave("/Net/Groups/BGI/people/jdenis/scripts/scripts_Nature_Perspective/mean_dom_var_rank_AIC2_adjr036_2081_2100.png", plot = gt, width = 9*1.5, height = 14*1.5, units = "in")

ggsave("/Net/Groups/BGI/people/jdenis/scripts/scripts_Nature_Perspective/mean_dom_var_AIC2_adjr036_2081_2100.png", plot = gt1, width = 9*1.5, height = 7*1.5, units = "in")

ggsave("/Net/Groups/BGI/people/jdenis/scripts/scripts_Nature_Perspective/mean_dom_var_cluster_rank_AIC2_adjr036_2081_2100.png", plot = ddraft_plot_cluster, width = 9*1.5, height = 14*1.5, units = "in")







