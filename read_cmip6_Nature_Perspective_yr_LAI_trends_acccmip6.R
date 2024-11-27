# By: Jasper Denissen
# 2023-06-25
# Script to read CMIP6 data
# 1981 - 2100, 2.0x2.0 grid cell resolution WITH THE TERRA PACKAGE
# 1) Read in all data and average to monthly timescale
# 2) detrend
# 3) anomalies
# 4) keep only anomalies when T > threshold (10 deg C)
# 5) compute ELI

# packages
library(fields)
library(ncdf4)
library(lubridate)
library(raster)
library(dplyr)
library(abind)
library(ppcor)
library(ncmeta)
library(doParallel)
library(foreach)
library(snow)
library(rgdal)
library(ppcor)
library(terra)

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

# load functions
source('/Net/Groups/BGI/people/jdenis/scripts/functions/parallel_raster_functions.R')

files <- list.files(path_cmip6_2.0x2.0,
                    pattern = "_2.0x2.0.nc$")
# make sure to fetch the right tas and tasmax: first half of grep files are tas and second half are tasmax
files <- list.files(path_cmip6_2.0x2.0,
                    pattern = "_2.0x2.0.nc$")

pre_files_tas <- files[grepl("tas_*",files)]
pre_files_tasmax <- files[grepl("tasmax_*",files)]
pre_files_mrsol <- files[grepl("mrsol_*",files)]
pre_files_hfls <- files[grepl("hfls_*",files)]
pre_files_rlus <- files[grepl("rlus_*",files)]
pre_files_rsus <- files[grepl("rsus_*",files)]
pre_files_rlds <- files[grepl("rlds_*",files)]
pre_files_rsds <- files[grepl("rsds_*",files)]
pre_files_lai <- files[grepl("lai_*",files)]
pre_files_hurs <- files[grepl("hurs_*",files)]
pre_files_pr <- files[grepl("pr_*",files)]
# DON'T FORGET TO REMOVE THE INDEX ON THE END IF ALL MODELS ARE TO BE PROCESSED!

# only keep all the files when the model is included in cmip6_data.df
files_tas <- 
  files_tasmax <- 
  files_mrsol <- 
  files_hfls <- 
  files_rlus <- 
  files_rsus <- 
  files_rlds <- 
  files_rsds <- 
  files_lai <- 
  files_hurs <- 
  files_pr <- c()
for(source in cmip6_data.df$source_id){
  files_tas <- c(files_tas, pre_files_tas[grepl(source,pre_files_tas)][1]) # the first one is tas!
  files_tasmax <- c(files_tasmax, pre_files_tasmax[grepl(source,pre_files_tasmax)])
  files_mrsol <- c(files_mrsol, pre_files_mrsol[grepl(source,pre_files_mrsol)])
  files_hfls <- c(files_hfls, pre_files_hfls[grepl(source,pre_files_hfls)])
  files_rlus <- c(files_rlus, pre_files_rlus[grepl(source,pre_files_rlus)])
  files_rsus <- c(files_rsus, pre_files_rsus[grepl(source,pre_files_rsus)])
  files_rlds <- c(files_rlds, pre_files_rlds[grepl(source,pre_files_rlds)])
  files_rsds <- c(files_rsds, pre_files_rsds[grepl(source,pre_files_rsds)])
  files_lai <- c(files_lai, pre_files_lai[grepl(source,pre_files_lai)])
  files_hurs <- c(files_hurs, pre_files_hurs[grepl(source,pre_files_hurs)])
  files_pr <- c(files_pr, pre_files_pr[grepl(source,pre_files_pr)])
}

month_year <- rep(1:120,each=12)
# Set mode 'all' for all years and mode 'aligned' for (1981-2001, 2002-2021, 2081-2100)
mode <- 'all'
print(paste0('mode == ', mode))
if(mode == 'all'){
  # month_dec <- rep(1:12,each=120)[1:1428] # for all years
  month_dec <- rep(1:12,each=120)[c(1:228,241:1440)] # for all years
}else if(mode == 'aligned'){
  month_dec <- rep(1:6, each=120)
}

av_lai.list <-
  av_lai_yr.list <-
  av_pr.list <-
  av_pr_yr.list <-
  av_vpd.list <-
  av_vpd_yr.list <-
  av_mrsol.list <-
  av_mrsol_yr.list <-
  max_tasmax.list <-
  max_tasmax_yr.list <-
  min_mrsol.list <-
  min_mrsol_yr.list <-
  av_netrad.list <-
  av_netrad_yr.list <-
  av_AI.list <-
  av_AI_yr.list <-
  av_rsds.list <-
  av_rsds_yr.list <-
  av_tas.list <-
  av_tas_yr.list <-
  list()
for(i in 1:length(cmip6_data.df$source_id)){
  stack_tas <- rast(paste0(path_cmip6_2.0x2.0,files_tas[i]))
  stack_mrsol <- rast(paste0(path_cmip6_2.0x2.0,files_mrsol[i]))
  stack_tasmax <- rast(paste0(path_cmip6_2.0x2.0,files_tasmax[i]))
  stack_rsus <- rast(paste0(path_cmip6_2.0x2.0,files_rsus[i]))/26.15741 # (conversion to mm/d: /26.15741)
  stack_rlus <- rast(paste0(path_cmip6_2.0x2.0,files_rlus[i]))/26.15741 # (conversion to mm/d: /26.15741)
  stack_rsds <- rast(paste0(path_cmip6_2.0x2.0,files_rsds[i]))/26.15741 # (conversion to mm/d: /26.15741)
  stack_rlds <- rast(paste0(path_cmip6_2.0x2.0,files_rlds[i]))/26.15741 # (conversion to mm/d: /26.15741)
  stack_lai <- rast(paste0(path_cmip6_2.0x2.0,files_lai[i]))
  stack_hurs <- rast(paste0(path_cmip6_2.0x2.0,files_hurs[i]))
  stack_pr <- rast(paste0(path_cmip6_2.0x2.0,files_pr[i]))*24*60*60 # (conversion to mm/d: *24*60*60)
  
  stack_netrad <- (stack_rsds - stack_rsus) + (stack_rlds - stack_rlus)
  stack_vpsat <- 610.7*10^((7.5*(stack_tas-273.15))/(237.3+(stack_tas-273.15)))/1000 
  stack_vpair <- (610.7*10^((7.5*(stack_tas-273.15))/(237.3+(stack_tas-273.15)))/1000)*stack_hurs/100
  stack_vpd <- stack_vpsat - stack_vpair
  
  if(mode == 'all'){
    av_lai_10yr <- tapp(stack_lai[[c(13:1440)]], index = month_dec, fun = mean, na.rm = T)
    av_pr_10yr <- tapp(stack_pr[[c(13:1440)]], index = month_dec, fun = mean, na.rm = T)
    av_vpd_10yr <- tapp(stack_vpd[[c(13:1440)]], index = month_dec, fun = mean, na.rm = T)
    av_mrsol_10yr <- tapp(stack_mrsol[[c(13:1440)]], index = month_dec, fun = mean, na.rm = T)
    max_tasmax_10yr <- tapp(stack_tasmax[[c(13:1440)]], index = month_dec, fun = max, na.rm = T)
    min_mrsol_10yr <- tapp(stack_mrsol[[c(13:1440)]], index = month_dec, fun = min, na.rm = T)
    av_netrad_10yr <- tapp(stack_netrad[[c(13:1440)]], index = month_dec, fun = mean, na.rm = T)
    av_rsds_10yr <- tapp(stack_rsds[[c(13:1440)]], index = month_dec, fun = mean, na.rm = T)
    av_tas_10yr <- tapp(stack_tas[[c(13:1440)]], index = month_dec, fun = mean, na.rm = T)
  }else if(mode == 'aligned'){
    av_lai_10yr <- tapp(stack_lai[[c(13:492,1201:1440)]], index = month_dec, fun = mean, na.rm = T)
    av_pr_10yr <- tapp(stack_pr[[c(13:492,1201:1440)]], index = month_dec, fun = mean, na.rm = T)
    av_vpd_10yr <- tapp(stack_vpd[[c(13:492,1201:1440)]], index = month_dec, fun = mean, na.rm = T)
    av_mrsol_10yr <- tapp(stack_mrsol[[c(13:492,1201:1440)]], index = month_dec, fun = mean, na.rm = T)
    max_tasmax_10yr <- tapp(stack_tasmax[[c(13:492,1201:1440)]], index = month_dec, fun = max, na.rm = T)
    min_mrsol_10yr <- tapp(stack_mrsol[[c(13:492,1201:1440)]], index = month_dec, fun = min, na.rm = T)
    av_netrad_10yr <- tapp(stack_netrad[[c(13:492,1201:1440)]], index = month_dec, fun = mean, na.rm = T)
    av_rsds_10yr <- tapp(stack_rsds[[c(13:492,1201:1440)]], index = month_dec, fun = mean, na.rm = T)
    av_tas_10yr <- tapp(stack_tas[[c(13:492,1201:1440)]], index = month_dec, fun = mean, na.rm = T)
  }
  av_AI_10yr <- av_netrad_10yr/av_pr_10yr
  
  av_lai_yr <- tapp(stack_lai, index = month_year, fun = mean, na.rm = T)  
  av_pr_yr <- tapp(stack_pr, index = month_year, fun = mean, na.rm = T)
  av_vpd_yr <- tapp(stack_vpd, index = month_year, fun = mean, na.rm = T)  
  av_mrsol_yr <- tapp(stack_mrsol, index = month_year, fun = mean, na.rm = T)
  max_tasmax_yr <- tapp(stack_tasmax, index = month_year, fun = max, na.rm = T)
  min_mrsol_yr <- tapp(stack_mrsol, index = month_year, fun = min, na.rm = T)
  av_netrad_yr <- tapp(stack_netrad, index = month_year, fun = mean, na.rm = T)  
  av_AI_yr <- av_netrad_yr/av_pr_yr
  av_rsds_yr <- tapp(stack_rsds, index = month_year, fun = mean, na.rm = T)  
  av_tas_yr <- tapp(stack_tas, index = month_year, fun = mean, na.rm = T)  
  
  av_lai.list[[i]] <- aperm(as.array(av_lai_10yr), c(2,1,3))[,90:1,]
  av_lai_yr.list[[i]] <- aperm(as.array(av_lai_yr), c(2,1,3))[,90:1,]
  av_pr.list[[i]] <- aperm(as.array(av_pr_10yr), c(2,1,3))[,90:1,]
  av_pr_yr.list[[i]] <- aperm(as.array(av_pr_yr), c(2,1,3))[,90:1,]
  av_vpd.list[[i]] <- aperm(as.array(av_vpd_10yr), c(2,1,3))[,90:1,]
  av_vpd_yr.list[[i]] <- aperm(as.array(av_vpd_yr), c(2,1,3))[,90:1,]
  av_mrsol.list[[i]] <- aperm(as.array(av_mrsol_10yr), c(2,1,3))[,90:1,]
  av_mrsol_yr.list[[i]] <- aperm(as.array(av_mrsol_yr), c(2,1,3))[,90:1,]
  max_tasmax.list[[i]] <- aperm(as.array(max_tasmax_10yr), c(2,1,3))[,90:1,]
  max_tasmax_yr.list[[i]] <- aperm(as.array(max_tasmax_yr), c(2,1,3))[,90:1,]
  min_mrsol.list[[i]] <- aperm(as.array(min_mrsol_10yr), c(2,1,3))[,90:1,]
  min_mrsol_yr.list[[i]] <- aperm(as.array(min_mrsol_yr), c(2,1,3))[,90:1,]
  av_netrad.list[[i]] <- aperm(as.array(av_netrad_10yr), c(2,1,3))[,90:1,]
  av_netrad_yr.list[[i]] <- aperm(as.array(av_netrad_yr), c(2,1,3))[,90:1,]
  av_AI.list[[i]] <- aperm(as.array(av_AI_10yr), c(2,1,3))[,90:1,]
  av_AI_yr.list[[i]] <- aperm(as.array(av_AI_yr), c(2,1,3))[,90:1,]
  av_rsds.list[[i]] <- aperm(as.array(av_rsds_10yr), c(2,1,3))[,90:1,]
  av_rsds_yr.list[[i]] <- aperm(as.array(av_rsds_yr), c(2,1,3))[,90:1,]
  av_tas.list[[i]] <- aperm(as.array(av_tas_10yr), c(2,1,3))[,90:1,]
  av_tas_yr.list[[i]] <- aperm(as.array(av_tas_yr), c(2,1,3))[,90:1,]
  
  print(paste("model ",i," is done...",sep=''))
  if(mode == 'all'){
    file_to_save <- "/Net/Groups/BGI/people/jdenis/scripts/Data/202306_cmip6_Nature_Perspective_yr_LAI_trends_acccmip6_all.RData"
  }else if(mode == 'aligned'){
    file_to_save <- "/Net/Groups/BGI/people/jdenis/scripts/Data/202306_cmip6_Nature_Perspective_yr_LAI_trends_acccmip6_aligned.RData"
  }
  save(
    av_lai.list,
    av_lai_yr.list,
    av_pr.list,
    av_pr_yr.list,
    av_vpd.list,
    av_vpd_yr.list,
    av_mrsol.list,
    av_mrsol_yr.list,
    max_tasmax.list,
    max_tasmax_yr.list,
    min_mrsol_yr.list,
    min_mrsol.list,
    av_netrad.list,
    av_netrad_yr.list,
    av_AI.list,
    av_AI_yr.list,
    av_rsds.list,
    av_rsds_yr.list,
    av_tas.list,
    av_tas_yr.list,
    
    file = file_to_save)
    
}
