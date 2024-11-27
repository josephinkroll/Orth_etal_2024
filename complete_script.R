#Nature water: Water limitation and global greening/browning trends
#Josephin Kroll
#started on 31.10.24

#Load packages
library(ncdf4)
library(fields)
library(lubridate)
library(raster)
library(RColorBrewer)
library(maps)
library(abind)
library(doParallel)
library(confintr)
library(dplyr)
library(tidyr)
library(raster)
library(fields)
library(ncdf4)
library(lubridate)
library(ggplot2)
library(maptools)
library(MuMIn)
library(relaimpo)
library(akima)
library(precrec)
library(purrr)
library(viridis)
library(gtable)
library(foreach)
library(gridExtra)
library(terra)
library(sf)

#Load .RData file
load("Rdata/nat_wat_pers/compl_scriptv08.RData")


############################
###### PRE PROCESSING ######

### Load data and compute monthly and annual averages

#LAI# 10daily data
LAI_80<-array(NaN, c(720, 360, 360)) #this array is also needed for filtering the vegetation data
count<-1
for(year in 1982:1991){
  ncin<-nc_open(paste("/mnt/share/data/lai/0d50_10daily/GEOV2-GCM_AVHRR.Lai/GEOV2-GCM_AVHRR.Lai.720_360.10daily.",year,".nc",sep=""))
  LAI_80[,,(count:(count+35))] <-ncvar_get(ncin,"Lai")  
  count<-count+36
  print(paste("Year", year, "is done", sep='')) #says which year is computed
}

lat <- ncvar_get(ncin, "lat")
lat <- lat[360:1]
lon <- ncvar_get(ncin, "lon")

LAI_80<-LAI_80[,360:1,]
plot(LAI_80[380,200,])
image.plot(LAI_80[,,25], main="daily LAI")

LAI_90<-array(NaN, c(720,360,360)) #this array is also needed for filtering the vegetation data
count<-1
for(year in 1992:2001){
  ncin<-nc_open(paste("/mnt/share/data/lai/0d50_10daily/GEOV2-GCM_AVHRR.Lai/GEOV2-GCM_AVHRR.Lai.720_360.10daily.",year,".nc",sep=""))
  LAI_90[,,(count:(count+35))] <-ncvar_get(ncin,"Lai")  
  count<-count+36
  print(paste("Year", year, "is done", sep='')) #says which year is computed
}
LAI_90<-LAI_90[,360:1,]
plot(LAI_90[380,200,], type="l")
image.plot(LAI_90[,,13], main="daily LAI")

LAI_00<-array(NaN, c(720,360,360)) #this array is also needed for filtering the vegetation data
count<-1
for(year in 2002:2011){
  ncin<-nc_open(paste("/mnt/share/data/lai/0d50_10daily/GEOV2-GCM_AVHRR.Lai/GEOV2-GCM_AVHRR.Lai.720_360.10daily.",year,".nc",sep=""))
  LAI_00[,,(count:(count+35))] <-ncvar_get(ncin,"Lai")  
  count<-count+36
  print(paste("Year", year, "is done", sep='')) #says which year is computed
}
LAI_00<-LAI_00[,360:1,]
plot(LAI_00[380,200,], type="l")
image.plot(LAI_00[,,13], main="daily LAI")

LAI_10 <- array(NaN, c(720, 360, 324)) #this array is also needed for filtering the vegetation data
count <- 1
for(year in 2012:2020){
  ncin<-nc_open(paste("/mnt/share/data/lai/0d50_10daily/GEOV2-GCM_AVHRR.Lai/GEOV2-GCM_AVHRR.Lai.720_360.10daily.", year, ".nc", sep=""))
  LAI_10[, , (count:(count+35))] <- ncvar_get(ncin, "Lai")  
  count <- count + 36
  print(paste("Year", year, "is done", sep = '')) #says which year is computed
}
LAI_10<-LAI_10[, 360:1, ]
plot(LAI_10[380, 200,], type = "l")
image.plot(LAI_10[, , 13], main = "daily LAI")

LAI_am_80 <- array(NaN, c(720, 360, 10))
count <- 1
year <- 1
for(x in 1:720){
  for(y in 1:360){
    if(sum(is.na(LAI_80[x ,y, ])) < 36){
      repeat{
        LAI_am_80[x,y,year] <- mean(LAI_80[x,y, count:(count+35)], na.rm=T)
        count <- count + 36
        year <- year+1
        if(year >10){
          break
        }
      }
      count <- 1
      year <- 1
    }
  }
}

image.plot(LAI_am_80[,,10])
rm(LAI_80)

#Creating the study area based on mean decadal LAI > 0.5 in all decades
r <- raster() # by default 1 by 1 degree 
res(r) <- 0.5 #change the resolution  to 0.5 degree
a <- raster::area(r) # calculate the area of a 0.5x0.5 degree grid from N - S, as area varies only by latitude, not longitude 
area <- a[,1] 
area.array <- array(NaN,c(720,360)) 
for(x in 1:720){ 
  area.array[x,] <- area # this is the array that carries the area per grid cell 
}

study_mask <- array(NaN, c(720, 360)) 
for(x in 1:720){ 
  for(y in 1:360){ 
    if(!is.na(mLAI80[x, y]) & !is.na(mLAI90[x, y]) & !is.na(mLAI00[x, y]) & !is.na(mLAI10[x, y])  & mLAI80[x, y] > 0.5 & mLAI90[x, y] > 0.5 & mLAI00[x, y] > 0.5 & mLAI10[x, y] > 0.5){ #only consider vegetated areas --> mLAI in all decades > 0.5
      study_mask[x, y] <- 1 
    } 
  } 
  print(x)
}


LAI_am_90 <- array(NaN, c(720,360,10))
count <- 1
year <- 1
for(x in 1:720){
  for(y in 1:360){
    if(sum(is.na(LAI_90[x,y,])) < 36){
      repeat{
        LAI_am_90[x,y,year] <- mean(LAI_90[x,y, count:(count+35)], na.rm=T)
        count <- count + 36
        year <- year+1
        if(year >10){
          break
        }
      }
      count <- 1
      year <- 1
    }
  }
}
image.plot(LAI_am_90[,,10])
rm(LAI_90)

LAI_am_00 <- array(NaN, c(720,360,10))
count <- 1
year <- 1
for(x in 1:720){
  for(y in 1:360){
    if(sum(is.na(LAI_00[x,y,])) < 36){
      repeat{
        LAI_am_00[x,y,year] <- mean(LAI_00[x,y, count:(count+35)], na.rm=T)
        count <- count + 36
        year <- year+1
        if(year >10){
          break
        }
      }
      count <- 1
      year <- 1
    }
  }
}
image.plot(LAI_am_00[,,10])
rm(LAI_00)

LAI_am_10 <- array(NaN, c(720,360,9))
count <- 1
year <- 1
for(x in 1:720){
  for(y in 1:360){
    if(sum(is.na(LAI_10[x,y,])) < 32){
      repeat{
        LAI_am_10[x,y,year] <- mean(LAI_10[x,y, count:(count+35)], na.rm=T)
        count <- count + 36
        year <- year+1
        if(year >9){
          break
        }
      }
      count <- 1
      year <- 1
    }
  }
}
image.plot(LAI_am_10[,,9])
rm(LAI_10)

mLAI80 <- array(NaN, c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(sum(!is.na(LAI_am_80[x,y,])) == 10){
      mLAI80[x,y] <- mean(LAI_am_80[x,y,], na.rm=T)
    }
  }
}

image.plot(mLAI80[,])

mLAI90 <- array(NaN, c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(sum(!is.na(LAI_am_90[x,y,])) == 10){
      mLAI90[x,y] <- mean(LAI_am_90[x,y,], na.rm=T)
    }
  }
}


mLAI00 <- array(NaN, c(720,360)) 
for(x in 1:720){
  for(y in 1:360){
    if(sum(!is.na(LAI_am_00[x,y,])) == 10){
      mLAI00[x,y] <- mean(LAI_am_00[x,y,], na.rm=T)
    }
  }
}


mLAI10 <-array(NaN, c(720,360)) 
for(x in 1:720){
  for(y in 1:360){
    if(sum(!is.na(LAI_am_10[x,y,])) == 9){
      mLAI10[x,y] <- mean(LAI_am_10[x,y,], na.rm=T)
    }
  }
}


#Convert daily data to annual data
#create time series
dates_80 <- seq( from=as.Date("1982/01/01"),
                 to = as.Date("1991/12/31"), 
                 by= "1 day")

y_80 <- substr(dates_80, 1,4)
y_80_idx <- unique(y_80)

dates_90 <- seq( from=as.Date("1992/01/01"),
                 to = as.Date("2001/12/31"), 
                 by= "1 day")
y_90 <- substr(dates_90, 1,4)
y_90_idx <- unique(y_90)

dates_00 <- seq( from=as.Date("2002/01/01"),
                 to = as.Date("2011/12/31"), 
                 by= "1 day")
y_00 <- substr(dates_00, 1,4)
y_00_idx <- unique(y_00)

dates_10 <- seq( from=as.Date("2012/01/01"),
                 to = as.Date("2020/12/31"), 
                 by= "1 day")

y_10 <- substr(dates_10, 1,4)
y_10_idx <- unique(y_10)

#extract months from the time series created before --> for monthly averaging of rGLEAM (needed in regression analysis)
months_80 <- paste(substr(dates_80, 3, 4), substr(dates_80, 6, 7), sep="")
months_80_idx <- unique(months_80)

months_90 <- paste(substr(dates_90, 3, 4), substr(dates_90, 6, 7), sep="")
months_90_idx <- unique(months_90)

months_00 <- paste(substr(dates_00, 3, 4), substr(dates_00, 6, 7), sep="")
months_00_idx <- unique(months_00)

months_10 <- paste(substr(dates_10, 3, 4), substr(dates_10, 6, 7), sep="")
months_10_idx <- unique(months_10)


#MSWEP#
MSWEP_tp_80<-array(NaN, c(720,360,length(y_80))) #this array is also needed for filtering the vegetation data
count<-1
for(year in 1982:1991){
  ncin<-nc_open(paste("/mnt/share/data/pr/0d50_daily/MSWEP/pr.MSWEP.720_360.",year,".daily.nc",sep=""))
  time <- ncvar_get(ncin, "time")
  if(length(time)==365){
    MSWEP_tp_80[,,(count:(count+364))] <-ncvar_get(ncin,"precipitation")  
    count<-count+365
  }else if(length(time)==366){
    MSWEP_tp_80[,,(count:(count+365))] <-ncvar_get(ncin,"precipitation")  
    count<-count+366
  }
  print(paste("Year", year, "is done", sep='')) 
}

MSWEP_tp_80<-MSWEP_tp_80[,360:1,]

y_MSWEPtp_80 <- array(NaN, c(720,360,length(y_80_idx)))
for(x in 1:720){
  for(y in 1:360){
    if(mLAI80[x,y] > 0.5 & !is.na(mLAI80[x,y])){
      for(t in 1:length(y_80_idx)){
        y_MSWEPtp_80[x,y,t] <- mean(MSWEP_tp_80[x,y,which(y_80_idx[t] == y_80)], na.rm=T)
      }
    }
  }
  print(x)
}

rm(MSWEP_tp_80)


MSWEP_tp_90<-array(NaN, c(720,360,length(y_90))) #this array is also needed for filtering the vegetation data
count<-1
for(year in 1992:2001){
  ncin<-nc_open(paste("/mnt/share/data/pr/0d50_daily/MSWEP/pr.MSWEP.720_360.",year,".daily.nc",sep=""))
  time <- ncvar_get(ncin, "time")
  if(length(time)==365){
    MSWEP_tp_90[,,(count:(count+364))] <-ncvar_get(ncin,"precipitation")  
    count<-count+365
  }else if(length(time)==366){
    MSWEP_tp_90[,,(count:(count+365))] <-ncvar_get(ncin,"precipitation")  
    count<-count+366
  }
  print(paste("Year", year, "is done", sep='')) 
}

MSWEP_tp_90<-MSWEP_tp_90[,360:1,]

y_MSWEPtp_90 <- array(NaN, c(720,360,length(y_90_idx)))
for(x in 1:720){
  for(y in 1:360){
    if(mLAI90[x,y] > 0.5 & !is.na(mLAI90[x,y])){
      for(t in 1:length(y_90_idx)){
        y_MSWEPtp_90[x,y,t] <- mean(MSWEP_tp_90[x,y,which(y_90_idx[t] == y_90)], na.rm=T)
      }
    }
  }
  print(x)
}

rm(MSWEP_tp_90)

MSWEP_tp_00<-array(NaN, c(720,360,length(y_00))) #this array is also needed for filtering the vegetation data
count<-1
for(year in 2002:2011){
  ncin<-nc_open(paste("/mnt/share/data/pr/0d50_daily/MSWEP/pr.MSWEP.720_360.",year,".daily.nc",sep=""))
  time <- ncvar_get(ncin, "time")
  if(length(time)==365){
    MSWEP_tp_00[,,(count:(count+364))] <-ncvar_get(ncin,"precipitation")  
    count<-count+365
  }else if(length(time)==366){
    MSWEP_tp_00[,,(count:(count+365))] <-ncvar_get(ncin,"precipitation")  
    count<-count+366
  }
  print(paste("Year", year, "is done", sep='')) 
}

MSWEP_tp_00<-MSWEP_tp_00[,360:1,]

y_MSWEPtp_00 <- array(NaN, c(720,360,length(y_00_idx)))
for(x in 1:720){
  for(y in 1:360){
    if(mLAI00[x,y] > 0.5 & !is.na(mLAI00[x,y])){
      for(t in 1:length(y_00_idx)){
        y_MSWEPtp_00[x,y,t] <- mean(MSWEP_tp_00[x,y,which(y_00_idx[t] == y_00)], na.rm=T)
      }
    }
  }
  print(x)
}

rm(MSWEP_tp_00)

MSWEP_tp_10<-array(NaN, c(720,360,length(y_10))) #this array is also needed for filtering the vegetation data
count<-1
for(year in 2012:2020){
  ncin<-nc_open(paste("/mnt/share/data/pr/0d50_daily/MSWEP/pr.MSWEP.720_360.",year,".daily.nc",sep=""))
  time <- ncvar_get(ncin, "time")
  if(length(time)==365){
    MSWEP_tp_10[,,(count:(count+364))] <-ncvar_get(ncin,"precipitation")  
    count<-count+365
  }else if(length(time)==366){
    MSWEP_tp_10[,,(count:(count+365))] <-ncvar_get(ncin,"precipitation")  
    count<-count+366
  }
  print(paste("Year", year, "is done", sep='')) 
}

MSWEP_tp_10<-MSWEP_tp_10[,360:1,]

y_MSWEPtp_10 <- array(NaN, c(720,360,length(y_10_idx)))
for(x in 1:720){
  for(y in 1:360){
    if(mLAI10[x,y] > 0.5 & !is.na(mLAI10[x,y])){
      for(t in 1:length(y_10_idx)){
        y_MSWEPtp_10[x,y,t] <- mean(MSWEP_tp_10[x,y,which(y_10_idx[t] == y_10)], na.rm=T)
      }
    }
  }
  print(x)
}

rm(MSWEP_tp_10)

mMSWEP80 <- array(NaN, c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(sum(!is.na(y_MSWEPtp_80[x,y,])) == 10){
      mMSWEP80[x,y] <- mean(y_MSWEPtp_80[x,y,], na.rm=T)
    }
  }
}

image.plot(mMSWEP10[,])

mMSWEP90 <- array(NaN, c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(sum(!is.na(y_MSWEPtp_90[x,y,])) == 10){
      mMSWEP90[x,y] <- mean(y_MSWEPtp_90[x,y,], na.rm=T)
    }
  }
}


mMSWEP00 <- array(NaN, c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(sum(!is.na(y_MSWEPtp_00[x,y,])) == 10){
      mMSWEP00[x,y] <- mean(y_MSWEPtp_00[x,y,], na.rm=T)
    }
  }
}


mMSWEP10 <-array(NaN, c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(sum(!is.na(y_MSWEPtp_10[x,y,])) == 9){
      mMSWEP10[x,y] <- mean(y_MSWEPtp_10[x,y,], na.rm=T)
    }
  }
}


#Gleam root zone 
rGLEAM_80<-array(NaN, c(720,360,length(y_80))) #this array is also needed for filtering the vegetation data
count <- 1
for(year in 1982:1991){
  ncin <- nc_open(paste("/mnt/share/data/sm/0d50_daily/Gleam4.1_SMroot/SMrz_", year, "_GLEAM_v4.1a_720_360.nc", sep=""))
  time <- ncvar_get(ncin, "time")
  if(length(time)==365){
    rGLEAM_80[,,(count:(count+364))] <-ncvar_get(ncin,"SMrz")  
    count<-count+365
  }else if(length(time)==366){
    rGLEAM_80[,,(count:(count+365))] <-ncvar_get(ncin,"SMrz")  
    count<-count+366
  }
  print(paste("Year", year, "is done", sep='')) 
}

rGLEAM_80<-rGLEAM_80[,360:1,]

y_rGLEAM_80 <- array(NaN, c(720,360,length(y_80_idx))) #calculate yearly GLEAM averages
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(mLAI80[x,y]) & mLAI80[x,y] > 0.5){
      for(t in 1:length(y_80_idx)){
        y_rGLEAM_80[x,y,t] <- mean(rGLEAM_80[x,y,which(y_80_idx[t] == y_80)], na.rm=T)
      }
    }
  }
  print(x)
}

total_land_mask <- array(NaN, c(720,360)) #calculate land mask based on availability of gleam SM
for(x in 1:720){
  for(y in 1:360){
    if(sum(!is.na(rGLEAM_80[x,y,])) != 0){
      total_land_mask[x,y] <- 1
    }
  }
}

png("Figures/nat_wat_pers/figures_v08/land_mask_gleam.png", width=7.5, height=4, units="in", res=1400)
par(mar=c(2,2,1,1))
image.plot(lon, lat, total_land_mask[,])
maps::map("world", add = T, interior = F)
dev.off()

total_land_area <- sum(total_land_mask*area.array, na.rm=T) #142'282'090

m_rGLEAM_80 <- array(NaN, c(720,360,length(months_80_idx))) #calculate monthly averages from daily gleam data
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      for(t in 1:length(months_80_idx)){
        m_rGLEAM_80[x,y,t] <- mean(rGLEAM_80[x,y,which(months_80_idx[t] == months_80)], na.rm=T)
      }
    }
  }
  print(x)
}

rm(rGLEAM_80)

y_minrGLEAM_80 <- array(NaN, c(720, 360,10))
year <- 1
count <- 1
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y]) & sum(!is.na(m_rGLEAM_80[x,y,])) != 0){
      repeat{
        y_minrGLEAM_80[x,y,year] <- min(m_rGLEAM_80[x,y, count:(count+11)], na.rm=T)
        count <- count + 12
        year <- year+1
        if(year > 10){
          break
        }
      }
      count <- 1
      year <- 1
    }
  }
}

rm(m_rGLEAM_80)

rGLEAM_90<-array(NaN, c(720,360,length(y_90))) #this array is also needed for filtering the vegetation data
count <- 1
for(year in 1992:2001){
  ncin <- nc_open(paste("/mnt/share/data/sm/0d50_daily/Gleam4.1_SMroot/SMrz_", year, "_GLEAM_v4.1a_720_360.nc", sep=""))
  time <- ncvar_get(ncin, "time")
  if(length(time)==365){
    rGLEAM_90[,,(count:(count+364))] <-ncvar_get(ncin,"SMrz")  
    count<-count+365
  }else if(length(time)==366){
    rGLEAM_90[,,(count:(count+365))] <-ncvar_get(ncin,"SMrz")  
    count<-count+366
  }
  print(paste("Year", year, "is done", sep='')) 
}

rGLEAM_90<-rGLEAM_90[,360:1,]

y_rGLEAM_90 <- array(NaN, c(720,360,length(y_90_idx)))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(mLAI90[x,y]) & mLAI90[x,y] > 0.5){
      for(t in 1:length(y_90_idx)){
        y_rGLEAM_90[x,y,t] <- mean(rGLEAM_90[x,y,which(y_90_idx[t] == y_90)], na.rm=T)
      }
    }
  }
  print(x)
}

m_rGLEAM_90 <- array(NaN, c(720,360,length(months_90_idx)))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      for(t in 1:length(months_90_idx)){
        m_rGLEAM_90[x,y,t] <- mean(rGLEAM_90[x,y,which(months_90_idx[t] == months_90)], na.rm=T)
      }
    }
  }
}


rm(rGLEAM_90)

y_minrGLEAM_90 <- array(NaN, c(720, 360,10))
year <- 1
count <- 1
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y]) & sum(!is.na(m_rGLEAM_90[x,y,])) != 0){
      repeat{
        y_minrGLEAM_90[x,y,year] <- min(m_rGLEAM_90[x,y, count:(count+11)], na.rm=T)
        count <- count + 12
        year <- year+1
        if(year > 10){
          break
        }
      }
      count <- 1
      year <- 1
    }
  }
}

rm(m_rGLEAM_90)

rGLEAM_00<-array(NaN, c(720,360,length(y_00))) #this array is also needed for filtering the vegetation data
count<-1
for(year in 2002:2011){
  ncin <- nc_open(paste("/mnt/share/data/sm/0d50_daily/Gleam4.1_SMroot/SMrz_", year, "_GLEAM_v4.1a_720_360.nc", sep=""))
  time <- ncvar_get(ncin, "time")
  if(length(time)==365){
    rGLEAM_00[,,(count:(count+364))] <-ncvar_get(ncin,"SMrz")  
    count<-count+365
  }else if(length(time)==366){
    rGLEAM_00[,,(count:(count+365))] <-ncvar_get(ncin,"SMrz")  
    count<-count+366
  }
  print(paste("Year", year, "is done", sep='')) 
}

rGLEAM_00<-rGLEAM_00[,360:1,]

y_rGLEAM_00 <- array(NaN, c(720,360,length(y_00_idx)))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(mLAI00[x,y]) & mLAI00[x,y] > 0.5){
      for(t in 1:length(y_00_idx)){
        y_rGLEAM_00[x,y,t] <- mean(rGLEAM_00[x,y,which(y_00_idx[t] == y_00)], na.rm=T)
      }
    }
  }
  print(x)
}

m_rGLEAM_00 <- array(NaN, c(720,360,length(months_00_idx)))
for(x in 1:720){
  for(y in 60:330){
    if(!is.na(study_mask[x,y])){
      for(t in 1:length(months_00_idx)){
        m_rGLEAM_00[x,y,t] <- mean(rGLEAM_00[x,y,which(months_00_idx[t] == months_00)], na.rm=T)
      }
    }
  }
}


rm(rGLEAM_00)

y_minrGLEAM_00 <- array(NaN, c(720, 360,10))
year <- 1
count <- 1
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y]) & sum(!is.na(m_rGLEAM_00[x,y,])) != 0){
      repeat{
        y_minrGLEAM_00[x,y,year] <- min(m_rGLEAM_00[x,y, count:(count+11)], na.rm=T)
        count <- count + 12
        year <- year+1
        if(year > 10){
          break
        }
      }
      count <- 1
      year <- 1
    }
  }
}

rm(m_rGLEAM_00)


rGLEAM_10<-array(NaN, c(720,360,length(y_10))) #this array is also needed for filtering the vegetation data
count<-1
for(year in 2012:2020){
  ncin <- nc_open(paste("/mnt/share/data/sm/0d50_daily/Gleam4.1_SMroot/SMrz_", year, "_GLEAM_v4.1a_720_360.nc", sep=""))
  time <- ncvar_get(ncin, "time")
  if(length(time)==365){
    rGLEAM_10[,,(count:(count+364))] <-ncvar_get(ncin,"SMrz")  
    count<-count+365
  }else if(length(time)==366){
    rGLEAM_10[,,(count:(count+365))] <-ncvar_get(ncin,"SMrz")  
    count<-count+366
  }
  print(paste("Year", year, "is done", sep='')) 
}

rGLEAM_10<-rGLEAM_10[,360:1,]

y_rGLEAM_10 <- array(NaN, c(720,360,length(y_10_idx)))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(mLAI10[x,y]) & mLAI10[x,y] > 0.5){
      for(t in 1:length(y_10_idx)){
        y_rGLEAM_10[x,y,t] <- mean(rGLEAM_10[x,y,which(y_10_idx[t] == y_10)], na.rm=T)
      }
    }
  }
  print(x)
}

m_rGLEAM_10 <- array(NaN, c(720,360,length(months_10_idx)))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      for(t in 1:length(months_10_idx)){
        m_rGLEAM_10[x,y,t] <- mean(rGLEAM_10[x,y,which(months_10_idx[t] == months_10)], na.rm=T)
      }
    }
  }
}

rm(rGLEAM_10)

y_minrGLEAM_10 <- array(NaN, c(720, 360,9))
year <- 1
count <- 1
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y]) & sum(!is.na(m_rGLEAM_10[x,y,])) != 0){
      repeat{
        y_minrGLEAM_10[x,y,year] <- min(m_rGLEAM_10[x,y, count:(count+11)], na.rm=T)
        count <- count + 12
        year <- year+1
        if(year > 9){
          break
        }
      }
      count <- 1
      year <- 1
    }
  }
}

rm(m_rGLEAM_10)

#calculate decadal mean rGLEAM
mrGLEAM80 <- array(NaN, c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(sum(!is.na(unlist(y_rGLEAM_80[x,y,]))) == 10){
      mrGLEAM80[x,y] <- mean(y_rGLEAM_80[x,y,], na.rm=T)
    }
  }
}

image.plot(mrGLEAM80[,])

mrGLEAM90 <- array(NaN, c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(sum(!is.na(unlist(y_rGLEAM_90[x,y,]))) == 10){
      mrGLEAM90[x,y] <- mean(y_rGLEAM_90[x,y,], na.rm=T)
    }
  }
}


mrGLEAM00 <- array(NaN, c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(sum(!is.na(unlist(y_rGLEAM_00[x,y,]))) == 10){
      mrGLEAM00[x,y] <- mean(unlist(y_rGLEAM_00[x,y,]), na.rm=T)
    }
  }
}


mrGLEAM10 <- array(NaN, c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(sum(!is.na(unlist(y_rGLEAM_10[x,y,]))) == 9){
      mrGLEAM10[x,y] <- mean(unlist(y_rGLEAM_10[x,y,]), na.rm=T)
    }
  }
}



#VPD#
VPD_80<-array(NaN, c(720,360,length(y_80))) #this array is also needed for filtering the vegetation data
count <- 1
for(year in 1982:1991){
  ncin <- nc_open(paste("/mnt/share/data/vpd/0d50_daily/ERA5/vpd.daily.era5.720_360.", year, ".nc", sep=""))
  time <- ncvar_get(ncin, "time")
  if(length(time)==365){
    VPD_80[,,(count:(count+364))] <-ncvar_get(ncin,"vpd")  
    count<-count+365
  }else if(length(time)==366){
    VPD_80[,,(count:(count+365))] <-ncvar_get(ncin,"vpd")  
    count<-count+366
  }
  print(paste("Year", year, "is done", sep='')) 
}

VPD_80 <- VPD_80[,360:1,]
plot(VPD_80[380,200,], type="l")
image.plot(VPD_80[,,11], main="daily VPD")

y_ERA5vpd_80 <- array(NaN, c(720,360,length(y_80_idx)))
for(x in 1:720){
  for(y in 1:360){
    if(mLAI80[x,y] > 0.5 & !is.na(mLAI80[x,y])){
      for(t in 1:length(y_80_idx)){
        y_ERA5vpd_80[x,y,t] <- mean(VPD_80[x,y,which(y_80_idx[t] == y_80)], na.rm=T)
      }
    }
  }
  print(x)
}

plot(y_ERA5vpd_80[382,282,], type="l")
rm(VPD_80)

VPD_90<-array(NaN, c(720,360,length(y_90))) #this array is also needed for filtering the vegetation data
count <- 1
for(year in 1992:2001){
  ncin <- nc_open(paste("/mnt/share/data/vpd/0d50_daily/ERA5/vpd.daily.era5.720_360.", year, ".nc", sep=""))
  time <- ncvar_get(ncin, "time")
  if(length(time)==365){
    VPD_90[,,(count:(count+364))] <-ncvar_get(ncin,"vpd")  
    count<-count+365
  }else if(length(time)==366){
    VPD_90[,,(count:(count+365))] <-ncvar_get(ncin,"vpd")  
    count<-count+366
  }
  print(paste("Year", year, "is done", sep='')) 
}

VPD_90 <- VPD_90[,360:1,]

y_ERA5vpd_90 <- array(NaN, c(720,360,length(y_90_idx)))
for(x in 1:720){
  for(y in 1:360){
    if(mLAI90[x,y] > 0.5 & !is.na(mLAI90[x,y])){
      for(t in 1:length(y_90_idx)){
        y_ERA5vpd_90[x,y,t] <- mean(VPD_90[x,y,which(y_90_idx[t] == y_90)], na.rm=T)
      }
    }
  }
  print(x)
}

rm(VPD_90)

VPD_00<-array(NaN, c(720,360,length(y_00))) #this array is also needed for filtering the vegetation data
count <- 1
for(year in 2002:2011){
  ncin <- nc_open(paste("/mnt/share/data/vpd/0d50_daily/ERA5/vpd.daily.era5.720_360.", year, ".nc", sep=""))
  time <- ncvar_get(ncin, "time")
  if(length(time)==365){
    VPD_00[,,(count:(count+364))] <-ncvar_get(ncin,"vpd")  
    count<-count+365
  }else if(length(time)==366){
    VPD_00[,,(count:(count+365))] <-ncvar_get(ncin,"vpd")  
    count<-count+366
  }
  print(paste("Year", year, "is done", sep='')) 
}

VPD_00 <- VPD_00[,360:1,]

y_ERA5vpd_00 <- array(NaN, c(720,360,length(y_00_idx)))
for(x in 1:720){
  for(y in 1:360){
    if(mLAI00[x,y] > 0.5 & !is.na(mLAI00[x,y])){
      for(t in 1:length(y_00_idx)){
        y_ERA5vpd_00[x,y,t] <- mean(VPD_00[x,y,which(y_00_idx[t] == y_00)], na.rm=T)
      }
    }
  }
  print(x)
}

rm(VPD_00)

VPD_10<-array(NaN, c(720,360,length(y_10))) #this array is also needed for filtering the vegetation data
count <- 1
for(year in 2012:2020){
  ncin <- nc_open(paste("/mnt/share/data/vpd/0d50_daily/ERA5/vpd.daily.era5.720_360.", year, ".nc", sep=""))
  time <- ncvar_get(ncin, "time")
  if(length(time)==365){
    VPD_10[,,(count:(count+364))] <-ncvar_get(ncin,"vpd")  
    count<-count+365
  }else if(length(time)==366){
    VPD_10[,,(count:(count+365))] <-ncvar_get(ncin,"vpd")  
    count<-count+366
  }
  print(paste("Year", year, "is done", sep='')) 
}
 
VPD_10 <- VPD_10[,360:1,]

y_ERA5vpd_10 <- array(NaN, c(720,360,length(y_10_idx)))
for(x in 1:720){
  for(y in 1:360){
    if(mLAI10[x,y] > 0.5 & !is.na(mLAI10[x,y])){
      for(t in 1:length(y_10_idx)){
        y_ERA5vpd_10[x,y,t] <- mean(VPD_10[x,y,which(y_10_idx[t] == y_10)], na.rm=T)
      }
    }
  }
  print(x)
}

rm(VPD_10)

mERA5_VPD80 <- array(NaN, c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(sum(!is.na(y_ERA5vpd_80[x,y,])) == 10){
      mERA5_VPD80[x,y] <- mean(y_ERA5vpd_80[x,y,], na.rm=T)
    }
  }
}

mERA5_VPD90 <- array(NaN, c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(sum(!is.na(y_ERA5vpd_90[x,y,])) == 10){
      mERA5_VPD90[x,y] <- mean(y_ERA5vpd_90[x,y,], na.rm=T)
    }
  }
}

mERA5_VPD00 <-array(NaN, c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(sum(!is.na(y_ERA5vpd_00[x,y,])) == 10){
      mERA5_VPD00[x,y] <- mean(y_ERA5vpd_00[x,y,], na.rm=T)
    }
  }
}

mERA5_VPD10 <- array(NaN, c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(sum(!is.na(y_ERA5vpd_10[x,y,])) == 9){
      mERA5_VPD10[x,y] <- mean(y_ERA5vpd_10[x,y,], na.rm=T)
    }
  }
}

#Aridity Index# 
ERA5_str_80<-array(NaN, c(720,360,length(y_80))) 
count<-1
for(year in 1982:1991){
  ncin<-nc_open(paste("/mnt/share/data/nr/0d50_daily/ERA_str/str.daily.calc.era5.720_360.",year,".nc",sep=""))
  time <- ncvar_get(ncin, "time")
  if(length(time)==365){
    ERA5_str_80[,,(count:(count+364))] <-ncvar_get(ncin,"str")  
    count<-count+365
  }else if(length(time)==366){
    ERA5_str_80[,,(count:(count+365))] <-ncvar_get(ncin,"str")  
    count<-count+366
  }
  print(paste("Year", year, "is done", sep='')) 
}

ERA5_str_80<-ERA5_str_80[,360:1,]

ERA5_ssr_80<-array(NaN, c(720,360,length(y_80))) #this array is also needed for filtering the vegetation data
count<-1
for(year in 1982:1991){
  ncin<-nc_open(paste("/mnt/share/data/nr/0d50_daily/ERA_ssr/ssr.daily.calc.era5.720_360.",year,".nc",sep=""))
  time <- ncvar_get(ncin, "time")
  if(length(time)==365){
    ERA5_ssr_80[,,(count:(count+364))] <-ncvar_get(ncin,"ssr")  
    count<-count+365
  }else if(length(time)==366){
    ERA5_ssr_80[,,(count:(count+365))] <-ncvar_get(ncin,"ssr")  
    count<-count+366
  }
  print(paste("Year", year, "is done", sep='')) 
}

ERA5_ssr_80<-ERA5_ssr_80[,360:1,]

y_ssr_80 <- array(NaN, c(720,360,length(y_80_idx))) #calculate yearly averages of ssr for regression analysis
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      for(t in 1:length(y_80_idx)){
        y_ssr_80[x,y,t] <- mean(ERA5_ssr_80[x,y,which(y_80_idx[t] == y_80)], na.rm=T)
      }
    }
  }
}

ERA5_snr_80 <- ERA5_str_80 + ERA5_ssr_80

rm(ERA5_ssr_80, ERA5_str_80)

y_snr_80 <- array(NaN, c(720,360,length(y_80_idx)))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      for(t in 1:length(y_80_idx)){
        y_snr_80[x,y,t] <- mean(ERA5_snr_80[x,y,which(y_80_idx[t] == y_80)], na.rm=T)
      }
    }
  }
}

plot(y_snr_80[382,282,], type="l")
rm(ERA5_snr_80)

ERA5_str_90<-array(NaN, c(720,360,length(y_90))) 
count<-1
for(year in 1992:2001){
  ncin<-nc_open(paste("/mnt/share/data/nr/0d50_daily/ERA_str/str.daily.calc.era5.720_360.",year,".nc",sep=""))
  time <- ncvar_get(ncin, "time")
  if(length(time)==365){
    ERA5_str_90[,,(count:(count+364))] <-ncvar_get(ncin,"str")  
    count<-count+365
  }else if(length(time)==366){
    ERA5_str_90[,,(count:(count+365))] <-ncvar_get(ncin,"str")  
    count<-count+366
  }
  print(paste("Year", year, "is done", sep='')) 
}

ERA5_str_90<-ERA5_str_90[,360:1,]

ERA5_ssr_90<-array(NaN, c(720,360,length(y_90))) #this array is also needed for filtering the vegetation data
count<-1
for(year in 1992:2001){
  ncin<-nc_open(paste("/mnt/share/data/nr/0d50_daily/ERA_ssr/ssr.daily.calc.era5.720_360.",year,".nc",sep=""))
  time <- ncvar_get(ncin, "time")
  if(length(time)==365){
    ERA5_ssr_90[,,(count:(count+364))] <-ncvar_get(ncin,"ssr")  
    count<-count+365
  }else if(length(time)==366){
    ERA5_ssr_90[,,(count:(count+365))] <-ncvar_get(ncin,"ssr")  
    count<-count+366
  }
  print(paste("Year", year, "is done", sep='')) 
}

ERA5_ssr_90<-ERA5_ssr_90[,360:1,]

y_ssr_90 <- array(NaN, c(720,360,length(y_90_idx)))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      for(t in 1:length(y_90_idx)){
        y_ssr_90[x,y,t] <- mean(ERA5_ssr_90[x,y,which(y_90_idx[t] == y_90)], na.rm=T)
      }
    }
  }
}

ERA5_snr_90 <- ERA5_str_90 + ERA5_ssr_90

rm(ERA5_str_90, ERA5_ssr_90)


y_snr_90 <- array(NaN, c(720,360,length(y_90_idx)))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      for(t in 1:length(y_90_idx)){
        y_snr_90[x,y,t] <- mean(ERA5_snr_90[x,y,which(y_90_idx[t] == y_90)], na.rm=T)
      }
    }
  }
  print(x)
}

rm(ERA5_snr_90)

ERA5_str_00<-array(NaN, c(720,360,length(y_00))) #this array is also needed for filtering the vegetation data
count<-1
for(year in 2002:2011){
  ncin<-nc_open(paste("/mnt/share/data/nr/0d50_daily/ERA_str/str.daily.calc.era5.720_360.",year,".nc",sep=""))
  time <- ncvar_get(ncin, "time")
  if(length(time)==365){
    ERA5_str_00[,,(count:(count+364))] <-ncvar_get(ncin,"str")  
    count<-count+365
  }else if(length(time)==366){
    ERA5_str_00[,,(count:(count+365))] <-ncvar_get(ncin,"str")  
    count<-count+366
  }
  print(paste("Year", year, "is done", sep='')) 
}

ERA5_str_00<-ERA5_str_00[,360:1,]

ERA5_ssr_00<-array(NaN, c(720,360,length(y_00))) #this array is also needed for filtering the vegetation data
count<-1
for(year in 2002:2011){
  ncin<-nc_open(paste("/mnt/share/data/nr/0d50_daily/ERA_ssr/ssr.daily.calc.era5.720_360.",year,".nc",sep=""))
  time <- ncvar_get(ncin, "time")
  if(length(time)==365){
    ERA5_ssr_00[,,(count:(count+364))] <-ncvar_get(ncin,"ssr")  
    count<-count+365
  }else if(length(time)==366){
    ERA5_ssr_00[,,(count:(count+365))] <-ncvar_get(ncin,"ssr")  
    count<-count+366
  }
  print(paste("Year", year, "is done", sep='')) 
}

ERA5_ssr_00<-ERA5_ssr_00[,360:1,]

y_ssr_00 <- array(NaN, c(720,360,length(y_00_idx)))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      for(t in 1:length(y_00_idx)){
        y_ssr_00[x,y,t] <- mean(ERA5_ssr_00[x,y,which(y_00_idx[t] == y_00)], na.rm=T)
      }
    }
  }
}


ERA5_snr_00 <- ERA5_str_00 + ERA5_ssr_00

rm(ERA5_str_00, ERA5_ssr_00)

y_snr_00 <- array(NaN, c(720,360,length(y_00_idx)))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      for(t in 1:length(y_00_idx)){
        y_snr_00[x,y,t] <- mean(ERA5_snr_00[x,y,which(y_00_idx[t] == y_00)], na.rm=T)
      }
    }
  }
  print(x)
}

rm(ERA5_snr_00)

ERA5_str_10<-array(NaN, c(720,360,length(y_10))) #this array is also needed for filtering the vegetation data
count<-1
for(year in 2012:2020){
  ncin<-nc_open(paste("/mnt/share/data/nr/0d50_daily/ERA_str/str.daily.calc.era5.720_360.",year,".nc",sep=""))
  time <- ncvar_get(ncin, "time")
  if(length(time)==365){
    ERA5_str_10[,,(count:(count+364))] <-ncvar_get(ncin,"str")  
    count<-count+365
  }else if(length(time)==366){
    ERA5_str_10[,,(count:(count+365))] <-ncvar_get(ncin,"str")  
    count<-count+366
  }
  print(paste("Year", year, "is done", sep='')) 
}

ERA5_str_10<-ERA5_str_10[,360:1,]

ERA5_ssr_10<-array(NaN, c(720,360,length(y_10))) #this array is also needed for filtering the vegetation data
count<-1
for(year in 2012:2020){
  ncin<-nc_open(paste("/mnt/share/data/nr/0d50_daily/ERA_ssr/ssr.daily.calc.era5.720_360.",year,".nc",sep=""))
  time <- ncvar_get(ncin, "time")
  if(length(time)==365){
    ERA5_ssr_10[,,(count:(count+364))] <-ncvar_get(ncin,"ssr")  
    count<-count+365
  }else if(length(time)==366){
    ERA5_ssr_10[,,(count:(count+365))] <-ncvar_get(ncin,"ssr")  
    count<-count+366
  }
  print(paste("Year", year, "is done", sep='')) 
}

ERA5_ssr_10<-ERA5_ssr_10[,360:1,]

y_ssr_10 <- array(NaN, c(720,360,length(y_10_idx)))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      for(t in 1:length(y_10_idx)){
        y_ssr_10[x,y,t] <- mean(ERA5_ssr_10[x,y,which(y_10_idx[t] == y_10)], na.rm=T)
      }
    }
  }
}

ERA5_snr_10 <- ERA5_str_10 + ERA5_ssr_10

rm(ERA5_str_10, ERA5_ssr_10)

y_snr_10 <- array(NaN, c(720,360,length(y_10_idx)))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      for(t in 1:length(y_10_idx)){
        y_snr_10[x,y,t] <- mean(ERA5_snr_10[x,y,which(y_10_idx[t] == y_10)], na.rm=T)
      }
    }
  }
  print(x)
}

rm(ERA5_snr_10)

#combine the two decades of each 20-year period
am_ssrd_8201 <- abind(y_ssr_80, y_ssr_90)
am_ssrd_0220 <- abind(y_ssr_00, y_ssr_10)

am_t2m_8201 <- abind(y_t2m_80, y_t2m_90)
am_t2m_0220 <- abind(y_t2m_00, y_t2m_10)


ERA5_t2m_80<-array(NaN, c(720,360,length(y_80))) #this array is also needed for filtering the vegetation data
count<-1
for(year in 1982:1991){
  ncin<-nc_open(paste("/mnt/share/data/t2m/0d50_daily/ERA5_t2m_mean/t2m_mean.daily.calc.era5.720_360.",year,".nc",sep=""))
  time <- ncvar_get(ncin, "time")
  if(length(time)==365){
    ERA5_t2m_80[,,(count:(count+364))] <-ncvar_get(ncin,"t2m")  
    count<-count+365
  }else if(length(time)==366){
    ERA5_t2m_80[,,(count:(count+365))] <-ncvar_get(ncin,"t2m")  
    count<-count+366
  }
  print(paste("Year", year, "is done", sep='')) 
}

ERA5_t2m_80<-ERA5_t2m_80[,360:1,]

y_t2m_80 <- array(NaN, c(720,360,length(y_80_idx)))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      for(t in 1:length(y_80_idx)){
        y_t2m_80[x,y,t] <- mean(ERA5_t2m_80[x,y,which(y_80_idx[t] == y_80)], na.rm=T)
      }
    }
  }
  print(x)
}

rm(ERA5_t2m_80)

ERA5_t2m_90<-array(NaN, c(720,360,length(y_90))) #this array is also needed for filtering the vegetation data
count<-1
for(year in 1992:2001){
  ncin<-nc_open(paste("/mnt/share/data/t2m/0d50_daily/ERA5_t2m_mean/t2m_mean.daily.calc.era5.720_360.",year,".nc",sep=""))
  time <- ncvar_get(ncin, "time")
  if(length(time)==365){
    ERA5_t2m_90[,,(count:(count+364))] <-ncvar_get(ncin,"t2m")  
    count<-count+365
  }else if(length(time)==366){
    ERA5_t2m_90[,,(count:(count+365))] <-ncvar_get(ncin,"t2m")  
    count<-count+366
  }
  print(paste("Year", year, "is done", sep='')) 
}

ERA5_t2m_90<-ERA5_t2m_90[,360:1,]

y_t2m_90 <- array(NaN, c(720,360,length(y_90_idx)))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      for(t in 1:length(y_90_idx)){
        y_t2m_90[x,y,t] <- mean(ERA5_t2m_90[x,y,which(y_90_idx[t] == y_90)], na.rm=T)
      }
    }
  }
  print(x)
}

rm(ERA5_t2m_90)

ERA5_t2m_00<-array(NaN, c(720,360,length(y_00))) #this array is also needed for filtering the vegetation data
count<-1
for(year in 2002:2011){
  ncin<-nc_open(paste("/mnt/share/data/t2m/0d50_daily/ERA5_t2m_mean/t2m_mean.daily.calc.era5.720_360.",year,".nc",sep=""))
  time <- ncvar_get(ncin, "time")
  if(length(time)==365){
    ERA5_t2m_00[,,(count:(count+364))] <-ncvar_get(ncin,"t2m")  
    count<-count+365
  }else if(length(time)==366){
    ERA5_t2m_00[,,(count:(count+365))] <-ncvar_get(ncin,"t2m")  
    count<-count+366
  }
  print(paste("Year", year, "is done", sep='')) 
}

ERA5_t2m_00<-ERA5_t2m_00[,360:1,]

y_t2m_00 <- array(NaN, c(720,360,length(y_00_idx)))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      for(t in 1:length(y_00_idx)){
        y_t2m_00[x,y,t] <- mean(ERA5_t2m_00[x,y,which(y_00_idx[t] == y_00)], na.rm=T)
      }
    }
  }
  print(x)
}

rm(ERA5_t2m_00)


ERA5_t2m_10<-array(NaN, c(720,360,length(y_10))) #this array is also needed for filtering the vegetation data
count<-1
for(year in 2012:2020){
  ncin<-nc_open(paste("/mnt/share/data/t2m/0d50_daily/ERA5_t2m_mean/t2m_mean.daily.calc.era5.720_360.",year,".nc",sep=""))
  time <- ncvar_get(ncin, "time")
  if(length(time)==365){
    ERA5_t2m_10[,,(count:(count+364))] <-ncvar_get(ncin,"t2m")  
    count<-count+365
  }else if(length(time)==366){
    ERA5_t2m_10[,,(count:(count+365))] <-ncvar_get(ncin,"t2m")  
    count<-count+366
  }
  print(paste("Year", year, "is done", sep='')) 
}

ERA5_t2m_10<-ERA5_t2m_10[,360:1,]

y_t2m_10 <- array(NaN, c(720,360,length(y_10_idx)))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      for(t in 1:length(y_10_idx)){
        y_t2m_10[x,y,t] <- mean(ERA5_t2m_10[x,y,which(y_10_idx[t] == y_10)], na.rm=T)
      }
    }
  }
  print(x)
}

rm(ERA5_t2m_10)


ERA5_maxt2m_80<-array(NaN, c(720,360,length(y_80))) #this array is also needed for filtering the vegetation data
count<-1
for(year in 1982:1991){
  ncin<-nc_open(paste("/mnt/share/data/t2m/0d50_daily/ERA5_t2m_max/t2m_max.daily.calc.era5.720_360.",year,".nc",sep=""))
  time <- ncvar_get(ncin, "time")
  if(length(time)==365){
    ERA5_maxt2m_80[,,(count:(count+364))] <-ncvar_get(ncin,"t2m")  
    count<-count+365
  }else if(length(time)==366){
    ERA5_maxt2m_80[,,(count:(count+365))] <-ncvar_get(ncin,"t2m")  
    count<-count+366
  }
  print(paste("Year", year, "is done", sep='')) 
}

ERA5_maxt2m_80<-ERA5_maxt2m_80[,360:1,]

y_maxt2m_80 <- array(NaN, c(720,360,length(y_80_idx)))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      for(t in 1:length(y_80_idx)){
        y_maxt2m_80[x,y,t] <- max(ERA5_maxt2m_80[x,y,which(y_80_idx[t] == y_80)], na.rm=T)
      }
    }
  }
  print(x)
}

rm(ERA5_maxt2m_80)

ERA5_maxt2m_90<-array(NaN, c(720,360,length(y_90))) #this array is also needed for filtering the vegetation data
count<-1
for(year in 1992:2001){
  ncin<-nc_open(paste("/mnt/share/data/t2m/0d50_daily/ERA5_t2m_max/t2m_max.daily.calc.era5.720_360.",year,".nc",sep=""))
  time <- ncvar_get(ncin, "time")
  if(length(time)==365){
    ERA5_maxt2m_90[,,(count:(count+364))] <-ncvar_get(ncin,"t2m")  
    count<-count+365
  }else if(length(time)==366){
    ERA5_maxt2m_90[,,(count:(count+365))] <-ncvar_get(ncin,"t2m")  
    count<-count+366
  }
  print(paste("Year", year, "is done", sep='')) 
}

ERA5_maxt2m_90<-ERA5_maxt2m_90[,360:1,]

y_maxt2m_90 <- array(NaN, c(720,360,length(y_90_idx)))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      for(t in 1:length(y_90_idx)){
        y_maxt2m_90[x,y,t] <- max(ERA5_maxt2m_90[x,y,which(y_90_idx[t] == y_90)], na.rm=T)
      }
    }
  }
  print(x)
}

rm(ERA5_maxt2m_90)

ERA5_maxt2m_00<-array(NaN, c(720,360,length(y_00))) #this array is also needed for filtering the vegetation data
count<-1
for(year in 2002:2011){
  ncin<-nc_open(paste("/mnt/share/data/t2m/0d50_daily/ERA5_t2m_max/t2m_max.daily.calc.era5.720_360.",year,".nc",sep=""))
  time <- ncvar_get(ncin, "time")
  if(length(time)==365){
    ERA5_maxt2m_00[,,(count:(count+364))] <-ncvar_get(ncin,"t2m")  
    count<-count+365
  }else if(length(time)==366){
    ERA5_maxt2m_00[,,(count:(count+365))] <-ncvar_get(ncin,"t2m")  
    count<-count+366
  }
  print(paste("Year", year, "is done", sep='')) 
}

ERA5_maxt2m_00<-ERA5_maxt2m_00[,360:1,]

y_maxt2m_00 <- array(NaN, c(720,360,length(y_00_idx)))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      for(t in 1:length(y_00_idx)){
        y_maxt2m_00[x,y,t] <- max(ERA5_maxt2m_00[x,y,which(y_00_idx[t] == y_00)], na.rm=T)
      }
    }
  }
  print(x)
}

rm(ERA5_maxt2m_00)


ERA5_maxt2m_10<-array(NaN, c(720,360,length(y_10))) #this array is also needed for filtering the vegetation data
count<-1
for(year in 2012:2020){
  ncin<-nc_open(paste("/mnt/share/data/t2m/0d50_daily/ERA5_t2m_max/t2m_max.daily.calc.era5.720_360.",year,".nc",sep=""))
  time <- ncvar_get(ncin, "time")
  if(length(time)==365){
    ERA5_maxt2m_10[,,(count:(count+364))] <-ncvar_get(ncin,"t2m")  
    count<-count+365
  }else if(length(time)==366){
    ERA5_maxt2m_10[,,(count:(count+365))] <-ncvar_get(ncin,"t2m")  
    count<-count+366
  }
  print(paste("Year", year, "is done", sep='')) 
}

ERA5_maxt2m_10<-ERA5_maxt2m_10[,360:1,]

y_maxt2m_10 <- array(NaN, c(720,360,length(y_10_idx)))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      for(t in 1:length(y_10_idx)){
        y_maxt2m_10[x,y,t] <- max(ERA5_maxt2m_10[x,y,which(y_10_idx[t] == y_10)], na.rm=T)
      }
    }
  }
  print(x)
}

rm(ERA5_maxt2m_10)


MODIS_00<-array(NaN, c(720,360,460)) #MODIS is 10 daily --> 45 or 46 time steps within a year
count<-1
for(year in 2002:2011){
  ncin<-nc_open(paste("/mnt/share/data/lai/0d50_8daily/MOD15A2H.061/lai.720.360.",year,".nc",sep=""))
  time <- ncvar_get(ncin, "time")
  if(length(time) != 46){
    MODIS_00[,,(count:(count+length(time)-1))] <-ncvar_get(ncin,"lai")
    count<-count+46
  }else if(length(time)==46){
    MODIS_00[,,(count:(count+45))] <-ncvar_get(ncin,"lai")  
    count<-count+46
  }
  print(paste("Year", year, "is done", sep='')) 
}

MODIS_10<-array(NaN, c(720,360,414)) 
for(year in 2012:2020){
  ncin<-nc_open(paste("/mnt/share/data/lai/0d50_8daily/MOD15A2H.061/lai.720.360.",year,".nc",sep=""))
  time <- ncvar_get(ncin, "time")
  if(length(time) != 46){
    MODIS_10[,,(count:(count+length(time)-1))] <-ncvar_get(ncin,"lai")
    count<-count+46
  }else if(length(time)==46){
    MODIS_10[,,(count:(count+45))] <-ncvar_get(ncin,"lai")  
    count<-count+46
  }
  print(paste("Year", year, "is done", sep='')) 
}

MODIS_am_00 <- array(NaN, c(720,360,10))
count <- 1
year <- 1
for(x in 1:720){
  for(y in 1:360){
    if(sum(!is.na(MODIS_00[x,y,])) > 46){
      repeat{
        MODIS_am_00[x,y,year] <- mean(MODIS_00[x,y, count:(count+45)], na.rm=T)
        count <- count + 46
        year <- year+1
        if(year >10){
          break
        }
      }
      count <- 1
      year <- 1
    }
  }
}


MODIS_am_10 <- array(NaN, c(720,360,9))
count <- 1
year <- 1
for(x in 1:720){
  for(y in 1:360){
    if(sum(!is.na(MODIS_10[x,y,])) > 46){
      repeat{
        MODIS_am_10[x,y,year] <- mean(MODIS_10[x,y, count:(count+45)], na.rm=T)
        count <- count + 46
        year <- year+1
        if(year > 9){
          break
        }
      }
      count <- 1
      year <- 1
    }
  }
}

rm(MODIS_00, MODIS_10)

mMODIS00 <- array(NaN, c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(sum(!is.na(MODIS_am_00[x,y,])) == 10){
      mMODIS00[x,y] <- mean(MODIS_am_00[x,y,], na.rm=T)
    }
  }
}

mMODIS10 <- array(NaN, c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(sum(!is.na(MODIS_am_10[x,y,])) == 9){
      mMODIS10[x,y] <- mean(MODIS_am_10[x,y,], na.rm=T)
    }
  }
}


##########################################
##### Analysis 1: Observational data #####

#function to retrieve a trend from randomly selected years - randomly unselected years out of the 19/20 year periods investigated
random_trend <- function(x, min=20){
  if(sum(!is.na(x)) == min & sum(x != 0) == min){
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


# first connect areas of subsequent decades, so that I have all possible values to sample from in one array, except for aridity as this is done parallelized
amLAI_8201 <- abind(LAI_am_80, LAI_am_90)
amLAI_0220 <- abind(LAI_am_00, LAI_am_10)

amMSWEP_8201 <- abind(y_MSWEPtp_80, y_MSWEPtp_90)
amMSWEP_0220 <- abind(y_MSWEPtp_00, y_MSWEPtp_10)

amrGLEAM_8201 <- abind(y_rGLEAM_80, y_rGLEAM_90)
amrGLEAM_0220 <- abind(y_rGLEAM_00, y_rGLEAM_10)

amERA5_VPD_8201 <- abind(y_ERA5vpd_80, y_ERA5vpd_90)
amERA5_VPD_0220 <- abind(y_ERA5vpd_00, y_ERA5vpd_10)

# 2nd compute difference between decade 1 and decade 2
diff_LAI_8090 <- mLAI90 - mLAI80
diff_LAI_0010 <- mLAI10 - mLAI00

diff_MSWEP_8090 <- mMSWEP90 - mMSWEP80
diff_MSWEP_0010 <- mMSWEP10 - mMSWEP00

diff_ERA5_VPD_8090 <- mERA5_VPD90 - mERA5_VPD80
diff_ERA5_VPD_0010 <- mERA5_VPD10 - mERA5_VPD00

diff_rGLEAM_8090 <- mrGLEAM90 - mrGLEAM80
diff_rGLEAM_0010 <- mrGLEAM10 - mrGLEAM00

#For checking the significance the difference between LAI in the first half vs 
#the second half of the 20/19 year period, it is evaluated against a distribution of differences 
#built based on the mean of randomly selected years out of the 20/19-year period; signficance when trend of real data is above 95th or below 5th percentile
sign_LAI0595_80 <- array(NaN, c(720, 360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y]) & var(amLAI_8201[x,y,]) != 0 ){
      t <- c()
      t <- unname(quantile(replicate(300, random_trend(amLAI_8201[x,y,])), probs=c(0.05, 0.95), na.rm=T))
      if(diff_LAI_8090[x,y] >= t[2] | diff_LAI_8090[x,y] <= t[1]){ # significant
        sign_LAI0595_80[x,y] <- 1
      } else if (diff_LAI_8090[x,y] < t[2] & diff_LAI_8090[x,y] > t[1]){ # insignificant
        sign_LAI0595_80[x,y] <- 2
      } 
    } else{
      sign_LAI0595_80[x,y] <- 2.3 #not study area 
    }
  }
  print(x)
}

image.plot(sign_LAI0595_80[,], breaks = c(0, 1.1, 2.1, 2.4), col = c("green4", "lightgreen", "grey"))


sign_LAI0595_00 <- array(NaN, c(720, 360)) 
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y]) & var(amLAI_0220[x,y,]) != 0){
      t <- c()
      t <- unname(quantile(replicate(300, random_trend(amLAI_0220[x,y,], min=19)), probs=c(0.05, 0.95), na.rm=T))
      if(diff_LAI_0010[x,y] >= t[2] | diff_LAI_0010[x,y] <= t[1]){
        sign_LAI0595_00[x,y] <- 1 #significant
      } else if(diff_LAI_0010[x,y] < t[2] | diff_LAI_0010[x,y] > t[1]){
        sign_LAI0595_00[x,y] <- 2 #insignificant
      } 
    } else{
      sign_LAI0595_00[x,y] <- 2.3 #not part of the study area
    }
  }
  print(x)
}

image.plot(sign_LAI0595_00[,])

#derive direction of trend for the 1st and 2nd period
trend_LAI_80 <- array(NaN, c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(mLAI80[x,y]) & !is.na(mLAI90[x,y]) & mLAI80[x,y] > 0.5 & mLAI90[x,y] > 0.5){
      if(mLAI80[x,y] > mLAI90[x,y]){
        trend_LAI_80[x,y] <- 1 #decreasing
      }
      if(mLAI80[x,y] < mLAI90[x,y]){
        trend_LAI_80[x,y] <- 2 #increasing
      }
    }
  }
}

trend_LAI_00 <- array(NaN, c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(mLAI00[x,y]) & !is.na(mLAI10[x,y]) & mLAI00[x,y] > 0.5 & mLAI10[x,y] > 0.5){
      if(mLAI00[x,y] > mLAI10[x,y]){
        trend_LAI_00[x,y] <- 1 #decreasing
      }
      if(mLAI00[x,y] < mLAI10[x,y]){
        trend_LAI_00[x,y] <- 2 #increasing
      }
    }
  }
}

#create categories based on trend direction and significance
sign_trend_LAI0595_8201 <- array(NaN,c(720,360)) #derive categories from significance and trend
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      if(!is.na(trend_LAI_80[x,y]) & !is.na(sign_LAI0595_80[x,y])){
        if(trend_LAI_80[x,y] == 1 & sign_LAI0595_80[x,y] == 1){ #decreasing & significant
          sign_trend_LAI0595_8201[x,y] <- 1
        } else if(trend_LAI_80[x,y] == 1 & sign_LAI0595_80[x,y] == 2){ #decreasing & insignificant
          sign_trend_LAI0595_8201[x,y] <- 2
        } else if(trend_LAI_80[x,y] == 2 & sign_LAI0595_80[x,y] == 2){ #increasing & insignificant
          sign_trend_LAI0595_8201[x,y] <- 3
        } else if(trend_LAI_80[x,y] == 2 & sign_LAI0595_80[x,y] == 1){ #increasing & significant
          sign_trend_LAI0595_8201[x,y] <- 4
        } 
      } else{
        sign_trend_LAI0595_8201[x,y] <- 4.3 #signficance or trend is missing
      }
    } else{
      sign_trend_LAI0595_8201[x,y] <- 4.6 #not part of the study area 
    }
  }
}


sign_trend_LAI0595_0220 <- array(NaN,c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      if(!is.na(trend_LAI_00[x,y]) & !is.na(sign_LAI0595_00[x,y])){
        if(trend_LAI_00[x,y] == 1 & sign_LAI0595_00[x,y] == 1){ #decreasing & significant
          sign_trend_LAI0595_0220[x,y] <- 1
        } else if(trend_LAI_00[x,y] == 1 & sign_LAI0595_00[x,y] == 2){ #decreasing & insignificant
          sign_trend_LAI0595_0220[x,y] <- 2
        } else if(trend_LAI_00[x,y] == 2 & sign_LAI0595_00[x,y] == 2){ #increasing & insignificant
          sign_trend_LAI0595_0220[x,y] <- 3
        } else if(trend_LAI_00[x,y] == 2 & sign_LAI0595_00[x,y] == 1){ #increasing & significant
          sign_trend_LAI0595_0220[x,y] <- 4
        } 
      } else{
        sign_trend_LAI0595_0220[x,y] <- 4.3 #significance or trend is missing
      }
    }else{
      sign_trend_LAI0595_0220[x,y] <- 4.6 #not part of the study area = NAN 
    }
  }
}


area_insigposLAI0595_80 <- (sum(area.array[which(sign_trend_LAI0595_8201 == 3)], na.rm=T))/total_land_area
area_sigposLAI0595_80 <- (sum(area.array[which(sign_trend_LAI0595_8201 == 4)], na.rm=T))/total_land_area
area_signegLAI0595_80 <- (sum(area.array[which(sign_trend_LAI0595_8201 == 1)], na.rm=T))/total_land_area
area_insignegLAI0595_80 <- (sum(area.array[which(sign_trend_LAI0595_8201 == 2)], na.rm=T))/total_land_area

area_insigposLAI0595_00 <- (sum(area.array[which(sign_trend_LAI0595_0220 == 3)], na.rm=T))/total_land_area
area_sigposLAI0595_00 <- (sum(area.array[which(sign_trend_LAI0595_0220 == 4)], na.rm=T))/total_land_area
area_signegLAI0595_00 <- (sum(area.array[which(sign_trend_LAI0595_0220 == 1)], na.rm=T))/total_land_area
area_insignegLAI0595_00 <- (sum(area.array[which(sign_trend_LAI0595_0220 == 2)], na.rm=T))/total_land_area

png("path/to/your/storage/fig_1a.png", width=7.5, height=4, units="in", res=1400)
par(mar=c(2,2,1,1))
image.plot(lon, lat[61:331], sign_trend_LAI0595_8201[, 61:331], 
      col=c("peru", "moccasin", "lightgreen", "green4"), 
      breaks=c(0,1.01,2.01,3.01,4.01), 
      axis.args=list(at=c(0.5, 1.5, 2.5, 3.5), 
      labels=c("- LAI*", "- LAI", "+ LAI", "+ LAI*")), 
      main="1982-2001", axes = F)
      axis(2, at=c(-50, -25, 0, 25, 50, 75), las=1) 
      axis(1)
      box(col="black") 
maps::map("world", interior=F, add=T, col='lightgrey')
par(fig = c(0.08,0.27, 0.1, 0.5), new = T, mar=c(1,2,2,0.5))
barplot(height=c(area_sigposLAI0595_80*100, area_insigposLAI0595_80*100, area_insignegLAI0595_80*100, area_signegLAI0595_80*100), 
      ylim=c(0,60), 
      col=c("green4", "lightgreen", "moccasin", "peru"), 
      width=0.25, 
      main="% of land area", cex.main=0.7)
text(c(0.175,0.47,0.77,1.1), c(area_sigposLAI0595_80*100+10, area_insigposLAI0595_80*100+10, area_insignegLAI0595_80*100+10, area_signegLAI0595_80*100+10), 
      c(round(area_sigposLAI0595_80*100), round(area_insigposLAI0595_80*100), round(area_insignegLAI0595_80*100), round(area_signegLAI0595_80*100)))
dev.off()

png("path/to/your/storage/fig_1b.png", width=7.5, height=4, units="in", res=1400)
par(mar=c(2,2,1,1))
image.plot(lon, lat[61:331], sign_trend_LAI0595_0220[,61:331], 
      col=c("peru", "moccasin", "lightgreen", "green4"), 
      breaks=c(0,1.01,2.01,3.01,4.01), 
      axis.args=list(at=c(0.5, 1.5, 2.5, 3.5), 
      labels=c("- LAI*", "- LAI", "+ LAI", "+ LAI*")), 
      main="2002-2020", axes = F)
      axis(2, at=c(-50, -25, 0, 25, 50, 75), las=1) 
      axis(1)
      box(col="black") 
maps::map("world", interior=F, add=T, col='lightgrey')
par(fig = c(0.08,0.27, 0.1, 0.5), new = T, mar=c(1,2,2,0.5))
barplot(height=c(area_sigposLAI0595_00*100, area_insigposLAI0595_00*100, area_insignegLAI0595_00*100, area_signegLAI0595_00*100), 
      ylim=c(0,60), 
      col=c("green4", "lightgreen", "moccasin", "peru"), 
      width=0.25, 
      main="% of land area", cex.main=0.7)
text(c(0.175,0.47,0.77,1.1), 
      c(area_sigposLAI0595_00*100+10, area_insigposLAI0595_00*100+10, area_insignegLAI0595_00*100+10, area_signegLAI0595_00*100+10), 
      c(round(area_sigposLAI0595_00*100), round(area_insigposLAI0595_00*100), round(area_insignegLAI0595_00*100), round(area_signegLAI0595_00*100)))
dev.off()

#MODIS
amMODIS_0220 <- abind(MODIS_am_00, MODIS_am_10)

diff_MODIS_0010 <- mMODIS10 - mMODIS00

#For checking the significance the difference between  MODIS in the first half vs 
#the second half of the 19 year period, it is evaluated against a distribution of differences 
#built based on the mean of randomly selected years out of the 19-year period
sign_MODIS_00 <- array(NaN, c(720, 360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      if(!is.na(diff_MODIS_0010[x,y]) & !is.na(random_trend(amMODIS_0220[x,y,], min=19))){
        t <- unname(quantile(replicate(300, random_trend(amMODIS_0220[x,y,], min=19)), probs=c(0.05, 0.95), na.rm=T))
        if(diff_MODIS_0010[x,y] >= t[2] | diff_MODIS_0010[x,y] <= t[1]){
          sign_MODIS_00[x,y] <- 1 #significant
        } else if(diff_MODIS_0010[x,y] < t[2] & diff_MODIS_0010[x,y] > t[1]) {
          sign_MODIS_00[x,y] <- 2 #insignificant
        }
      } else{
        sign_MODIS_00[x,y] <- 2.6 #missing data
      }
    } else{
      sign_MODIS_00[x,y] <- 2.3 #not study area
    }
  }
  print(x)
}

#derive direction of trend for the 2nd period
trend_MODIS_00 <- array(NaN, c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y]) & !is.na(mMODIS00[x,y]) & !is.na(mMODIS10[x,y])){
      if(mMODIS00[x,y] > mMODIS10[x,y]){
        trend_MODIS_00[x,y] <- 1 #decreasing
      }
      if(mMODIS00[x,y] < mMODIS10[x,y]){
        trend_MODIS_00[x,y] <- 2 #increasing
      }
    }
  }
}

#create categories based on trend direction and significance
sign_trend_MODIS_0220 <- array(NaN,c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      if(trend_MODIS_00[x,y] == 1 & sign_MODIS_00[x,y] == 1){ #decreasing & significant
        sign_trend_MODIS_0220[x,y] <- 1
      }else if(trend_MODIS_00[x,y] == 1 & sign_MODIS_00[x,y] == 2){ #decreasing & insignificant
        sign_trend_MODIS_0220[x,y] <- 2
      }else if(trend_MODIS_00[x,y] == 2 & sign_MODIS_00[x,y] == 2){ #increasing & insignificant
        sign_trend_MODIS_0220[x,y] <- 3
      }else if(trend_MODIS_00[x,y] == 2 & sign_MODIS_00[x,y] == 1){ #increasing & significiant
        sign_trend_MODIS_0220[x,y] <- 4
      }
    } else{
      sign_trend_MODIS_0220[x,y] <- 4.3
    }
  }
}

area_insigposMODIS_00 <- (sum(area.array[which(sign_trend_MODIS_0220 == 3)], na.rm=T))/total_land_area
area_sigposMODIS_00 <- (sum(area.array[which(sign_trend_MODIS_0220 == 4)], na.rm=T))/total_land_area
area_signegMODIS_00 <- (sum(area.array[which(sign_trend_MODIS_0220 == 1)], na.rm=T))/total_land_area
area_insignegMODIS_00 <- (sum(area.array[which(sign_trend_MODIS_0220 == 2)], na.rm=T))/total_land_area


png("path/to/your/storage/fig_S1.png", width=7.5, height=4, units="in", res=1400)
par(mar=c(2,2,1,1))
image.plot(lon, lat[61:331], sign_trend_MODIS_0220[,61:331], 
      col=c("peru", "moccasin", "lightgreen","green4"), 
      breaks=c(0,1.01,2.01,3.01,4.01), 
      axis.args=list(at=c(0.5, 1.5, 2.5, 3.5), labels=c("- LAI*", "- LAI", "+ LAI", "+ LAI*")), 
      main="2002-2020", axes = F)
      axis(2, at=c(-50, -25, 0, 25, 50, 75), las=1) 
      axis(1)
      box(col="black") 
maps::map("world", interior=F, add=T, col= "lightgrey")
par(fig = c(0.08,0.27, 0.1, 0.5), new = T, mar=c(1,2,2,0.5))
barplot(height=c(area_sigposMODIS_00*100, area_insigposMODIS_00*100, area_insignegMODIS_00*100, area_signegMODIS_00*100), 
      ylim=c(0,60), 
      col=c("green4", "lightgreen", "moccasin", "peru"), 
      width=0.25,
      main = "% of land area", cex.main = 0.7)
text(c(0.175,0.47,0.77,1.1), 
      c(area_sigposMODIS_00*100+10, area_insigposMODIS_00*100+10, area_insignegMODIS_00*100+10, area_signegMODIS_00*100+10), 
      c(round(area_sigposMODIS_00*100), round(area_insigposMODIS_00*100), round(area_insignegMODIS_00*100), round(area_signegMODIS_00*100)))
dev.off()


# MSWEP

#derive direction of trend for the 1st and 2nd period
trend_MSWEP_80 <- array(NaN, c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      if(mMSWEP80[x,y] > mMSWEP90[x,y]){
        trend_MSWEP_80[x,y] <- 1 #decreasing
      } else if(mMSWEP80[x,y] < mMSWEP90[x,y]){
        trend_MSWEP_80[x,y] <- 2 #increasing
      }
    } else {
      trend_MSWEP_80[x,y] <- 2.3 #not part of study area
    }
  }
}

trend_MSWEP_00 <- array(NaN, c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      if(mMSWEP00[x,y] > mMSWEP10[x,y]){
        trend_MSWEP_00[x,y] <- 1 #decreasing
      } else if(mMSWEP00[x,y] < mean(mMSWEP10[x,y])){
        trend_MSWEP_00[x,y] <- 2 #increasing
      } 
    } else{
      trend_MSWEP_00[x,y] <- 2.3 #not study area
    }
  }
}

#For checking the significance the difference between MSWEP in the first half vs 
#the second half of the 20/19 year period, it is evaluated against a distribution of differences 
#built based on the mean of randomly selected years out of the 20/19-year period
sign_MSWEP_80 <- array(NaN, c(720, 360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      t <- c()
      t <- unname(quantile(replicate(300, random_trend(amMSWEP_8201[x,y,])), probs=c(0.05, 0.95), na.rm=T))
      if(diff_MSWEP_8090[x,y] >= t[2] | diff_MSWEP_8090[x,y] <= t[1]){
        sign_MSWEP_80[x,y] <- 1
      } else if(diff_MSWEP_8090[x,y] < t[2] & diff_MSWEP_8090[x,y] > t[1]){
        sign_MSWEP_80[x,y] <- 2
      } 
    } else{
      sign_MSWEP_80[x,y] <- 2.3 #not study area
    }
  }
  print(x)
}

sign_MSWEP_00 <- array(NaN, c(720, 360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      t <- c()
      t <- unname(quantile(replicate(300, random_trend(amMSWEP_0220[x,y,], min=19)), probs=c(0.05, 0.95), na.rm=T))
      if(diff_MSWEP_0010[x,y] >= t[2] | diff_MSWEP_0010[x,y] <= t[1]){
        sign_MSWEP_00[x,y] <- 1 #significant
      } else if(diff_MSWEP_0010[x,y] < t[2] & diff_MSWEP_0010[x,y] > t[1]){
        sign_MSWEP_00[x,y] <- 2 #insignificant
      } 
    } else{
      sign_MSWEP_00[x,y] <- 2.3 #not study area
    }
  }
  print(x)
}

#create categories based on trend direction and significance
sign_trend_MSWEP_8201 <- array(NaN,c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      if(!is.na(sign_MSWEP_80[x,y]) & !is.na(trend_MSWEP_80[x,y])){
        if(trend_MSWEP_80[x,y] == 1 & sign_MSWEP_80[x,y] == 1){ #decreasing & significant
          sign_trend_MSWEP_8201[x,y] <- 1
        } else if(trend_MSWEP_80[x,y] == 1 & sign_MSWEP_80[x,y] == 2){ #decreasing & insignificant
          sign_trend_MSWEP_8201[x,y] <- 2
        } else if(trend_MSWEP_80[x,y] == 2 & sign_MSWEP_80[x,y] == 2){ #increasing & insignificant
          sign_trend_MSWEP_8201[x,y] <- 3
        } else if(trend_MSWEP_80[x,y] == 2 & sign_MSWEP_80[x,y] == 1){ #increasing & significiant
          sign_trend_MSWEP_8201[x,y] <- 4
        } 
      }else {
        sign_trend_MSWEP_8201[x,y] <- 4.6 #sign or trend calc is missing
      }
    } else{
      sign_trend_MSWEP_8201[x,y] <- 4.3 #no study area
    }
  }
}

sign_trend_MSWEP_0220 <- array(NaN,c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      if(!is.na(sign_MSWEP_00[x,y]) & !is.na(trend_MSWEP_00[x,y])){
        if(trend_MSWEP_00[x,y] == 1 & sign_MSWEP_00[x,y] == 1){ #decreasing & significant
          sign_trend_MSWEP_0220[x,y] <- 1
        } else if(trend_MSWEP_00[x,y] == 1 & sign_MSWEP_00[x,y] == 2){ #decreasing & insignificant
          sign_trend_MSWEP_0220[x,y] <- 2
        } else if(trend_MSWEP_00[x,y] == 2 & sign_MSWEP_00[x,y] == 2){ #increasing & insignificant
          sign_trend_MSWEP_0220[x,y] <- 3
        } else if(trend_MSWEP_00[x,y] == 2 & sign_MSWEP_00[x,y] == 1){ #increasing & significiant
          sign_trend_MSWEP_0220[x,y] <- 4
        } 
      }else {
        sign_trend_MSWEP_0220[x,y] <- 4.6 #trend or significance is missing
      }
    } else{
      sign_trend_MSWEP_0220[x,y] <- 4.3 #no study area
    }
  }
}

area_insigposMSWEP_80 <- (sum(area.array[which(sign_trend_MSWEP_8201 == 3)], na.rm=T))/total_land_area
area_sigposMSWEP_80 <- (sum(area.array[which(sign_trend_MSWEP_8201 == 4)], na.rm=T))/total_land_area
area_signegMSWEP_80 <- (sum(area.array[which(sign_trend_MSWEP_8201 == 1)], na.rm=T))/total_land_area
area_insignegMSWEP_80 <- (sum(area.array[which(sign_trend_MSWEP_8201 == 2)], na.rm=T))/total_land_area

area_insigposMSWEP_00 <- (sum(area.array[which(sign_trend_MSWEP_0220 == 3)], na.rm=T))/total_land_area
area_sigposMSWEP_00 <- (sum(area.array[which(sign_trend_MSWEP_0220 == 4)], na.rm=T))/total_land_area
area_signegMSWEP_00 <- (sum(area.array[which(sign_trend_MSWEP_0220 == 1)], na.rm=T))/total_land_area
area_insignegMSWEP_00 <- (sum(area.array[which(sign_trend_MSWEP_0220 == 2)], na.rm=T))/total_land_area

png("path/to/your/storage/fig_S2a.png", width = 600, height=350)
par(mar=c(2,2,2,0.5))
image.plot(lon, lat[61:331], sign_trend_MSWEP_8201[,61:331], 
      col=c("peru", "moccasin", "deepskyblue", "dodgerblue"), 
      breaks=c(0,1.01,2.01,3.01,4.01), 
      axis.args=list(at=c(0.5, 1.5, 2.5, 3.5), labels=c("- MSWEP*", "- MSWEP", "+ MSWEP", "+ MSWEP*")), 
      main="Trend 1982-2001", axes = F)
      axis(2, at=c(-50, -25, 0, 25, 50, 75), las=1) 
      axis(1)
      box(col="black") 
maps::map("world", interior=F, add=T)
par(fig = c(0.08,0.27, 0.1, 0.5), new = T, mar=c(1,2,2,0.5))
barplot(height=c(area_sigposMSWEP_80*100, area_insigposMSWEP_80*100, area_insignegMSWEP_80*100, area_signegMSWEP_80*100), 
      ylim=c(0,50), col=c("dodgerblue", "deepskyblue", "moccasin", "peru"), 
      width=0.25,
      main="% of land area", cex.main=0.7)
text(c(0.175,0.47,0.77,1.1), 
      c(area_sigposMSWEP_80*100+10, area_insigposMSWEP_80*100+10, area_insignegMSWEP_80*100+10, area_signegMSWEP_80*100+10), 
      c(round(area_sigposMSWEP_80*100), round(area_insigposMSWEP_80*100), round(area_insignegMSWEP_80*100), round(area_signegMSWEP_80*100)))
dev.off()

png("path/to/your/storage/fig_S2b.png", width = 600, height=350)
par(mar=c(2,2,2,0.5))
image.plot(lon, lat[61:331], sign_trend_MSWEP_0220[,61:331], 
      col=c("peru", "moccasin", "deepskyblue", "dodgerblue"), 
      breaks=c(0,1.01,2.01,3.01,4.01), 
      axis.args=list(at=c(0.5, 1.5, 2.5, 3.5), labels=c("- MSWEP*", "- MSWEP", "+ MSWEP", "+ MSWEP*")), 
      main="Trend 2002-2020", axes = F)
      axis(2, at=c(-50, -25, 0, 25, 50, 75), las=1) 
      axis(1)
      box(col="black") 
maps::map("world", interior=F, add=T)
par(fig = c(0.08,0.27, 0.1, 0.5), new = T, mar=c(1,2,2,0.5))
barplot(height=c(area_sigposMSWEP_00*100, area_insigposMSWEP_00*100, area_insignegMSWEP_00*100, area_signegMSWEP_00*100), 
      ylim=c(0,50), col=c("dodgerblue", "deepskyblue", "moccasin", "peru"), 
      width=0.25,
      main="% of land area", cex.main=0.7)
text(c(0.175,0.47,0.77,1.1), 
      c(area_sigposMSWEP_00*100+10, area_insigposMSWEP_00*100+10, area_insignegMSWEP_00*100+10, area_signegMSWEP_00*100+10), 
      c(round(area_sigposMSWEP_00*100), round(area_insigposMSWEP_00*100), round(area_insignegMSWEP_00*100), round(area_signegMSWEP_00*100)))
dev.off()

#rGLEAM#

#derive direction of trend for the 1st and 2nd period
trend_rGLEAM_80 <- array(NaN, c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      if(!is.na(mrGLEAM80[x,y]) & !is.na(mrGLEAM90[x,y])){
        if(mrGLEAM80[x,y] > mrGLEAM90[x,y]){
          trend_rGLEAM_80[x,y] <- 1 #decreasing
        } else if(mrGLEAM80[x,y] < mrGLEAM90[x,y]){
          trend_rGLEAM_80[x,y] <- 2 #increasing
        }
      }
    } else{
      trend_rGLEAM_80[x,y] <- 2.3 #not study area 
    }
  }
}

trend_rGLEAM_00 <- array(NaN, c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      if(!is.na(mrGLEAM00[x,y]) & !is.na(mrGLEAM10[x,y])){
        if(mrGLEAM00[x,y] > mrGLEAM10[x,y]){
          trend_rGLEAM_00[x,y] <- 1 #decreasing
        } else if(mrGLEAM00[x,y] < mrGLEAM10[x,y]){
          trend_rGLEAM_00[x,y] <- 2 #increasing
        }
      }
    } else {
      trend_rGLEAM_00[x,y] <- 2.3 #no study area
    }
  }
}

#For checking the significance the difference between  rGLEAM in the first half vs 
#the second half of the 20/19 year period, it is evaluated against a distribution of differences 
#built based on the mean of randomly selected years out of the 20/19-year period
sign_rGLEAM_80 <- array(NaN, c(720, 360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      if(!is.na(diff_rGLEAM_8090[x,y])){
        t <- c()
        t <- unname(quantile(replicate(300, random_trend(amrGLEAM_8201[x,y,])), probs=c(0.05, 0.95), na.rm=T))
        if(diff_rGLEAM_8090[x,y] >= t[2] | diff_rGLEAM_8090[x,y] <= t[1]){
          sign_rGLEAM_80[x,y] <- 1
        } else if(diff_rGLEAM_8090[x,y] < t[2] & diff_rGLEAM_8090[x,y] > t[1]){
          sign_rGLEAM_80[x,y] <- 2
        }
      }
    } else{
      sign_rGLEAM_80[x,y] <- 2.3 #no study area
    }
  }
  print(x)
}

sign_rGLEAM_00 <- array(NaN, c(720, 360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      if(!is.na(diff_rGLEAM_0010[x,y])){
        t <- c()
        t <- unname(quantile(replicate(300, random_trend(amrGLEAM_0220[x,y,], min=19)), probs=c(0.05, 0.95), na.rm=T))
        if(diff_rGLEAM_0010[x,y] >= t[2] | diff_rGLEAM_0010[x,y] <= t[1]){
          sign_rGLEAM_00[x,y] <- 1 #significant
        } else if(diff_rGLEAM_0010[x,y] < t[2] & diff_rGLEAM_0010[x,y] > t[1]){
          sign_rGLEAM_00[x,y] <- 2 #insignificant
        }
      }
    } else {
      sign_rGLEAM_00[x,y] <- 2.3 #no study area
    }
  }
  print(x)
}

#create categories based on trend direction and significance
sign_trend_rGLEAM_8201 <- array(NaN,c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      if(!is.na(sign_rGLEAM_80[x,y]) & !is.na(trend_rGLEAM_80[x,y])){
        if(trend_rGLEAM_80[x,y] == 1 & sign_rGLEAM_80[x,y] == 1){ #decreasing & significant
          sign_trend_rGLEAM_8201[x,y] <- 1
        } else if(trend_rGLEAM_80[x,y] == 1 & sign_rGLEAM_80[x,y] == 2){ #decreasing & insignificant
          sign_trend_rGLEAM_8201[x,y] <- 2
        } else if(trend_rGLEAM_80[x,y] == 2 & sign_rGLEAM_80[x,y] == 2){ #increasing & insignificant
          sign_trend_rGLEAM_8201[x,y] <- 3
        } else if(trend_rGLEAM_80[x,y] == 2 & sign_rGLEAM_80[x,y] == 1){ #increasing & significiant
          sign_trend_rGLEAM_8201[x,y] <- 4
        }
      } else {
        sign_trend_rGLEAM_8201[x,y] <- 4.6 #trend or sign. missing
      }
    } else {
      sign_trend_rGLEAM_8201[x,y] <- 4.3 #no study area
    }
  }
}

sign_trend_rGLEAM_0220 <- array(NaN,c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      if(!is.na(sign_rGLEAM_00[x,y]) & !is.na(trend_rGLEAM_00[x,y])){
        if(trend_rGLEAM_00[x,y] == 1 & sign_rGLEAM_00[x,y] == 1){ #decreasing & significant
          sign_trend_rGLEAM_0220[x,y] <- 1
        } else if(trend_rGLEAM_00[x,y] == 1 & sign_rGLEAM_00[x,y] == 2){ #decreasing & insignificant
          sign_trend_rGLEAM_0220[x,y] <- 2
        } else if(trend_rGLEAM_00[x,y] == 2 & sign_rGLEAM_00[x,y] == 2){ #increasing & insignificant
          sign_trend_rGLEAM_0220[x,y] <- 3
        } else if(trend_rGLEAM_00[x,y] == 2 & sign_rGLEAM_00[x,y] == 1){ #increasing & significiant
          sign_trend_rGLEAM_0220[x,y] <- 4
        }
      } else{
        sign_trend_rGLEAM_0220[x,y] <- 4.6 #trend or sign missing
      }
    } else{
      sign_trend_rGLEAM_0220[x,y] <- 4.3 #no study area
    }
  }
}

area_insigposrGLEAM_80 <- (sum(area.array[which(sign_trend_rGLEAM_8201 == 3)], na.rm=T))/total_land_area
area_sigposrGLEAM_80 <- (sum(area.array[which(sign_trend_rGLEAM_8201 == 4)], na.rm=T))/total_land_area
area_signegrGLEAM_80 <- (sum(area.array[which(sign_trend_rGLEAM_8201 == 1)], na.rm=T))/total_land_area
area_insignegrGLEAM_80 <- (sum(area.array[which(sign_trend_rGLEAM_8201 == 2)], na.rm=T))/total_land_area

area_insigposrGLEAM_00 <- (sum(area.array[which(sign_trend_rGLEAM_0220 == 3)], na.rm=T))/total_land_area
area_sigposrGLEAM_00 <- (sum(area.array[which(sign_trend_rGLEAM_0220 == 4)], na.rm=T))/total_land_area
area_signegrGLEAM_00 <- (sum(area.array[which(sign_trend_rGLEAM_0220 == 1)], na.rm=T))/total_land_area
area_insignegrGLEAM_00 <- (sum(area.array[which(sign_trend_rGLEAM_0220 == 2)], na.rm=T))/total_land_area

png("path/to/your/storage/fig_S2c.png", width = 600, height=350)
par(mar=c(2,2,2,0.5))
image.plot(lon, lat[61:331], sign_trend_rGLEAM_8201[,61:331], 
      col=c("peru", "moccasin", "deepskyblue", "dodgerblue"), 
      breaks=c(0,1.01,2.01,3.01,4.01), 
      axis.args=list(at=c(0.5, 1.5, 2.5, 3.5), labels=c("- rGLEAM*", "- rGLEAM", "+ rGLEAM", "+ rGLEAM*")), 
      main="Trend 1982-2001", axes = F)
      axis(2, at=c(-50, -25, 0, 25, 50, 75), las=1) 
      axis(1)
      box(col="black") 
maps::map("world", interior=F, add=T)
par(fig = c(0.08,0.27, 0.1, 0.5), new = T, mar=c(1,2,2,0.5))
barplot(height=c(area_sigposrGLEAM_80*100, area_insigposrGLEAM_80*100, area_insignegrGLEAM_80*100, area_signegrGLEAM_80*100), 
      ylim=c(0,90), 
      col=c("dodgerblue", "deepskyblue", "moccasin", "peru"), 
      width=0.25,
      main="% of land area", cex.main=0.7)
text(c(0.175,0.47,0.77,1.1), 
      c(area_sigposrGLEAM_80*100+10, area_insigposrGLEAM_80*100+10, area_insignegrGLEAM_80*100+10, area_signegrGLEAM_80*100+10), 
      c(round(area_sigposrGLEAM_80*100), round(area_insigposrGLEAM_80*100), round(area_insignegrGLEAM_80*100), round(area_signegrGLEAM_80*100)))
dev.off()

png("path/to/your/storage/fig_S2d.png", width = 600, height=350)
par(mar=c(2,2,2,0.5))
image.plot(lon, lat[61:331], sign_trend_rGLEAM_0220[,61:331], 
      col=c("peru", "moccasin", "deepskyblue", "dodgerblue"), 
      breaks=c(0,1.01,2.01,3.01,4.01), axis.args=list(at=c(0.5, 1.5, 2.5, 3.5), labels=c("- rGLEAM*", "- rGLEAM", "+ rGLEAM", "+ rGLEAM*")), 
      main="Trend 2002-2020", axes = F)
      axis(2, at=c(-50, -25, 0, 25, 50, 75), las=1) 
      axis(1)
      box(col="black") 
maps::map("world", interior=F, add=T)
par(fig = c(0.08,0.27, 0.1, 0.5), new = T, mar=c(1,2,2,0.5))
barplot(height=c(area_sigposrGLEAM_00*100, area_insigposrGLEAM_00*100, area_insignegrGLEAM_00*100, area_signegrGLEAM_00*100), 
      ylim=c(0,90), 
      col=c("dodgerblue", "deepskyblue", "moccasin", "peru"), 
      width=0.25,
      main="% of land area", cex.main=0.7)
text(c(0.175,0.47,0.77,1.1), 
      c(area_sigposrGLEAM_00*100+10, area_insigposrGLEAM_00*100+10, area_insignegrGLEAM_00*100+10, area_signegrGLEAM_00*100+10), 
      c(round(area_sigposrGLEAM_00*100), round(area_insigposrGLEAM_00*100), round(area_insignegrGLEAM_00*100), round(area_signegrGLEAM_00*100)))
dev.off()


#VPD#

#derive direction of trend for the 1st and 2nd period
trend_ERA5_VPD_80 <- array(NaN, c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      if(mERA5_VPD80[x,y] > mERA5_VPD90[x,y]){
        trend_ERA5_VPD_80[x,y] <- 1 #decreasing
      }
      if(mERA5_VPD80[x,y] < mERA5_VPD90[x,y]){
        trend_ERA5_VPD_80[x,y] <- 2 #increasing
      }
    } else {
      trend_ERA5_VPD_80[x,y] <- 2.3 #no study area
    }
  }
}

trend_ERA5_VPD_00 <- array(NaN, c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      if(mERA5_VPD00[x,y] > mERA5_VPD10[x,y]){
        trend_ERA5_VPD_00[x,y] <- 1 #decreasing
      }
      if(mERA5_VPD00[x,y] < mERA5_VPD10[x,y]){
        trend_ERA5_VPD_00[x,y] <- 2 #increasing
      }
    } else {
      trend_ERA5_VPD_00[x,y] <- 2.3 #no study area 
    }
  }
}

#For checking the significance the difference between  VPD in the first half vs 
#the second half of the 20/19 year period, it is evaluated against a distribution of differences 
#built based on the mean of randomly selected years out of the 20/19-year period
sign_ERA5_VPD_80 <- array(NaN, c(720, 360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      t <- c()
      t <- unname(quantile(replicate(300, random_trend(amERA5_VPD_8201[x,y,])), probs=c(0.05, 0.95), na.rm=T))
      if(diff_ERA5_VPD_8090[x,y] >= t[2] | diff_ERA5_VPD_8090[x,y] <= t[1]){
        sign_ERA5_VPD_80[x,y] <- 1
      } else if (diff_ERA5_VPD_8090[x,y] < t[2] & diff_ERA5_VPD_8090[x,y] > t[1]){
        sign_ERA5_VPD_80[x,y] <- 2
      }
    } else {
      sign_ERA5_VPD_80[x,y] <- 2.3 #no study area
    }
  }
  print(x)
}

sign_ERA5_VPD_00 <- array(NaN, c(720, 360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      t <- c()
      t <- unname(quantile(replicate(300, random_trend(amERA5_VPD_0220[x,y,], min = 19)), probs=c(0.05, 0.95), na.rm=T))
      if(diff_ERA5_VPD_0010[x,y] >= t[2] | diff_ERA5_VPD_0010[x,y] <= t[1]){
        sign_ERA5_VPD_00[x,y] <- 1 #significant
      } else if (diff_ERA5_VPD_0010[x,y] < t[2] & diff_ERA5_VPD_0010[x,y] > t[1]){
        sign_ERA5_VPD_00[x,y] <- 2 #insignificant
      }
    } else {
      sign_ERA5_VPD_00[x,y] <- 2.3 # no study area 
    }
  }
  print(x)
}

#create categories based on trend direction and significance
sign_trend_ERA5_VPD_8201 <- array(NaN,c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      if(!is.na(sign_ERA5_VPD_80[x,y]) & !is.na(trend_ERA5_VPD_80[x,y])){
        if(trend_ERA5_VPD_80[x,y] == 1 & sign_ERA5_VPD_80[x,y] == 1){ #decreasing & significant
          sign_trend_ERA5_VPD_8201[x,y] <- 1
        } else if(trend_ERA5_VPD_80[x,y] == 1 & sign_ERA5_VPD_80[x,y] == 2){ #decreasing & insignificant
          sign_trend_ERA5_VPD_8201[x,y] <- 2
        } else if(trend_ERA5_VPD_80[x,y] == 2 & sign_ERA5_VPD_80[x,y] == 2){ #increasing & insignificant
          sign_trend_ERA5_VPD_8201[x,y] <- 3
        } else if(trend_ERA5_VPD_80[x,y] == 2 & sign_ERA5_VPD_80[x,y] == 1){ #increasing & significiant
          sign_trend_ERA5_VPD_8201[x,y] <- 4
        }
      } else {
        sign_trend_ERA5_VPD_8201[x,y] <- 4.6 #no trend or significance
      }
    } else{
      sign_trend_ERA5_VPD_8201[x,y] <- 4.3 # no study area 
    }
  }
}

sign_trend_ERA5_VPD_0220 <- array(NaN,c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      if(!is.na(sign_ERA5_VPD_00[x,y]) & !is.na(trend_ERA5_VPD_00[x,y])){
        if(trend_ERA5_VPD_00[x,y] == 1 & sign_ERA5_VPD_00[x,y] == 1){ #decreasing & significant
          sign_trend_ERA5_VPD_0220[x,y] <- 1
        } else if(trend_ERA5_VPD_00[x,y] == 1 & sign_ERA5_VPD_00[x,y] == 2){ #decreasing & insignificant
          sign_trend_ERA5_VPD_0220[x,y] <- 2
        } else if(trend_ERA5_VPD_00[x,y] == 2 & sign_ERA5_VPD_00[x,y] == 2){ #increasing & insignificant
          sign_trend_ERA5_VPD_0220[x,y] <- 3
        } else if(trend_ERA5_VPD_00[x,y] == 2 & sign_ERA5_VPD_00[x,y] == 1){ #increasing & significiant
          sign_trend_ERA5_VPD_0220[x,y] <- 4
        }
      } else{
        sign_trend_ERA5_VPD_0220[x,y] <- 4.6 #no trend or sign.
      }
    } else{
      sign_trend_ERA5_VPD_0220[x,y] <- 4.3 #no study area 
    }
  }
}

area_insigposERA5_VPD_80 <- (sum(area.array[which(sign_trend_ERA5_VPD_8201 == 3)], na.rm=T))/total_land_area
area_sigposERA5_VPD_80 <- (sum(area.array[which(sign_trend_ERA5_VPD_8201 == 4)], na.rm=T))/total_land_area
area_signegERA5_VPD_80 <- (sum(area.array[which(sign_trend_ERA5_VPD_8201 == 1)], na.rm=T))/total_land_area
area_insignegERA5_VPD_80 <- (sum(area.array[which(sign_trend_ERA5_VPD_8201 == 2)], na.rm=T))/total_land_area

area_insigposERA5_VPD_00 <- (sum(area.array[which(sign_trend_ERA5_VPD_0220 == 3)], na.rm=T))/total_land_area
area_sigposERA5_VPD_00 <- (sum(area.array[which(sign_trend_ERA5_VPD_0220 == 4)], na.rm=T))/total_land_area
area_signegERA5_VPD_00 <- (sum(area.array[which(sign_trend_ERA5_VPD_0220 == 1)], na.rm=T))/total_land_area
area_insignegERA5_VPD_00 <- (sum(area.array[which(sign_trend_ERA5_VPD_0220 == 2)], na.rm=T))/total_land_area

png("path/to/your/storage/fig_S2e.png", width = 600, height=350)
par(mar=c(2,2,2,0.5))
image.plot(lon, lat[61:331], sign_trend_ERA5_VPD_8201[,61:331], 
      col=c("dodgerblue", "deepskyblue", "moccasin", "peru"), 
      breaks=c(0,1.01,2.01,3.01,4.01), 
      axis.args=list(at=c(0.5, 1.5, 2.5, 3.5), labels=c("- ERA5_VPD*", "- ERA5_VPD", "+ ERA5_VPD", "+ ERA5_VPD*")), 
      main="Trend 1982-2001", axes = F)
      axis(2, at=c(-50, -25, 0, 25, 50, 75), las=1) 
      axis(1)
      box(col="black") 
maps::map("world", interior=F, add=T)
par(fig = c(0.08,0.27, 0.1, 0.5), new = T, mar=c(1,2,2,0.5))
barplot(height=c(area_signegERA5_VPD_80*100, area_insignegERA5_VPD_80*100, area_insigposERA5_VPD_80*100, area_sigposERA5_VPD_80*100), 
      ylim=c(0,90), 
      col=c("dodgerblue", "deepskyblue", "moccasin", "peru"), 
      width=0.25,
      main="% of land area", cex.main=0.7)
text(c(0.175,0.47,0.77,1.1), 
      c(area_signegERA5_VPD_80*100+10, area_insignegERA5_VPD_80*100+10, area_insigposERA5_VPD_80*100+10, area_sigposERA5_VPD_80*100+10), 
      c(round(area_signegERA5_VPD_80*100), round(area_insignegERA5_VPD_80*100), round(area_insigposERA5_VPD_80*100), round(area_sigposERA5_VPD_80*100)))
dev.off()

png("path/to/your/storage/fig_S2f.png", width = 600, height=350)
par(mar=c(2,2,2,0.5))
image.plot(lon, lat[61:331], sign_trend_ERA5_VPD_0220[,61:331], 
      col=c("dodgerblue", "deepskyblue", "moccasin", "peru"), 
      breaks=c(0,1.01,2.01,3.01,4.01), 
      axis.args=list(at=c(0.5, 1.5, 2.5, 3.5), labels=c("- ERA5_VPD*", "- ERA5_VPD", "+ ERA5_VPD", "+ ERA5_VPD*")), 
      main="Trend 2002-2020", axes = F)
      axis(2, at=c(-50, -25, 0, 25, 50, 75), las=1) 
      axis(1)
      box(col="black") 
maps::map("world", interior=F, add=T)
par(fig = c(0.08,0.27, 0.1, 0.5), new = T, mar=c(1,2,2,0.5))
barplot(height=c(area_signegERA5_VPD_00*100, area_insignegERA5_VPD_00*100, area_insigposERA5_VPD_00*100, area_sigposERA5_VPD_00*100), 
      ylim=c(0,90), 
      col=c("dodgerblue", "deepskyblue", "moccasin", "peru"), 
      width=0.25,
      main="% of land area", cex.main=0.7)
text(c(0.175,0.47,0.77,1.1), 
      c(area_signegERA5_VPD_00*100+10, area_insignegERA5_VPD_00*100+10, area_insigposERA5_VPD_00*100+10, area_sigposERA5_VPD_00*100+10), 
      c(round(area_signegERA5_VPD_00*100), round(area_insignegERA5_VPD_00*100), round(area_insigposERA5_VPD_00*100), round(area_sigposERA5_VPD_00*100)))
dev.off()


# Dryness/Aridity Index
lambda<-2.45*10^6 #evaporation conversion factor for water in J/kg

y_MSWEPtp_1st <- abind(y_MSWEPtp_80*lambda, y_MSWEPtp_90*lambda)
y_MSWEPtp_2nd <- abind(y_MSWEPtp_00*lambda, y_MSWEPtp_10*lambda)

y_snr_1st <- abind(y_snr_80, y_snr_90)
y_snr_2nd <- abind(y_snr_00, y_snr_10)

year_idx_1st <- as.numeric(abind(y_80_idx, y_90_idx))
year_idx_2nd <- as.numeric(abind(y_00_idx, y_10_idx))

#just execute once
rndm_years_1st <- replicate(300,sample(year_idx_1st, 10, replace=F)) #out of the 20 year period, select 10 years randomly, repeat 300 times
rndm_years_2nd <- replicate(300,sample(year_idx_2nd, 10, replace=F)) #out of the 19 year period, select 10 years randomly, repeat 300 times

valid_indices_1st <- array(NaN,c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(sum(!is.na(y_snr_1st[x,y,])) == 20 & sum(!is.na(y_MSWEPtp_1st[x,y,])) == 20 &!is.na(study_mask[x,y])){
      valid_indices_1st[x,y] <- 1
    }
  }
  print(x)
}
image.plot(valid_indices_1st[,])

valid_indices_2nd <- array(NaN,c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(sum(!is.na(y_snr_2nd[x,y,])) == 19 & sum(!is.na(y_MSWEPtp_2nd[x,y,])) == 19 & !is.na(study_mask[x,y])){
      valid_indices_2nd[x,y] <- 1
    }
  }
  print(x)
}
image.plot(valid_indices_2nd[,])


#The following part uses parallel computation
# Number of cores to use
num_cores <- 8  # Adjust according to your system

# Register parallel backend
registerDoParallel(cores = num_cores)


# Compute dryness for valid grid cells across multiple years in parallel
AIidx_rndm_1st <- foreach(x = 1:720, .combine = "c") %:% #calculate dryness across randomly selected 10years out of the first 20-year period
  foreach(y = 1:360, .combine = "c") %dopar% {
    if (!is.na(valid_indices_1st[x, y])) {
      AI_idx <- rep(NaN, 300)
      for (s in 1:300) {
        years <- which(year_idx_1st %in% rndm_years_1st[, s])
        AI_idx[s] <- mean(y_snr_1st[x,y,years])/mean(y_MSWEPtp_1st[x,y,years])
      }
      AI_idx
    } else {
      rep(NaN, 300)
    }
  }

AIidx_rndm_1st_nonsel <- foreach(x = 1:720, .combine = "c") %:% #calculate dryness across the non selected 10 years out of the first 20year period
  foreach(y = 1:360, .combine = "c") %dopar% {
    if (!is.na(valid_indices_1st[x, y])) {
      AI_idx <- rep(NaN, 300)
      for (s in 1:300) {
        years <- which(year_idx_1st %in% setdiff(year_idx_1st, rndm_years_1st[, s]))
        AI_idx[s] <- mean(y_snr_1st[x,y,years])/mean(y_MSWEPtp_1st[x,y,years])
      }
      AI_idx
    } else {
      rep(NaN, 300)
    }
  }


AIidx_rndm_2nd <- foreach(x = 1:720, .combine = "c") %:% #calculate dryness across randomly selected 10years out of the second 19-year period
  foreach(y = 1:360, .combine = "c") %dopar% {
    if (!is.na(valid_indices_2nd[x, y])) {
      AI_idx <- rep(NaN, 300)
      for (s in 1:300) {
        years <- which(year_idx_2nd %in% rndm_years_2nd[, s])
        AI_idx[s] <- mean(y_snr_2nd[x,y,years])/mean(y_MSWEPtp_2nd[x,y,years])
      }
      AI_idx
    } else {
      rep(NaN, 300)
    }
  }

AIidx_rndm_2nd_nonsel <- foreach(x = 1:720, .combine = "c") %:% #calculate dryness across the non selected 9 years out of the second 19year period
  foreach(y = 1:360, .combine = "c") %dopar% {
    if (!is.na(valid_indices_2nd[x, y])) {
      AI_idx <- rep(NaN, 300)
      for (s in 1:300) {
        years <- which(year_idx_2nd %in% setdiff(year_idx_2nd, rndm_years_2nd[, s]))
        AI_idx[s] <- mean(y_snr_2nd[x,y,years])/mean(y_MSWEPtp_2nd[x,y,years])
      }
      AI_idx
    } else {
      rep(NaN, 300)
    }
  }


#Deregister parallel backend
stopImplicitCluster()
## The part with parallel computation ends here

#Reshaping the array to the common dimension and order (=720,360)
AI_rndm_1st_ar <- aperm(array(AIidx_rndm_1st, c(300, 360, 720)), c(3,2,1))
AI_rndm_1st_ar_nonsel <- aperm(array(AIidx_rndm_1st_nonsel, c(300, 360, 720)), c(3,2,1))

AI_rndm_2nd_ar <- aperm(array(AIidx_rndm_2nd, c(300, 360, 720)), c(3,2,1))
AI_rndm_2nd_ar_nonsel <- aperm(array(AIidx_rndm_2nd_nonsel, c(300, 360, 720)), c(3,2,1))


#Exclude every dryness index > 10, as it is unrealistic
AI_rndm_1st_ar[which(AI_rndm_1st_ar > 10)] <- NaN
AI_rndm_1st_ar_nonsel[which(AI_rndm_1st_ar_nonsel > 10)] <- NaN
AI_rndm_2nd_ar[which(AI_rndm_2nd_ar > 10)] <- NaN
AI_rndm_2nd_ar_nonsel[which(AI_rndm_2nd_ar_nonsel > 10)] <- NaN

image.plot(AI_rndm_1st_ar[,,1], breaks=c(0, 0.5, 1, 2, 4, 8, 10), col=c("forestgreen", "olivedrab3", "khaki3", "burlywood3", "burlywood4", "tan4"))

#Derive the trend direction
AI_trend_1st <- AI_rndm_1st_ar_nonsel - AI_rndm_1st_ar
AI_trend_2nd <- AI_rndm_2nd_ar_nonsel - AI_rndm_2nd_ar

#Compute decadal mean precipitation
d_MSWEPtp_80 <- 
  d_MSWEPtp_90 <-
  d_MSWEPtp_00 <-
  d_MSWEPtp_10 <- array(NaN, c(720, 360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      d_MSWEPtp_80[x,y] <- mean(y_MSWEPtp_80[x,y,], na.rm=T)
      d_MSWEPtp_90[x,y] <- mean(y_MSWEPtp_90[x,y,], na.rm=T)
      d_MSWEPtp_00[x,y] <- mean(y_MSWEPtp_00[x,y,], na.rm=T)
      d_MSWEPtp_10[x,y] <- mean(y_MSWEPtp_10[x,y,], na.rm=T)
    }
  }
}

#compute decadal mean net radiation
d_snr_80 <- 
  d_snr_90 <-
  d_snr_00 <-
  d_snr_10 <- array(NaN, c(720, 360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      d_snr_80[x,y] <- mean(y_snr_80[x,y,], na.rm=T)
      d_snr_90[x,y] <- mean(y_snr_90[x,y,], na.rm=T)
      d_snr_00[x,y] <- mean(y_snr_00[x,y,], na.rm=T)
      d_snr_10[x,y] <- mean(y_snr_10[x,y,], na.rm=T)
    }
  }
}

#Unit transformation of precipitation to radiation unit
d_MSWEPtp_80_ua <- d_MSWEPtp_80*lambda
d_MSWEPtp_90_ua <- d_MSWEPtp_90*lambda
d_MSWEPtp_00_ua <- d_MSWEPtp_00*lambda
d_MSWEPtp_10_ua <- d_MSWEPtp_10*lambda

#for dryness index divide net radiation by unit adjusted precipitation
d_AI_80 <- d_snr_80/d_MSWEPtp_80_ua
d_AI_90 <- d_snr_90/d_MSWEPtp_90_ua
d_AI_00 <- d_snr_00/d_MSWEPtp_00_ua
d_AI_10 <- d_snr_10/d_MSWEPtp_10_ua

#exclude dryness indices with a value > 10, as they resample unrealistic conditions
d_AI_80[which(d_AI_80 > 10)] <- NaN
d_AI_90[which(d_AI_90 > 10)] <- NaN
d_AI_00[which(d_AI_00 > 10)] <- NaN
d_AI_10[which(d_AI_10 > 10)] <- NaN

#check for difference between dryness index in the two 10-year periods of one 20-year period
trend_AI_80 <- array(NaN, c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(d_AI_80[x,y]) & !is.na(d_AI_90[x,y])){
      if(d_AI_80[x,y] > d_AI_90[x,y]){
        trend_AI_80[x,y] <- 1 #decreasing
      }
      if(d_AI_80[x,y] < d_AI_90[x,y]){
        trend_AI_80[x,y] <- 2 #increasing
      }
    }
  }
}

#For checking the significance the difference between dryness index in the first half vs 
#the second half of the 20 year period is evaluated against a distribution of differences 
#built based on the mean of randomly selected years out of the 20-year period
diff_AI_8090 <- d_AI_90 - d_AI_80
diff_AI_0010 <- d_AI_10 - d_AI_00

sign_AI_1st <- array(NaN, c(720, 360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      if(!is.na(diff_AI_8090[x,y]) & sum(!is.na(AI_trend_1st[x,y,])) != 0){
        t <- c()
        t <- unname(quantile(AI_trend_1st[x,y,], probs=c(0.05, 0.95), na.rm=T))
        if(diff_AI_8090[x,y] > t[2] | diff_AI_8090[x,y] < t[1]){
          sign_AI_1st[x,y] <- 1 #significant
        } else if(diff_AI_8090[x,y] < t[2] & diff_AI_8090[x,y] > t[1]){
          sign_AI_1st[x,y] <- 2 #insignificant
        }
      } else{
        sign_AI_1st[x,y] <- 2.6 #missing trend or sign. 
      }
    } else {
      sign_AI_1st[x,y] <- 2.3 #not study area
    }
  }
  print(x)
}


#derive direction of trend for the 2nd period
trend_AI_00 <- array(NaN, c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(d_AI_00[x,y]) & !is.na(d_AI_10[x,y])){
      if(d_AI_00[x,y] > d_AI_10[x,y]){
        trend_AI_00[x,y] <- 1 #decreasing
      }
      if(d_AI_00[x,y] < d_AI_10[x,y]){
        trend_AI_00[x,y] <- 2 #increasing
      }
    }
  }
}

#assess significance of the trend for the 2nd period
sign_AI_2nd <- array(NaN, c(720, 360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      if(!is.na(diff_AI_0010[x,y]) & sum(!is.na(AI_trend_2nd[x,y,])) != 0){
        t <- c()
        t <- unname(quantile(AI_trend_2nd[x,y,], probs=c(0.05, 0.95), na.rm=T))
        if(diff_AI_0010[x,y] >= t[2] | diff_AI_0010[x,y] <= t[1]){
          sign_AI_2nd[x,y] <- 1 #significant
        } else if (diff_AI_0010[x,y] < t[2] & diff_AI_0010[x,y] > t[1]){
          sign_AI_2nd[x,y] <- 2 #insignificant
        }
      }else{
        sign_AI_2nd[x,y] <- 2.6 #missing trend or sign. 
      }
    } else {
      sign_AI_2nd[x,y] <- 2.3 #not study area
    }
  }
  print(x)
}


#create categories based on trend direction and significance
sign_trend_AI_8201 <- array(NaN,c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      if(!is.na(sign_AI_1st[x,y]) & !is.na(trend_AI_80[x,y])){
        if(trend_AI_80[x,y] == 1 & sign_AI_1st[x,y] == 1){ #decreasing & significant
          sign_trend_AI_8201[x,y] <- 1
        } else if(trend_AI_80[x,y] == 1 & sign_AI_1st[x,y] == 2){ #decreasing & insignificant
          sign_trend_AI_8201[x,y] <- 2
        } else if(trend_AI_80[x,y] == 2 & sign_AI_1st[x,y] == 2){ #increasing & insignificant
          sign_trend_AI_8201[x,y] <- 3
        } else if(trend_AI_80[x,y] == 2 & sign_AI_1st[x,y] == 1){ #increasing & significiant
          sign_trend_AI_8201[x,y] <- 4
        }
      } else{
        sign_trend_AI_8201[x,y] <- 4.6 #no trend or significance
      }
    } else{
      sign_trend_AI_8201[x,y] <- 4.3 # not study area
    }
  }
}

sign_trend_AI_0220 <- array(NaN,c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      if(!is.na(sign_AI_2nd[x,y]) & !is.na(trend_AI_00[x,y])){
        if(trend_AI_00[x,y] == 1 & sign_AI_2nd[x,y] == 1){ #decreasing & significant
          sign_trend_AI_0220[x,y] <- 1
        } else if(trend_AI_00[x,y] == 1 & sign_AI_2nd[x,y] == 2){ #decreasing & insignificant
          sign_trend_AI_0220[x,y] <- 2
        } else if(trend_AI_00[x,y] == 2 & sign_AI_2nd[x,y] == 2){ #increasing & insignificant
          sign_trend_AI_0220[x,y] <- 3
        } else if(trend_AI_00[x,y] == 2 & sign_AI_2nd[x,y] == 1){ #increasing & significiant
          sign_trend_AI_0220[x,y] <- 4
        }
      } else{
        sign_trend_AI_0220[x,y] <- 4.6
      }
    } else{
      sign_trend_AI_0220[x,y] <- 4.3
    }
  }
}

#derive portion of total land area in which each trend 
area_insigposAI_80 <- (sum(area.array[which(sign_trend_AI_8201 == 3)], na.rm=T))/total_land_area
area_sigposAI_80 <- (sum(area.array[which(sign_trend_AI_8201 == 4)], na.rm=T))/total_land_area
area_signegAI_80 <- (sum(area.array[which(sign_trend_AI_8201 == 1)], na.rm=T))/total_land_area
area_insignegAI_80 <- (sum(area.array[which(sign_trend_AI_8201 == 2)], na.rm=T))/total_land_area

area_insigposAI_00 <- (sum(area.array[which(sign_trend_AI_0220 == 3)], na.rm=T))/total_land_area
area_sigposAI_00 <- (sum(area.array[which(sign_trend_AI_0220 == 4)], na.rm=T))/total_land_area
area_signegAI_00 <- (sum(area.array[which(sign_trend_AI_0220 == 1)], na.rm=T))/total_land_area
area_insignegAI_00 <- (sum(area.array[which(sign_trend_AI_0220 == 2)], na.rm=T))/total_land_area

#plot and save the figures
png("path/to/your/storage/fig_S2g.png", width = 600, height=350)
par(mar=c(2,2,2,0.5))
image.plot(lon, lat[61:331], sign_trend_AI_8201[,61:331], #plot only area between 60S and 75N
      col=c("dodgerblue", "deepskyblue", "moccasin", "peru"), 
      breaks=c(0,1.01,2.01,3.01,4.01), 
      axis.args=list(at=c(0.5, 1.5, 2.5, 3.5), 
      labels=c("- AI*", "- AI", "+ AI", "+ AI*")), 
      main="1982-2001", axes = F)
      axis(2, at=c(-50, -25, 0, 25, 50, 75), las=1) 
      axis(1)
      box(col="black") 
maps::map("world", interior=F, add=T)
par(fig = c(0.08,0.27, 0.1, 0.5), new = T, mar=c(1,2,2,0.5))
barplot(height=c(area_signegAI_80*100, area_insignegAI_80*100, area_insigposAI_80*100, area_sigposAI_80*100), 
      ylim=c(0,90), 
      col=c("dodgerblue", "deepskyblue", "moccasin", "peru"), 
      width=0.25,
      main="% of land area", cex.main=0.7)
text(c(0.175,0.47,0.77,1.1), c(area_signegAI_80*100+10, area_insignegAI_80*100+10, area_insigposAI_80*100+10, area_sigposAI_80*100+10), 
      c(round(area_signegAI_80*100), round(area_insignegAI_80*100), round(area_insigposAI_80*100), round(area_sigposAI_80*100)))
dev.off()

png("path/to/your/storage/fig_S2h.png", width = 600, height=350)
par(mar=c(2,2,2,0.5))
image.plot(lon, lat[61:331], sign_trend_AI_0220[,61:331], 
      col=c("dodgerblue", "deepskyblue", "moccasin", "peru"), 
      breaks=c(0,1.01,2.01,3.01,4.01), axis.args=list(at=c(0.5, 1.5, 2.5, 3.5), 
      labels=c("- AI*", "- AI", "+ AI", "+ AI*")), 
      main="2002-2020", axes = F)
      axis(2, at=c(-50, -25, 0, 25, 50, 75), las=1) 
      axis(1)
      box(col="black") 
maps::map("world", interior=F, add=T)
par(fig = c(0.08,0.27, 0.1, 0.5), new = T, mar=c(1,2,2,0.5))
barplot(height=c(area_signegAI_00*100, area_insignegAI_00*100, area_insigposAI_00*100, area_sigposAI_00*100), 
      ylim=c(0,90), 
      col=c("dodgerblue", "deepskyblue", "moccasin", "peru"), 
      width=0.25,
      main="% of land area", cex.main=0.7)
text(c(0.175,0.47,0.77,1.1), c(area_signegAI_00*100+10, area_insignegAI_00*100+10, area_insigposAI_00*100+10, area_sigposAI_00*100+10), 
      c(round(area_signegAI_00*100), round(area_insignegAI_00*100), round(area_insigposAI_00*100), round(area_sigposAI_00*100)))
dev.off()

#####################################################################
###### Spatial correlation of LAI - individual water variables ######

#as we have categorial water variables, we can't use normal correlation but we use Cramer's V
#Create vectors of each array
vec_LAI_trend_8201 <- c(sign_trend_LAI0595_8201)
vec_LAI_trend_8201[which(vec_LAI_trend_8201 > 4)] <- NaN #values >4 are excluded and replaced by NAN as they indicate grid cells with missing data

vec_MSWEPtp_trend_8201 <- c(sign_trend_MSWEP_8201)
vec_MSWEPtp_trend_8201[which(vec_MSWEPtp_trend_8201 > 4)] <- NaN

vec_rGLEAM_trend_8201 <- c(sign_trend_rGLEAM_8201)
vec_rGLEAM_trend_8201[which(vec_rGLEAM_trend_8201 > 4)] <- NaN

vec_ERA5_VPD_trend_8201 <- c(sign_trend_ERA5_VPD_8201)
vec_ERA5_VPD_trend_8201[which(vec_ERA5_VPD_trend_8201 > 4)] <- NaN

vec_AI_trend_8201 <- c(sign_trend_AI_8201)
vec_AI_trend_8201[which(vec_AI_trend_8201 > 4)] <- NaN

vec_LAI_trend_0220 <- c(sign_trend_LAI0595_0220)
vec_LAI_trend_0220[which(vec_LAI_trend_0220 > 4)] <- NaN

vec_MSWEPtp_trend_0220 <- c(sign_trend_MSWEP_0220)
vec_MSWEPtp_trend_0220[which(vec_MSWEPtp_trend_0220 > 4)] <- NaN

vec_rGLEAM_trend_0220 <- c(sign_trend_rGLEAM_0220)
vec_rGLEAM_trend_0220[which(vec_rGLEAM_trend_0220 > 4)] <- NaN

vec_ERA5_VPD_trend_0220 <- c(sign_trend_ERA5_VPD_0220)
vec_ERA5_VPD_trend_0220[which(vec_ERA5_VPD_trend_0220 > 4)] <- NaN

vec_AI_trend_0220 <- c(sign_trend_AI_0220)
vec_AI_trend_0220[which(vec_AI_trend_0220 > 4)] <- NaN

#Compute cramers V
cramersv(na.omit(data.frame(vec_LAI_trend_8201, vec_MSWEPtp_trend_8201)))
cramersv(na.omit(data.frame(vec_LAI_trend_8201, vec_rGLEAM_trend_8201)))
cramersv(na.omit(data.frame(vec_LAI_trend_8201, vec_ERA5_VPD_trend_8201)))
cramersv(na.omit(data.frame(vec_LAI_trend_8201, vec_AI_trend_8201)))

cramersv(data.frame(vec_LAI_trend_0220, vec_MSWEPtp_trend_0220))
cramersv(data.frame(vec_LAI_trend_0220, vec_rGLEAM_trend_0220))
cramersv(data.frame(vec_LAI_trend_0220, vec_ERA5_VPD_trend_0220))
cramersv(data.frame(vec_LAI_trend_0220, vec_AI_trend_0220))


###################################
###### Water trend agreement ######

posdecr_trend <- array(NaN, c(720,360)) #number of data sets per grid cell agreeing on significant drying (VPD and dryness = 4, SM and precip = 1)
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
        posdecr_trend[x,y] <- length(which(c(sign_trend_ERA5_VPD_8201[x,y] == 4, sign_trend_AI_8201[x,y] == 4, sign_trend_MSWEP_8201[x,y] == 1, sign_trend_rGLEAM_8201[x,y] == 1) == T))
    }
  }
  print(x)
}


posdecr_trend_2002 <- array(NaN, c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      posdecr_trend_2002[x,y] <- length(which(c(sign_trend_ERA5_VPD_0220[x,y] ==4, sign_trend_AI_0220[x,y] ==4, sign_trend_MSWEP_0220[x,y] ==1, sign_trend_rGLEAM_0220[x,y]==1) == T))
    } 
  }
  print(x)
}


#total land area in which X/4 data sets agree on drying
area_1_80 <- (sum(area.array[which(posdecr_trend == 1)], na.rm=T))/total_land_area
area_2_80 <- (sum(area.array[which(posdecr_trend == 2)], na.rm=T))/total_land_area
area_3_80 <- (sum(area.array[which(posdecr_trend == 3)], na.rm=T))/total_land_area
area_4_80 <- (sum(area.array[which(posdecr_trend == 4)], na.rm=T))/total_land_area
area_0_80 <- (sum(area.array[which(posdecr_trend == 0)], na.rm=T))/total_land_area

area_1_00 <- (sum(area.array[which(posdecr_trend_2002 == 1)], na.rm=T))/total_land_area
area_2_00 <- (sum(area.array[which(posdecr_trend_2002 == 2)], na.rm=T))/total_land_area
area_3_00 <- (sum(area.array[which(posdecr_trend_2002 == 3)], na.rm=T))/total_land_area
area_4_00 <- (sum(area.array[which(posdecr_trend_2002 == 4)], na.rm=T))/total_land_area
area_0_00 <- (sum(area.array[which(posdecr_trend_2002 == 0)], na.rm=T))/total_land_area


png("path/to/your/storage/fig_1c.png", width=7.5, height=4, units="in", res=1400)
par(mar=c(2,2,1,1))
image.plot(lon, lat[61:331], posdecr_trend[,61:331], 
          breaks = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5), 
          col=c('skyblue3','skyblue1', 'lightblue1', "orange",'saddlebrown'), 
          axis.args=list(at=c(0, 1, 2, 3, 4), labels=c("0/4", "1/4", "2/4", "3/4", "4/4")), 
          legend.args = list(text="Agreement \n btw.",  side=3, line=0, adj=0.2), axes = F)
          axis(2, at=c(-50, -25, 0, 25, 50, 75), las=1) 
      axis(1)
      box(col="black") 
maps::map("world", add=T, interior = F, col='lightgrey')
par(fig = c(0.08,0.27, 0.1, 0.5), new = T, mar=c(1,2,2,0.5))
barplot(height=c(area_0_80*100, area_1_80*100, area_2_80*100, area_3_80*100, area_4_80*100), 
        ylim=c(0,60), 
        col=c('skyblue3','skyblue1', 'lightblue1', "orange",'saddlebrown'), 
        width=0.25, 
        main="% of land area", cex.main=0.7)
text(c(0.175,0.47, 0.765, 1.06, 1.355), 
        c(area_0_80*100 + 5, area_1_80*100 + 5, area_2_80*100 + 5, area_3_80*100 + 5, area_4_80*100 + 5), 
        c(round(area_0_80*100), round(area_1_80*100), round(area_2_80*100), round(area_3_80*100), round(area_4_80*100)))
dev.off()

png("path/to/your/storage/fig_1d.png", width=7.5, height=4, units="in", res=1400)
par(mar=c(2,2,1,1))
image.plot(lon, lat[61:331], posdecr_trend_2002[,61:331], 
          breaks = c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5), 
          col=c('skyblue3','skyblue1', 'lightblue1', "orange",'saddlebrown'), 
          axis.args=list(at=c(0, 1, 2, 3, 4), labels=c("0/4", "1/4", "2/4", "3/4", "4/4")),
          legend.args = list(text="Agreement \n btw.",  side=3, line=0, adj=0.2), axes = F)
          axis(2, at=c(-50, -25, 0, 25, 50, 75), las=1) 
      axis(1)
      box(col="black") 
maps::map("world", add=T, interior = F, col='lightgrey')
par(fig = c(0.08,0.27, 0.1, 0.5), new = T, mar=c(1,2,2,0.5))
barplot(height=c(area_0_00*100, area_1_00*100, area_2_00*100, area_3_00*100, area_4_00*100), 
          ylim=c(0,60), 
          col=c('skyblue3','skyblue1', 'lightblue1', "orange",'saddlebrown'), 
          width=0.25, 
        main="% of land area", cex.main=0.7)
text(c(0.175,0.47, 0.765, 1.06, 1.355), 
        c(area_0_00*100 + 5, area_1_00*100 + 5, area_2_00*100 + 5, area_3_00*100 + 5, area_4_00*100 + 5), 
        c(round(area_0_00*100), round(area_1_00*100), round(area_2_00*100), round(area_3_00*100), round(area_4_00*100)))
dev.off()

#######################################################################
##### LAI trend and agreement with water decline across data sets #####
LAI0595_wat_8201 <- array(NaN, c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      if(!is.na(posdecr_trend[x,y]) & !is.na(sign_trend_LAI0595_8201[x,y])){
        if(posdecr_trend[x,y] >= 3 & sign_trend_LAI0595_8201[x,y]==1){
          LAI0595_wat_8201[x,y] <- 2 #browning, >50% agreement in drying trend
        }  else if(posdecr_trend[x,y] < 3 & posdecr_trend[x,y] != 0 & sign_trend_LAI0595_8201[x,y]==1){
          LAI0595_wat_8201[x,y] <- 1 #browning, <50% agreement in drying trend
        } else{
          LAI0595_wat_8201[x,y] <- 0
        }
      } else {
        LAI0595_wat_8201[x,y] <- 0 #no trend or significance
      }
    }
  }
  print(x)
}

#percentage of total land area that has one of the two categories (> or < 50% drying and browning)
area_brown_m50_8201 <- (sum(area.array[which(LAI0595_wat_8201 == 2)], na.rm=T))/total_land_area
area_brown_l50_8201 <- (sum(area.array[which(LAI0595_wat_8201 == 1)], na.rm=T))/total_land_area


png('path/to/your/storage/fig1_e.png', width=7.5, height=4, units="in", res=1400)
par(mar=c(2,2,1,1))
image.plot(lon, lat[61:331],LAI0595_wat_8201[,61:331], 
    breaks = c(-0.5, 0.5, 1.5, 2.5), 
    col=c( "lightgrey", 'tan', 'sienna'), 
    axis.args=list(at=c(0, 1, 2), 
    labels=c('Other', "-LAI* \n & 1-2 data \n sets drying", "-LAI* \n & 3-4 data \n sets drying"),
    cex.axis=0.58), axes = F)
    axis(2, at=c(-50, -25, 0, 25, 50, 75), las=1) 
      axis(1)
      box(col="black") 
maps::map('world', interior=F, add=T, col='lightgrey')
par(fig = c(0.08,0.27, 0.1, 0.5), new = T, mar=c(1,2,2,0.5))
barplot(height=c(area_brown_m50_8201*100, area_brown_l50_8201*100), 
    ylim=c(0,5), 
    col=c('sienna', 'tan'), 
    width=0.25, 
    main="% of land area", cex.main=0.7)
text(c(0.175,0.47), 
     c(area_brown_m50_8201*100+1, area_brown_l50_8201*100+1), 
     c(round(area_brown_m50_8201*100, 1), round(area_brown_l50_8201*100, 1)))
dev.off()



LAI0595_wat_0220 <- array(NaN, c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      if(!is.na(posdecr_trend_2002[x,y])  & !is.na(sign_trend_LAI0595_0220[x,y])){
        if(posdecr_trend_2002[x,y] >= 3 & sign_trend_LAI0595_0220[x,y]==1){
          LAI0595_wat_0220[x,y] <- 2 #browning, >50% agreement in drying trend
        }else if(posdecr_trend_2002[x,y] < 3 & posdecr_trend_2002[x,y] != 0 & sign_trend_LAI0595_0220[x,y]==1){
          LAI0595_wat_0220[x,y] <- 1 #browning, <50% agreement in drying trend
        }else {
          LAI0595_wat_0220[x,y] <- 0
        }
      } else{
        LAI0595_wat_0220[x,y] <- 0
      }
    }
  }
}

area_brown_m50_0220 <- (sum(area.array[which(LAI0595_wat_0220 == 2)], na.rm=T))/total_land_area
area_brown_l50_0220 <- (sum(area.array[which(LAI0595_wat_0220 == 1)], na.rm=T))/total_land_area

#plotting the maps and adding rectangels around the areas where browning coincides with >= 50% of the data sets agreeing on drying
png('path/to/your/storage/fig_1f.png', width=7.5, height=4, units="in", res=1400)
par(mar=c(2,2,1,1))
image.plot(lon, lat[61:331],LAI0595_wat_0220[,61:331], 
    breaks = c(-0.5, 0.5, 1.5, 2.5), 
    col=c( "lightgrey", 'tan', 'sienna'), 
    axis.args=list(at=c(0, 1, 2), 
    labels=c('Other', "-LAI* \n & 1-2 data \n sets drying", "-LAI* \n & 3-4 data \n sets drying"),
    cex.axis = 0.58), axes = F)
axis(2, at=c(-50, -25, 0, 25, 50, 75), las=1) 
axis(1)
box(col="black") 
maps::map("world", interior=F, add=T, col='lightgrey')
segments(x0 = -48, y0 = -19, x1 = -35, col = "black")
segments(x0 = -48, y0 = -19, y1 = -4, col = "black")
segments(x0 = -48, y0 = -4, x1 = -35, col = "black")
segments(x0 = -35, y0 = -19, y1 = -4, col = "black")

segments(x0 = 13, y0 = -16, x1 = 20, col = "black")
segments(x0 = 13, y0 = -16, y1 = -10, col = "black")
segments(x0 = 13, y0 = -10, x1 = 20, col = "black")
segments(x0 = 20, y0 = -16, y1 = -10, col = "black")

segments(x0 = 100, y0 = 50, x1 = 124, col = "black")
segments(x0 = 100, y0 = 66, y1 = 50, col = "black")
segments(x0 = 100, y0 = 66, x1 = 124, col = "black")
segments(x0 = 124, y0 = 50, y1 = 66, col = "black")


par(fig = c(0.08,0.27, 0.1, 0.5), new = T, mar=c(1,2,2,0.5))
barplot(height=c(area_brown_m50_0220*100, area_brown_l50_0220*100), 
      main="% of land area", cex.main=0.7, 
      ylim=c(0,5), 
      col=c('sienna', 'tan'), 
      width=0.25)
text(c(0.175,0.47), c(area_brown_m50_0220*100+1, area_brown_l50_0220*100+1), c(round(area_brown_m50_0220*100), round(area_brown_l50_0220*100)))
dev.off()


###############################################################
##### Stacked barplot: Drying agreement in browning areas #####

#compute area of X data sets agreeing on drying (== posdecr_trend) in sign. browning regions (sign_trend_LAI0595_8201 ==1)
area_sigbrown_0_80 <- (sum(area.array[which(posdecr_trend == 0 & sign_trend_LAI0595_8201 == 1)], na.rm=T))/total_land_area
area_sigbrown_1_80 <- (sum(area.array[which(posdecr_trend == 1 & sign_trend_LAI0595_8201 == 1)], na.rm=T))/total_land_area
area_sigbrown_2_80 <- (sum(area.array[which(posdecr_trend == 2 & sign_trend_LAI0595_8201 == 1)], na.rm=T))/total_land_area
area_sigbrown_3_80 <- (sum(area.array[which(posdecr_trend == 3 & sign_trend_LAI0595_8201 == 1)], na.rm=T))/total_land_area
area_sigbrown_4_80 <- (sum(area.array[which(posdecr_trend == 4 & sign_trend_LAI0595_8201 == 1)], na.rm=T))/total_land_area

area_sigbrown_0_00 <- (sum(area.array[which(posdecr_trend_2002 == 0 & sign_trend_LAI0595_0220 == 1)], na.rm=T))/total_land_area
area_sigbrown_1_00 <- (sum(area.array[which(posdecr_trend_2002 == 1 & sign_trend_LAI0595_0220 == 1)], na.rm=T))/total_land_area
area_sigbrown_2_00 <- (sum(area.array[which(posdecr_trend_2002 == 2 & sign_trend_LAI0595_0220 == 1)], na.rm=T))/total_land_area
area_sigbrown_3_00 <- (sum(area.array[which(posdecr_trend_2002 == 3 & sign_trend_LAI0595_0220 == 1)], na.rm=T))/total_land_area
area_sigbrown_4_00 <- (sum(area.array[which(posdecr_trend_2002 == 4 & sign_trend_LAI0595_0220 == 1)], na.rm=T))/total_land_area


#Repeat for MODIS data
area_sigbrown_0_00_MOD <- (sum(area.array[which(posdecr_trend_2002 == 0 & sign_trend_MODIS_0220 == 1)], na.rm=T))/total_land_area
area_sigbrown_1_00_MOD <- (sum(area.array[which(posdecr_trend_2002 == 1 & sign_trend_MODIS_0220 == 1)], na.rm=T))/total_land_area
area_sigbrown_2_00_MOD <- (sum(area.array[which(posdecr_trend_2002 == 2 & sign_trend_MODIS_0220 == 1)], na.rm=T))/total_land_area
area_sigbrown_3_00_MOD <- (sum(area.array[which(posdecr_trend_2002 == 3 & sign_trend_MODIS_0220 == 1)], na.rm=T))/total_land_area
area_sigbrown_4_00_MOD <- (sum(area.array[which(posdecr_trend_2002 == 4 & sign_trend_MODIS_0220 == 1)], na.rm=T))/total_land_area

#Create dataframe with colors, labels and observations
drying_sigbrown_80 <- data.frame( Cols = c('skyblue3','skyblue1', 'lightblue1', "orange",'saddlebrown'),
                              Obs = c(area_sigbrown_0_80, area_sigbrown_1_80, area_sigbrown_2_80, area_sigbrown_3_80, area_sigbrown_4_80))
rownames(drying_sigbrown_80) <- c("0/4 drying", "1/4 drying", "2/4 drying", "3/4 drying", "4/4 drying")

drying_sigbrown_00 <- data.frame( Cols = c('skyblue3','skyblue1', 'lightblue1', "orange",'saddlebrown'),
                              Obs = c(area_sigbrown_0_00, area_sigbrown_1_00, area_sigbrown_2_00, area_sigbrown_3_00, area_sigbrown_4_00))
rownames(drying_sigbrown_00) <- c("0/4 drying", "1/4 drying", "2/4 drying", "3/4 drying", "4/4 drying")


#### Lets summarize data from the CMIP6 model analysis
#data is provided by analysis part from Jasper Denissen
r_2 <- raster() # by default 1 by 1 degree 
res(r_2) <- 2 #change resolution to 2 degree
a_2 <- raster::area(r_2) # calculate the area of a 2x2 degree grid from N - S, as area varies only by latitude, not longitude 
area_2 <- a_2[,1] 
area_2.array <- array(NaN,c(180,90)) 
for(x in 1:180){ 
  area_2.array[x,] <- area_2 # this is the array that carries the area per grid cell 
}

image.plot(area_2.array[,])
total_land_area_2 <- 199346889 

lat_2 <- seq(-89, 91, 2) #all latitudes according to a 2° resolution
lat_2_used <- which(lat_2 %in% sort(unique(sign_trend_LAI.df$lat))) #all the latitudes for which there is data in the data frame

# For every latitude in sign_trend_LAI.df add size of the grid cell
lat_to_area <- data.frame(
  lat = sort(unique(sign_trend_LAI.df$lat)),
  area = area_2[lat_2_used]
)


#Agreement of water variables in significantly browning area; show number of water data sets agreeing on significant drying 
#Create a table consisting of the trend for every water variable
water_trends <- sign_trend_vpd.df

water_trends <- water_trends %>% 
  left_join(sign_trend_AI.df, by=c("lat", 'lon', 'period', 'source_id'))

water_trends <- water_trends %>% 
  left_join(sign_trend_mrsol.df, by=c("lat", 'lon', 'period', 'source_id'))

water_trends <- water_trends %>% 
  left_join(sign_trend_pr.df, by=c("lat", 'lon', 'period', 'source_id'))


water_trends <- water_trends[,c(1,2,4,5,6,8,10,12,3,7,9,11)] #Reorder the columns
water_trends$length <- apply(water_trends, 1, function(row) { #calculate row sums to see how many water data sets show decreasing water availability
  sum(row[9] == 4,  # Condition for vpd
      row[10] == 4,  # Condition for AI
      row[11] == 1,  # Condition for mrsol
      row[12] == 1)  # Condition for pr
})

#Combine with the latitudes
water_trends <- water_trends %>%
  left_join(lat_to_area, by = "lat")

#Combine with the LAI trends
LAI_water_trends <- water_trends %>% 
  left_join(sign_trend_LAI.df, by=c('lat', 'lon', 'source_id', 'period'))


#calculate area that is significantly browning and i)0/4 drying, ii)1/4 drying, iii)2/4 drying, iv) 3/4 drying and v) 4/4 drying
watertrend_sigbrown_80 <- LAI_water_trends %>%
  filter(period == c("1982 - 2001") & LAI == 1) %>%
  group_by(length, source_id) %>%
  summarize(area_sum = sum(area))

watertrend_sigbrown_00 <- LAI_water_trends %>%
  filter(period == c("2002 - 2020") & LAI == 1) %>%
  group_by(length, source_id) %>%
  summarize(area_sum = sum(area))

#convert area in to % of total land area
watertrend_sigbrown_00$area_sum <- (watertrend_sigbrown_00$area_sum / total_land_area_2) *100
watertrend_sigbrown_80$area_sum <- (watertrend_sigbrown_80$area_sum / total_land_area_2) *100

drying_sigbrown_00$drying <- c(0:4)
drying_sigbrown_80$drying <- c(0:4)

#reorder data frame (watertrend_sigbrown_80) so that it is sorted by model and number of data sets agreeing on drying
watertrend_sigbrown_80_res <-  as.data.frame.matrix(xtabs(area_sum ~ length + source_id, data = watertrend_sigbrown_80))
watertrend_sigbrown_00_res <-  as.data.frame.matrix(xtabs(area_sum ~ length + source_id, data = watertrend_sigbrown_00))

watertrend_sigbrown_80_res$drying <- c(0:4)
watertrend_sigbrown_00_res$drying <- c(0:4)

#Combine modelled data &observational data
drying_sigbrown_80_new <- drying_sigbrown_80 %>% 
  left_join(watertrend_sigbrown_80_res, by=c("drying"))

drying_sigbrown_00_new <- drying_sigbrown_00 %>% 
  left_join(watertrend_sigbrown_00_res, by=c("drying"))

#Convert portions into %
drying_sigbrown_80_new$Obs <- drying_sigbrown_80_new$Obs *100
drying_sigbrown_00_new$Obs <- drying_sigbrown_00_new$Obs *100

colnames(drying_sigbrown_80_new)[2] <- "GEOV2"
colnames(drying_sigbrown_00_new)[2] <- "GEOV2"

#add MODIS data to the dataframe of the 2nd period
drying_sigbrown_00_new$MODIS <- c(area_sigbrown_0_00_MOD, area_sigbrown_1_00_MOD, area_sigbrown_2_00_MOD, area_sigbrown_3_00_MOD, area_sigbrown_4_00_MOD)*100

#Calculate multi-model means
drying_sigbrown_00_new$"Multi-model mean" <- c(rowMeans(drying_sigbrown_00_new[,4:12]))
drying_sigbrown_80_new$"Multi-model mean" <- c(rowMeans(drying_sigbrown_80_new[,4:12]))

#re-order columns
drying_sigbrown_00_new <- drying_sigbrown_00_new[,c(1,3, 2, 13, 14, 4:12)]
drying_sigbrown_80_new <- drying_sigbrown_80_new[, c(1, 3, 2, 13, 4:12)]

png("path/to/your/storage/fig_03.png", width=6.5, height=4, units="in", res=1200)
par(mfrow=c(1,2), mar=c(7,4,2,1), mgp = c(1, 0.5, 0))
barplot(as.matrix(drying_sigbrown_80_new)[,c(3:13)],
        col=drying_sigbrown_80_new$Cols,
        border="white",
        ylab="% land area",
        main="a) 1982-2001",
        las=2,
        ylim=c(0,17),
        space = c(0.2, 0.4, 0.4, rep(0.1, 8)),
        width=c(1, 1, rep(0.5, 9)),
        cex.main = 0.65,
        cex.axis = 0.65,
        cex.names = 0.65,
        cex.lab = 0.65)
segments(x0 = 1.25, x1 = 1.25, y0 = 0, y1 = 10, lwd=1.5, col = "lightgrey")
segments(x0 = 2.5, x1 = 2.5, y0 = 0, y1 = 10, lwd=1.5, col = "lightgrey", lty = "dotted")
legend("top", fill = drying_sigbrown_80_new$Cols, c("0/4", "1/4", "2/4", "3/4", '4/4'), bty = "n", cex = 0.65, horiz = T, border = 'transparent', bg = 'lightgrey', title = "Agreement btw.")

barplot(as.matrix(drying_sigbrown_00_new)[,c(3:14)], 
        col=drying_sigbrown_00_new$Cols, 
        border="white", 
        ylab="% land area",
        main="b) 2002-2020",
        las=2,
        ylim=c(0,17), 
        space = c(0.2, 0.2, 0.4, 0.4, rep(0.1, 8)),
        width=c(1, 1, 1, rep(0.5, 9)),
        cex.main=0.65, 
        cex.axis=0.65,
        cex.names=0.65, 
        cex.lab=0.65)
segments(x0=2.35, x1=2.35, y0=0, y1=10, lwd=1.5, col="lightgrey")
segments(x0=3.6, x1=3.6, y0=0, y1=10, lwd=1.5, col="lightgrey", lty="dotted")
text(4.2, 15.5, "Barheight indicates % land area \n significantly browning", cex = 0.65)


dev.off()

################################################################
###### Sppl. Fig: Drying agreement in all browning areas ######

#this is the same procedure as described above but for areas with significant and insignificant browning (sign_trend_LAI0595_8201 <= 2). 

area_allbrown_0_80 <- (sum(area.array[which(posdecr_trend == 0 & sign_trend_LAI0595_8201 <= 2)], na.rm=T))/total_land_area
area_allbrown_1_80 <- (sum(area.array[which(posdecr_trend == 1 & sign_trend_LAI0595_8201 <= 2)], na.rm=T))/total_land_area
area_allbrown_2_80 <- (sum(area.array[which(posdecr_trend == 2 & sign_trend_LAI0595_8201 <= 2)], na.rm=T))/total_land_area
area_allbrown_3_80 <- (sum(area.array[which(posdecr_trend == 3 & sign_trend_LAI0595_8201 <= 2)], na.rm=T))/total_land_area
area_allbrown_4_80 <- (sum(area.array[which(posdecr_trend == 4 & sign_trend_LAI0595_8201 <= 2)], na.rm=T))/total_land_area

area_allbrown_0_00 <- (sum(area.array[which(posdecr_trend_2002 == 0 & sign_trend_LAI0595_0220 <= 2)], na.rm=T))/total_land_area
area_allbrown_1_00 <- (sum(area.array[which(posdecr_trend_2002 == 1 & sign_trend_LAI0595_0220 <= 2)], na.rm=T))/total_land_area
area_allbrown_2_00 <- (sum(area.array[which(posdecr_trend_2002 == 2 & sign_trend_LAI0595_0220 <= 2)], na.rm=T))/total_land_area
area_allbrown_3_00 <- (sum(area.array[which(posdecr_trend_2002 == 3 & sign_trend_LAI0595_0220 <= 2)], na.rm=T))/total_land_area
area_allbrown_4_00 <- (sum(area.array[which(posdecr_trend_2002 == 4 & sign_trend_LAI0595_0220 <= 2)], na.rm=T))/total_land_area


area_allbrown_0_00_MOD <- (sum(area.array[which(posdecr_trend_2002 == 0 & sign_trend_MODIS_0220 <= 2)], na.rm=T))/total_land_area
area_allbrown_1_00_MOD <- (sum(area.array[which(posdecr_trend_2002 == 1 & sign_trend_MODIS_0220 <= 2)], na.rm=T))/total_land_area
area_allbrown_2_00_MOD <- (sum(area.array[which(posdecr_trend_2002 == 2 & sign_trend_MODIS_0220 <= 2)], na.rm=T))/total_land_area
area_allbrown_3_00_MOD <- (sum(area.array[which(posdecr_trend_2002 == 3 & sign_trend_MODIS_0220 <= 2)], na.rm=T))/total_land_area
area_allbrown_4_00_MOD <- (sum(area.array[which(posdecr_trend_2002 == 4 & sign_trend_MODIS_0220 <= 2)], na.rm=T))/total_land_area

drying_allbrown_80 <- data.frame( Cols = c('skyblue3','skyblue1', 'lightblue1', "orange",'saddlebrown'),
                              Obs = c(area_allbrown_0_80, area_allbrown_1_80, area_allbrown_2_80, area_allbrown_3_80, area_allbrown_4_80))
rownames(drying_allbrown_80) <- c("0/4 drying", "1/4 drying", "2/4 drying", "3/4 drying", "4/4 drying")

drying_allbrown_00 <- data.frame( Cols = c('skyblue3','skyblue1', 'lightblue1', "orange",'saddlebrown'),
                              Obs = c(area_allbrown_0_00, area_allbrown_1_00, area_allbrown_2_00, area_allbrown_3_00, area_allbrown_4_00))
rownames(drying_allbrown_00) <- c("0/4 drying", "1/4 drying", "2/4 drying", "3/4 drying", "4/4 drying")


watertrend_allbrown_80 <- LAI_water_trends %>%
  filter(period == c("1982 - 2001") & LAI <= 2) %>%
  group_by(length, source_id) %>%
  summarize(area_sum = sum(area))

watertrend_allbrown_00 <- LAI_water_trends %>%
  filter(period == c("2002 - 2020") & LAI <= 2) %>%
  group_by(length, source_id) %>%
  summarize(area_sum = sum(area))

watertrend_allbrown_00$area_sum <- (watertrend_allbrown_00$area_sum / total_land_area_2) *100
watertrend_allbrown_80$area_sum <- (watertrend_allbrown_80$area_sum / total_land_area_2) *100

drying_allbrown_00$drying <- c(0:4)
drying_allbrown_80$drying <- c(0:4)

#add column with LAI response to drying information
watertrend_allbrown_80_res <-  as.data.frame.matrix(xtabs(area_sum ~ length + source_id, data = watertrend_allbrown_80))
watertrend_allbrown_00_res <-  as.data.frame.matrix(xtabs(area_sum ~ length + source_id, data = watertrend_allbrown_00))

watertrend_allbrown_80_res$drying <- c(0:4)
watertrend_allbrown_00_res$drying <- c(0:4)

drying_allbrown_80_new <- drying_allbrown_80 %>% 
  left_join(watertrend_allbrown_80_res, by=c("drying"))

drying_allbrown_00_new <- drying_allbrown_00 %>% 
  left_join(watertrend_allbrown_00_res, by=c("drying"))


drying_allbrown_80_new$Obs <- drying_allbrown_80_new$Obs *100
drying_allbrown_00_new$Obs <- drying_allbrown_00_new$Obs *100

colnames(drying_allbrown_80_new)[2] <- "GEOV2"
colnames(drying_allbrown_00_new)[2] <- "GEOV2"

drying_allbrown_00_new$MODIS <- c(area_allbrown_0_00_MOD, area_allbrown_1_00_MOD, area_allbrown_2_00_MOD, area_allbrown_3_00_MOD, area_allbrown_4_00_MOD)*100
drying_allbrown_00_new$"Multi-model mean" <- c(rowMeans(drying_allbrown_00_new[,4:12]))
drying_allbrown_80_new$"Multi-model mean" <- c(rowMeans(drying_allbrown_80_new[,4:12]))

drying_allbrown_00_new <- drying_allbrown_00_new[,c(1,3, 2, 13, 14, 4:12)]
drying_allbrown_80_new <- drying_allbrown_80_new[, c(1, 3, 2, 13, 4:12)]

png("path/to/your/storage/fig_S5.png", width=6.5, height=4, units="in", res=1200)
par(mfrow=c(1,2), mar=c(7,4,2,1), mgp = c(1, 0.5, 0))
barplot(as.matrix(drying_allbrown_80_new)[,c(3:13)],
        col=drying_allbrown_80_new$Cols,
        border="white",
        ylab="% land area",
        main="a) 1982-2001",
        las=2,
        ylim=c(0,30),
        space = c(0.2, 0.4, 0.4, rep(0.1, 8)),
        width=c(1, 1, rep(0.5, 9)),
        cex.main = 0.65,
        cex.axis = 0.65,
        cex.names = 0.65,
        cex.lab = 0.65)
segments(x0 = 1.25, x1 = 1.25, y0 = 0, y1 = 10, lwd=1.5, col = "lightgrey")
segments(x0 = 2.5, x1 = 2.5, y0 = 0, y1 = 10, lwd=1.5, col = "lightgrey", lty = "dotted")
legend("top", fill = drying_allbrown_80_new$Cols, c("0/4", "1/4", "2/4", "3/4", '4/4'), bty = "n", cex = 0.65, horiz = T, border = 'transparent', bg = 'lightgrey', title = "Agreement btw.")

barplot(as.matrix(drying_allbrown_00_new)[,c(3:14)], 
        col=drying_allbrown_00_new$Cols, 
        border="white", 
        ylab="% land area",
        main="b) 2002-2020",
        las=2,
        ylim=c(0,30), 
        space = c(0.2, 0.2, 0.4, 0.4, rep(0.1, 8)),
        width=c(1, 1, 1, rep(0.5, 9)),
        cex.main=0.65, 
        cex.axis=0.65,
        cex.names=0.65, 
        cex.lab=0.65)
segments(x0=2.35, x1=2.35, y0=0, y1=10, lwd=1.5, col="lightgrey")
segments(x0=3.6, x1=3.6, y0=0, y1=10, lwd=1.5, col="lightgrey", lty="dotted")
text(4.2, 28.5, "Barheight indicates % land area \n significantly and insignificantly browning", cex = 0.65)


dev.off()

##########################################################################################################
###### Sppl. Fig:Multi-model mean of drying agreement in all browning areas for all 20-year periods ######

#again repeating the same analysis as above but only for significantly browning areas, but for all the time periods from 1982-2100
watertrend_sigbrown_20 <- LAI_water_trends %>%
  filter(period == c("2021 - 2040") & LAI == 1) %>%
  group_by(length, source_id) %>%
  summarize(area_sum = sum(area))

watertrend_sigbrown_40 <- LAI_water_trends %>%
  filter(period == c("2041 - 2060") & LAI == 1) %>%
  group_by(length, source_id) %>%
  summarize(area_sum = sum(area))

watertrend_sigbrown_60 <- LAI_water_trends %>%
  filter(period == c("2061 - 2080") & LAI == 1) %>%
  group_by(length, source_id) %>%
  summarize(area_sum = sum(area))

watertrend_sigbrown_100 <- LAI_water_trends %>%
  filter(period == c("2081 - 2100") & LAI == 1) %>%
  group_by(length, source_id) %>%
  summarize(area_sum = sum(area))


watertrend_sigbrown_20$area_sum <- (watertrend_sigbrown_20$area_sum / total_land_area_2) *100
watertrend_sigbrown_40$area_sum <- (watertrend_sigbrown_40$area_sum / total_land_area_2) *100
watertrend_sigbrown_60$area_sum <- (watertrend_sigbrown_60$area_sum / total_land_area_2) *100
watertrend_sigbrown_100$area_sum <- (watertrend_sigbrown_100$area_sum / total_land_area_2) *100


#add column with LAI response to drying information
watertrend_sigbrown_20_res <-  as.data.frame.matrix(xtabs(area_sum ~ length + source_id, data = watertrend_sigbrown_20))
watertrend_sigbrown_40_res <-  as.data.frame.matrix(xtabs(area_sum ~ length + source_id, data = watertrend_sigbrown_40))
watertrend_sigbrown_60_res <-  as.data.frame.matrix(xtabs(area_sum ~ length + source_id, data = watertrend_sigbrown_60))
watertrend_sigbrown_100_res <-  as.data.frame.matrix(xtabs(area_sum ~ length + source_id, data = watertrend_sigbrown_100))

watertrend_sigbrown_20_res$"Multi-model mean" <- c(rowMeans(watertrend_sigbrown_20_res[,]))
watertrend_sigbrown_40_res$"Multi-model mean" <- c(rowMeans(watertrend_sigbrown_40_res[,]))

watertrend_sigbrown_60_res$"Multi-model mean" <- c(rowMeans(watertrend_sigbrown_60_res[,]))
watertrend_sigbrown_100_res$"Multi-model mean" <- c(rowMeans(watertrend_sigbrown_100_res[,]))

watertrends_sigbrown_acrosstime <- data.frame("1980" = drying_sigbrown_80_new$"Multi-model mean", 
                                              "2000" = drying_sigbrown_00_new$"Multi-model mean",
                                              "2020" = watertrend_sigbrown_20_res$"Multi-model mean",
                                              "2040" = watertrend_sigbrown_40_res$"Multi-model mean",
                                              "2060" = watertrend_sigbrown_60_res$"Multi-model mean",
                                              "2080" = watertrend_sigbrown_100_res$"Multi-model mean")

colnames(watertrends_sigbrown_acrosstime) <- unique(LAI_water_trends$period)

png("path/to/your/storage/fig_S6.png", width=4, height=4, units="in", res=1200)
barplot(as.matrix(watertrends_sigbrown_acrosstime), 
        col=drying_allbrown_00_new$Cols, 
        border="white", 
        ylab="% land area",
        main="Drying agreement in significantly browning areas across time",
        ylim=c(0,6), 
        las = 2,
        cex.main=0.5, 
        cex.axis=0.5,
        cex.names=0.5, 
        cex.lab=0.5)
legend("top", fill = drying_allbrown_80_new$Cols, c("0/4", "1/4", "2/4", "3/4", '4/4'), bty = "n", cex = 0.65, horiz = T, border = 'transparent', bg = 'lightgrey', title = "Agreement btw.")
dev.off()

##############################################
####### Analysis 2: Regression analysis ######

load("Rdata/nat_wat_pers/v08_regressionanalysis_plots.RData")

#Normalize the hydro-met data that goes in there
norm_y_LAI_8201 <- array(NaN, c(720, 360, 20))
minLAI_8201 <- rep(min(amLAI_8201, na.rm = T), 20) #define array minimum
maxLAI_8201 <- rep(max(amLAI_8201, na.rm = T), 20) #define array maximum
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      norm_y_LAI_8201[x,y,] <- (amLAI_8201[x,y,] - minLAI_8201)/(maxLAI_8201-minLAI_8201) #subtract minimum to set the lowest value to 0, divide by maximum to set highest value to 1
    }
  }
}


norm_y_LAI_0220 <- array(NaN, c(720, 360, 19))
minLAI_0220 <- rep(min(amLAI_0220, na.rm = T), 19) #for the second period 2002-2021 the last years LAI data is not available, that's why I have only 19 years here
maxLAI_0220 <- rep(max(amLAI_0220, na.rm = T), 19) 
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      norm_y_LAI_0220[x,y,] <- (amLAI_0220[x,y,] - minLAI_0220)/(maxLAI_0220-minLAI_0220)
    }
  }
}


norm_y_rGLEAM_8201 <- array(NaN, c(720, 360, 20))
minrGLEAM_8201 <- rep(min(amrGLEAM_8201, na.rm = T), 20)
maxrGLEAM_8201 <- rep(max(amrGLEAM_8201, na.rm = T), 20) 
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      norm_y_rGLEAM_8201[x,y,] <- (amrGLEAM_8201[x,y,] - minrGLEAM_8201)/(maxrGLEAM_8201-minrGLEAM_8201)
    }
  }
}


norm_y_rGLEAM_0220 <- array(NaN, c(720, 360, 19))
minrGLEAM_0220 <- rep(min(amrGLEAM_0220, na.rm = T), 19)
maxrGLEAM_0220 <- rep(max(amrGLEAM_0220, na.rm = T), 19) 
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      norm_y_rGLEAM_0220[x,y,] <- (amrGLEAM_0220[x,y,] - minrGLEAM_0220)/(maxrGLEAM_0220- minrGLEAM_0220)
    }
  }
}

norm_y_MSWEP_8201 <- array(NaN, c(720, 360, 20))
minMSWEP_8201 <- rep(min(amMSWEP_8201, na.rm = T), 20)
maxMSWEP_8201 <- rep(max(amMSWEP_8201, na.rm = T), 20) 
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      norm_y_MSWEP_8201[x,y,] <- (amMSWEP_8201[x,y,] - minMSWEP_8201)/(maxMSWEP_8201-minMSWEP_8201)
    }
  }
}


norm_y_MSWEP_0220 <- array(NaN, c(720, 360, 19))
minMSWEP_0220 <- rep(min(amMSWEP_0220, na.rm = T), 19)
maxMSWEP_0220 <- rep(max(amMSWEP_0220, na.rm = T), 19) 
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      norm_y_MSWEP_0220[x,y,] <- (amMSWEP_0220[x,y,] - minMSWEP_0220)/(maxMSWEP_0220-minMSWEP_0220)
    }
  }
}

norm_y_ERA5_VPD_8201 <- array(NaN, c(720, 360, 20))
minERA5_VPD_8201 <- rep(min(amERA5_VPD_8201, na.rm = T), 20)
maxERA5_VPD_8201 <- rep(max(amERA5_VPD_8201, na.rm = T), 20) 
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      norm_y_ERA5_VPD_8201[x,y,] <- (amERA5_VPD_8201[x,y,] - minERA5_VPD_8201)/(maxERA5_VPD_8201-minERA5_VPD_8201)
    }
  }
}


norm_y_ERA5_VPD_0220 <- array(NaN, c(720, 360, 19))
minERA5_VPD_0220 <- rep(min(amERA5_VPD_0220, na.rm = T), 19)
maxERA5_VPD_0220 <- rep(max(amERA5_VPD_0220, na.rm = T), 19) 
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      norm_y_ERA5_VPD_0220[x,y,] <- (amERA5_VPD_0220[x,y,] - minERA5_VPD_0220)/(maxERA5_VPD_0220-minERA5_VPD_0220)
    }
  }
}

norm_y_t2m_8201 <- array(NaN, c(720, 360, 20))
mint2m_8201 <- rep(min(am_t2m_8201, na.rm = T), 20)
maxt2m_8201 <- rep(max(am_t2m_8201, na.rm = T), 20) 
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      norm_y_t2m_8201[x,y,] <- (am_t2m_8201[x,y,] - mint2m_8201)/(maxt2m_8201- mint2m_8201)
    }
  }
}


norm_y_t2m_0220 <- array(NaN, c(720, 360, 19))
mint2m_0220 <- rep(min(am_t2m_0220, na.rm = T), 19)
maxt2m_0220 <- rep(max(am_t2m_0220, na.rm = T), 19) 
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      norm_y_t2m_0220[x,y,] <- (am_t2m_0220[x,y,] - mint2m_0220)/(maxt2m_0220-mint2m_0220)
    }
  }
}

norm_y_ssrd_8201 <- array(NaN, c(720, 360, 20))
minssrd_8201 <- rep(min(am_ssrd_8201, na.rm = T), 20)
maxssrd_8201 <- rep(max(am_ssrd_8201, na.rm = T), 20) 
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      norm_y_ssrd_8201[x,y,] <- (am_ssrd_8201[x,y,] - minssrd_8201)/(maxssrd_8201-minssrd_8201)
    }
  }
}


norm_y_ssrd_0220 <- array(NaN, c(720, 360, 19))
minssrd_0220 <- rep(min(am_ssrd_0220, na.rm = T), 19)
maxssrd_0220 <- rep(max(am_ssrd_0220, na.rm = T), 19) 
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      norm_y_ssrd_0220[x,y,] <- (am_ssrd_0220[x,y,] - minssrd_0220)/(maxssrd_0220-minssrd_0220)
    }
  }
}


norm_y_snr_8201 <- array(NaN, c(720, 360, 20))
minsnr_8201 <- rep(min(y_snr_1st, na.rm = T), 20)
maxsnr_8201 <- rep(max(y_snr_1st, na.rm = T), 20) 
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      norm_y_snr_8201[x,y,] <- (y_snr_1st[x,y,] - minsnr_8201)/(maxsnr_8201-minsnr_8201)
    }
  }
}


norm_y_snr_0220 <- array(NaN, c(720, 360, 19))
minsnr_0220 <- rep(min(y_snr_2nd, na.rm = T), 19)
maxsnr_0220 <- rep(max(y_snr_2nd, na.rm = T), 19) 
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      norm_y_snr_0220[x,y,] <- (y_snr_2nd[x,y,] - minsnr_0220)/(maxsnr_0220 - minsnr_0220)
    }
  }
}

am_maxt2m_8201 <- abind(y_maxt2m_80, y_maxt2m_90)
am_maxt2m_0220 <- abind (y_maxt2m_00, y_maxt2m_10)

norm_y_t2mmax_8201 <- array(NaN, c(720, 360, 20))
min_maxt2m_8201 <- rep(min(am_maxt2m_8201, na.rm = T), 20)
max_maxt2m_8201 <- rep(max(am_maxt2m_8201, na.rm = T), 20) 
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      norm_y_t2mmax_8201[x,y,] <- (am_maxt2m_8201[x,y,] - min_maxt2m_8201)/(max_maxt2m_8201-min_maxt2m_8201)
    }
  }
}


norm_y_t2mmax_0220 <- array(NaN, c(720, 360, 19))
min_maxt2m_0220 <- rep(min(am_maxt2m_0220, na.rm = T), 19)
max_maxt2m_0220 <- rep(max(am_maxt2m_0220, na.rm = T), 19) 
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      norm_y_t2mmax_0220[x,y,] <- (am_maxt2m_0220[x,y,] - min_maxt2m_0220)/(max_maxt2m_0220-min_maxt2m_0220)
    }
  }
}

am_minrGLEAM_8201 <- abind(y_minrGLEAM_80, y_minrGLEAM_90)
am_minrGLEAM_0220 <- abind(y_minrGLEAM_00, y_minrGLEAM_10)

norm_y_rGLEAMmin_8201 <- array(NaN, c(720, 360, 20))
am_minrGLEAM_8201[which(is.infinite(am_minrGLEAM_8201))] <- NaN
min_minrGLEAM_8201 <- rep(min(am_minrGLEAM_8201, na.rm = T), 20)
max_minrGLEAM_8201 <- rep(max(am_minrGLEAM_8201, na.rm = T), 20) 
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      norm_y_rGLEAMmin_8201[x,y,] <- (am_minrGLEAM_8201[x,y,] - min_minrGLEAM_8201)/(max_minrGLEAM_8201-min_minrGLEAM_8201)
    }
  }
}


norm_y_rGLEAMmin_0220 <- array(NaN, c(720, 360, 19))
am_minrGLEAM_0220[which(is.infinite(am_minrGLEAM_0220))] <- NaN
min_minrGLEAM_0220 <- rep(min(am_minrGLEAM_0220, na.rm = T), 19)
max_minrGLEAM_0220 <- rep(max(am_minrGLEAM_0220, na.rm = T), 19) 
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y])){
      norm_y_rGLEAMmin_0220[x,y,] <- (am_minrGLEAM_0220[x,y,] - min_minrGLEAM_0220)/(max_minrGLEAM_0220-min_minrGLEAM_0220)
    }
  }
}

#Create mask to kick out grid cells, where we don't have data for one of the years in the two study periods
mask_obs_yr <- array(NaN,c(720,360,20))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y]) & sum(!is.na(amLAI_8201[x,y,])) == 20 & sum(!is.na(amrGLEAM_8201[x,y,])) == 20 & sum(!is.na(amMSWEP_8201[x,y,])) == 20 & sum(!is.na(amERA5_VPD_8201[x,y,])) == 20 & sum(!is.na(am_ssrd_8201[x,y,])) == 20 & sum(!is.na(am_t2m_8201[x,y,])) == 20 & sum(!is.na(y_snr_1st[x,y,])) == 20){ #study mask is only !NaN if mLAI in all decades > 0.5
      mask_obs_yr[x,y,] <- rep(1, 20)
    }
  }
}

mask_obs_yr_0220 <- array(NaN,c(720,360,19))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(study_mask[x,y]) & sum(!is.na(amLAI_0220[x,y,])) == 19 & sum(!is.na(amrGLEAM_0220[x,y,])) == 19 & sum(!is.na(amMSWEP_0220[x,y,])) == 19 & sum(!is.na(amERA5_VPD_0220[x,y,])) == 19 & sum(!is.na(am_ssrd_0220[x,y,])) == 19 & sum(!is.na(am_t2m_0220[x,y,])) == 19 & sum(!is.na(y_snr_2nd[x,y,])) == 19){ #study mask is only !NaN if mLAI in all decades > 0.5
      mask_obs_yr_0220[x,y,] <- rep(1, 19)
    }
  }
}

#filter data by the masks
av_lai_yr_8201.array <-
  av_mrsol_yr_8201.array <-
  max_tasmax_yr_8201.array <-
  min_mrsol_yr_8201.array <-
  av_vpd_yr_8201.array <-
  av_pr_yr_8201.array <-
  av_netrad_yr_8201.array <-
  av_rsds_yr_8201.array <-
  av_tas_yr_8201.array <-
  array(NaN,c(720,360,20))


av_lai_yr_8201.array <- norm_y_LAI_8201 * mask_obs_yr
av_mrsol_yr_8201.array <-norm_y_rGLEAM_8201 * mask_obs_yr
max_tasmax_yr_8201.array <- norm_y_t2mmax_8201 * mask_obs_yr
min_mrsol_yr_8201.array <- norm_y_rGLEAMmin_8201 * mask_obs_yr
av_vpd_yr_8201.array <-norm_y_ERA5_VPD_8201 * mask_obs_yr
av_pr_yr_8201.array <-norm_y_MSWEP_8201 * mask_obs_yr
av_netrad_yr_8201.array <-norm_y_snr_8201 * mask_obs_yr
av_rsds_yr_8201.array <-norm_y_ssrd_8201 * mask_obs_yr
av_tas_yr_8201.array <-norm_y_t2m_8201 * mask_obs_yr

#add variable names and their categories
vars <- c("mrsol","min_mrsol","pr","vpd","tas","max_tasmax","rsds","netrad","adj. R2 < 0.36") # 9 variables
var_type <- c("water","water","water","demand","energy","energy","energy","energy")

#create arrays that are filled with the output from the linear regression 
options(na.action = "na.fail")
dom_var_dredge_relimp_ng_8201 <- array(NaN,c(720,360,1,2))
dom_var_dredge_relimp_ng_all_8201 <- array(0,c(720,360,1,(length(vars)-1)))
adj_R2_dredge_relimp_ng_8201 <- array(NaN,c(720,360,1))
dom_water_var_ng_8201 <- array(NaN,c(720,360,1,2))
rank_dom_water_var_ng_8201 <- array(NaN,c(720,360,1))
lin_mod_ng_8201.array <- array(NaN,c(720,360,1))

#here the linear model starts, take around 5-6 hours to run; progress is given as output
for(x in 1:720){
  for(y in 1:360){
    if(sum(!is.na(mask_obs_yr[x,y,])) == 20 & var(av_lai_yr_8201.array[x,y,]) != 0){
      relimp_variables.list <- list()
      rsq.vec <- c()
      # linear model
      predictors.df <- data.frame("lai" = av_lai_yr_8201.array[x,y,],
                                  "mrsol" = av_mrsol_yr_8201.array[x,y,],
                                  "min_mrsol" = min_mrsol_yr_8201.array[x,y,],
                                  "pr" = av_pr_yr_8201.array[x,y,],
                                  "vpd" = av_vpd_yr_8201.array[x,y,],
                                  # "AI" = av_AI_yr.array[x,y,],
                                  "tas" = av_tas_yr_8201.array[x,y,],
                                  "max_tasmax" = max_tasmax_yr_8201.array[x,y,],
                                  "rsds" = av_rsds_yr_8201.array[x,y,],
                                  "netrad" = av_netrad_yr_8201.array[x,y,])
      
      lm_dcorr_rsds.df <- lm(formula = lai ~ mrsol + min_mrsol + pr + vpd + tas + max_tasmax + rsds + netrad, data = predictors.df)
      dd <- dredge(lm_dcorr_rsds.df, extra = list(AIC))
      
      #lm_dcorr_rsds.df <- lm(formula = lai ~ mrsol +  pr + vpd + tas  + rsds + netrad, data = predictors.df)
      #dd <- dredge(lm_dcorr_rsds.df, extra = list(AIC))
      
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
                dom_var_dredge_relimp_ng_all_8201[x,y,1,i] <- dom_var_dredge_relimp_ng_all_8201[x,y,1,i] + 1
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
                dom_var_dredge_relimp_ng_all_8201[x,y,1,i] <- dom_var_dredge_relimp_ng_all_8201[x,y,1,i] + 1
                next
              }
            }
          }
        }
        if(lin_mod == dim(dd_subset)[1]){ # on the last model, to summarize all linear models for this grid cell
          lin_mod_ng_8201.array[x,y,1] <- sum(rsq.vec > 0.36) # number of linear models with sufficient explanatory power
          adj_R2_dredge_relimp_ng_8201[x,y,1] <- mean(rsq.vec, na.rm = T)
          for(j in 1:(length(vars)-1)){
            total_var_expl.c[j] <- weighted.mean(x = relimp_var_lmg.df$lmg[which(relimp_var_lmg.df$var == j)], w = relimp_var_lmg.df$weight[which(relimp_var_lmg.df$var == j)], na.rm = T)
          }
          if(sum(is.na(total_var_expl.c)) == length(vars)-1){
            dom_var_dredge_relimp_ng_8201[x,y,1,1] <- length(vars) # this will substitute the value length(vars) when there are linear models, but they are not of sufficient explanatory power. 
          }else if(length(which(total_var_expl.c == max(total_var_expl.c, na.rm = T))) == 1){ # if there is one dominant variable
            dom_var_dredge_relimp_ng_8201[x,y,1,1] <- which(total_var_expl.c == max(total_var_expl.c, na.rm = T))
          }else if(length(which(total_var_expl.c == max(total_var_expl.c, na.rm = T))) > 1){ # if there is a two way tie
            dom_var_dredge_relimp_ng_8201[x,y,1,] <- which(total_var_expl.c == max(total_var_expl.c, na.rm = T))
          }
          # # ranks across models
          if(dom_var_dredge_relimp_ng_8201[x,y,1,1] < length(vars)){ # go into this loop only if there are any explanatory linear models
            total_var_expl.c[which(is.na(total_var_expl.c))] <- 0 # Replace NaN with 0 in total_var_expl.c such that variables not appearing in explanatory models come last in the ranking
            total_ranks.c <- rank(total_var_expl.c, ties.method = 'random', na.last = T) # rank the total explained variance per variable
            # total_ranks.c[which(is.na(total_var_expl.c))] <- NaN # Enter NaN if the variable appears in no model at all
            total_ranks.c <- (length(vars)) - total_ranks.c # revert the ranks so that 1 is the highest rank!
            
            if(length(which(!is.na(total_var_expl.c[1:length(which(var_type != 'energy'))]))) > 0){ # Only compute when water variables have any explanatory power.
              if(length(which(total_var_expl.c[1:length(which(var_type != 'energy'))] == max(total_var_expl.c[1:length(which(var_type != 'energy'))], na.rm = T))) == 1){
                dom_water_var_ng_8201[x,y,1,1] <- which(total_var_expl.c[1:length(which(var_type != 'energy'))] == max(total_var_expl.c[1:length(which(var_type != 'energy'))], na.rm = T)) # what is the most important water availability variable?
              }else if(length(which(total_var_expl.c[1:length(which(var_type != 'energy'))] == max(total_var_expl.c[1:length(which(var_type != 'energy'))], na.rm = T))) > 1 & sum(total_var_expl.c[1:length(which(var_type != 'energy'))]) > 0){
                dom_water_var_ng_8201[x,y,1,] <- which(total_var_expl.c[1:length(which(var_type != 'energy'))] == max(total_var_expl.c[1:length(which(var_type != 'energy'))], na.rm = T)) # what is the most important water availability variable?
              }
              rank_dom_water_var_ng_8201[x,y,1] <- min(total_ranks.c[1:length(which(var_type != 'energy'))], na.rm = T)
            }
          }
        }
      }
    }
  }
  print(paste(round(x/(720)*100,2),"%"))
}

#in between save for the results for the first period
save.image("/add/your/path/to/storage/here.RData")

av_lai_yr_0220.array <-
  av_mrsol_yr_0220.array <-
  max_tasmax_yr_0220.array <-
  min_mrsol_yr_0220.array <-
  av_vpd_yr_0220.array <-
  av_pr_yr_0220.array <-
  av_netrad_yr_0220.array <-
  av_rsds_yr_0220.array <-
  av_tas_yr_0220.array <-
  array(NaN,c(720,360,20))


av_lai_yr_0220.array <- norm_y_LAI_0220 * mask_obs_yr_0220
av_mrsol_yr_0220.array <-norm_y_rGLEAM_0220 * mask_obs_yr_0220
max_tasmax_yr_0220.array <- norm_y_t2mmax_0220 * mask_obs_yr_0220
min_mrsol_yr_0220.array <- norm_y_rGLEAMmin_0220 * mask_obs_yr_0220
av_vpd_yr_0220.array <-norm_y_ERA5_VPD_0220 * mask_obs_yr_0220
av_pr_yr_0220.array <-norm_y_MSWEP_0220 * mask_obs_yr_0220
av_netrad_yr_0220.array <-norm_y_snr_0220 * mask_obs_yr_0220
av_rsds_yr_0220.array <-norm_y_ssrd_0220 * mask_obs_yr_0220
av_tas_yr_0220.array <-norm_y_t2m_0220 * mask_obs_yr_0220

options(na.action = "na.fail")
dom_var_dredge_relimp_ng_0220 <- array(NaN,c(720,360,1,2))
dom_var_dredge_relimp_ng_all_0220 <- array(0,c(720,360,1,(length(vars)-1)))
adj_R2_dredge_relimp_ng_0220 <- array(NaN,c(720,360,1))
dom_water_var_ng_0220 <- array(NaN,c(720,360,1,2))
rank_dom_water_var_ng_0220 <- array(NaN,c(720,360,1))
lin_mod_ng_0220.array <- array(NaN,c(720,360,1))


for(x in 1:720){
  for(y in 1:360){
    if(sum(!is.na(mask_obs_yr_0220[x,y,])) == 20 & var(av_lai_yr_0220.array[x,y,]) != 0){
      relimp_variables.list <- list()
      rsq.vec <- c()
      # linear model
      predictors.df <- data.frame("lai" = av_lai_yr_0220.array[x,y,],
                                  "mrsol" = av_mrsol_yr_0220.array[x,y,],
                                  "min_mrsol" = min_mrsol_yr_0220.array[x,y,],
                                  "pr" = av_pr_yr_0220.array[x,y,],
                                  "vpd" = av_vpd_yr_0220.array[x,y,],
                                  # "AI" = av_AI_yr.array[x,y,],
                                  "tas" = av_tas_yr_0220.array[x,y,],
                                  "max_tasmax" = max_tasmax_yr_0220.array[x,y,],
                                  "rsds" = av_rsds_yr_0220.array[x,y,],
                                  "netrad" = av_netrad_yr_0220.array[x,y,])
      
      lm_dcorr_rsds.df <- lm(formula = lai ~ mrsol + min_mrsol + pr + vpd + tas + max_tasmax + rsds + netrad, data = predictors.df)
      dd <- dredge(lm_dcorr_rsds.df, extra = list(AIC))
      
      #lm_dcorr_rsds.df <- lm(formula = lai ~ mrsol +  pr + vpd + tas  + rsds + netrad, data = predictors.df)
      #dd <- dredge(lm_dcorr_rsds.df, extra = list(AIC))
      
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
                dom_var_dredge_relimp_ng_all_0220[x,y,1,i] <- dom_var_dredge_relimp_ng_all_0220[x,y,1,i] + 1
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
                dom_var_dredge_relimp_ng_all_0220[x,y,1,i] <- dom_var_dredge_relimp_ng_all_0220[x,y,1,i] + 1
                next
              }
            }
          }
        }
        if(lin_mod == dim(dd_subset)[1]){ # on the last model, to summarize all linear models for this grid cell
          lin_mod_ng_0220.array[x,y,1] <- sum(rsq.vec > 0.36) # number of linear models with sufficient explanatory power
          adj_R2_dredge_relimp_ng_0220[x,y,1] <- mean(rsq.vec, na.rm = T)
          for(j in 1:(length(vars)-1)){
            total_var_expl.c[j] <- weighted.mean(x = relimp_var_lmg.df$lmg[which(relimp_var_lmg.df$var == j)], w = relimp_var_lmg.df$weight[which(relimp_var_lmg.df$var == j)], na.rm = T)
          }
          if(sum(is.na(total_var_expl.c)) == length(vars)-1){
            dom_var_dredge_relimp_ng_0220[x,y,1,1] <- length(vars) # this will substitute the value length(vars) when there are linear models, but they are not of sufficient explanatory power. 
          }else if(length(which(total_var_expl.c == max(total_var_expl.c, na.rm = T))) == 1){ # if there is one dominant variable
            dom_var_dredge_relimp_ng_0220[x,y,1,1] <- which(total_var_expl.c == max(total_var_expl.c, na.rm = T))
          }else if(length(which(total_var_expl.c == max(total_var_expl.c, na.rm = T))) > 1){ # if there is a two way tie
            dom_var_dredge_relimp_ng_0220[x,y,1,] <- which(total_var_expl.c == max(total_var_expl.c, na.rm = T))
          }
          # # ranks across models
          if(dom_var_dredge_relimp_ng_0220[x,y,1,1] < length(vars)){ # go into this loop only if there are any explanatory linear models
            total_var_expl.c[which(is.na(total_var_expl.c))] <- 0 # Replace NaN with 0 in total_var_expl.c such that variables not appearing in explanatory models come last in the ranking
            total_ranks.c <- rank(total_var_expl.c, ties.method = 'random', na.last = T) # rank the total explained variance per variable
            # total_ranks.c[which(is.na(total_var_expl.c))] <- NaN # Enter NaN if the variable appears in no model at all
            total_ranks.c <- (length(vars)) - total_ranks.c # revert the ranks so that 1 is the highest rank!
            
            if(length(which(!is.na(total_var_expl.c[1:length(which(var_type != 'energy'))]))) > 0){ # Only compute when water variables have any explanatory power.
              if(length(which(total_var_expl.c[1:length(which(var_type != 'energy'))] == max(total_var_expl.c[1:length(which(var_type != 'energy'))], na.rm = T))) == 1){
                dom_water_var_ng_0220[x,y,1,1] <- which(total_var_expl.c[1:length(which(var_type != 'energy'))] == max(total_var_expl.c[1:length(which(var_type != 'energy'))], na.rm = T)) # what is the most important water availability variable?
              }else if(length(which(total_var_expl.c[1:length(which(var_type != 'energy'))] == max(total_var_expl.c[1:length(which(var_type != 'energy'))], na.rm = T))) > 1 & sum(total_var_expl.c[1:length(which(var_type != 'energy'))]) > 0){
                dom_water_var_ng_0220[x,y,1,] <- which(total_var_expl.c[1:length(which(var_type != 'energy'))] == max(total_var_expl.c[1:length(which(var_type != 'energy'))], na.rm = T)) # what is the most important water availability variable?
              }
              rank_dom_water_var_ng_0220[x,y,1] <- min(total_ranks.c[1:length(which(var_type != 'energy'))], na.rm = T)
            }
          }
        }
      }
    }
  }
  print(paste(round(x/(720)*100,2),"%"))
}

#in between save for the 2nd part of the model
save.image("/add/your/path/to/storage/here.RData")


# There are ties in the underlying data too...
# make an array that holds the ties
dom_var_dredge_relimp_ng_tie_8201 <- array(NaN,c(720,360,1,2))

  for(x in 1:720){
    for(y in 1:360){
      if(!is.na(dom_var_dredge_relimp_ng_8201[x,y,1,2])){
        dom_var_dredge_relimp_ng_tie_8201[x,y,1,] <- dom_var_dredge_relimp_ng_8201[x,y,1,]
      }
    }
  }


dom_var_dredge_relimp_ng_tie_0220 <- array(NaN,c(720,360,1,2))

  for(x in 1:720){
    for(y in 1:360){
      if(!is.na(dom_var_dredge_relimp_ng_0220[x,y,1,2])){
        dom_var_dredge_relimp_ng_tie_0220[x,y,1,] <- dom_var_dredge_relimp_ng_0220[x,y,1,]
      }
    }
  }


# remove one useless dimension
dom_var_dredge_relimp_ng_8201 <- dom_var_dredge_relimp_ng_8201[,,1,]
dom_var_dredge_relimp_ng_tie_8201 <- dom_var_dredge_relimp_ng_tie_8201[,,1,]
dom_water_var_ng_8201 <- dom_water_var_ng_8201[,,1,]
rank_dom_water_var_ng_8201 <- rank_dom_water_var_ng_8201[,,1]
dom_var_dredge_relimp_ng_all_8201 <- dom_var_dredge_relimp_ng_all_8201[,,1,]

dom_var_dredge_relimp_ng_0220 <- dom_var_dredge_relimp_ng_0220[,,1,]
dom_var_dredge_relimp_ng_tie_0220 <- dom_var_dredge_relimp_ng_tie_0220[,,1,]
dom_water_var_ng_0220 <- dom_water_var_ng_0220[,,1,]
rank_dom_water_var_ng_0220 <- rank_dom_water_var_ng_0220[,,1]

dom_var_dredge_relimp_ng_all_0220 <- dom_var_dredge_relimp_ng_all_0220[,,1,]


vars_bar <- c(vars[1:8],'tie') #variables displayed in barplot for figure showing all individual variables of the regression analysis 
vars_bar_cluster <- c('Water','Energy') #variables displayed in barplot for figure showing only energy vs water variables regression analysis 

vars_bar_expl_extr <- c('Water average','Water extreme','Energy average','Energy extreme','Stats')
wtr_rgy_bar_expl_extr <- c('Water','Water','Energy','Energy','Stats')

area_mean_dom_var_8201 <- setNames(data.frame(matrix(ncol = 2, nrow = 0)),
                              c("area","var"))

area_expl_extr_8201 <- setNames(data.frame(matrix(ncol = 3, nrow = 0)),
                              c("wtr_rgy","area","var"))
dom_var_expl_extr_8201 <- dom_var_dredge_relimp_ng_8201
dom_var_expl_extr_8201[,,1][which(dom_var_dredge_relimp_ng_8201[,,1] < 5)] <- 1
dom_var_expl_extr_8201[,,1][which(dom_var_dredge_relimp_ng_8201[,,1] == 2)] <- 2
dom_var_expl_extr_8201[,,1][which(dom_var_dredge_relimp_ng_8201[,,1] > 4)] <- 3
dom_var_expl_extr_8201[,,1][which(dom_var_dredge_relimp_ng_8201[,,1] == 6)] <- 4
dom_var_expl_extr_8201[,,1][which(dom_var_dredge_relimp_ng_8201[,,1] == 9)] <- 5


for(v in 1:(length(vars)-1)){ # loop over all variables
  area_mean_dom_var_8201 <- rbind(area_mean_dom_var_8201,
                             data.frame("area" = (sum(area.array[which(dom_var_dredge_relimp_ng_8201[,,1] == v)], na.rm = T)/total_land_area)*100,
                                        "var" = vars_bar[v]))
}
area_mean_dom_var_cluster_8201 <- data.frame("area" = (sum(area.array[which(dom_var_dredge_relimp_ng_8201[,,1] < 5)], na.rm = T)/total_land_area)*100,
                                        "var" = "Water")
area_mean_dom_var_cluster_8201 <- rbind(area_mean_dom_var_cluster_8201,
                                   data.frame("area" = (sum(area.array[which(dom_var_dredge_relimp_ng_8201[,,1] > 4 & dom_var_dredge_relimp_ng_8201[,,1] < 9)], na.rm = T)/total_land_area)*100,
                                              "var" = "Energy"))

for(v in 1:5){ # loop over all variables
  area_expl_extr_8201 <- rbind(area_expl_extr_8201,
                          data.frame("wtr_rgy" = wtr_rgy_bar_expl_extr[v],
                                     "area" = (sum(area.array[which(dom_var_expl_extr_8201[,,1] == v)], na.rm = T)/total_land_area)*100,
                                     "var" = vars_bar_expl_extr[v]))
}


# This computes the area-% where there are ties and add 1/2 of that area to the respective tied variables
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(dom_var_dredge_relimp_ng_tie_8201[x,y,1])){
      # convert array into vector for one grid cell
      mean_dom_var_tie_c <- c(dom_var_dredge_relimp_ng_tie_8201[x,y,][which(!is.na(dom_var_dredge_relimp_ng_tie_8201[x,y,]))])
      mean_dom_var_tie_c_cluster <- c()
      # now cluster energy and water variables together in a new variable
      for(tie in mean_dom_var_tie_c){
        if(tie < 5){
          mean_dom_var_tie_c_cluster <- c(mean_dom_var_tie_c_cluster, 1)
        }else{
          mean_dom_var_tie_c_cluster <- c(mean_dom_var_tie_c_cluster, 2)
        }
      }
      area_mean_dom_var_8201$area[mean_dom_var_tie_c] <- area_mean_dom_var_8201$area[mean_dom_var_tie_c] + ((area.array[x,y]/total_land_area)*100)/length(mean_dom_var_tie_c)
      
      area_mean_dom_var_cluster_8201$area[mean_dom_var_tie_c_cluster] <- area_mean_dom_var_cluster_8201$area[mean_dom_var_tie_c_cluster] + ((area.array[x,y]/total_land_area)*100)/length(mean_dom_var_tie_c_cluster)
    }
  }
}

area_mean_dom_var_8201$var_fac <- factor(area_mean_dom_var_8201$var, levels = vars_bar)
area_mean_dom_var_cluster_8201$var_fac <- factor(area_mean_dom_var_cluster_8201$var, levels = vars_bar_cluster)

area_expl_extr_8201$var_fac <- factor(area_expl_extr_8201$var, levels = vars_bar_expl_extr)
area_expl_extr_8201$wtr_rgy_fac <- factor(area_expl_extr_8201$wtr_rgy, levels = unique(wtr_rgy_bar_expl_extr))


# use this to find out which models only have a single explanatory linear model with a single variable in it (all LAI variability is explained by one single variable)
how_many_ties_8201 <- array(NaN,c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(dom_var_dredge_relimp_ng_tie_8201[x,y,1])){
      how_many_ties_8201[x,y] <- 1
    }
  }
}

dom_var_ts_8201.df <- setNames(data.frame(matrix(ncol = 6, nrow = 0)),
                          c("dom_var_dredge_relimp_ng","dom_water_var_ng","rank_dom_water_var_ng","lon","lat","tie"))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(dom_var_dredge_relimp_ng_8201[x,y,1])){ # every grid cell that has linear models with values. If there is no water variable in any of the regression models of that grid cell, dom_water_var_ng[x,y,h] will be NaN and in the plot the last value of cols_dom_water_var will be assigned ('no water variable' = 'grey40')
      dom_var_ts_8201.df <- rbind(dom_var_ts_8201.df,
                             data.frame("dom_var_dredge_relimp_ng" = dom_var_dredge_relimp_ng_8201[x,y,1],
                                        "dom_water_var_ng" = dom_water_var_ng_8201[x,y,1],
                                        "rank_dom_water_var_ng" = rank_dom_water_var_ng_8201[x,y],
                                        "lon" = lon[x],
                                        "lat" = lat[y],
                                        "tie" = how_many_ties_8201[x,y]
                             ))
    }
  }
}
# now cluster energy and water variables together in a new variable
dom_var_ts_8201.df$dom_var_dredge_relimp_ng_cluster <- NaN
dom_var_ts_8201.df$dom_var_dredge_relimp_ng_cluster[which(dom_var_ts_8201.df$dom_var_dredge_relimp_ng < 5)] <- 1
dom_var_ts_8201.df$dom_var_dredge_relimp_ng_cluster[which(dom_var_ts_8201.df$dom_var_dredge_relimp_ng > 4)] <- 2
dom_var_ts_8201.df$dom_var_dredge_relimp_ng_cluster[which(dom_var_ts_8201.df$dom_var_dredge_relimp_ng == 9)] <- 3

# another cluster for explicit average and extremes
dom_var_ts_8201.df$expl_extr <- NaN
dom_var_ts_8201.df$expl_extr[which(dom_var_ts_8201.df$dom_var_dredge_relimp_ng < 5)] <- 1
dom_var_ts_8201.df$expl_extr[which(dom_var_ts_8201.df$dom_var_dredge_relimp_ng == 2)] <- 2
dom_var_ts_8201.df$expl_extr[which(dom_var_ts_8201.df$dom_var_dredge_relimp_ng > 4)] <- 3
dom_var_ts_8201.df$expl_extr[which(dom_var_ts_8201.df$dom_var_dredge_relimp_ng == 6)] <- 4
dom_var_ts_8201.df$expl_extr[which(dom_var_ts_8201.df$dom_var_dredge_relimp_ng == 9)] <- 5


# enter in rank_dom_water_var_ng the lowest rank + 1, which indicates that there are linear models
dom_var_ts_8201.df$dom_water_var_ng[which(dom_var_ts_8201.df$dom_var_dredge_relimp_ng == length(vars) & is.na(dom_var_ts_8201.df$dom_water_var_ng))] <- max(unique(dom_var_ts_8201.df$dom_water_var_ng), na.rm = T) + 1
dom_var_ts_8201.df$rank_dom_water_var_ng[which(dom_var_ts_8201.df$dom_var_dredge_relimp_ng == length(vars) & is.na(dom_var_ts_8201.df$rank_dom_water_var_ng))] <- max(unique(dom_var_ts_8201.df$rank_dom_water_var_ng), na.rm = T) + 1

#Define labels and colors
labels_dom_var <- c("Soil moisture","Minimum soil moisture","Precipitation","Vapour Pressure Deficit","Air Temperature","Maximum daily air temperature","Incoming shortwave radiation","Net radiation",expression("adjusted R"^2*" < 0.36"))
labels_dom_var_cluster <- c('Water','Energy',expression("adjusted R"^2*" < 0.36"))
labels_expl_extr <- c('Water average','Water extreme','Average extreme','Energy extreme',expression("adjusted R"^2*" < 0.36"))

cols_dom_var <- c(brewer.pal(11,"RdYlBu")[9], 'darkblue', 'darkturquoise', 'sienna', brewer.pal(11,"RdYlBu")[3], 'red4', 'mediumorchid','gold1', 'snow2')
cols_dom_var_cluster <- c('darkturquoise', brewer.pal(11,"RdYlBu")[3], 'snow2')
cols_expl_extr <- c('darkturquoise', 'darkblue', brewer.pal(11,"RdYlBu")[3], 'red4', 'snow2')

cols_dom_water_var <- c(cols_dom_var[1:length(which(var_type != 'energy'))],'snow2','grey40')
labels_dom_water_var <- c(labels_dom_var[1:length(which(var_type != 'energy'))],expression("adjusted R"^2*" < 0.36"),'No water variable')
myvalues_dom_var <- seq(0.5,length(vars)+0.5,1)

dom_var_ts_8201.df$cuts_dom_var_dredge_relimp_ng <- cut(dom_var_ts_8201.df$dom_var_dredge_relimp_ng, myvalues_dom_var, include.lowest = T)
dom_var_ts_8201.df$cuts_dom_var_dredge_relimp_ng_cluster <- cut(dom_var_ts_8201.df$dom_var_dredge_relimp_ng_cluster, myvalues_dom_var[1:4], include.lowest = T)

dom_var_ts_8201.df$cuts_expl_extr <- cut(dom_var_ts_8201.df$expl_extr, myvalues_dom_var[1:6], include.lowest = T)

dom_var_ts_8201.df$cuts_rank_dom_water_var_ng <- cut(dom_var_ts_8201.df$rank_dom_water_var_ng, myvalues_dom_var, include.lowest = T)

ties_8201.df <- dom_var_ts_8201.df[which(dom_var_ts_8201.df$tie == 1),]

# This is a data set from the maptools package
data(wrld_simpl)

# Create a data.frame object for ggplot. ggplot requires a data frame.
mymap <- fortify(wrld_simpl)

#Plot map with the dominant variable resulting of the linear model
a_8201 <- ggplot(dom_var_ts_8201.df, aes(x=lon,y=lat,fill=cuts_dom_var_dredge_relimp_ng)) +
  geom_tile() +
  geom_point(inherit.aes = F, data = ties_8201.df, aes(x=lon,y=lat), size = .3) +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  # geom_sf(data = coastlines, color = "blue", fill = NA, size = .1) +
  scale_fill_manual("Dominant variable",
                    values = cols_dom_var,
                    labels = labels_dom_var,
                    drop = F) +
  scale_x_continuous("longitude",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("latitude",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, nrow = 2)) +
  ggtitle("1982 - 2001") +
  theme(legend.position = "bottom",
        legend.text = element_text(size=25),
        legend.title = element_text(size=25, hjust = 0.5),
        legend.spacing.x = unit(2.5, "lines"), 
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=25),
        axis.title = element_blank(),
        plot.title = element_text(size=24, hjust = 0.5),
        plot.tag.position = c(.55,0.03)
  )
a_8201
a_8201 <- ggplotGrob(a_8201)


bar_mean_dom_var_8201 <- ggplot(area_mean_dom_var_8201, aes(x = var_fac, y = area, fill = var_fac)) +
  geom_bar(stat='identity',col = 'black', width = 1, position = "stack") +
  geom_text(inherit.aes = F, data = subset(area_mean_dom_var_8201, area<1), aes(x=var,y=area+3.5,label=round(area,1)), size = 8) +
  geom_text(inherit.aes = F, data = subset(area_mean_dom_var_8201, area>1), aes(x=var,y=area+3.5,label=round(area,0)), size = 8) +
  scale_x_discrete("") +
  scale_y_continuous(" ", expand = c(0,0), limits = c(0, 30), position = "left") +
  scale_fill_manual(values = c("mrsol" = cols_dom_var[1],
                               "min_mrsol" = cols_dom_var[2],
                               "pr" = cols_dom_var[3],
                               "vpd" = cols_dom_var[4],
                               "tas" = cols_dom_var[5],
                               "max_tasmax" = cols_dom_var[6],
                               "rsds" = cols_dom_var[7],
                               "netrad" = cols_dom_var[8]),
                    drop = F) +
ggtitle(expression(paste("% of land area"))) +
  theme(legend.position = "none",
        legend.text = element_text(size=20),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=20, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=20),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.title = element_text(size=9),
        plot.title = element_text(size=16, hjust = 0.5)
  )
bar_mean_dom_var_8201
bar_mean_dom_var_8201 <- ggplotGrob(bar_mean_dom_var_8201)

gt1_8201 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1_8201 <- gtable_add_grob(gt1_8201, a_8201, t=1, b=32, l=1, r=32)
gt1_8201 <- gtable_add_grob(gt1_8201, bar_mean_dom_var_8201, t = 25, l = 3, b = 17, r = 10)
grid.draw(gt1_8201)

ggsave("path/to/your/storage/fig_S7a.png", plot = gt1_8201, width = 10*1.5, height = 7*1.5, units = "in")


lev_count_8201 <- 1
lev_c_8201 <- c()
for(lev in levels(dom_var_ts_8201.df$cuts_rank_dom_water_var_ng)){
  print(lev)
  if(lev %in% unique(dom_var_ts_8201.df$cuts_rank_dom_water_var_ng)){
    print("bingo")
    lev_c_8201 <- c(lev_c_8201, lev_count_8201)
  }
  lev_count_8201 <- lev_count_8201 + 1
}

labels_ranks <- c()
for(i in 1:(length(vars)-1)){
  labels_ranks <- c(labels_ranks, paste0("Rank ", i))
}



area_rank_8201 <- setNames(data.frame(matrix(ncol = 2, nrow = 0)),
                      c("area","rank"))
for(r in 1:5){ # loop over all ranks
  area_rank_8201 <- rbind(area_rank_8201,
                          data.frame("area" = (sum(area.array[which(rank_dom_water_var_ng_8201 >= myvalues_dom_var[c(1:6)][r] & rank_dom_water_var_ng_8201 < myvalues_dom_var[c(1:6)][r+1])], na.rm = T)/total_land_area)*100,
                                     "rank" = paste0('Rank', r)))
}

area_rank_8201 <- rbind(area_rank_8201,
                   data.frame("area" = (sum(area.array[which(dom_var_dredge_relimp_ng_8201[,,1] == 9)], na.rm = T)/total_land_area)*100,
                              "rank" = "Stats"))



a_cluster_8201 <- ggplot(dom_var_ts_8201.df, aes(x=lon,y=lat,fill=cuts_expl_extr)) +
  geom_tile() +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  scale_fill_manual("Dominant variable",
                    values = cols_expl_extr,
                    labels = labels_expl_extr,
                    drop = F) +
  scale_x_continuous("longitude",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("latitude",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, nrow = 2)) +
  ggtitle("1982 - 2001") +
  theme(legend.position = "none",
        # theme(legend.position = c(.75,.15),
        legend.text = element_text(size=18),
        legend.title = element_text(size=20, hjust = 0.5),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=25),
        axis.title = element_blank(),
        plot.title = element_text(size=24, hjust = 0.5),
        plot.tag.position = c(.55,0.03)
  )
a_cluster_8201

a_cluster_8201 <- ggplotGrob(a_cluster_8201)

for_legend <- ggplot(dom_var_ts_8201.df, aes(x=lon,y=lat,fill=cuts_expl_extr)) +
  geom_tile() +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  scale_fill_manual("Dominant variable",
                    values = cols_expl_extr,
                    labels = labels_expl_extr,
                    drop = F) +
  scale_x_continuous("longitude",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("latitude",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, nrow = 2)) +
  ggtitle("1982 - 2001") +
  theme(legend.position = "bottom",
        # theme(legend.position = c(.75,.15),
        legend.text = element_text(size=25),
        legend.title = element_text(size=25, hjust = 0.5),
        legend.spacing.x = unit(2, "lines"), 
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=25),
        axis.title = element_blank(),
        plot.title = element_text(size=24, hjust = 0.5)
  )
a_legend_8201 <- cowplot::get_legend(for_legend)

a_bar_expl_extr_8201 <- ggplot(area_expl_extr_8201, aes(x = wtr_rgy_fac, y = area, fill = var_fac)) +
  geom_bar(stat='identity',col = 'black', width = .75, position = "stack") +
  geom_text(inherit.aes = F, data = subset(area_expl_extr_8201, area<1), aes(x=wtr_rgy_fac,y=area+3.5,label=round(area,1)), size = 8) +
  geom_text(inherit.aes = F, data = subset(area_expl_extr_8201, area>1), aes(x=wtr_rgy_fac,y=area+3.5,label=round(area,0)), size = 8) +
  scale_x_discrete("") +
  scale_y_continuous("", expand = c(0,0), limits = c(0, 30)) +
  scale_fill_manual(values = c("Water average" = cols_dom_var[3],
                               "Water extreme" = cols_dom_var[2],
                               "Energy average" = cols_dom_var[5],
                               "Energy extreme" = cols_dom_var[6],
                               "Stats" = cols_dom_var[9]),
                    drop = F) +
  ggtitle(expression(paste("% of land area"))) +
  theme(legend.position = "none",
        legend.text = element_text(size=20),
        legend.title = element_text(size=20),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=20, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size=20),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.title = element_text(size=9),
        plot.title = element_text(size=16, hjust = 0.5)
  )

a_bar_expl_extr_8201
a_bar_expl_extr_8201 <- ggplotGrob(a_bar_expl_extr_8201)

gt1_cluster_8201 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1_cluster_8201 <- gtable_add_grob(gt1_cluster_8201, a_cluster_8201, t=1, b=32, l=1, r=32)
gt1_cluster_8201 <- gtable_add_grob(gt1_cluster_8201, a_bar_expl_extr_8201, t = 25, l = 3, b = 17, r = 10)
grid.draw(gt1_cluster_8201)


cc_8201 <- ggplot(dom_var_ts_8201.df, aes(x=lon,y=lat,fill=cuts_rank_dom_water_var_ng)) +
  geom_tile() +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  scale_fill_manual("Rank of highest ranking water variable",
                    values = c(rev(brewer.pal(11, "RdYlBu"))[c(1,2,3,5,7)],'snow2'),
                    # values = c(rev(brewer.pal(length(lev_c)+2, "RdYlBu"))[lev_c[1:(length(lev_c)-1)]],'snow2'),
                    labels = c(labels_ranks[lev_c_8201][1:(length(lev_c_8201)-1)],expression("adjusted R"^2*" < 0.36"))) +
  scale_x_continuous("",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, nrow = 2)) +
  theme(legend.position = "none",
        legend.text = element_text(size=18),
        legend.title = element_text(size=20, hjust = 0.5),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=25),
        # axis.title = element_text(size=20),
        axis.title = element_blank(),
        plot.title = element_text(size=24, hjust = 0.5),
        plot.tag.position = c(.55,0.03)
  )
cc_8201
cc_8201 <- ggplotGrob(cc_8201)

for_legend <- ggplot(dom_var_ts_8201.df, aes(x=lon,y=lat,fill=cuts_rank_dom_water_var_ng)) +
  geom_tile() +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  scale_fill_manual("Rank of highest ranking water variable",
                    values = c(rev(brewer.pal(11, "RdYlBu"))[c(1,2,3,5,7)],'snow2'),
                    # values = c(rev(brewer.pal(length(lev_c)+2, "RdYlBu"))[lev_c[1:(length(lev_c)-1)]],'snow2'),
                    labels = c(labels_ranks[lev_c_8201][1:(length(lev_c_8201)-1)],expression("adjusted R"^2*" < 0.36"))) +
  scale_x_continuous("",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, nrow = 2)) +
  theme(legend.position = "bottom",
        legend.text = element_text(size=25),
        legend.title = element_text(size=25, hjust = 0.5),
        legend.spacing.x = unit(2, "lines"), 
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
        plot.title = element_text(size=24, hjust = 0.5)
  )
c_legend_8201 <- cowplot::get_legend(for_legend)

c_bar_rank_8201 <- ggplot(area_rank_8201, aes(x = rank, y = area, fill = rank)) +
  geom_bar(stat='identity',col = 'black', width = 1, position = "stack") +
  geom_text(inherit.aes = F, data = subset(area_rank_8201, area<1), aes(x=rank,y=area+3.5,label=round(area,1)), size = 8) +
  geom_text(inherit.aes = F, data = subset(area_rank_8201, area>1), aes(x=rank,y=area+3.5,label=round(area,0)), size = 8) +
  scale_x_discrete("") +
  scale_y_continuous("", expand = c(0,0), limits = c(0, 30)) +
  scale_fill_manual(values = c("Rank1" = rev(brewer.pal(11, "RdYlBu"))[1],
                               "Rank2" = rev(brewer.pal(11, "RdYlBu"))[2],
                               "Rank3" = rev(brewer.pal(11, "RdYlBu"))[3],
                               "Rank4" = rev(brewer.pal(11, "RdYlBu"))[5],
                               "Rank5" = rev(brewer.pal(11, "RdYlBu"))[7],
                               "Stats" = 'snow2'),
                    drop = F) +
  ggtitle(expression(paste("% of land area"))) +
  theme(legend.position = "none",
        legend.text = element_text(size=20),
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
        axis.text = element_text(size=20),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.title = element_text(size=9),
        plot.title = element_text(size=16, hjust = 0.5)
  )
c_bar_rank_8201
c_bar_rank_8201 <- ggplotGrob(c_bar_rank_8201)


gt2_8201 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt2_8201 <- gtable_add_grob(gt2_8201, cc_8201, t=1, b=32, l=1, r=32)
gt2_8201 <- gtable_add_grob(gt2_8201, c_bar_rank_8201, t = 30, l = 3, b = 22, r = 10)
grid.draw(gt2_8201)

ddraft_plot_8201 <- grid.arrange(gt1_cluster_8201, a_legend_8201, gt2_8201, c_legend_8201, ncol = 1, heights = c(2, 0.5, 2, 0.5))

ggsave("path/to/your/storage/fig_4ac.png", plot = ddraft_plot_8201, width = 9*1.5, height = 14*1.5, units = "in")


#########################
###### 2002 - 2020 ######

area_mean_dom_var_0220 <- setNames(data.frame(matrix(ncol = 2, nrow = 0)),
                                   c("area","var"))

area_expl_extr_0220 <- setNames(data.frame(matrix(ncol = 3, nrow = 0)),
                                c("wtr_rgy","area","var"))
dom_var_expl_extr_0220 <- dom_var_dredge_relimp_ng_0220
dom_var_expl_extr_0220[,,1][which(dom_var_dredge_relimp_ng_0220[,,1] < 5)] <- 1
dom_var_expl_extr_0220[,,1][which(dom_var_dredge_relimp_ng_0220[,,1] == 2)] <- 2
dom_var_expl_extr_0220[,,1][which(dom_var_dredge_relimp_ng_0220[,,1] > 4)] <- 3
dom_var_expl_extr_0220[,,1][which(dom_var_dredge_relimp_ng_0220[,,1] == 6)] <- 4

dom_var_expl_extr_0220[,,1][which(dom_var_dredge_relimp_ng_0220[,,1] == 9)] <- 5


for(v in 1:(length(vars)-1)){ # loop over all variables
  area_mean_dom_var_0220 <- rbind(area_mean_dom_var_0220,
                                  data.frame("area" = (sum(area.array[which(dom_var_dredge_relimp_ng_0220[,,1] == v)], na.rm = T)/total_land_area)*100,
                                             "var" = vars_bar[v]))
}
area_mean_dom_var_cluster_0220 <- data.frame("area" = (sum(area.array[which(dom_var_dredge_relimp_ng_0220[,,1] < 5)], na.rm = T)/total_land_area)*100,
                                             "var" = "Water")
area_mean_dom_var_cluster_0220 <- rbind(area_mean_dom_var_cluster_0220,
                                        data.frame("area" = (sum(area.array[which(dom_var_dredge_relimp_ng_0220[,,1] > 4 & dom_var_dredge_relimp_ng_0220[,,1] < 9)], na.rm = T)/total_land_area)*100,
                                                   "var" = "Energy"))

for(v in 1:5){ # loop over all variables
  area_expl_extr_0220 <- rbind(area_expl_extr_0220,
                               data.frame("wtr_rgy" = wtr_rgy_bar_expl_extr[v],
                                          "area" = (sum(area.array[which(dom_var_expl_extr_0220[,,1] == v)], na.rm = T)/total_land_area)*100,
                                          "var" = vars_bar_expl_extr[v]))
}

# This computes the area-% where there are ties and add 1/2 of that area to the respective tied variables
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(dom_var_dredge_relimp_ng_tie_0220[x,y,1])){
      # convert array into vector for one grid cell
      mean_dom_var_tie_c <- c(dom_var_dredge_relimp_ng_tie_0220[x,y,][which(!is.na(dom_var_dredge_relimp_ng_tie_0220[x,y,]))])
      mean_dom_var_tie_c_cluster <- c()
      # now cluster energy and water variables together in a new variable
      for(tie in mean_dom_var_tie_c){
        if(tie < 5){
          mean_dom_var_tie_c_cluster <- c(mean_dom_var_tie_c_cluster, 1)
        }else{
          mean_dom_var_tie_c_cluster <- c(mean_dom_var_tie_c_cluster, 2)
        }
      }
      area_mean_dom_var_0220$area[mean_dom_var_tie_c] <- area_mean_dom_var_0220$area[mean_dom_var_tie_c] + ((area.array[x,y]/total_land_area)*100)/length(mean_dom_var_tie_c)
      
      area_mean_dom_var_cluster_0220$area[mean_dom_var_tie_c_cluster] <- area_mean_dom_var_cluster_0220$area[mean_dom_var_tie_c_cluster] + ((area.array[x,y]/total_land_area)*100)/length(mean_dom_var_tie_c_cluster)
    }
  }
}
area_mean_dom_var_0220$var_fac <- factor(area_mean_dom_var_0220$var, levels = vars_bar)
area_mean_dom_var_cluster_0220$var_fac <- factor(area_mean_dom_var_cluster_0220$var, levels = vars_bar_cluster)

area_expl_extr_0220$var_fac <- factor(area_expl_extr_0220$var, levels = vars_bar_expl_extr)
area_expl_extr_0220$wtr_rgy_fac <- factor(area_expl_extr_0220$wtr_rgy, levels = unique(wtr_rgy_bar_expl_extr))


# use this to find out which models only have a single explanatory linear model with a single variable in it (all LAI variability is explained by one single variable)
how_many_ties_0220 <- array(NaN,c(720,360))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(dom_var_dredge_relimp_ng_tie_0220[x,y,1])){
      how_many_ties_0220[x,y] <- 1
    }
  }
}

dom_var_ts_0220.df <- setNames(data.frame(matrix(ncol = 6, nrow = 0)),
                               c("dom_var_dredge_relimp_ng","dom_water_var_ng","rank_dom_water_var_ng","lon","lat","tie"))
for(x in 1:720){
  for(y in 1:360){
    if(!is.na(dom_var_dredge_relimp_ng_0220[x,y,1])){ # every grid cell that has linear models with values. If there is no water variable in any of the regression models of that grid cell, dom_water_var_ng[x,y,h] will be NaN and in the plot the last value of cols_dom_water_var will be assigned ('no water variable' = 'grey40')
      dom_var_ts_0220.df <- rbind(dom_var_ts_0220.df,
                                  data.frame("dom_var_dredge_relimp_ng" = dom_var_dredge_relimp_ng_0220[x,y,1],
                                             "dom_water_var_ng" = dom_water_var_ng_0220[x,y,1],
                                             "rank_dom_water_var_ng" = rank_dom_water_var_ng_0220[x,y],
                                             "lon" = lon[x],
                                             "lat" = lat[y],
                                             "tie" = how_many_ties_0220[x,y]
                                  ))
    }
  }
}
# now cluster energy and water variables together in a new variable
dom_var_ts_0220.df$dom_var_dredge_relimp_ng_cluster <- NaN
dom_var_ts_0220.df$dom_var_dredge_relimp_ng_cluster[which(dom_var_ts_0220.df$dom_var_dredge_relimp_ng < 5)] <- 1
dom_var_ts_0220.df$dom_var_dredge_relimp_ng_cluster[which(dom_var_ts_0220.df$dom_var_dredge_relimp_ng > 4)] <- 2
dom_var_ts_0220.df$dom_var_dredge_relimp_ng_cluster[which(dom_var_ts_0220.df$dom_var_dredge_relimp_ng == 9)] <- 3

# another cluster for explicit average and extremes
dom_var_ts_0220.df$expl_extr <- NaN
dom_var_ts_0220.df$expl_extr[which(dom_var_ts_0220.df$dom_var_dredge_relimp_ng < 5)] <- 1
dom_var_ts_0220.df$expl_extr[which(dom_var_ts_0220.df$dom_var_dredge_relimp_ng == 2)] <- 2
dom_var_ts_0220.df$expl_extr[which(dom_var_ts_0220.df$dom_var_dredge_relimp_ng > 4)] <- 3
dom_var_ts_0220.df$expl_extr[which(dom_var_ts_0220.df$dom_var_dredge_relimp_ng == 6)] <- 4
dom_var_ts_0220.df$expl_extr[which(dom_var_ts_0220.df$dom_var_dredge_relimp_ng == 9)] <- 5


# enter in rank_dom_water_var_ng the lowest rank + 1, which indicates that there are linear models
dom_var_ts_0220.df$dom_water_var_ng[which(dom_var_ts_0220.df$dom_var_dredge_relimp_ng == length(vars) & is.na(dom_var_ts_0220.df$dom_water_var_ng))] <- max(unique(dom_var_ts_0220.df$dom_water_var_ng), na.rm = T) + 1
dom_var_ts_0220.df$rank_dom_water_var_ng[which(dom_var_ts_0220.df$dom_var_dredge_relimp_ng == length(vars) & is.na(dom_var_ts_0220.df$rank_dom_water_var_ng))] <- max(unique(dom_var_ts_0220.df$rank_dom_water_var_ng), na.rm = T) + 1

dom_var_ts_0220.df$cuts_dom_var_dredge_relimp_ng <- cut(dom_var_ts_0220.df$dom_var_dredge_relimp_ng, myvalues_dom_var, include.lowest = T)
dom_var_ts_0220.df$cuts_dom_var_dredge_relimp_ng_cluster <- cut(dom_var_ts_0220.df$dom_var_dredge_relimp_ng_cluster, myvalues_dom_var[1:4], include.lowest = T)

dom_var_ts_0220.df$cuts_expl_extr <- cut(dom_var_ts_0220.df$expl_extr, myvalues_dom_var[1:6], include.lowest = T)

dom_var_ts_0220.df$cuts_dom_water_var_ng <- cut(dom_var_ts_0220.df$dom_water_var_ng, myvalues_dom_var[1:(length(which(var_type != 'energy'))+2)], include.lowest = T)
dom_var_ts_0220.df$cuts_rank_dom_water_var_ng <- cut(dom_var_ts_0220.df$rank_dom_water_var_ng, myvalues_dom_var, include.lowest = T)

ties_0220.df <- dom_var_ts_0220.df[which(dom_var_ts_0220.df$tie == 1),]


a_0220 <- ggplot(dom_var_ts_0220.df, aes(x=lon,y=lat,fill=cuts_dom_var_dredge_relimp_ng)) +
  geom_tile() +
  geom_point(inherit.aes = F, data = ties_0220.df, aes(x=lon,y=lat), size = .5) +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  scale_fill_manual("Dominant variable",
                    values = cols_dom_var,
                    labels = labels_dom_var,
                    drop = F) +
  scale_x_continuous("longitude",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("latitude",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, nrow = 2)) +
  ggtitle("2002 - 2020") +
  theme(legend.position = "bottom",
        # theme(legend.position = c(.75,.15),
        legend.text = element_text(size=25),
        legend.title = element_text(size=25, hjust = 3.5),
        legend.spacing.x = unit(2.5, "lines"), 
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=25),
        axis.title = element_blank(),
        plot.title = element_text(size=24, hjust = 0.5),
        plot.tag.position = c(.55,0.03)
  )
a_0220
a_0220 <- ggplotGrob(a_0220)


bar_mean_dom_var_0220 <- ggplot(area_mean_dom_var_0220, aes(x = var_fac, y = area, fill = var_fac)) +
  geom_bar(stat='identity',col = 'black', width = 1, position = "stack") +
  geom_text(inherit.aes = F, data = subset(area_mean_dom_var_0220, area<1), aes(x=var,y=area+3.5,label=round(area,1)), size = 8) +
  geom_text(inherit.aes = F, data = subset(area_mean_dom_var_0220, area>1), aes(x=var,y=area+3.5,label=round(area,0)), size = 8) +
  scale_x_discrete("") +
  scale_y_continuous(" ", expand = c(0,0), limits = c(0, 30), position = "left") +
  scale_fill_manual(values = c("mrsol" = cols_dom_var[1],
                               "min_mrsol" = cols_dom_var[2],
                               "pr" = cols_dom_var[3],
                               "vpd" = cols_dom_var[4],
                               "tas" = cols_dom_var[5],
                               "max_tasmax" = cols_dom_var[6],
                               "rsds" = cols_dom_var[7],
                               "netrad" = cols_dom_var[8]),
                    drop = F) +
ggtitle(expression(paste("% of land area"))) +
  theme(legend.position = "none",
        legend.text = element_text(size=20),
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
        axis.text = element_text(size=20),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.title = element_text(size=9),
        plot.title = element_text(size=16, hjust = 0.5)
  )
bar_mean_dom_var_0220
bar_mean_dom_var_0220 <- ggplotGrob(bar_mean_dom_var_0220)

gt1_0220 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1_0220 <- gtable_add_grob(gt1_0220, a_0220, t=1, b=32, l=1, r=32)
gt1_0220 <- gtable_add_grob(gt1_0220, bar_mean_dom_var_0220, t = 25, l = 3, b = 17, r = 10)
grid.draw(gt1_0220)

ggsave("path/to/your/storage/fig_S7b.png", plot = gt1_0220, width = 10*1.5, height = 7*1.5, units = "in")


lev_count_0220 <- 1
lev_c_0220 <- c()
for(lev in levels(dom_var_ts_0220.df$cuts_rank_dom_water_var_ng)){
  print(lev)
  if(lev %in% unique(dom_var_ts_0220.df$cuts_rank_dom_water_var_ng)){
    print("bingo")
    lev_c_0220 <- c(lev_c_0220, lev_count_0220)
  }
  lev_count_0220 <- lev_count_0220 + 1
}

labels_ranks <- c()
for(i in 1:(length(vars)-1)){
  labels_ranks <- c(labels_ranks, paste0("Rank ", i))
}


area_rank_0220 <- setNames(data.frame(matrix(ncol = 2, nrow = 0)),
                           c("area","rank"))

for(r in 1:5){ # loop over all ranks
  area_rank_0220 <- rbind(area_rank_0220,
                          data.frame("area" = (sum(area.array[which(rank_dom_water_var_ng_0220 >= myvalues_dom_var[c(1:6)][r] & rank_dom_water_var_ng_0220 < myvalues_dom_var[c(1:6)][r+1])], na.rm = T)/total_land_area)*100,
                                     "rank" = paste0('Rank', r)))
}

area_rank_0220 <- rbind(area_rank_0220,
                        data.frame("area" = (sum(area.array[which(dom_var_dredge_relimp_ng_0220[,,1] == 9)], na.rm = T)/total_land_area)*100,
                                   "rank" = "Stats"))



a_cluster_0220 <- ggplot(dom_var_ts_0220.df, aes(x=lon,y=lat,fill=cuts_expl_extr)) +
  geom_tile() +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  geom_rect(data = boxes, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), 
              fill= NA, col = "deeppink", inherit.aes = F, size = 1.5) + 
  scale_fill_manual("Dominant variable",
                    values = cols_expl_extr,
                    labels = labels_expl_extr,
                    drop = F) +
  scale_x_continuous("longitude",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("latitude",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, nrow = 2)) +
  ggtitle("2002 - 2020") +
  theme(legend.position = "none",
        # theme(legend.position = c(.75,.15),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20, hjust = 0.5),
        strip.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(size=18, face="bold", margin = margin(0, 0, .5, 0, "cm")),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA, size = 1),
        panel.background = element_blank(),
        axis.text = element_text(size=25),
        axis.title = element_blank(),
        plot.title = element_text(size=24, hjust = 0.5),
        plot.tag.position = c(.55,0.03)
  )
a_cluster_0220

a_cluster_0220 <- ggplotGrob(a_cluster_0220)

a_bar_expl_extr_0220 <- ggplot(area_expl_extr_0220, aes(x = wtr_rgy_fac, y = area, fill = var_fac)) +
  geom_bar(stat='identity',col = 'black', width = .75, position = "stack") +
  geom_text(inherit.aes = F, data = subset(area_expl_extr_0220, area<1), aes(x=wtr_rgy_fac,y=area+3.5,label=round(area,1)), size = 8) +
  geom_text(inherit.aes = F, data = subset(area_expl_extr_0220, area>1), aes(x=wtr_rgy_fac,y=area+3.5,label=round(area,0)), size = 8) +
  scale_x_discrete("") +
  scale_y_continuous("", expand = c(0,0), limits = c(0, 30)) +
  scale_fill_manual(values = c("Water average" = cols_dom_var[3],
                               "Water extreme" = cols_dom_var[2],
                               "Energy average" = cols_dom_var[5],
                               "Energy extreme" = cols_dom_var[6],
                               "Stats" = cols_dom_var[9]),
                    drop = F) +
  ggtitle(expression(paste("% of land area"))) +
  theme(legend.position = "none",
        legend.text = element_text(size=20),
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
        axis.text = element_text(size=20),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.title = element_text(size=9),
        plot.title = element_text(size=16, hjust = 0.5)
  )
a_bar_expl_extr_0220
a_bar_expl_extr_0220 <- ggplotGrob(a_bar_expl_extr_0220)

gt1_cluster_0220 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1_cluster_0220 <- gtable_add_grob(gt1_cluster_0220, a_cluster_0220, t=1, b=32, l=1, r=32)
gt1_cluster_0220 <- gtable_add_grob(gt1_cluster_0220, a_bar_expl_extr_0220, t = 25, l = 3, b = 17, r = 10)
grid.draw(gt1_cluster_0220)


cc_0220 <- ggplot(dom_var_ts_0220.df, aes(x=lon,y=lat,fill=cuts_rank_dom_water_var_ng)) +
  geom_tile() +
  geom_map(data = mymap, map = mymap, aes(x = long, y = lat, map_id = id),
           inherit.aes = F, fill = NA, color = "black", size = .1) +
  geom_rect(data = boxes, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), 
              fill= NA, col = "deeppink", inherit.aes = F, size = 1.5) + 
  scale_fill_manual("Rank of highest ranking water variable",
                    values = c(rev(brewer.pal(11, "RdYlBu"))[c(1,2,3,5,7)],'snow2'),
                    # values = c(rev(brewer.pal(length(lev_c)+2, "RdYlBu"))[lev_c[1:(length(lev_c)-1)]],'snow2'),
                    labels = c(labels_ranks[lev_c_0220][1:(length(lev_c_0220)-1)],expression("adjusted R"^2*" < 0.36"))) +
  scale_x_continuous("",
                     limits=c(-180,180),
                     expand=c(0,0)) +
  scale_y_continuous("",
                     limits=c(-60,70),
                     expand=c(0,0)) +
  guides(fill = guide_legend(order = 1, title.position = "top", title.hjust = .5, nrow = 2)) +
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
        axis.text = element_text(size=25),
        # axis.title = element_text(size=20),
        axis.title = element_blank(),
        plot.title = element_text(size=24, hjust = 0.5),
        plot.tag.position = c(.55,0.03)
  )
cc_0220
cc_0220 <- ggplotGrob(cc_0220)


c_bar_rank_0220 <- ggplot(area_rank_0220, aes(x = rank, y = area, fill = rank)) +
  geom_bar(stat='identity',col = 'black', width = 1, position = "stack") +
  geom_text(inherit.aes = F, data = subset(area_rank_0220, area<1), aes(x=rank,y=area+3.5,label=round(area,1)), size = 8) +
  geom_text(inherit.aes = F, data = subset(area_rank_0220, area>1), aes(x=rank,y=area+3.5,label=round(area,0)), size = 8) +
  scale_x_discrete("") +
  scale_y_continuous("", expand = c(0,0), limits = c(0, 30)) +
  scale_fill_manual(values = c("Rank1" = rev(brewer.pal(11, "RdYlBu"))[1],
                               "Rank2" = rev(brewer.pal(11, "RdYlBu"))[2],
                               "Rank3" = rev(brewer.pal(11, "RdYlBu"))[3],
                               "Rank4" = rev(brewer.pal(11, "RdYlBu"))[5],
                               "Rank5" = rev(brewer.pal(11, "RdYlBu"))[7],
                               "Stats" = 'snow2'),
                    drop = F) +
  ggtitle(expression(paste("% of land area"))) +
  theme(legend.position = "none",
        legend.text = element_text(size=20),
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
        axis.text = element_text(size=20),
        axis.text.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.title = element_text(size=9),
        plot.title = element_text(size=16, hjust = 0.5)
  )
c_bar_rank_0220
c_bar_rank_0220 <- ggplotGrob(c_bar_rank_0220)


gt2_0220 <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt2_0220 <- gtable_add_grob(gt2_0220, cc_0220, t=1, b=32, l=1, r=32)
gt2_0220 <- gtable_add_grob(gt2_0220, c_bar_rank_0220, t = 30, l = 3, b = 22, r = 10)
grid.draw(gt2_0220)

ddraft_plot_0220 <- grid.arrange(gt1_cluster_0220, a_legend_8201, gt2_0220, c_legend_8201, ncol = 1, heights = c(2, 0.5, 2, 0.5))

ggsave("path/to/your/storage/fig_4bd.png", plot = ddraft_plot_0220, width = 9*1.5, height = 14*1.5, units = "in")


save.image("Rdata/nat_wat_pers/compl_scriptv08.RData")
