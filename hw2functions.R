
# HOMEWORK 2

# Load packages
library(ncdf4)
library(tidyverse)
library(here)
library(chron)
library(RColorBrewer)
library(lattice)
library(abind)
library(lubridate)

# Load data
cesm2historical <- nc_open(here('data/cesm2historical1.nc'))

# historical 
  lon <- ncvar_get(cesm2historical, "lon")
  lat <- ncvar_get(cesm2historical,  "lat")
  nlon <- dim(lon)
  nlat <- dim(lat)
  
  # define time values
  time <- ncvar_get(cesm2historical, "time_bnds")
  tunits <- ncatt_get(cesm2historical, "time_bnds", "units")
  nt <- dim(time)
  tustr <- strsplit(tunits$value, " ")
  tdstr <- strsplit(unlist(tustr)[3], "-")
  tmonth <- as.integer(unlist(tdstr)[2])
  tday <- as.integer(unlist(tdstr)[3])
  tyear <- as.integer(unlist(tdstr)[1])
  rtime <- chron(time, origin= c(tmonth, tday, tyear))

  # work with temperature values
  TS <- ncvar_get(cesm2historical, "tas")
  dlname <- ncatt_get(cesm2historical, "tas", "longname")
  dunits <- ncatt_get(cesm2historical, "tas", "units")
  fillvalue <- ncatt_get(cesm2historical, "tas", "_FillValue")

lats = which(lat >= 32 & lat <= 35)
lons = which(lon >= 241 & lon <= 243)

tsavg <- apply(TS[lons, lats, ], 3, mean)

clim <- data.frame(time = rtime, tsavg = tsavg)

yrclim = clim %>% 
  group_by(year(rtime)) %>% 
  summarize(Tann = mean(tsavg))

yrclim$dt = unique(year(rtime))

ggplot(yrclim, aes(dt, Tann-273.15)) +
  geom_point() +
  labs(y = "Southern CA Temperature", x ="Year")+ geom_smooth(method="lm")



