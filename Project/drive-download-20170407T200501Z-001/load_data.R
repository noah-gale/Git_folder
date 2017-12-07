setwd("~/Box_Sync/UM_Winter_2017/STATS_503/Project/503_proj")
library(foreign)
library(dplyr)
library(ggplot2)

files = dir()[c(1:5)]

data = lapply(files, read.arff)

polish_dt = data[[1]]
polish_dt$year = 1
for(i in 2:length(data)) { 
        
        df = data[[i]]
        df$year = i
        
        polish_dt = rbind(polish_dt,df)
        
}