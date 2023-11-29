



cat("\014")
rm(list = ls())

library(ggplot2)
library(plot3D)
library(lubridate)
library(tidyr)
library(zoo)
library(ggcorrplot)


# -------------------------- SOURCE FILES ----------------------------- 

source('C:/Users/camil/Documents/Programs/Thetis/loadFunctions/loadCTD.R')
source('C:/Users/camil/Documents/Programs/Thetis/loadFunctions/loadTriplet_ECO2.R')
source('C:/Users/camil/Documents/Programs/Thetis/loadFunctions/get_dxdy.R')

source('C:/Users/camil/Documents/Programs/Thetis/get_thermocline.R')
source('C:/Users/camil/Documents/Programs/Thetis/find_ML_depth.R')

# -------------------------- SETTINGS -------------------------

path_data <- "C:/Users/camil/Documents/Data/Thetis_Data/decimated_data_extracted/2020q1/"

# ---------------------- GET LIST OF FILES ---------------------- 

list.my_files <- list.files(path_data, recursive = T)
if(strtrim(list.my_files[1],3) == '201') {
  deb = 8
} else {
  deb = 1
}

list_records_all <- substr(list.my_files, start = deb, stop = deb+7)
list_extensions_all <- substr(list.my_files, start = deb+13, stop = deb+17)

isF <- TRUE
for (extension in c("CTD.t")){
  my_ind <- which(list_extensions_all == extension)
  list_records <- unique(list_records_all[my_ind])
  list_files <- unique(list.my_files[my_ind])
  
  df_records.temp <- data.frame(extension = extension,
                                list_records = list_records,
                                list_files = list_files)
  if (isF){
    df_records <- df_records.temp
    isF = FALSE
  } else {
    df_records <- rbind(df_records, df_records.temp)
  }
}

df_records_gather_all <- spread(df_records, extension, list_files)
df_records_gather_all <- df_records_gather_all[!is.na(df_records_gather_all$CTD.t),]



# ---------------------- MAIN ---------------------- 
setwd(path_data)
isF = T
for (p in seq(1, length(df_records_gather_all$CTD.t))){
  print(paste("profile ",p,"/",length(df_records_gather_all$CTD.t),sep = ""))
  
  my_profile = loadCTD(as.character(df_records_gather_all$CTD.t[p]))
  my_profile <- my_profile[my_profile$depth < 70,]
  
  if (min(my_profile$depth)<5 & max(my_profile$depth)>25){
    
    ML <- find_ML_depth(depth = my_profile$depth, w_Temp = my_profile$Temperature, CV_threshold = 0.02, diff_threshold = 0.95)
    z_thrmcln <- get_thermocline(depth = my_profile$depth, w_Temp = my_profile$Temperature, rho = my_profile$rho)
    
    df_res_temp <- data.frame(profile_ID = p,
                              ID = df_records_gather_all$list_records[p],
                              filename = df_records_gather_all$CTD.t[p],
                              date = min(my_profile$date),
                              z_ML = ML,
                              z_thrmcln = z_thrmcln$thermocline_depth,
                              z_thrmcln_top = z_thrmcln$top,
                              z_thrmcln_bottom = z_thrmcln$bottom)
    if(isF){
      df_res <- df_res_temp
      isF = F
    } else {
      df_res <- rbind(df_res, df_res_temp)
    }
  }
}

df_res <- df_res[order(df_res$date),]
df_res$profile_ID <- seq(1,length(df_res$filename))


ggplot(df_res)+
  geom_line(aes(profile_ID, z_ML, colour = "z_ML"))+
  geom_line(aes(profile_ID, z_thrmcln_top, colour = "z_thrmcln"))+
  geom_line(aes(profile_ID, z_thrmcln, colour = "z_thrmcln"))+
  geom_line(aes(profile_ID, z_thrmcln_bottom, colour = "z_thrmcln"))+
  ylim(c(70,0))+
  theme_bw()





ggplot(df_res)+
  geom_point(aes(z_thrmcln, z_thrmcln_bottom-z_thrmcln_top))+
  theme_bw()




# save results
setwd("C:/Users/camil/Documents/Results/level2/")
filename <- "z_thermocline_decimated.csv"
write.table(x = df_res, file = filename, row.names = F, col.names = T, sep = ",")




