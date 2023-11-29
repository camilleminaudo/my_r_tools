# Camille, July 2019

# -------------------  PRIMARY PRODUCTION PROJECT ------------------- #
#
# This function identify the mixed layer depth based on Temperature profiles
# ------------------------------------------------------------------- #

# depth <- df$depth
# w_Temp <- df$w_Temp
# CV_threshold = 0.02
# diff_threshold = 0.95

find_ML_depth <- function(depth, w_Temp, CV_threshold, diff_threshold){
  # if (missing(CV_threshold)){ CV_threshold == 0.02}
  # if (missing(diff_threshold)){ diff_threshold == 0.95}
  
  my_profile <- data.frame(depth = depth, X = w_Temp)
  my_profile <- my_profile[order(my_profile$depth),]
  my_profile <- my_profile[!is.na(my_profile$X),]
  
  CV_T <- sd(my_profile$X)/mean(my_profile$X)
  
  if ((sum(!is.na(my_profile$X))>0) & (CV_T > CV_threshold)){
    smooth <-  smooth.spline(x = my_profile$depth, y = my_profile$X, df = ceiling(length(my_profile$X)/10))
    my_profile$smooth <- approx(x = smooth$x, y = smooth$y, my_profile$depth)$y
    my_profile <- my_profile[!is.na(my_profile$smooth),]
    my_profile$Xnorm <- (my_profile$smooth - min(my_profile$smooth))/(max(my_profile$smooth) - min(my_profile$smooth))
    
    for (d in seq(1, length(my_profile$depth))){
      my_profile$avg_cum[d] <- mean(my_profile$Xnorm[seq(1,d)])
    }
    my_profile$diff <- my_profile$Xnorm/my_profile$avg_cum
    
    MLD <- my_profile$depth[which(my_profile$diff < diff_threshold)][1]
  } else {
    MLD <- NA
  }
  return(MLD)
}
