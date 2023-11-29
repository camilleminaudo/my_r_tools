
# depth = seq(0,30,1)
# PP = seq(15,7,length.out = 31)
# 
# my_integrate(x = depth, y = PP, from = 0, to = 29)



my_integrate <- function(x, y, from, to) {
  
  x <- x[!is.na(y)]
  y <- y[!is.na(y)]
  
  if (length(y) > 5){
    if (!is.na(from) & !is.na(to)){
      my_int = 0
      ind_start <- which.min(abs(x-from))
      ind_end <- which.min(abs(x-to))
      
      x_select <- x[seq(ind_start, ind_end)]
      y_select <- y[seq(ind_start, ind_end)]
      
      for (i in seq(2,length(x_select))){
        my_int <- my_int + (x_select[i] - x_select[i-1]) * (y_select[i] + y_select[i-1])/2
      }
    } else {
      my_int = NA
    }
  } else {
    my_int = NA
  }
  return(my_int)
}



