UsAr_equalizer <- function(data_frame, username){
  
  data_temp <- unique(data_frame[
    data_frame$cleaned.User.Id == username, 5:9
  ])
  
  artists <- unique(
    data_frame[
      data_frame$cleaned.User.Id == username, 1:4]
  )
  
  data_list <- lapply(1:length(artists), 
                      function(i) cbind(artists[i,], data_temp)
                      )
  
  data_list_new <- unique(do.call(rbind, data_list))
  
  return(data_list_new)
}
