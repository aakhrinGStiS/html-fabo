temp <- unique(collated_merged[,c(2,5)])
temp <- temp[order(temp[,2]),]
temp_num <- table(temp[,2])
temp <- names(temp_num[temp_num > 1])

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

data_list <- lapply(
  1:length(temp),
  function(i) UsAr_equalizer(collated_merged, temp[i])
  )
data_list_new <- do.call(
  rbind,
  data_list
)

collated_merged1 <- rbind(collated_merged, data_list_new)           
