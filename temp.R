data_expander <- function(data_frame, row_num){
        col_num <- which(names(data_frame) == "Likes")
        
        temp <- html_curator(data_frame, col_num, row_num)
        
        if(length(temp) != 0){
                df_temp <- as.data.frame(
                        matrix(NA,
                               nrow = temp[[3]],
                               ncol = ncol(data_frame)
                              )
                )
                
                names(df_temp) <- names(data_frame)
                
                df_temp[1:nrow(df_temp),] <- data_frame[row_num,]
                df_temp$cleaned.likes <- temp[[1]]
                df_temp$cleaned.likes.catergory <- temp[[2]]
                
                return(df_temp[, -4])
                }
}
