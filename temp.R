html_curator <- function(data_frame, col_num, row_num){
        temp <- data_frame[row_num, col_num]
        
        temp <- unlist(strsplit(temp, split = "<li class"))
        
        temp_num <- sapply(
                1:length(temp), 
                function(i) length(grep(
                        temp[i], 
                        pattern = "data-collection-item"))
                )
        
        temp <- temp[which(temp_num == 1)]
        
        if(length(temp) != 0){
        
        }
}
