# Convert the columns Artist.Name, Artist.Id, User.Id, Likes to characters from factors
data1[,1] <- as.character(data1[,1])
data1[,2] <- as.character(data1[,2])
data1[,3] <- as.character(data1[,3])
data1[,4] <- as.character(data1[,4])

# Remove non focus url tags
data1$cleaned.User.Id <- sapply(1:nrow(data1), function(i) unlist(strsplit(data1[i,3], split = "([?|&]fref)|([?|&]hc_ref)"))[1])

# Replacing the NAs with the page's IDs
data1[which(is.na(data1[,1])), 1] <- data1[(which(is.na(data1[,1]))-1), 1]
data1[which(is.na(data1[,2])), 2] <- data1[(which(is.na(data1[,2]))-1), 2]
data1[which(is.na(data1[,3])), 3] <- data1[(which(is.na(data1[,3]))-1), 3]
data1$cleaned.User.Id[which(is.na(data1$cleaned.User.Id))] <- data1$cleaned.User.Id[which(is.na(data1$cleaned.User.Id))-1]

# Function to curate the HTML code to a page format
html_curator <- function(data_frame, col_num, row_num){
temp <- c(data_frame[row_num,col_num], data_frame$Artist.Id[row_num])
temp <- unlist(strsplit(temp, split = "http[s]?://"))
temp <- temp[grep(temp, pattern = "facebook")]
temp <- sapply(1:length(temp), function(i) unlist(strsplit(temp[i], split = " "))[1])
temp <- temp[grep(temp, pattern = "[f]?ref")]
temp <- unique(unlist(strsplit(temp, split = "[/]?\\?[f]?ref")))
temp <- unique(temp[grep(temp, pattern = "facebook")])
return(list(temp, length(temp)))
}

# Function to expand the data set to include all the pages liked by a FaUs
data_expander <- function(data_frame, row_num){
temp <- html_curator(data_frame, 4, row_num)
df_temp <- as.data.frame(
        matrix(NA, 
        nrow = temp[[2]], 
        ncol = ncol(data_frame)))
names(df_temp) <- names(data_frame)
df_temp[1:nrow(df_temp),] <- data_frame[1,]
df_temp$cleaned.likes <- temp[[1]]
return(df_temp[,-4])
}

# Creating the dataset to include all the pages liked by a FaUs
data1_new <- data_expander(data1, 1)
for(i in 2:nrow(data1)){
data1_new <- rbind(data1_new, data_expander(data1, i))}


