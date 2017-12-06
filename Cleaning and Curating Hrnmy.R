# Function to curate the HTML code to a page format
html_curator <- function(data_frame, col_num, row_num){
        temp <- c(
                data_frame[row_num,col_num],
                data_frame$Artist.Id[row_num]
        )
        
        temp <- unlist(strsplit(temp, split = "<li class"))
        temp_num <- sapply(
                1:length(temp), 
                function(i) length(grep(
                        temp[i], 
                        pattern = "data-collection-item"))
                )
        temp <- temp[which(temp_num == 1)]
        
        if(length(temp) != 0){
                temp_page <- sapply(
                        1:length(temp),
                        function(i) unlist(strsplit(
                                unlist(strsplit(
                                        temp[i],
                                        split= "href="
                                ))[2],
                                split = " "
                        ))[1]
                        )
                
                temp_page <- unlist(strsplit(
                        temp_page,
                        split = "\""
                ))[(1:length(temp_page))*2]
                
                temp_catergory <- sapply(
                        1:length(temp),
                        function(i) unlist(strsplit(
                                unlist(strsplit(
                                        temp[i],
                                        split = "fsm fwn fcg"
                                ))[2],
                                split = "</div"
                        ))[1]
                        )
                
                temp_category <- unlist(strsplit(
                        temp_catergory,
                        split = ">"
                ))[(1:length(temp_catergory))*2]
                
                if(length(temp_page) == length(temp_category)){
                        return(list(temp_page,
                                    temp_category,
                                    length(temp_page))
                              )} else {
                        return("ERROOOOOOOOORRRRRR!!!")
                }
        }
}

# Function to expand the data set to include all the pages liked by a FaUs
data_expander <- function(data_frame, row_num){
        col_num <- which(names(data_list[[1]]) == "Likes")
        
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

# required package: googlesheets
library(googlesheets)

# authenitcate browser
gs_ls()

# read the required spreadsheet data
data <- gs_title("Recomally_Data_Collator")
df <- gs_read(data, "Tracker")

# curate the data set
df <- as.data.frame(df)
df <- df[,-c(1,4,6,7)]

data_list <- list()
sheets <- sapply(
        c(1,3:nrow(df)), 
        function(i) paste0(df[i, 3], "_raw")
        )

sheets_internal <- lapply(
        sheets, 
        function(i) gs_ws_ls(gs_title(i))
        )

data_list <- lapply(
        sheets, 
        function(i) as.data.frame(gs_read(gs_title(i), "Sheet1"))
        )

data_list[[2]] <- data_list[[2]][!is.na(data_list[[2]][,4]), ]
data_list[[3]] <- data_list[[3]][!is.na(data_list[[3]][,4]), ]
data_list[[6]] <- data_list[[6]][!is.na(data_list[[6]][,4]), ]

data_list_standoff <- list()
data_list_standoff[[1]] <- gs_read(gs_title(sheets[[2]]), "Sheet2")

data_list_standoff <- append(
        data_list_standoff, 
        list(gs_read(gs_title(df[2, 3]), "Sheet1"))
)

data_list_new <- list()

# Convert the columns Artist.Name, Artist.Id, User.Id, Likes to characters from factors
for(j in 1:length(data_list)){
        data_list[[j]][, 1] <- as.character(data_list[[j]][, 1])
        data_list[[j]][, 2] <- as.character(data_list[[j]][, 2])
        data_list[[j]][, 3] <- as.character(data_list[[j]][, 3])
        data_list[[j]][, 4] <- as.character(data_list[[j]][, 4])

        # Remove non focus url tags
        data_list[[j]]$cleaned.User.Id <- sapply(
                1:nrow(data_list[[j]]), 
                function(i) unlist(strsplit(
                        data_list[[j]][i, 3], 
                        split = "([?|&]fref)|([?|&]hc_ref)")
                                  )[1]
                )
        
        # Replacing the NAs with the page's IDs
        data_list[[j]][which(is.na(data_list[[j]][, 1])), 1] <- data_list[[j]][(which(is.na(data_list[[j]][, 1])) - 1), 1]
        data_list[[j]][which(is.na(data_list[[j]][, 2])), 2] <- data_list[[j]][(which(is.na(data_list[[j]][, 2])) - 1), 2]
        data_list[[j]][which(is.na(data_list[[j]][, 3])), 3] <- data_list[[j]][(which(is.na(data_list[[j]][, 3])) - 1), 3]
        data_list[[j]]$cleaned.User.Id[which(is.na(data_list[[j]]$cleaned.User.Id))] <- data_list[[j]]$cleaned.User.Id[which(is.na(data_list[[j]]$cleaned.User.Id))-1]
        
        
        # Creating the dataset to include all the pages liked by a FaUs
        data_list_new[[j]] <- data_expander(data_list[[j]], 1)
        for(i in 2:nrow(data_list[[j]])){
                temp <- data_expander(data_list[[j]], i)
                if(length(temp) != 0){
                        if(ncol(temp) == ncol(data_list[[j]])){
                                data_list_new[[j]] <- rbind(
                                        data_list_new[[j]],
                                        data_expander(data_list[[j]], i)
                                )
                        }
                }
        }
}

for(i in 1:length(data_list_new)
   ) write.csv(
        data_list_new[[i]], 
        paste0(df[i, 3], "_curated.csv")
)

write.csv(data_list_standoff[[1]], "pruthvi_17112017_1_curated.csv")
write.csv(data_list_standoff[[2]], "hirank_17112017_2_curated.csv")
