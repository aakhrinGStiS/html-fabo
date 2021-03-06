# Function to curate the HTML code to a page format
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
                page_link <- sapply(
                        1:length(temp),
                        function(i) unlist(strsplit(
                                unlist(strsplit(
                                        temp[i],
                                        split = "lfloat _ohe"
                                ))[2],
                                split = "tabindex"
                        ))[1]
                        )
                
                page_link <- sapply(
                        1:length(page_link),
                        function(i) substr(
                                page_link[i],
                                start = 3,
                                stop = nchar(page_link[i]) - 2)
                        )
                
                page_link <- sapply(
                        1:length(page_link),
                        function(i) substr(
                                unlist(strsplit(
                                        page_link[i],
                                        split = "\\?"
                                ))[1],
                                start = 7,
                                stop = 10000)
                        )
                
                page_name <- sapply(
                        1:length(temp),
                        function(i) unlist(strsplit(
                                unlist(strsplit(
                                        temp[i],
                                        split = "aria-label"
                                ))[2],
                                split = "role="
                        ))[1]
                        )
                
                page_name <- sapply(
                        1:length(page_name),
                        function(i) substr(
                                page_name[i],
                                start = 3,
                                stop = nchar(page_name[i]) - 2)
                        )
                
                page_cat <- sapply(
                        1:length(temp),
                        function(i) unlist(strsplit(
                                unlist(strsplit(
                                        temp[i],
                                        split="fsm fwn fcg"
                                ))[2],
                                split = "</div"
                        ))[1]
                        )
                
                page_cat <- sapply(
                        1:length(page_cat),
                        function(i) substr(
                                page_cat[i],
                                start = 3,
                                stop = 100000)
                        )
                
                page_verify <- sapply(
                        1:length(temp),
                        function(i) length(
                                grep(temp[i],
                                     pattern = "Verified PageFacebook confirmed this is an authentic Page"
                                    ))
                        )
                
                        
        }
        
        df <- as.data.frame(matrix(ncol = 4, NA, nrow = length(temp)))
        df[,1] <- page_link
        df[,2] <- page_name
        df[,3] <- page_cat
        df[,4] <- page_verify
        names(df) <- c("page_Link", "page_Name", "page_Category", "page_Verified")
        return(df)
}

# Function to expand the data set to include all the pages liked by a FaUs
data_expander <- function(data_frame, row_num){
        col_num <- which(names(data_frame) == "Likes")
        
        temp <- html_curator(data_frame, col_num, row_num)
        
        if(sum(dim(temp) > 0) == 2){
                df_temp <- unique(cbind(
                        data_frame[row_num, -col_num],
                        html_curator(data_frame, col_num, row_num)
                ))
        }
        
        return(df_temp)
}

# required package: googlesheets
library(googlesheets)

# authenitcate browser
gs_ls()

# read the required spreadsheet data
data <- gs_title("Recomally_Data_Collator")
df <- gs_read(data, "Tracker", range = "A104:E151")

# curate the data set
df <- as.data.frame(df)
df <- df[,-c(1,4,6,7)]

data_list <- list()
sheets <- sapply(
        c(1, 3:nrow(df)), 
        function(i) paste0(df[i, 3], "_raw")
        )

data_list <- lapply(
        sheets, 
        function(i) as.data.frame(gs_read(gs_title(i), "Sheet1"))
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
                data_list_new[[j]] <- rbind(
                        data_list_new[[j]],
                        temp
                )
        }
}

for(i in 1:length(data_list_new)
   ) write.csv(
        data_list_new[[i]], 
        paste0(df[i, 3], "_curated.csv")
)

collated <- unique(do.call(rbind, data_list_new))

