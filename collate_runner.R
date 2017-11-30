# required package: googlesheets
library(googlesheets)

# authenitcate browser
gs_ls()

# read the required spreadsheet data
data <- gs_title("Recomally_Data_Collator")
df <- gs_read(data, "Tracker")

# curate the data set
df <- as.data.frame(df)
df <- df[,-c(1,8,9)]

df_list <- list()

for(i in 1:(min(which(is.na(df[,6]))) - 1)){
temp_data <- gs_title(df[i,4])
temp_df <- gs_read(temp_data, "Sheet 1")
df_list[[i]] <- as.data.frame(temp_df)
print(paste0(i, " is done bro!"))
}

df_collated <- df_list[[1]]
for(i in 2:13){df_collated <- rbind(df_collated, df_list[[i]])}
df_collated <- df_collated[,-c(1,4)]
df_collated <- df_collated[df_collated[,3] != 0,]
df_collated <- unique(df_collated)

temp_names <- unique(df_collated[,5])
temp_names <- temp_names[
    which(
        sapply(
            1:length(temp_names), 
            function(i) length(grep(
                          unlist(strsplit(
                            temp_names[i], 
                            split = "https://www.facebook.com/"))[2], 
                           pattern = "/"))) == 1
                              )
                         ]
df_collated[df_collated[, 5] %in% temp_names, 5] <- 
  sapply(
    1:sum(df_collated[, 5] %in% temp_names), 
    function(i) substring(
          df_collated[df_collated[,5] %in% temp_names, 5][i], 
          first = 1, 
          last = nchar(df_collated[df_collated[,5] %in% temp_names,5][i]) - 1
                         )
         )

temp_names <- unique(df_collated[,6])
temp_names <- temp_names[which(
                  sapply(
                      1:length(temp_names), 
                      function(i) length(grep(unlist(
                                  strsplit(temp_names[i], 
                                  split = "https://www.facebook.com/"))[2], 
                                pattern = "/"))) == 1
                                                    )
                         ]
df_collated[df_collated[, 6] %in% temp_names, 6] <- 
  sapply(
    1:sum(df_collated[, 6] %in% temp_names), 
    function(i) substring(
          df_collated[df_collated[, 6] %in% temp_names, 6][i], 
          first = 1,
          last = nchar(df_collated[df_collated[, 6] %in% temp_names, 6][i]) - 1
                          )
        )
