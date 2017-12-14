html_Correcter <- function(temp){  
  temp_num <- grep(temp, pattern = "fref")
  temp[temp_num] <- sapply(
    temp_num,
    function(i) unlist(strsplit(temp[i], split = "[?|&]fref"))[1]
    )
  
  temp_char <- sapply(
    1:length(temp),
    function(i) substr(
      temp[i],
      start = nchar(temp[i]),
      stop = nchar(temp[i]))
    )
  
  temp_num <- which(temp_char != "/")
  temp[temp_num] <- sapply(
    temp_num,
    function(i) paste0(temp[i], "/")
    )
  
  return(temp)
}
