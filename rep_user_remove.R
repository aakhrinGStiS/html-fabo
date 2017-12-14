rep_users_high <- max(
  which(
    sort(
      table(unique(collated_all_2[,c(1,5)])[,2]),
      decreasing = T) > 1
  ))

rep_users <- names(
  sort(
    table(unique(collated_all_2[,c(1,5)])[,2]),
    decreasing = T
  )[1:rep_users_high]
)

rm(rep_users_high)

rep_user_remover <- function(data_frame, user_Id){
  saved <- data_frame[data_frame$cleaned.User.ID_1 != user_Id,]
  temp <- data_frame[data_frame$cleaned.User.ID_1 == user_Id,]
}
