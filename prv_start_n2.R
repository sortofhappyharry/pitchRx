#Inputs a data frame df of pitcher starts and velocities, outputs a data frame of the start followed by the previous n starts by the same pitcher

prv_start_n2<-function(df,n){
  len<-nrow(df)
  df$id<-0
  df$avg_velo<-as.numeric(df$avg_velo)
  for(i in 1:len){df[i,"id"]<- i}
  final_frame<-df
  bool<-FALSE
  for(k in 1:n){  # We are retrieving the -kth start
    next_frame<-df[1,]
    for(i in 2:len){ # We are making the ith row equal to the i-kth row, provided the pitcher is the same
      if(i>k){bool<- df[i-k, "pitcher_name"] == df[i, "pitcher_name"] & as.Date(df[i-k+1,"date"],format ="%Y_%m_%d" )-as.Date(df[i-k,"date"],format="%Y_%m_%d")<10}
      if(is.na(bool)){bool<-FALSE}
      if(bool) {
            new_row <- df[i-k,]
            new_row$id<-i
      } else {new_row <- c(NA,NA,NA,NA,NA,NA,i)}
      next_frame<-rbind(next_frame,new_row)
    }
    keep<-c(5,7)
    next_frame[,5]<-as.numeric(next_frame[,5])
    next_frame<-next_frame[keep]
    final_frame<-merge(final_frame,next_frame,by="id")
  }
  final_frame<-final_frame[-c(1,3,6,7)]
final_frame
}
velos2<-prv_start_n2(velo1,10)
View(velos2)
