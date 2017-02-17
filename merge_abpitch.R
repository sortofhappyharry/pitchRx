library('DBI')
library('plyr')
library('dplyr')
library('ggplot2')
library('pitchRx')
library('RSQLite')
db<-src_sqlite('./pitchRx.sqlite3')

player_dataframe<-function(pid){
  atbat1<-tbl(db,'atbat')
  player1<-filter(atbat1,pitcher_name==pid)
  pitches<-tbl(db,'pitch')
  player2<-inner_join(pitches,player1,by=c('num','gameday_link'))
  player3<-collect(player2)
  player3
}

merge_abpitch <- function(database,start,end) {
  atbat <- tbl(database, 'atbat')
  atbat<-select(atbat,c(num,gameday_link,date,pitcher_name,pitcher))
  pitches <- tbl(database, 'pitch')
  pitches<-select(pitches,c(num,gameday_link,start_speed,pitch_type))
  atbat <- filter(atbat, date >= start & date <= end)
  pitches<-filter(pitches,substr(gameday_link,5,14)>=start & substr(gameday_link,5,14)<=end)
  ABpitch<-inner_join(pitches,atbat,by=c('num','gameday_link'))
}
s<-'2016_09_20'
e<-'2016_10_02'
ABpitch<-merge_abpitch(db,s,e)
abpitch<-collect(ABpitch,n=Inf)
print("Finished collecting")
velo<-ddply(abpitch,.(pitcher_name,gameday_link,date,pitch_type),summarize,avg_velo=mean(start_speed,na.rm=TRUE),num_pitches=sum(start_speed,na.rm=TRUE)/avg_velo)
velo<-velo[order(velo$pitcher_name,velo$pitch_type,velo$date),]
View(velo)