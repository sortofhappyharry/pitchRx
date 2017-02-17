library('DBI')
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

