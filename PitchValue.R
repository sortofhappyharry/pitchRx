#Pitch Value
#Make that change!!!
library('DBI')
library('plyr')
library('dplyr')
library('ggplot2')
library('pitchRx')
library('RSQLite')
db<-src_sqlite('./pitchRx.sqlite3')
merge_abpitch <- function(database,start,end) {
  atbat <- tbl(database, 'atbat')
  #atbat<-select(atbat,c(num,gameday_link,date,pitcher_name,pitcher))
  pitches <- tbl(database, 'pitch')
  #pitches<-select(pitches,c(num,gameday_link,start_speed,pitch_type))
  atbat <- filter(atbat, date >= start & date <= end)
  pitches<-filter(pitches,substr(gameday_link,5,14)>=start & substr(gameday_link,5,14)<=end)
  ABpitch<-inner_join(pitches,atbat,by=c('num','gameday_link'))
  ABpitch<-select(ABpitch,c(des,id,type,pitch_type,type_confidence,gameday_link,num,inning.y,inning_side.y,on_1b,on_2b,on_3b,count,b,s,o,event,batter_name,pitcher_name,home_team_runs,away_team_runs))
}
s<-'2016_04_01'
e<-'2016_10_02'
#ABpitch<-merge_abpitch(db,s,e)
#abpitch<-collect(ABpitch,n=Inf)
abpitch$runner1B<-1-is.na(abpitch$on_1b)
abpitch$runner2B<-1-is.na(abpitch$on_2b)
abpitch$runner3B<-1-is.na(abpitch$on_3b)
get.state<-function(runner1,runner2,runner3,outs){
runners<-paste(runner1,runner2,runner3,sep="")
paste(runners, outs)
}
abpitch$home_team_runs<-as.numeric(abpitch$home_team_runs)
abpitch$away_team_runs<-as.numeric(abpitch$away_team_runs)
abpitch$runs.scored.pitch<-ifelse(abpitch$num>1,abpitch$home_team_runs+abpitch$away_team_runs-lag(abpitch$home_team_runs,default=0)-lag(abpitch$away_team_runs,default=0),abpitch$away_team_runs)
abpitch$half.inning<-with(abpitch,paste(gameday_link,inning.y,inning_side[inning_side.y,1],inning_side.y))
abpitch$total_runs<-abpitch$home_team_runs+abpitch$away_team_runs
# pas is a list of each pa (plate appearance), from which we can get initial/final states
pas<-subset(abpitch,count=="0-0")
pas$initial_state<-with(pas,ifelse(lag(half.inning)!=half.inning | is.na(lag(half.inning)), get.state(0,0,0,0),get.state(runner1B,runner2B,runner3B,lag(o))))
pas$final_state<-with(pas,get.state(lead(runner1B),lead(runner2B),lead(runner3B),o))
pas$initial_total_score<-with(pas,ifelse(inning.y==1 & inning_side.y=="top",0,lag(total_runs)))
# Now put these back into abpitch
pas2<-pas[,c("num","half.inning","initial_state","final_state","initial_total_score")]
abpitch<-join(abpitch,pas2,by=c("num","half.inning"),type="left")
#agg computes how many total runs are scored in each half inning
agg<-aggregate(abpitch$runs.scored.pitch,list(half.inning=abpitch$half.inning),FUN=sum)
colnames(agg)[2]<-"runs.inning"
agg2<-aggregate(abpitch$total_runs,list(half.inning=abpitch$half.inning),FUN=max)
colnames(agg2)[2]<-"inning.end.runs"
agg2$inning.start.runs<-with(agg2,lag(inning.end.runs))
# next we merge it back with abpitch
abpitch<-join(abpitch,agg,by=("half.inning"),type="left")
abpitch<-join(abpitch,agg2,by=("half.inning"),type="left")
abpitch$inning.start.runs<-ifelse(abpitch$inning.y==1 & abpitch$inning_side.y=="top",0,abpitch$inning.start.runs)
abpitch$runs.roi<-abpitch$inning.end.runs-abpitch$initial_total_score
abpitch$final_pitch<-with(abpitch,ifelse(lead(batter_name)!=batter_name,1,0))
abpitch$pitch_value<-with(abpitch,ifelse(final_pitch==1,play_matrix2[event,1]+count_value2[count,1]-count_value2["0-0",1],count_value2[lead(count),1]-count_value2[count,1]))
#events<-data.frame(c(0,0,0,.55,.7,0,0,.57,1.67,1.27,1,0,.55,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
#rownames(events)<-unique(abpitch$event)
#names(events)<-"Event Value"
abpitch$event_value<-play_matrix2[abpitch$event,1]
#count_value<-ddply(abpitch,.(count),summarize,value=mean(event_value, na.rm=TRUE))