#Pitch Value
#Make that change!!!
library('DBI')
library('plyr')
library('dplyr')
library('ggplot2')
library('pitchRx')
library('RSQLite')
Add_Event_Values<-function(abpitch){
  abpitch$runner1B<-1-is.na(abpitch$on_1b)
  abpitch$runner2B<-1-is.na(abpitch$on_2b)
  abpitch$runner3B<-1-is.na(abpitch$on_3b)
  
  get.state<-function(runner1,runner2,runner3,outs) {
    runners<-paste(runner1,runner2,runner3,sep="")
    paste(runners, outs)
  }
  
  abpitch$home_team_runs<-as.numeric(abpitch$home_team_runs)
  abpitch$away_team_runs<-as.numeric(abpitch$away_team_runs)
  
  abpitch$runs.scored.pitch<-ifelse(
    abpitch$num>1,
    abpitch$home_team_runs+abpitch$away_team_runs-lag(abpitch$home_team_runs,default=0)-lag(abpitch$away_team_runs,default=0),
    abpitch$away_team_runs
  )
  
  inning_side <- data.frame(c(1, 2))
  rownames(inning_side) <- c("top", "bottom")
  abpitch$half.inning<-with(abpitch,paste(gameday_link,inning.y,inning_side[inning_side.y,1],inning_side.y))
  
  abpitch$total_runs<-abpitch$home_team_runs+abpitch$away_team_runs # after the pitch
  # pas is a list of each pa (plate appearance), from which we can get initial/final states
  pas<-subset(abpitch,count=="0-0")
  pas$initial_state<-with(pas,ifelse(
    lag(half.inning)!=half.inning | is.na(lag(half.inning)), 
    get.state(0,0,0,0),
    get.state(runner1B,runner2B,runner3B,lag(o))
    )
  )
  
  pas$final_state<-with(pas,get.state(lead(runner1B),lead(runner2B),lead(runner3B),o))
  pas$initial_total_score<-with(pas,ifelse(
    num == 1,
    0,
    lag(total_runs)
  ))

  # Now put these back into abpitch
  pas2<-pas[,c("num","half.inning","initial_state","final_state","initial_total_score")]
  abpitch<-join(abpitch,pas2,by=c("num","half.inning"),type="left")
  #agg computes how many total runs are scored in each half inning
  agg<-aggregate(abpitch$runs.scored.pitch,list(half.inning=abpitch$half.inning),FUN=sum)
  colnames(agg)[2]<-"runs.inning"
  agg2<-aggregate(abpitch$total_runs,list(half.inning=abpitch$half.inning),FUN=max)
  colnames(agg2)[2]<-"inning.end.runs"
  agg3<-aggregate(abpitch$total_runs,list(half.inning=abpitch$half.inning),FUN=min)
  colnames(agg3)[2]<-"inning.start.runs"
  # next we merge it back with abpitch
  abpitch<-join(abpitch,agg,by=("half.inning"),type="left")
  abpitch<-join(abpitch,agg2,by=("half.inning"),type="left")
  abpitch<-join(abpitch,agg3,by=("half.inning"),type="left")
  
  abpitch$runs.roi<-abpitch$inning.end.runs-abpitch$initial_total_score
  abpitch$final_pitch<-with(abpitch,ifelse(lead(batter)!=batter,1,0)) # did PA terminate on this pitch
  abpitch
}
#now we can create the run matrix
Run_Matrix<-function(abpitch){
    abpitch_events<-unique(abpitch[c("half.inning","initial_state","final_state","event","num","initial_total_score","inning.end.runs","runs.roi")])
    run_matrix1<-ddply(abpitch_events,.(initial_state),summarize,mean(runs.roi, na.rm = TRUE))
    run_matrix<-data.frame(run_matrix1[,2]) #run_matrix can be used more easily to convert states into run values to put back into abpitch
    rownames(run_matrix)<-run_matrix1[,1]
    run_matrix
}
#play value tells us how valuable a given PA was
#Now we create the play_matrix to determine how valuable each play was
Play_Matrix<-function(abpitch,run_matrix){
    abpitch_eventvalues<-unique(abpitch[c("half.inning","initial_state","final_state","event","num","play_value")])
    play_matrix1<-ddply(abpitch_eventvalues,.(event),summarize, mean(play_value, na.rm=TRUE))
    play_matrix<-data.frame(play_matrix1[,2])
    rownames(play_matrix)<-play_matrix1[,1]
    play_matrix
}
Pitch_Values<-function(abpitch,play_matrix){
    abpitch$event_value<-play_matrix[abpitch$event,1]
    count_value<-ddply(abpitch,.(count),summarize,value=mean(event_value, na.rm=TRUE))
    count_value2<-data.frame(count_value[,2])
    rownames(count_value2)<-count_value[,1]
    abpitch$pitch_value<-with(abpitch,ifelse(
    final_pitch==1,
    event_value-count_value2[count,1],
    count_value2[lead(count),1]-count_value2[count,1]
    )
    )
    abpitch
}

#fangraphs_event_values<-data.frame(c(0,0,0,.55,.7,0,0,.57,1.67,1.27,1,0,.55,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
#rownames(events)<-unique(abpitch$event)
#names(events)<-"Event Value"
Best_Pitches<-function(abpitch,n){
  pitches<-ddply(abpitch,.(pitcher_name,pitch_type),summarize,avg_pitch_value=mean(pitch_value,na.rm=TRUE),N=sum(pitch_value,na.rm=TRUE)/mean(pitch_value,na.rm=TRUE))
  pitches2<-subset(pitches,N>=n)
  pitches2<-pitches2[order(pitches2$avg_pitch_value),]
  number<-nrow(pitches2)
  rownames(pitches2)<-c(1:number)
  pitches2
}
Batters_wOBA2<-function(data,n){
  pas<-subset(data,count=="0-0")
  outs <- c("Batter Interference", "Bunt Groundout","Bunt Lineout","Bunt Pop Out","Double Play", "Fan interference","Fielders Choice Out", "Flyout","Forceout","Grounded Into DP","Groundout","Lineout","Pop Out","Runner Out","Sac Bunt","Sac Fly", "Sac Fly DP", "Sacrifice Bunt DP", "Strikeout", "Strikeout - DP", "Triple Play")
  Outs<-subset(pas,event %in% outs)
  avg_out_value<-with(Outs,mean(play_value,na.rm=TRUE))
  avg_woba<-with(pas,mean(event_value,na.rm=TRUE))-avg_out_value
  LEAGUE_AVG_OBP <- 0.322
  batters_all<-ddply(pas,.(batter,batter_name),summarize,Runs_PA=mean(event_value,na.rm=TRUE),PA=sum(event_value,na.rm=TRUE)/Runs_PA)
  batters_all$wOBA2 <- (batters_all$Runs_PA-avg_out_value)*LEAGUE_AVG_OBP/avg_woba
  batters<-subset(batters_all,PA>=n)
  batters = batters[order(-batters$wOBA2),]
  number<-nrow(batters)
  rownames(batters)<-c(1:number)
  batters
}

#script
PV_script<-function(data,start,end){
  data= subset(abpitch, date>=start & date<=end)
  data = Add_Event_Values(data)
  #run_matrix <- Run_Matrix(data)
  data$play_value<-run_matrix[data$final_state,1]-run_matrix[data$initial_state,1]+data$total_runs-data$initial_total_score
  #play_matrix <- Play_Matrix(data,run_matrix)
  data = Pitch_Values(data,play_matrix)
  data
}
s='2015_04_01'
e='2015_10_03'
abpitchse<-PV_script(abpitch,s,e)
View(abpitchse)
wOBA2_2015<-Batters_wOBA2(abpitchse,200)
View(wOBA2_2015)
beep(4)

wOBAs<-join(wOBAs_2015,wOBAs_2016,by="batter")
names(wOBAs)[9]<-"vOBA"
lin_wOBA_1516<-lm(wOBA~vOBA, data = wOBAs)
summary(lin_wOBA_1516)
View(wOBAs)




