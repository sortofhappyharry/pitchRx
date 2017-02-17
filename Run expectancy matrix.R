#Run expectancy matrix, using abpitch data
abpitch_events<-unique(abpitch[c("half.inning","initial_state","final_state","event","num","initial_total_score","inning.end.runs","runs.roi")])
run_matrix<-ddply(abpitch_events,.(initial_state),summarize,mean(runs.roi))
#run_matrix2 can be used more easily to convert states into run values to put back into abpitch
run_matrix2<-data.frame(run_matrix[,2])
rownames(run_matrix2)<-run_matrix[,1]
abpitch$play_value<-run_matrix2[abpitch$final_state,1]-run_matrix2[abpitch$initial_state,1]+abpitch$total_runs-abpitch$initial_total_score
#Create play value matrix
abpitch_eventvalues<-unique(abpitch[c("half.inning","initial_state","final_state","event","num","play_value")])
Outs<-subset(abpitch_eventvalues,event %in% outs)
avg_out_value<-with(Outs,mean(play_value,na.rm=TRUE))
play_matrix<-ddply(abpitch_eventvalues,.(event),summarize,mean(play_value))
play_matrix2<-data.frame(play_matrix[,2])
rownames(play_matrix2)<-play_matrix[,1]
pitches<-ddply(abpitch,.(pitcher_name,pitch_type),summarize,avg_pitch_value=mean(pitch_value,na.rm=TRUE),N=sum(pitch_value,na.rm=TRUE)/mean(pitch_value,na.rm=TRUE))
pitches2<-subset(pitches,N>=250)
pitches2<-pitches2[order(pitches2$avg_pitch_value),]
number<-nrow(pitches2)
rownames(pitches2)<-c(1:number)
View(pitches2)
#Do same for batters
atbats<-subset(abpitch,count=="0-0")
avg_woba<-with(atbats,mean(event_value,na.rm=TRUE)+avg_out_value)
batter_wOBAmax<-ddply(subset(abpitch,count=="0-0"),.(batter_name),summarize,wOBAmax = mean(event_value,na.rm=TRUE),wOBA2= (wOBAmax - avg_out_value)*.322/avg_woba,PA=sum(event_value,na.rm=TRUE)/wOBAmax)
batters500<-subset(batter_wOBAmax,PA>=500)
names(batters500)[1]<-"Batter"
wOBA<-read.csv("2016 wobas.csv")
batters<-join(batters500,wOBA,by="Batter",type="full")
batters$diff<-batters$wOBA2-batters$wOBA
View(batters)
batter_pitches<-ddply(abpitch,.(batter_name,pitch_type),summarize,avg_pitch_value=mean(pitch_value,na.rm=TRUE),N=sum(pitch_value,na.rm=TRUE)/mean(pitch_value,na.rm=TRUE))
batter_pitches2<-subset(batter_pitches,N>=250)
batter_pitches2<-batter_pitches2[order(-batter_pitches2$avg_pitch_value),]
number2<-nrow(batter_pitches2)

rownames(batter_pitches2)<-c(1:number2)
View(batter_pitches2)
