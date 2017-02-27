#Park Factors
# How to compute Park Factors? Take home and away team's average wOBAs in home games versus road games,
# repeat process three times. Adjust for home field advantage.
# Assumptions: Home batters/pitchers faced are equivalent to away batters/pitchers faced.

#wOBA Home Field Advantage
#data<-abpitchse

#First create abpitchse to include the data you want - initialize PV_script first from play value
s='2016_04_01'
e='2016_10_03'
#abpitchse<-PV_script(abpitch,s,e) 
Home_field_wOBA<-function(data){
  pas<-subset(data,count=="0-0")
  home_away<-ddply(pas,.(inning_side.y),summarize,Runs_PA=mean(event_value,na.rm=TRUE))
  home_away
}
#home_away<-Home_field_wOBA(abpitchse)
#Next we can compute park factors, first level, in the form of Runs/PA from neutral.
Pas<-function(data){
  pas<-subset(data,count=="0-0")
  pas$away_team<-with(pas,substr(gameday_link,16,18))
  pas$home_team<-with(pas,substr(gameday_link,23,25))
  pas
}
pas<-Pas(abpitchse)
Park_Factors<-function(pas){
  home_park_factors<-ddply(pas,.(Team = home_team,inning_side.y),summarize,Home_Runs_PA=mean(event_value,na.rm=TRUE))
  away_park_factors<-ddply(pas,.(Team = away_team,inning_side.y),summarize,Away_Runs_PA=mean(event_value,na.rm=TRUE))
  parks<-join(home_park_factors,away_park_factors,by=c("Team","inning_side.y"), type="inner")
  parks<-ddply(parks,.(Team),summarize,home_avg=mean(Home_Runs_PA),away_avg=mean(Away_Runs_PA))
  parks$diff<-with(parks,home_avg-away_avg)
  parks<-parks[order(-park_factors$diff),]
  parks
}
parks<-Park_Factors(pas)
View(parks)

# Iterate the process n times
Park_Factors_Iteration<-function(data,n){
  parks<-Park_Factors(data)
  parks_hash<-data.frame(parks$diff)
  final<-parks_hash
  final$Team<-parks$Team
  names(final)<-c('parkfactor','Team')
  final$parkfactor<-0
  for(i in 1:n){
    parks<-Park_Factors(data)
    parks_hash<-data.frame(parks$diff)
    rownames(parks_hash)<-parks$Team
    final$parkfactor<-final$parkfactor + parks_hash[final$Team,1]
    data$event_value<-with(data,event_value - parks_hash[home_team,1])
    i=i+1
  }
  final<-final[order(-final$parkfactor),]
  list(parks,final)
}
L<-Park_Factors_Iteration(pas,10)
View(L[[2]])


