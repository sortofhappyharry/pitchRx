#Hannah sheet
hannah_operation<-function(data){
  result=data.frame(0)
  colnames(result)<-"Sup"
  n=nrow(data)
  empty=data.frame(" ")
  colnames(empty)<-"Sup"
  for(i in 1:n){
    new=t(data[i,])
    colnames(new)<-"Sup"
    result=rbind(result,new)
    result=rbind(result,empty)
  }
  result
}
