corr<-function(directory,threshold=0){
  
  file<-list.files(directory,"\\", pattern="*.csv", full.names=TRUE);
  data.frame<-lapply(file,read.csv);
  table<-do.call(rbind,data.frame);
  good<-complete.cases(table);
  table1<- table[good,][];
  
  cr<-vector(mode="numeric")
  for(i in split(table1,table1[4])){
    if(nrow(i)<threshold){
      next
    }
   
    cr<-append(cr,cor(i[2],i[3]))
  
  }

  cr
}

