complete <- function (directory,id=1:332){
  file<-paste0(directory,"\\",formatC(id, width=3, flag="0"),".csv");
  data.frame<-lapply(file,read.csv);
  
  table<-do.call(rbind,data.frame);
  good<-complete.cases(table);
  
  table1<- table[good,][];
  as.data.frame(cbind(id=sort(id),nobs=lapply(split(table1,table1[4]),nrow)),row.names = 1:length(id));
    
}
