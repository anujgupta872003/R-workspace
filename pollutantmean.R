pollutantmean <- function (directory,pollutant,id=1:332){
  file<-paste0(directory,"\\",formatC(id, width=3, flag="0"),".csv");
  data.frame<-lapply(file,read.csv);
  table<-do.call(rbind,data.frame);
  good<-complete.cases(table[,pollutant]);
  mean(table[,pollutant][good]);
}
