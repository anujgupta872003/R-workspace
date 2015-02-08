best <- function(state,outcome){
  
  if(length(state) !=1 | nchar(state[1])!=2){
      stop("invalid state")
  }
  
  if(length(outcome) !=1 | !(outcome %in% c("heart attack","heart failure","pneumonia"))) {
    stop("invalid outcome");
  }
  
  dataframe <- read.csv("rprog-data-ProgAssignment3-data\\outcome-of-care-measures.csv");
  
  if(!(state %in% levels(dataframe$State))){
    stop("invalid state");
  }
  colnames<-names(dataframe)
  if(outcome=="heart attack"){
    colname<-colnames[11];
  }
  if(outcome=="heart failure"){
    colname<-colnames[17];
  }
  if(outcome=="pneumonia"){
    colname<-colnames[23];
  }
  
  fin_table <- subset(dataframe,State==state & 
                        dataframe[,colname]!="Not Available",c("Hospital.Name", colname))
  
  
  fin_table <- subset(fin_table,fin_table[,2]==min(as.numeric(as.vector(fin_table[,2])),na.rm=T),c("Hospital.Name"));
  fin_table<-as.vector(sort(fin_table$Hospital.Name));
  fin_table[1];
}
