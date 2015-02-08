rankhospital <- function(state,outcome,num="best"){
  
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
  else if(outcome=="heart failure"){
    colname<-colnames[17];
  }
  else if(outcome=="pneumonia"){
    colname<-colnames[23];
  }
  
  fin_table <- subset(dataframe,State==state & 
                        dataframe[,colname]!="Not Available",c("Hospital.Name", colname))
  
  
  if(num=="best"){
    fin_table<-fin_table[order(as.numeric(as.vector(fin_table[,colname])),fin_table$Hospital.Name),];
    as.vector(fin_table[1,1]);
  }else if(num=="worst"){
    fin_table<-fin_table[order(-as.numeric(as.vector(fin_table[,colname])),fin_table$Hospital.Name),];  
    as.vector(fin_table[1,1]);
  }else if (class(num)!="numeric"){
    stop("invalid rank");
  }else{
    if(num>nrow(fin_table) | num<1){
      NA;
    }else
      fin_table<-fin_table[order(as.numeric(as.vector(fin_table[,colname])),fin_table$Hospital.Name),];
    as.vector(fin_table[num,1]);
  }
    
}
