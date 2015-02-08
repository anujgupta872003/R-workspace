rankall <- function(outcome, num="best"){
  
  if(length(outcome) !=1 | !(outcome %in% c("heart attack","heart failure","pneumonia"))) {
    stop("invalid outcome");
  }
  
  dataframe <- read.csv("rprog-data-ProgAssignment3-data\\outcome-of-care-measures.csv");
  
  colnames<-names(dataframe);
  
  if(outcome=="heart attack"){
    colname<-colnames[11];
  }
  else if(outcome=="heart failure"){
    colname<-colnames[17];
  }
  else if(outcome=="pneumonia"){
    colname<-colnames[23];
  }
  
  fin_table <- subset(dataframe,
                        dataframe[,colname]!="Not Available",c("Hospital.Name","State", colname));
  cr <- data.frame("Hospital"=character(),"State"=character(),stringsAsFactors = FALSE);
  #dimnames(cr)<-(,"Hospital","State");
  if(num=="best"){
    
    for (i in split(fin_table,fin_table$State)){
      
      cr <- rbind(cr,data.frame("Hospital"=as.vector(i[order(as.numeric(as.vector(i[,colname])),i$Hospital.Name),][1,1]),"State"=as.vector(i$State[1])));
    }
    cr;
    
  }
  else if(num=="worst"){
    #fin_table<-fin_table[order(-as.numeric(as.vector(fin_table[,colname])),fin_table$Hospital.Name),];  
    for (i in split(fin_table,fin_table$State)){

      cr <- rbind(cr,data.frame("Hospital"=as.vector(i[order(-as.numeric(as.vector(i[,colname])),i$Hospital.Name),][1,1]),"State"=as.vector(i$State[1])));
    }
    cr;
    
   
    
  }
 else if (class(num)!="numeric"){
    stop("invalid rank");
  }
 else{
    if(num>nrow(fin_table) | num<1){
      NA;
    }else{
      for (i in split(fin_table,fin_table$State)){
        
        cr <- rbind(cr,data.frame("Hospital"=as.vector(i[order(as.numeric(as.vector(i[,colname])),i$Hospital.Name),][num,1]),"State"=as.vector(i$State[1])));
      }
      cr;
      
    }
  
  }
}



