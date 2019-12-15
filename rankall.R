rankall<-function(outcome, num="best"){
  ## Read outcome data
  df<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
  
  ## Check that outcome is valid
    if(outcome!="heart attack" && outcome!="heart failure" && outcome!="pneumonia"){
    stop("invalid outcome")  
  }
  
  ## For each state, find the hospital of the given rank
  
  #sort hospitals in alphabetic order per state
  sorted_hname_df<-df[order(df$State,df$Hospital.Name),]
  
  ##sort hospitals per state as per condition, taking ties into account
  if(outcome=="heart attack"){
    ##browser()
    temp1<-sorted_hname_df[order(sorted_hname_df$State, as.numeric(sorted_hname_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),]
    finaldf<-temp1[temp1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack!="Not Available",]
  }
  else if(outcome=="heart failure"){
    
    temp1<-sorted_hname_df[order(sorted_hname_df$State, as.numeric(sorted_hname_df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),]
    finaldf<-temp1[temp1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure!="Not Available",]
  }
  else{
    temp1<-sorted_hname_df[order(sorted_hname_df$State, as.numeric(sorted_hname_df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),]
    finaldf<-temp1[temp1$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia!="Not Available",]
  }
  
  ## Return a data frame with the hospital names and the (abbreviated) state name
  ## state_names vector used to loop over dataframe to get results for assignment
  state_names<-sort(unique(df$State))
  hospital<-vector()
  state<-vector()
  for (i in state_names){
    ranked_state_frame<-finaldf[finaldf$State==i,]
    if(class(num)!="character" && num>nrow(ranked_state_frame)){
      hospital<-c(hospital,ranked_state_frame$Hospital.Name[num])
      state<-c(state,i)
    }
    else if(num=="best"){
      hospital<-c(hospital,ranked_state_frame$Hospital.Name[1])
      state<-c(state,i)
    }
    else if (num=="worst"){
      hospital<-c(hospital,ranked_state_frame$Hospital.Name[length(ranked_state_frame$Hospital.Name)])
      state<-c(state,i)
    }
    else{
      hospital<-c(hospital,ranked_state_frame$Hospital.Name[num])
      state<-c(state,i)
    }
    
  }
  
  data.frame(hospital,state)
  
}