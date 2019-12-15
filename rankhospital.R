rankhospital<-function(state,outcome,num="best"){
  ## Read outcome data
  df<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
  ## Check that state and outcome are valid
  temp<-unique(df$State)
  if (!(state %in% temp)){
    stop("invalid state")
  }
  if(outcome!="heart attack" && outcome!="heart failure" && outcome!="pneumonia"){
    stop("invalid outcome")  
  }
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  unsorted_state_frame<-df[df$State==state,]
  state_frame<-unsorted_state_frame[order(unsorted_state_frame$Hospital.Name),]
  
  if(outcome=="heart attack"){
    ##browser()
    temp1<-state_frame[order(as.numeric(state_frame$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),]
    ranked_state_frame<-temp1[temp1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack!="Not Available",]
  }
  else if(outcome=="heart failure"){
    
    temp1<-state_frame[order(as.numeric(state_frame$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),]
    ranked_state_frame<-temp1[temp1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure!="Not Available",]
  }
  else{
    temp1<-state_frame[order(as.numeric(state_frame$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),]
    ranked_state_frame<-temp1[temp1$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia!="Not Available",]
  }
  
  if(class(num)!="character" && num>length(ranked_state_frame)){
    return ("NA")
  }
  else if(num=="best"){
    return(ranked_state_frame$Hospital.Name[1])
  }
  else if (num=="worst"){
    return(ranked_state_frame$Hospital.Name[length(ranked_state_frame$Hospital.Name)])
  }
  else{
    ranked_state_frame$Hospital.Name[num]
  }
  
}
