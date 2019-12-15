best<-function(state,outcome){
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

  ## Return hospital name in that state with lowest 30-day death rate
  
  ##split dataframe per input state for computation
  unsorted_state_frame<-df[df$State==state,]
  ##sort the dataframe so that in case of ties, hospital earlier in the alphabet is output
  state_frame<-unsorted_state_frame[order(unsorted_state_frame$Hospital.Name),]
  
  ##conditional statement to search for input condition
  
  if(outcome=="heart attack"){
    temp1<-as.numeric(state_frame[,11])
    low_index<-which.min(state_frame[,11])
    ##lowest count for given conditions' index in the dataframe
  }
  else if(outcome=="heart failure"){
    temp1<-as.numeric(state_frame[,17])
    low_index<-which.min(state_frame[,17])
    ##lowest count for given conditions' index in the dataframe
  }
  else {
    temp1<-as.numeric(state_frame[,23])
    low_index<-which.min(state_frame[,23])
    ##lowest count for given conditions' index in the dataframe
  }
  
  ##index counted in conditional statements used to find best hospital as per lowest rate of condition in state
  state_frame[low_index,2]
}