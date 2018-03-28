best <- function(state, outcome) {
## Read outcome data
outcomex<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
## Check that state and outcome are valid
if (!state %in% outcomex[, "State"]) {
        stop('invalid state')
    } else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop('invalid outcome')}
else{


## Return hospital name in that state with lowest 30-day death
## rate
	if (outcome=="heart attack"){
 	min<-min(as.numeric(outcomex[outcomex$State==state,11]),na.rm=TRUE)
     oi<-outcomex[outcomex$State==state,11]
	hosp <-outcomex[outcomex$State==state,2][which(oi == min)]
	
		}
	if (outcome=="heart failure"){
 	min<-min(as.numeric( outcomex[outcomex$State==state,17]),na.rm=TRUE)
     oi<-as.numeric(outcomex[outcomex$State==state,17])
	hosp <-outcomex[outcomex$State==state,2][which(oi == min)]

		}
	if (outcome=="pneumonia"){
 	min<-min( as.numeric(outcomex[outcomex$State==state,23]),na.rm=TRUE)
oi<-as.numeric(outcomex[outcomex$State==state,23])
	hosp <-outcomex[outcomex$State==state,2][which(oi == min)]

		}

}
return (hosp)
}