rankhospital <- function(state, outcome,num) {
## Read outcome data
outcomex<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
 x<-outcomex[outcomex$State==state,]
if (!state %in% outcomex[, "State"]) {
        stop('invalid state')
    } else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop('invalid outcome')}
else{

	if (outcome=="heart attack"){
 	
    if (is.numeric(num))
		{
	index<-na.omit(order(as.numeric(x[,11]),x[,2],decreasing=FALSE))
		y<-x[,2][index]
		y<-y[num]
	}
	
	else{
		if (num=="worst"){
  		index<-na.omit(order(as.numeric(x[,11]),x[,2],decreasing=TRUE,na.last=NA))
		num=1
	y<-x[,2][index]
	y<-y[num]
	
}
		if(num=="best")
		y<-best(state,outcome)
		}

		}

	if (outcome=="heart failure"){
 	
    	
    if (is.numeric(num))
	{
	index<-na.omit(order(as.numeric(x[,17]),x[,2],decreasing=FALSE))
	y<-x[,2][index]
		y<-y[num]
	}
	
	else{
		if (num=="worst"){
  		index<-na.omit(order(as.numeric(x[,17]),x[,2],decreasing=TRUE))

		num=1
		y<-x[,2][index]
		y<-y[num]
		}
		if(num=="best")
		y<-best(state,outcome)

		}
	


		}
	if (outcome=="pneumonia"){
 	
  		
    if (is.numeric(num)){

	index<-na.omit(order(as.numeric(x[,23]),x[,2],decreasing=FALSE))

	y<-x[,2][index]
		y<-y[num]}
	
	else{
		if (num=="worst"){
  		index<-na.omit(order(as.numeric(x[,23]),x[,2],decreasing=TRUE))
		num=1
		y<-x[,2][index]
		y<-y[num]
		}
		if(num=="best")
y<-best(state,outcome)
		}
	

		}
	
}
return (y)
}