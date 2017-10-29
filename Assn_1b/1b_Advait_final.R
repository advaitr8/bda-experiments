#Assignment 1b
#Define Outcome Variables
patients <- NULL
number_waiting <- NULL
mean_wait_time <- NULL
close_past_four <- NULL

#Loop
for(n in 1:100){
  
  #nullify intermediate variables
  pat <- NULL
  arrival_time <-NULL
  leave_time <-NULL
  pats <- NULL
  time_between <- NULL
  doc_time <- NULL
  wait_time <- NULL

    #generate number of patients
    pat <- rexp(1000, 1/10)
    pats <- NULL
    for(i in 1:1000){
        pats[i] <- sum(pat[1:i])
    }
    #patients[n] <- length(pats[pats < 420])
    patients <- length(pats[pats < 420])

    #generate time between patient arrivals  
    #time_between <- pat[1:patients[n]]
    time_between <- pat[1:patients]

    #generate time with doctors
    doc_time <- runif(patients, 5, 20)

    #generate arrival times (in minutes after 9am)
    arrival_time <- NULL
    for(i in 1:patients) {
      arrival_time[i] <- sum(time_between[1:i])
    }

    #generate wait times
    wait_time <- NULL
    wait_time[1:3] <- c(0,0,0)
    leave_time[1:2] <- arrival_time[1:2] + doc_time[1:2]

   

    for(i in 4:patients){
    	leave_time[i-1] <- arrival_time[i-1] + wait_time[i-1] + doc_time[i-1]
      wait_time[i] <- ifelse(
        length((leave_time[1:(i-1)])[leave_time[1:(i-1)] > arrival_time[i]]) < 3, 
        0,  
       min((leave_time[1:(i-1)])[leave_time[1:(i-1)] > arrival_time[i]]) - arrival_time[i ])
    }

    #generate other outcome variables
    number_waiting[n] <- length(wait_time[wait_time > 0])
    mean_wait_time[n] <- ifelse(length(wait_time[wait_time > 0]) == 0, 
                                0, mean(wait_time[wait_time>0]))
    close_past_four[n] <- ifelse(max(leave_time) > 420, max(leave_time) - 420, 0)

}

mean(wait_time)
median(wait_time)

#Get medians and quartiles
summary(patients)
summary(number_waiting)
summary(mean_wait_time) 
summary(close_past_four)
