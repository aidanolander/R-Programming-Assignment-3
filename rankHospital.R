## Programming Assignment 3
#Part 3, rankhospital.R

rankhospital <- function(state, outcome, num = "best") {
        #read the outcome data
        data <- read.csv("outcomes.csv")
        
        #make sure inputs are valid
        states <- levels(data[, 7])[data[, 7]]
        state.exist <- FALSE
        for (i in 1:length(states)) {
                if (state == states[i]) {
                        state.exist <- TRUE
                }
        }
        if (!state.exist) {
                stop("invalid state")
        }
        if (!((outcome == "heart attack") | (outcome == "heart failure") | 
              (outcome == "pneumonia"))) {
                stop("invalid outcome")
        }
        
        #return the hospital name in the desired rank
        
        col <- if (outcome == "heart attack") {
                11
        } else if (outcome == "heart failure") {
                17
        } else {
                23
        }
        
        data[, col] <- suppressWarnings(as.numeric(levels(data[, col])[data[, col]]))
        data[, 2] <- as.character(data[, 2])
        statedata <- data[grep(state, data$State), ]
        orderdata <- statedata[order(statedata[, col], statedata[, 2], na.last = NA), ]
        
        if (num =="best") {
                orderdata[1,2]
        } else if (num == "worst") {
                orderdata[nrow(orderdata), 2]
        } else {
                orderdata[num, 2]
        }
}