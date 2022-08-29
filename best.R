## Program Assignment 3
##Part 2, best.R

best <- function(state, outcome) {
        #read outcome data
        data <- read.csv("outcomes.csv")
        
        #check variables are valid
        
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
        if (!((outcome == "heart attack") | (outcome == "heart failure") 
              | (outcome == "pneumonia"))) {
                stop('invalid outcome')
        }
        
        #return the hospital name with lowest mortality rate
        col <- if (outcome == "heart attack") {
                11
        } else if (outcome == "heart failure") {
                17
        } else {
                23
        }
        
        data[, col] <- suppressWarnings(as.numeric(levels(data[, col])[data[,col]]))
        data[, 2] <- as.character(data[, 2])
        statedata <- data[grep(state, data$State), ]
        orderdata <- statedata[order(statedata[, col], statedata[, 2], na.last = NA), ]
        orderdata[1, 2]
}