## Program Assignment 3
#Part 4, rankall.R

rankall <- function(outcome, num = "best") {
        #read outcome data
        data <- read.csv("outcomes.csv")
        
        #make sure inputs are valid
        if (!((outcome =="heart attack") | (outcome == "heart failure") |
              (outcome == "pneumonia"))) {
                stop("invalid outcome")
        }
        
        #find hospital of given rank for each state
        
        col <- if (outcome == "heart attack") {
                11
        } else if (outcome == "heart failure") {
                17
        } else {
                23
        }
        
        data[, col] <- suppressWarnings(as.numeric(levels(data[, col])[data[, col]]))
        data[, 2] <- as.character(data[, 2])
        
        #create empty vector to be filled with answer
        
        answer <- vector()
        
        states <- levels(data[, 7])
        
        for (i in 1:length(states)) {
                statedata <- data[grep(states[i], data$State), ]
                orderdata <- statedata[order(statedata[, col], statedata[, 2],
                                             na.last = NA), ]
                hospital <- if (num == "best") {
                        orderdata[1, 2]
                } else if (num == "worst") {
                        orderdata[nrow(orderdata), 2]
                } else {
                        orderdata[num, 2]
                }
                answer <- append(answer, c(hospital, states[i]))
        }
        
        #return data frame with hospital names and the state name
        answer <- as.data.frame(matrix(answer, length(states), 2, byrow = TRUE))
        colnames(answer) <- c("hospital", "state")
        rownames(answer) <- states
        
        answer
                
}