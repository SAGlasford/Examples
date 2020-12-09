library(tidyverse)
BetNode <- function(BankRoll, Vig, BetSize) {         
    BankRoll_W <- BankRoll + BetSize / (abs(Vig)/100)
    BankRoll_L <- BankRoll - BetSize
    out <- array()
    out[1] <- BankRoll_W
    out[2] <- BankRoll_L
    return(out)
}

Scen_Gen <- function(bankroll, vig, betsize, win_prob,totalbets,seed_start) { 
    set.seed(seed_start)
    for (jj in 1:totalbets) {
        w_px <- runif(1)
        if (jj == 1) { 
        scen_arry <- array()
        dummy_array <- BetNode(bankroll, vig, betsize)
            if (w_px <= win_prob) {
            loop_result <- round(dummy_array[1],2)
        }
        else {
            loop_result <- round(dummy_array[2],2)
        }
    }
    else {
        dummy_array <- BetNode(loop_result, vig, betsize)
        if (w_px <= win_prob) {
            loop_result <- dummy_array[1]
        }
        else {
            loop_result <- dummy_array[2]
        }
    }
    scen_arry[jj] <- loop_result
}
    return(scen_arry)
}


analytics <- function(investment, bonus, rollover, betsize, win_prob, vig, scencount) { 
    trials <- investment * rollover / betsize
    scen_store <- matrix(nrow = scencount, ncol = trials)
    for (zz in 1:scencount) {
        scen_store[zz,] <- Scen_Gen(investment, vig, betsize, win_prob, trials,zz)
    }
    prob_of_ruin     <- sum(apply(scen_store, 1, FUN = min) < 0) / scencount
    expected_return  <- mean(scen_store[, trials])
    Ex_afterbets     <- matrix(scen_store[, trials])
    broke            <- matrix(ifelse(apply(scen_store, 1, FUN = min) > 0, 1, 0))
    exp_gobroke      <- t(Ex_afterbets) %*% broke / scencount
    result_df        <- cbind(investment, bonus, rollover, betsize, win_prob, vig, scencount, round(prob_of_ruin,4), expected_return, exp_gobroke) %>% data.frame()
    names(result_df) <- c("Investment", "Bonus", "Rollover", "Betsize", "Win_prob", "Vig", "Scencount", "prob_of_ruin", "expected_return", "exp_gobroke")
    return(result_df)
}

win_prob_sens        <- c(.5,.51,.52,.53,.54,.55)
bet_size_sens        <- c(10, 20, 30, 40, 50, 100)
rollover             <- c(14,17,20) 
sensitivities        <- expand.grid(win_prob_sens, bet_size_sens, rollover)

   
for (kk in 1:nrow(sensitivities)) {
    if (kk == 1) { 
        result_df <- analytics(2000, 1000, sensitivities$Var3[kk], sensitivities$Var2[kk], sensitivities$Var1[kk], 110, 1000)
    }
    else {
        dummy_df  <- analytics(2000, 1000, sensitivities$Var3[kk], sensitivities$Var2[kk], sensitivities$Var1[kk], 110, 1000)
        result_df <- rbind(result_df, dummy_df)
    }
}

setwd("C:/Users/e5615200/OneDrive - FIS")
write.csv(result_df,"bonus_clear.csv",row.names = FALSE)