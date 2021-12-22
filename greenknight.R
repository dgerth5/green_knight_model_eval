gn_test <- function(pred1, pred2, actual){
  
  # complement
  pred1c <- 1 - pred1
  pred2c <- 1 - pred2
  
  # book odds
  pred1_odds <- 1/pred1
  pred1c_odds <- 1/pred1c
  
  pred2_odds <- 1/pred2
  pred2c_odds <- 1/pred2c
  
  # computing bet size
  
  bet1 <- ifelse(pred1 > pred2, pred1 - pred1c/pred2_odds, pred1c - pred1/(pred2c_odds-1))
  bet2 <- ifelse(pred2 > pred1, pred2 - pred2c/pred1_odds, pred2c - pred2/(pred1c_odds-1))
  
  # compute winnings
  
  win1 <- actual*bet1*ifelse(pred1 > pred2, pred2_odds, pred2c_odds)
  win2 <- actual*bet2*ifelse(pred2 > pred1, pred1_odds, pred1c_odds)
  
  print(paste0("Model A wins $",round(sum(win1),2), " Model B wins $", round(sum(win2),2)))
  
}
