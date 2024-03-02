voting_rep_dem <- function() {
  raw_df <- read.csv("~/Documents/Cal/Github/DS_203/lab-1-stat-pack/datasets/anes_pilot_2022_csv_20221214.csv", header = TRUE)
  df <- raw_df[, c('caseid', 'house22p', 'senate22p', 'gov22p', 'vote20', 'vote24dt', 'pid1d', 'pid1r', 'pidlean',  'apppres', 'econpres', 'frnpres')]
  
  # Overwrite the nulls and non-relevant values
  df$house22p[df$house22p == -1] <- 0 # Skip
  df$house22p[df$house22p == 3] <- 0 # Independent
  
  df$senate22p[df$senate22p == -1] <- 0 # Skip
  df$senate22p[df$senate22p == 3] <- 0 # Another Party
  
  df$gov22p[df$gov22p == -1] <- 0 # Skip
  df$gov22p[df$gov22p == 3] <- 0 # Another Party
  
  df$vote20[df$vote20 == -7] <- 0 # No Answer
  df$vote20[df$vote20 == -1] <- 0 # Skip
  df$vote20[df$vote20 == 3] <- 0 # Another Party
  
  df$vote24dt[df$vote24dt == -7] <- 0 # No Answer
  df$vote24dt[df$vote24dt == -1] <- 0 # Skip
  df$vote24dt[df$vote24dt == 3] <- 0 # Another Party
  df$vote24dt[df$vote24dt == 4] <- 0 # Probably Not Vote
  
  df$pid1d[df$pid1d == -1] <- 0 # Skip
  df$pid1d[df$pid1d == 3] <- 0 # Independent
  df$pid1d[df$pid1d == 4] <- 0 # Something else
  
  df$pid1r[df$pid1r == -1] <- 0 # Skip
  
  df$pidlean[df$pidlean == -1] <- 0 # Skip
  df$pidlean[df$pidlean == 3] <- 0 # Neither
  
  
  df$dem_score <- ifelse(df$house22p == 1, 1, 0) + ifelse(df$senate22p == 1, 1, 0) + ifelse(df$gov22p == 1, 1, 0) + ifelse(df$vote20 == 2, 1, 0) + ifelse(df$vote24dt == 2, 1, 0) + ifelse(df$pid1d == 1, 1, 0) + ifelse(df$pid1r == 1, 1, 0) + ifelse(df$pidlean == 2, 1, 0)
  df$rep_score <- ifelse(df$house22p == 2, 1, 0) + ifelse(df$senate22p == 2, 1, 0) + ifelse(df$gov22p == 2, 1, 0) + ifelse(df$vote20 == 1, 1, 0) + ifelse(df$vote24dt == 1, 1, 0) + ifelse(df$pid1d == 2, 1, 0) + ifelse(df$pid1r == 2, 1, 0) + ifelse(df$pidlean == 1, 1, 0)
  
  df$party <- ifelse(df$dem_score > df$rep_score, 'DEM', 
                     ifelse(df$rep_score > df$dem_score, 'REP', 'TIE'))
  
  df$dem_voting <- ifelse(df$house22p == 1, 1, 0) + ifelse(df$senate22p == 1, 1, 0) + ifelse(df$gov22p == 1, 1, 0) + ifelse(df$vote20 == 2, 1, 0) + ifelse(df$vote24dt == 2, 1, 0)
  df$dem_identifying <- ifelse(df$pid1d == 1, 1, 0) + ifelse(df$pid1r == 1, 1, 0) + ifelse(df$pidlean == 2, 1, 0)
  
  df$rep_voting <- ifelse(df$house22p == 2, 1, 0) + ifelse(df$senate22p == 2, 1, 0) + ifelse(df$gov22p == 2, 1, 0) + ifelse(df$vote20 == 1, 1, 0) + ifelse(df$vote24dt == 1, 1, 0)
  df$rep_identifying <- ifelse(df$pid1d == 2, 1, 0) + ifelse(df$pid1r == 2, 1, 0) + ifelse(df$pidlean == 1, 1, 0)
  
  
  # df$voting_party <- ifelse(df$dem_voting > df$rep_voting, 'DEM', 
  #                           ifelse(df$rep_voting > df$dem_voting, 'REP', 'TIE'))
  # 
  # df$voting_party <- ifelse(df$dem_voting > df$rep_voting, 'DEM', 
  #                           ifelse(df$rep_voting > df$dem_voting, 'REP',
  #                                  ifelse(df$rep_voting == 0 & df$dem_voting == 0, '0 TIE'), 'TIE'))
  
  df$voting_party <- case_when(
    df$dem_voting > df$rep_voting ~ 'DEM',
    df$rep_voting > df$dem_voting ~ 'REP',
    df$rep_voting == 0 & df$dem_voting == 0 ~ '0 TIE', 
    TRUE ~ 'TIE'
    
  )
  
  df$identifying_party <- ifelse(df$dem_identifying > df$rep_identifying, 'DEM', 
                                 ifelse(df$rep_identifying > df$dem_identifying, 'REP', 'TIE'))
  
  return(df$voting_party)
}