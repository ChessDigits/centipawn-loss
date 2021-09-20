"
2021.09.12
Centipawn Loss

fn
"

#### imports ####
library(dplyr)
library(ggplot2)
library(mgcv)


#### helper fn ####
view <- utils::View


#### variables ####
WHITE_MATE_EVAL <- 200
BLACK_MATE_EVAL <- WHITE_MATE_EVAL*-1
MAX_PLY <- 200
PLIES_COLOR <- list(
  white=seq(1, MAX_PLY, by=2),
  black=seq(2, MAX_PLY, by=2)
)


#### load data ####
load_data <- function(k_games=c(200,500), use_local_file=TRUE, dir=NULL)
{
  if (!use_local_file)
  {
    cat("Loading data directly from ChessDigits.com is very time consuming.\nWe recommend saving the data locally and setting use_local_file to TRUE.\n")
    dir <- "https://chessdigits.com/data/"
  } else if(is.null(dir)) dir <- "d:/Chess/databases/lichess_May2019/out/"
  fpath <- paste0(dir, k_games, "k_blitz_rapid_classical_bullet.csv")
  df <- read.csv(fpath, stringsAsFactors = T)
  return(df)
}


#### data prep ####
keep_games_with_minimum_number_of_moves <- function(df, min_moves=10, add_n_moves=FALSE)
{
  # determine number of moves
  ply <- apply(df, 1, \(x)which(x[grep("Move_ply_", colnames(df))] == "")[1])
  ply[is.na(ply)] <- MAX_PLY
  n_moves <- ply/2
  
  # filter
  df <- df[n_moves >= min_moves,]
  
  # add if requested
  if(add_n_moves)
  {
    df$n_moves <- n_moves
    print("Added column 'n_moves'")
  }
  
  # out
  print(paste0("Retained only games with minimum number of moves: ", min_moves))
  return(df)
}

add_rating_differential <- function(df)
{
  df$rating_diff <- df$WhiteElo - df$BlackElo
  
  # out
  print("Added 'rating_diff' as WhiteElo - BlackElo")
  return(df)
}


#### evals ####

# helper fn
# replace mates with extreme evaluations
replace_mates_with_extreme_evaluations <- function(df)
{
  eval_cols <- grep(pattern = "Eval_ply_", x=colnames(df), value=TRUE)
  for (c in eval_cols)
  {
    # get row numbers at which eval is mate
    ix_mate <- grep(pattern="#", x=df[,c], fixed = T)
    if (length(ix_mate)==0) # no mate
    {
      df[,c] <- as.numeric(as.character(df[,c]))
      next 
    }
    
    # remove mate sign and make var numeric
    new_col <- gsub(pattern = "#", replacement="", x = df[,c], fixed=T)
    new_col <- as.numeric(as.character(new_col))
    
    # replace mate eval with extreme val
    for (ix in ix_mate)
    {
      new_col[ix] <- ifelse(new_col[ix] < 0, BLACK_MATE_EVAL, WHITE_MATE_EVAL)
    }
    
    # replace in df
    df[,c] <- new_col
  }
  
  
  # out
  print(paste("Replaced mate evaluations with", WHITE_MATE_EVAL, "or", BLACK_MATE_EVAL))
  return(df)
}


# create var eval_change
add_eval_change_at_each_ply <- function(df)
{
  "
  input: df
  output: df with mate evals replaced with 200/-200; added 'Eval_change_ply_' vars
  "
  # replace mates by extreme evals
  df <- replace_mates_with_extreme_evaluations(df)
  
  # select eval vars
  v <- grep(pattern = "Eval_ply_", x=colnames(df), value=TRUE)
  
  # calculate diff at each ply
  eval_diffs <- t(apply(df[,v], 1, diff))
  colnames(eval_diffs) <- paste0("Eval_change_ply_", (1:ncol(eval_diffs))+1)
  
  # merge
  df <- as.data.frame(cbind(df, eval_diffs))
  
  # out
  print("Added columns Eval_change_ply_")
  return(df)
  
}


# add average centipawn loss for both players at each game
add_acpl_for_each_player <- function(df, fn=mean) # half moves led to mate
{
  "
  input: df with eval change vars
  output: df with vars acpl_white and acpl_black added
  "
  
  # odd is white (3, 5, 7, ...)
  # even is black (2, 4, 6, ...)
  for (player in c("white", "black"))
  {
    # add acpl_player
    v <- paste0("acpl_", player)
    cols <- paste0("Eval_change_ply_", PLIES_COLOR[[player]])
    cols <- cols[cols %in% colnames(df)] # Eval_change_ply_1 doesn't exist
    df[,v] <- apply(df[,cols], 1, fn, na.rm=TRUE)
    
    # multiply by -1 if player is white
    if(player == "white") df[,v] <- -1*df[,v]
    
    # make into actual centipawns
    df[,v] <- 100*df[,v]
  }
  
  
  # out
  print("Added variables acpl_white and acpl_black")
  return(df)
}


#### POV ####

# add variables from the stronger player's POV
add_vars_from_stronger_player_pov <- function(df)
{
  # for output
  start_columns <- names(df)
  
  # make POV vars
  # get stronger player
  pl_stronger <- ifelse(df$rating_diff > 0, "white", "black")
  
  # add result
  df$POV_Result <- case_when(
    (df$Result == "1-0" & pl_stronger=="white") | (df$Result == "0-1" & pl_stronger=="black") ~ "Win",
    (df$Result == "0-1" & pl_stronger=="white") | (df$Result == "1-0" & pl_stronger=="black") ~ "Loss",
    df$Result=="1/2-1/2" ~ "Draw",
    TRUE ~ NA_character_
  )
  
  # add aCPL
  df$POV_acpl <- ifelse(pl_stronger=="white", df$acpl_white, df$acpl_black)
  
  # add Elo
  df$POV_Elo <- ifelse(pl_stronger=="white", df$WhiteElo, df$BlackElo)
  
  # make rating_diff POV
  df$POV_rating_diff <- abs(df$rating_diff)
  
  # out
  print("Added variables:")
  print(setdiff(names(df), start_columns))
  return(df)
}