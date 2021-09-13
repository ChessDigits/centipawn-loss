"
2021.09.12
Centipawn Loss

pip
"

source("centipawn_fn.R")
df <- load_data(k_games = 200, use_local_file = TRUE)
df <- replace_mates_with_extreme_evaluations(df)
df <- add_eval_change_at_each_ply(df)



