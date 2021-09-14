"
2021.09.12
Centipawn Loss

pip
"

source("centipawn_fn.R")
df <- load_data(k_games = 200, use_local_file = TRUE)
bu <- df

#
df <- bu
df <- keep_games_with_minimum_number_of_moves(df, min_moves = 10, add_n_moves = FALSE)
df <- df %>% slice_sample(n=10000)
df <- replace_mates_with_extreme_evaluations(df)
df <- add_eval_change_at_each_ply(df)

# how to calculate centipawn loss though?? 
# eg for white: eval after black plays is SF best move, then one ply later after white moves, this different is cpl
# the way add_eval_change_at_each_ply() is defined seems to be this
# need to take columns with even or odd numbers for a player

df <- add_acpl_for_each_player(df, fn = median)

# acpl#1 j'avais enlevé les swings qui avaient duré juste 1 ply je pense; idem capture patterns jpense...


ggplot(df, aes(x=WhiteElo, y=acpl_white))+geom_point()+ylim(c(1,200))
hist(df$acpl_white, breaks=1000, xlim=c(0, 100))
view(df %>% filter(acpl_white > 120))



#
df <- add_rating_differential(df)
df_scaled <- df %>% mutate_if(is.numeric, scale)
ana <- list()
ana$lm <- lm(acpl_white ~ rating_diff + WhiteElo + Result, df_scaled)
summary(ana$lm)
plot(ana$lm)

# try gam !!!! lol
ana$gam <- gam(acpl_white ~ s(rating_diff) + s(WhiteElo) + Result, data=df_scaled, method="REML")
summary(ana$gam)
gam.check(ana$gam)


