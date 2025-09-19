# RUN "presentation0functions_betaS.R" BEFOREHAND

pbp2024 = load_pbp(2024)
pbp2024 = pbp2024 %>% filter(season_type == "REG")
players = load_players()

factors = c("posteam_type", "qtr", "down", 
            "posteam_timeouts_remaining", "defteam_timeouts_remaining")
numerics = c("yardline_100", "quarter_seconds_remaining", "half_seconds_remaining", 
             "game_seconds_remaining", "ydstogo", "score_differential")

set.seed("1234")
pbp2024_model = full_xwpa_model(pbp2024, factors, numerics)
pbp2024_model = pbp2024_model %>% mutate(fitted_leverage = 1 / (fitted_betasize + 1))
pbp2024_model = pbp2024_model %>% mutate(fitted_variance = wp * (1 - wp) / (fitted_betasize + 1))
pbp2024_model = pbp2024_model %>% mutate(fitted_stdev = sqrt(fitted_variance))

pbp2024_model_change = pbp2024_model %>% filter(abs(pbp2024_model$wpa) > 0)

normal_time = pbp2024_model_change %>% filter(qtr != 5)
