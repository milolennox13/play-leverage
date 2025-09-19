library(ggplot2)
library(quantreg)
library(tidyr)
library(splines)

# RUN "presentation0setup.R" BEFOREHAND

# ---------------------------
# BETA DISTRIBUTION EXAMPLES
# ---------------------------

# ---------------------------
# 1. Varying S with constant mu
# ---------------------------

# Choose parameter values
params <- expand.grid(
  mu = 0.5,
  betasize = c(1, 10, 100, 1000)
)

# Convert to alpha and beta
params$alpha = params$betasize * params$mu
params$beta = params$betasize * (1 - params$mu)

# Evaluate density
x <- seq(0, 1, length.out = 500)
df <- do.call(rbind, apply(params, 1, function(p) {
  data.frame(
    x = x,
    density = dbeta(x, p["alpha"], p["beta"]),
    alpha = p["alpha"],
    beta = p["beta"],
    mu = p["mu"],
    S = p["betasize"]
  )
}))

# Plot
ggplot(df, aes(x, density, color = factor(paste0("μ=", mu, ", S=", S)))) +
  geom_line(linewidth = 1.2) +
  labs(color = "Parameters") +
  theme_minimal() +
  labs(
    title = "Example WP distribution for different values of S",
    x = "Post-Play Win Probability",
    y = "Density"
  )

# ---------------------------
# 2. Varying mu with constant S
# ---------------------------

# Choose parameter values
params <- expand.grid(
  mu = c(0.1, 0.25, 0.5, 0.75, 0.9),
  betasize = 300
)

# Convert to alpha and beta
params$alpha = params$betasize * params$mu
params$beta = params$betasize * (1 - params$mu)

# Evaluate density
x <- seq(0, 1, length.out = 500)
df <- do.call(rbind, apply(params, 1, function(p) {
  data.frame(
    x = x,
    density = dbeta(x, p["alpha"], p["beta"]),
    alpha = p["alpha"],
    beta = p["beta"],
    mu = p["mu"],
    S = p["betasize"]
  )
}))

# Plot
ggplot(df, aes(x, density, color = factor(paste0("μ=", mu, ", S=", S)))) +
  geom_line(linewidth = 1.2) +
  labs(color = "Parameters") +
  theme_minimal() +
  labs(
    title = "Example WP distribution for different values of μ",
    x = "Post-Play Win Probability",
    y = "Density"
  )

# ---------------------------
# ---------------------------
# PLAY LEVERAGE PLOTS
# ---------------------------
# ---------------------------

# ---------------------------
# PL HISTOGRAM
# ---------------------------

ggplot(pbp2024_model_change, aes(x = fitted_leverage)) +
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth = 1e-3,
    fill = "#002244",
    color = "white",
    alpha = 0.85
  ) +
  labs(
    title = "Distribution of Play Leverage",
    subtitle = "Model-estimated importance of each play on the match result",
    x = "Play Leverage",
    y = "Density"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 13, color = "gray30"),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  coord_cartesian(xlim = c(0, 5e-2))


# ---------------------------
# TIME REMAINING VS PLAY LEVERAGE
# ---------------------------

# Choosing quantiles
taus <- c(0.1, 0.5, 0.9)

# Fitting a curve for each quantile
fits <- lapply(taus, function(tau) {
  rq(fitted_leverage ~ ns(game_seconds_remaining, df = 6),
     tau = tau,
     data = normal_time)
})

breaks <- c(3600, 2700, 1800, 900, 0)
labels <- c("START", "Start Q2", "HALF", "Start Q4", "END")

# New data for predictions
newdata <- data.frame(game_seconds_remaining = seq(min(normal_time$game_seconds_remaining, na.rm = TRUE),
                                       max(normal_time$game_seconds_remaining, na.rm = TRUE),
                                       length.out = 200))

# Collect predictions into one data frame
pred_long <- do.call(rbind, lapply(1:length(taus), function(i) {
  data.frame(game_seconds_remaining = newdata$game_seconds_remaining,
             value = predict(fits[[i]], newdata = newdata),
             quantile = paste0("tau_", taus[i]))
}))

# ---------------------------
# 1. Median, 10th and 90th percentile lines
# ---------------------------

y_lower = min(pred_long$value)
y_upper = max(pred_long$value)

ggplot(normal_time, aes(x = game_seconds_remaining, y = fitted_leverage)) +
  geom_point(alpha = 0.01, color = "#002244") +
  geom_line(
    data = pred_long %>% filter(quantile == "tau_0.5"),
    aes(x = game_seconds_remaining, y = value),
    color = "#002244", linewidth = 1, inherit.aes = FALSE
  ) +
  geom_line(
    data = pred_long %>% filter(quantile == "tau_0.1"),
    aes(x = game_seconds_remaining, y = value),
    color = "#C60C30", linetype = "dashed", linewidth = 0.75, inherit.aes = FALSE
  ) +
  geom_line(
    data = pred_long %>% filter(quantile == "tau_0.9"),
    aes(x = game_seconds_remaining, y = value),
    color = "#C60C30", linetype = "dashed", linewidth = 0.75, inherit.aes = FALSE
  ) +
  labs(
    title = "How Play Leverage Changes Throughout the Game",
    subtitle = "Median (navy) and 10th–90th percentiles (red dashed)",
    x = "Game Time",
    y = "Play Leverage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_line(linewidth = 0.5, color = "grey40"),
    panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80"),
    panel.grid.minor = element_blank()
  ) +
  scale_x_reverse(breaks = breaks, labels = labels) +
  coord_cartesian(ylim = c(y_lower, y_upper))

# ---------------------------
# 2. Median line only
# ---------------------------

# narrower Y-limits and no ribbons
medians = pred_long %>% filter(quantile == "tau_0.5")
y_lower = min(medians$value)
y_upper = max(medians$value)

ggplot(normal_time, aes(x = game_seconds_remaining, y = fitted_leverage)) +
  geom_point(alpha = 0.01, color = "#002244") +
  geom_line(
    data = pred_long %>% filter(quantile == "tau_0.5"),
    aes(x = game_seconds_remaining, y = value),
    color = "#002244", linewidth = 1, inherit.aes = FALSE
  ) +
  labs(
    title = "How Median Play Leverage Changes Throughout the Game",
    x = "Game Time",
    y = "Play Leverage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_line(linewidth = 0.5, color = "grey40"),
    panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80"),
    panel.grid.minor = element_blank()
  ) +
  scale_x_reverse(breaks = breaks, labels = labels) +
  coord_cartesian(ylim = c(y_lower, y_upper))


# ---------------------------
# SCORE DIFFERENTIAL VS PLAY LEVERAGE
# ---------------------------

# Choosing quantiles
taus <- c(0.1, 0.5, 0.9)

# Fitting a curve for each quantile
fits <- lapply(taus, function(tau) {
  rq(fitted_leverage ~ ns(score_differential, df = 6),
     tau = tau,
     data = normal_time)
})

# New data for predictions
newdata <- data.frame(score_differential = seq(min(normal_time$score_differential, na.rm = TRUE),
                                                   max(normal_time$score_differential, na.rm = TRUE),
                                                   by = 1))

# Collect predictions into one data frame
pred_long <- do.call(rbind, lapply(1:length(taus), function(i) {
  data.frame(score_differential = newdata$score_differential,
             value = predict(fits[[i]], newdata = newdata),
             quantile = paste0("tau_", taus[i]))
}))

breaks <- c(-16, -8, 0, 8, 16)
labels <- c("-16", "-8", "0", "+8", "+16")

medians = pred_long %>% filter(quantile == "tau_0.5" & (abs(score_differential) <= 16))
y_lower = min(medians$value)
y_upper = max(medians$value)

ggplot(normal_time, aes(x = score_differential, y = fitted_leverage)) +
  geom_point(alpha = 0.01, color = "#002244") +
  geom_line(
    data = medians,
    aes(x = score_differential, y = value),
    color = "#002244", linewidth = 1, inherit.aes = FALSE
  ) +
  labs(
    title = "How Median Play Leverage Changes with Game State",
    x = "Score differential (team in possession)",
    y = "Play Leverage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.x = element_line(linewidth = 0.5, color = "grey40"),
    panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80"),
  ) +
  scale_x_continuous(breaks = breaks, labels = labels) +
  coord_cartesian(ylim = c(y_lower, y_upper), xlim = c(-16, 16))


# ---------------------------
# ---------------------------
# WP STANDARD DEVIATION PLOTS
# ---------------------------
# ---------------------------

# ---------------------------
# TIME REMAINING VS WP STANDARD DEVIATION
# ---------------------------

# Choosing quantiles
taus <- c(0.1, 0.5, 0.9)

# Fitting a curve for each quantile
fits <- lapply(taus, function(tau) {
  rq(fitted_stdev ~ ns(game_seconds_remaining, df = 6),
     tau = tau,
     data = normal_time)
})

breaks <- c(3600, 2700, 1800, 900, 0)
labels <- c("START", "Start Q2", "HALF", "Start Q4", "END")

# New data for predictions
newdata <- data.frame(game_seconds_remaining = seq(min(normal_time$game_seconds_remaining, na.rm = TRUE),
                                                   max(normal_time$game_seconds_remaining, na.rm = TRUE),
                                                   length.out = 200))

# Collect predictions into one data frame
pred_long <- do.call(rbind, lapply(1:length(taus), function(i) {
  data.frame(game_seconds_remaining = newdata$game_seconds_remaining,
             value = predict(fits[[i]], newdata = newdata),
             quantile = paste0("tau_", taus[i]))
}))

# ---------------------------
# 1. Median, 10th and 90th percentile lines
# ---------------------------

y_lower = min(pred_long$value)
y_upper = max(pred_long$value)

ggplot(normal_time, aes(x = game_seconds_remaining, y = fitted_stdev)) +
  geom_point(alpha = 0.01, color = "#002244") +
  geom_line(
    data = pred_long %>% filter(quantile == "tau_0.5"),
    aes(x = game_seconds_remaining, y = value),
    color = "#002244", linewidth = 1, inherit.aes = FALSE
  ) +
  geom_line(
    data = pred_long %>% filter(quantile == "tau_0.1"),
    aes(x = game_seconds_remaining, y = value),
    color = "#C60C30", linetype = "dashed", linewidth = 0.75, inherit.aes = FALSE
  ) +
  geom_line(
    data = pred_long %>% filter(quantile == "tau_0.9"),
    aes(x = game_seconds_remaining, y = value),
    color = "#C60C30", linetype = "dashed", linewidth = 0.75, inherit.aes = FALSE
  ) +
  labs(
    title = "How WP Standard Deviation Changes Throughout the Game",
    subtitle = "Median (navy) and 10th–90th percentiles (red dashed)",
    x = "Game Time",
    y = "WP Standard Deviation"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_line(linewidth = 0.5, color = "grey40"),
    panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80"),
    panel.grid.minor = element_blank()
  ) +
  scale_x_reverse(breaks = breaks, labels = labels) +
  coord_cartesian(ylim = c(y_lower, y_upper))

# ---------------------------
# 2. Median line only
# ---------------------------

# narrower Y-limits and no ribbons
medians = pred_long %>% filter(quantile == "tau_0.5")
y_lower = min(medians$value)
y_upper = max(medians$value)

ggplot(normal_time, aes(x = game_seconds_remaining, y = fitted_stdev)) +
  geom_point(alpha = 0.01, color = "#002244") +
  geom_line(
    data = pred_long %>% filter(quantile == "tau_0.5"),
    aes(x = game_seconds_remaining, y = value),
    color = "#002244", linewidth = 1, inherit.aes = FALSE
  ) +
  labs(
    title = "How Median WP Standard Deviation Changes Throughout the Game",
    x = "Game Time",
    y = "WP Standard Deviation"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_line(linewidth = 0.5, color = "grey40"),
    panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80"),
    panel.grid.minor = element_blank()
  ) +
  scale_x_reverse(breaks = breaks, labels = labels) +
  coord_cartesian(ylim = c(y_lower, y_upper))


# ---------------------------
# SCORE DIFFERENTIAL VS WP Standard Deviation
# ---------------------------

# Choosing quantiles
taus <- c(0.1, 0.5, 0.9)

# Fitting a curve for each quantile
fits <- lapply(taus, function(tau) {
  rq(fitted_stdev ~ ns(score_differential, df = 6),
     tau = tau,
     data = normal_time)
})

# New data for predictions
newdata <- data.frame(score_differential = seq(min(normal_time$score_differential, na.rm = TRUE),
                                               max(normal_time$score_differential, na.rm = TRUE),
                                               by = 1))

# Collect predictions into one data frame
pred_long <- do.call(rbind, lapply(1:length(taus), function(i) {
  data.frame(score_differential = newdata$score_differential,
             value = predict(fits[[i]], newdata = newdata),
             quantile = paste0("tau_", taus[i]))
}))

breaks <- c(-16, -8, 0, 8, 16)
labels <- c("-16", "-8", "0", "+8", "+16")

medians = pred_long %>% filter(quantile == "tau_0.5" & (abs(score_differential) <= 16))
y_lower = min(medians$value)
y_upper = max(medians$value)

ggplot(normal_time, aes(x = score_differential, y = fitted_stdev)) +
  geom_point(alpha = 0.01, color = "#002244") +
  geom_line(
    data = medians,
    aes(x = score_differential, y = value),
    color = "#002244", linewidth = 1, inherit.aes = FALSE
  ) +
  labs(
    title = "How Median WP Standard Deviation Changes with Game State",
    x = "Score differential (team in possession)",
    y = "WP Standard Deviation"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.x = element_line(linewidth = 0.5, color = "grey40"),
    panel.grid.major.y = element_line(linewidth = 0.5, color = "grey80"),
  ) +
  scale_x_continuous(breaks = breaks, labels = labels) +
  coord_cartesian(ylim = c(y_lower, y_upper), xlim = c(-16, 16))

