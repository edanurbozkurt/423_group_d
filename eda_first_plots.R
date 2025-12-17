library(dplyr)
library(ggplot2)
df <- read.csv("FALL25IE423ProjectData_v2.csv")
cat("Rows:", nrow(df), " Cols:", ncol(df), "\n")
df <- janitor::clean_names(df)

df_150 <- df %>%
  filter(match_id %in% unique(match_id)[1:150])

df_150_90 <- df_150 %>%
  filter(minute <= 90)

ball_possession_min_away <- df_150_90 %>%
  group_by(match_id, minute) %>%
  summarise(ball_possession_away = mean(ball_possession_away, na.rm = TRUE),
            .groups = "drop")

ball_possession_min_home <- df_150_90 %>%
  group_by(match_id, minute) %>%
  summarise(ball_possession_home = mean(ball_possession_home, na.rm = TRUE),
            .groups = "drop")


avg_ball_home <- ball_possession_min_home %>%
  group_by(minute) %>%
  summarise(ball_possession_home = mean(ball_possession_home, na.rm = TRUE))


plot_possession_away <-ggplot(avg_ball_away,
       aes(x = minute, y = ball_possession_away)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Average Away Team Ball Possession (First 150 Matches)",
       x = "Minute",
       y = "Average Ball Possession") +
  theme_minimal()

#print(plot_possession_away)

plot_possession_home <-ggplot(avg_ball_home,
       aes(x = minute, y = ball_possession_home)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Average Home Team Ball Possession (First 150 Matches)",
       x = "Minute",
       y = "Average Ball Possession") +
  theme_minimal()

#print(plot_possession_home)

fauls_min_home <- df_150_90 %>%
  group_by(match_id, minute) %>%
  summarise(fouls_home = mean(fouls_home, na.rm = TRUE),
            .groups = "drop")

avg_fauls_home <- fauls_min_home %>%
  group_by(minute) %>%
  summarise(fouls_home = mean(fouls_home, na.rm = TRUE))

plot_fauls_home <-ggplot(avg_fauls_home,
       aes(x = minute, y = fouls_home)) +
  geom_line(linewidth = 1) +
  labs(title = "Average Home Team Fouls (First 150 Matches)",
       x = "Minute",
       y = "Average Fouls") +
  theme_minimal()

#print(plot_fauls_home)



# ===============================
# Average possession per minute
# ===============================
avg_ball_away <- ball_possession_min_away %>%
  group_by(minute) %>%
  summarise(
    ball_possession_away = mean(ball_possession_away, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(minute)

# ===============================
# EWMA parameters
# ===============================
lambda <- 0.2   # smoothing parameter
L <- 3          # control limit width

# ===============================
# Compute EWMA statistic
# ===============================
ewma_data_away <- avg_ball_away %>%
  mutate(
    ewma = {
      z <- numeric(n())
      z[1] <- ball_possession_away[1]
      for (i in 2:n()) {
        z[i] <- lambda * ball_possession_away[i] +
                (1 - lambda) * z[i - 1]
      }
      z
    }
  )

# ===============================
# Control limits
# ===============================
mu <- mean(ewma_data_away$ball_possession_away, na.rm = TRUE)
sigma <- sd(ewma_data_away$ball_possession_away, na.rm = TRUE)

ewma_data_away <- ewma_data_away %>%
  mutate(
    ucl = mu + L * sigma * sqrt(lambda / (2 - lambda)),
    lcl = mu - L * sigma * sqrt(lambda / (2 - lambda))
  )

# ===============================
# EWMA plot
# ===============================
plot_ewma_away <- ggplot(ewma_data_away, aes(x = minute)) +
  geom_line(aes(y = ewma), linewidth = 1) +
  geom_hline(aes(yintercept = mu), linetype = "dotted") +
  geom_hline(aes(yintercept = ucl), linetype = "dashed") +
  geom_hline(aes(yintercept = lcl), linetype = "dashed") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "EWMA Control Chart – Away Ball Possession",
    subtitle = "First 150 Matches, Minutes 1–90 (λ = 0.2, L = 3)",
    x = "Minute",
    y = "EWMA Ball Possession (%)"
  ) +
  theme_minimal()

#print(plot_ewma_away)


library(dplyr)
library(ggplot2)
library(janitor)

# Veriyi oku ve temizle
df <- read.csv("FALL25IE423ProjectData_v2.csv")
df <- clean_names(df)

# İlk 150 maçı al
df_150 <- df %>%
  filter(match_id %in% unique(match_id)[1:150])

# Toplam maç sayısı
n_matches <- n_distinct(df_150$match_id)

# KÜMÜLATİF GOLLERDEN GOL OLAYINI ÇIKAR
away_goal_minutes <- df_150 %>%
  arrange(match_id, minute) %>%
  group_by(match_id) %>%
  mutate(
    goal_event_away = goals_away - lag(goals_away, default = 0)
  ) %>%
  ungroup() %>%
  filter(goal_event_away == 1)

# Dakika bazında maç başına ortalama gol
avg_away_goals_by_minute <- away_goal_minutes %>%
  filter(minute <= 90) %>%
  group_by(minute) %>%
  summarise(
    avg_goals = n() / n_matches,
    .groups = "drop"
  )

# Plot
plot_goals_away <-ggplot(avg_away_goals_by_minute,
       aes(x = minute, y = avg_goals)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  labs(
    title = "Average Away Goals by Minute (First 150 Matches)",
    x = "Minute",
    y = "Average Goals per Match"
  ) +
  theme_minimal()
#print(plot_goals_away)

