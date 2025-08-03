library(ggplot2)
library(dplyr)
library(tidyr)
set.seed(123)

# --- Part 1: Conceptual sector curves (as before) ---
x <- seq(-10, 10, length.out = 500)
sector_curve_df <- data.frame(
  x = x,
  Private = -0.5*(x + 4)^2 + 8 + 3 * sin(0.7 * x),      # dynamic, high/volatile motivation
  Public = -0.05*(x - 8)^2 + 9,                        # stable valley
  Unemployed = 0.1 * x^2 - 5                           # unstable (no equilibrium)
) %>% pivot_longer(cols = c("Private", "Public", "Unemployed"),
                   names_to = "Sector", values_to = "CurveMotivation")

# Critical annotation points
points <- data.frame(
  Sector = c("Private", "Public", "Unemployed"),
  x = c(-4, 8, 0),
  y = c(8, 9, -5),
  Label = c("High Motivation\n(Low Stability)",
            "Stable Equilibrium\n(High Job Security)",
            "Unstable Equilibrium\n(Low Motivation)")
)

# --- Part 2: Individual-level synthetic data aligned to sectors ---
n <- 200
# Core psychological predictors (same scale)
Autonomy <- rnorm(n, mean = 5, sd = 1.5)
Uncertainty <- rnorm(n, mean = 5, sd = 1.5)
Internal <- rnorm(n, mean = 5, sd = 1.5)

# Assign each synthetic individual a sector: Private / Public / Unemployed
sector_assignment <- sample(c("Private", "Public", "Unemployed"), size = n, replace = TRUE, prob = c(0.4, 0.4, 0.2))
# Base motivation from true underlying psychological model
base_motivation <- 0.6 * Autonomy - 0.5 * Uncertainty + 0.8 * Internal

# Add sector-level offsets to reflect conceptual curves:
sector_offset <- ifelse(sector_assignment == "Private", 1.5,    # slightly elevated but volatile
                        ifelse(sector_assignment == "Public", 2.5,  # stable boost
                               -2))                            # unemployment depresses
# Add noise
MotivationScore <- base_motivation + sector_offset + rnorm(n, sd = 1)

# Construct data frame
data <- data.frame(
  MotivationScore,
  Autonomy,
  Uncertainty,
  Internal,
  Sector = factor(sector_assignment, levels = c("Private", "Public", "Unemployed"))
)

# --- Part 3: Regression model with sector as factor (and interactions if desired) ---
# Main effects model
model_main <- lm(MotivationScore ~ Autonomy + Uncertainty + Internal + Sector, data = data)

# Optional: allow interaction between sector and psychological predictors
model_interact <- lm(MotivationScore ~ (Autonomy + Uncertainty + Internal) * Sector, data = data)

# Summaries
summary_main <- summary(model_main)
summary_interact <- summary(model_interact)
print(summary_main)
print(summary_interact)

# --- Part 4: Visualization ---
# 4a. Plot conceptual curves as backdrop
p1 <- ggplot() +
  geom_line(data = sector_curve_df, aes(x = x, y = CurveMotivation, color = Sector), size = 1.2) +
  geom_point(data = points, aes(x = x, y = y), size = 4) +
  geom_text(data = points, aes(x = x, y = y, label = Label), vjust = -0.5, size = 3.5) +
  scale_color_manual(values = c("Private" = "blue", "Public" = "darkgreen", "Unemployed" = "red")) +
  labs(
    title = "Employment Dynamics and Motivation Relationship",
    x = "Work Environment Dynamism →",
    y = "Motivation Level",
    color = "Sector"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  coord_cartesian(ylim = c(-6, 12))

# 4b. Overlay individual data predictions (using main model) projected by sector on an abstract x-axis
# We'll assign canonical x positions for sectors to overlay
sector_positions <- data.frame(Sector = c("Private", "Public", "Unemployed"),
                               x = c(-4, 8, 0))
data$predicted_main <- predict(model_main, newdata = data)

# Jittered points for individuals colored by sector
p2 <- ggplot(data, aes(x = Sector, y = MotivationScore, color = Sector)) +
  geom_jitter(width = 0.2, alpha = 0.6) +
  geom_point(aes(y = predicted_main), shape = 18, size = 2, position = position_nudge(x = 0.2)) +
  labs(title = "Observed vs Predicted Motivation by Sector (Main Model)",
       x = "Sector",
       y = "Motivation Score") +
  scale_color_manual(values = c("Private" = "blue", "Public" = "darkgreen", "Unemployed" = "red")) +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

# Show plots
print(p1)
print(p2)

# --- Optional diagnostics ---
par(mfrow = c(2,2))
plot(model_main, main = "Diagnostics: Main Model")
