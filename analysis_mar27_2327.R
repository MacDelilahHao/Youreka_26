#install.packages("haven")
#install.packages("ggplot2")
#install.packages("ggeffects")

library(haven)
library(ggplot2)
library(ggeffects)

dt <- read.csv("~/Downloads/Youreka_25/20260322_1001data.csv")
#dt <- na.omit(dt) - this deletes the last row w/out the lag but lag has been removed
dt <- subset(dt, !is.na(CO2)) # this works

#head(dt, 5)

#plot(dt$CO2, y_var)

#str(dt)
#dt$CO2 <- as.numeric(dt$CO2) # turns CO2 into numeric

# --- multivariate cleaned dataset ---

y_var <- dt$Total # y_variable for multivariate regression

c_dt <- dt[, c("Total", "Temperature", "CO2", "PM2.5", "Time")]
#head(c_dt, 3)

plot(c_dt)

# block 1 for total
time_mr <- lm(y_var ~ Time, data = dt)
summary(time_mr)

#abline(time_mr, col="red", lwd=2)

# block 2 for total
all_mr <- lm(y_var ~ Temperature + CO2 + PM2.5 + Time, data = c_dt)
summary(all_mr)


anova(time_mr, all_mr) # Comparing block 1 w/ block 2


# COEFF GRAPH FOR POSTER

confint(all_mr)

# Coefficient estimates
coef_mat <- summary(all_mr)$coefficients
coef_df <- as.data.frame(coef_mat)
coef_df$term <- rownames(coef_df)

# Confidence intervals (default = 95%)
ci <- confint(all_mr)
ci_df <- as.data.frame(ci)
ci_df$term <- rownames(ci_df)

# Merge coefficients + CI
plot_df <- merge(coef_df, ci_df, by = "term")

# Remove intercept
plot_df <- plot_df[plot_df$term != "(Intercept)", ]

# Set order
plot_df$term <- factor(plot_df$term,
                       levels = c("Temperature", "CO2", "PM2.5", "Time"))

# Plot
ggplot(plot_df, aes(x = term, y = Estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_point(color = "blue", size = 3) +
  geom_errorbar(aes(ymin = `2.5 %`, ymax = `97.5 %`),
                width = 0.2, color = "black") +
  labs(
    title = "Coefficient Plot for Climate Change Variables",
    x = "Variable",
    y = "Coefficient (95% CI)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    text = element_text(family = "Times New Roman")
  ) +
  coord_flip()


# PARTIAL PLOT FOR POSTER

# Create a sequence of Temperature values while holding other predictors constant
partial_temp <- data.frame(
  Temperature = seq(min(c_dt$Temperature), max(c_dt$Temperature), length.out = 100),
  CO2 = mean(c_dt$CO2),     # hold other predictors at their mean
  PM2.5 = mean(c_dt$PM2.5),
  Time = mean(c_dt$Time)
)

# Generate predicted values + 95% confidence intervals
pred <- predict(all_mr, partial_temp, interval = "confidence")
partial_temp <- cbind(partial_temp, pred)  # combine predictions with data

# Plot partial effect using ggplot2

ggplot(partial_temp, aes(x = Temperature, y = fit)) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, fill = "blue") +
  geom_point(data = c_dt, aes(x = Temperature, y = Total), color = "black", alpha = 0.6) +
  labs(
    title = "Partial Effect of Temperature (˚F) on Total ARGs",
    x = "Temperature (˚F)",
    y = "Predicted Total"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )


# --- multivariate cleaned dataset (adjusted total) ---

y_adjvar <- dt$AdjTotal # y_variable for multivariate regression

adj_dt <- dt[, c("AdjTotal", "Temperature", "CO2", "PM2.5", "Time")]

plot(adj_dt)

# block 1 for adjusted
adj_mr <- lm(y_adjvar ~ Time, data = adj_dt)
summary(adj_mr)

# block 2 for adjusted
adj_all_mr <- lm(y_adjvar ~ Temperature + CO2 + PM2.5 + Time, data = adj_dt)
summary(adj_all_mr)


anova(adj_mr, adj_all_mr) # Comparing block 1 w/ block 2


