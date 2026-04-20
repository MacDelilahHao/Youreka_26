#install.packages("haven")
#install.packages("ggplot2")
#install.packages("ggeffects")
#install.packages("lmtest")

#library(haven)
library(ggplot2)
library(ggeffects)
library(lmtest)

dt <- read.csv("~/Downloads/Youreka_25/20260326_1011data.csv")
dt <- subset(dt, !is.na(CO2)) # this cleans it

#head(dt, 5)

#plot(dt$CO2, y_var)

#str(dt)
#dt$CO2 <- as.numeric(dt$CO2) # turns CO2 into numeric

# --- multivariate cleaned dataset ---

y_var <- dt$Total # y_variable for multivariate regression
print(y_var)

c_dt <- dt[, c("Total", "Temperature", "CO2", "PM2.5", "Time")]
#head(c_dt, 3)

#nrow(c_dt) # count the number of observations used in the analysis

plot(c_dt)


# --- ASSUMPTIONS ---

# outliers
boxplot(c_dt$Total, main="Boxplot for Total ARGs", ylab="Value")
boxplot(c_dt$Temperature, main="Boxplot for Temperature", ylab="Value")
boxplot(c_dt$CO2, main="Boxplot for CO2", ylab="Value")
boxplot(c_dt$PM2.5, main="Boxplot for PM2.5", ylab="Value")

all_mr <- lm(y_var ~ Temperature + CO2 + PM2.5 + Time, data = c_dt)

# normality of residuals

residuals = all_mr$residuals
hist(residuals)

# multicollinearity assumption plotted 

# linearity
resettest(all_mr)
plot(all_mr, 1)

# independence (Durbin-Watson)
dw_stat <- sum((diff(residuals))^2) / sum(residuals^2)
print(dw_stat)

# homoscedascity (Breush-Pagan Test)
plot(all_mr)
bp_test <- bptest(all_mr)
print(bp_test)


# block 1 for total
time_mr <- lm(y_var ~ Time, data = c_dt)
summary(time_mr)

#abline(time_mr, col="red", lwd=2)

# block 2 for total
all_mr <- lm(y_var ~ Temperature + CO2 + PM2.5 + Time, data = c_dt)
summary(all_mr)


anova(time_mr, all_mr) # Comparing block 1 w/ block 2

# COEFF GRAPH FOR JUST TIME -> POSTER

confint(time_mr)

# Coefficient estimates
coef_mat <- summary(time_mr)$coefficients
coef_df <- as.data.frame(coef_mat)
coef_df$term <- rownames(coef_df)

# Confidence intervals (default = 95%)
ci <- confint(time_mr)
ci_df <- as.data.frame(ci)
ci_df$term <- rownames(ci_df)

# Merge coefficients + CI
plot_df <- merge(coef_df, ci_df, by = "term")

# Remove intercept
plot_df <- plot_df[plot_df$term != "(Intercept)", ]

# Set order
plot_df$term <- factor(plot_df$term,
                       levels = c("Time"))

# Plot
ggplot(plot_df, aes(x = term, y = Estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_point(color = "#5d5387", size = 3) +
  geom_errorbar(aes(ymin = `2.5 %`, ymax = `97.5 %`),
                width = 0.2, color = "black") +
  labs(
    title = "Coefficient Plot for Time on Total ARGs",
    x = "Variable",
    y = "Coefficient (95% CI)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    text = element_text(family = "Times New Roman")
  ) +
  coord_flip()


# COEFF GRAPH FOR TOTAL -> POSTER

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
  geom_point(color = "#5d5387", size = 3) +
  geom_errorbar(aes(ymin = `2.5 %`, ymax = `97.5 %`),
                width = 0.2, color = "black") +
  labs(
    title = "Coefficient Plot for Climate Change Variables on Total ARGs",
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
  geom_line(color = "#5d5387", linewidth = 1.2) +
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


# COEFF GRAPH FOR ADJUSTED ->POSTER

confint(adj_all_mr)

# Coefficient estimates
coef_mat <- summary(adj_all_mr)$coefficients
coef_df <- as.data.frame(coef_mat)
coef_df$term <- rownames(coef_df)

# Confidence intervals (default = 95%)
ci <- confint(adj_all_mr)
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
  geom_point(color = "#5d5387", size = 3) +
  geom_errorbar(aes(ymin = `2.5 %`, ymax = `97.5 %`),
                width = 0.2, color = "black") +
  labs(
    title = "Coefficient Plot for Climate Change Variables on Perfect and Strict ARGs",
    x = "Variable",
    y = "Coefficient (95% CI)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    text = element_text(family = "Times New Roman")
  ) +
  coord_flip()
