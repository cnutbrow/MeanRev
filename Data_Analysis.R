subtitle = "Grouped by strategy aggressiveness (m), aggregated over all k",
x = "Window (Moving Average Window)",
y = "Average Total Return (%)",
color = "m"
) +
theme_minimal(base_size = 13) +
theme(legend.position = "right")
b2 <- lm(`Total Return` ~ Window * k * m, data = bitcoin)
summary(b2)
b1 <- lm(`Total Return` ~ Window + k + m, data = bitcoin)
summary(b1)
bitcoin %>%
filter(Window == 20) %>%
ggplot(aes(x = k, y = `Total Return`, color = factor(m), group = m)) +
geom_line(linewidth = 1.2) +
geom_point(size = 1.5, alpha = 0.8) +
scale_color_manual(values = color_palette) +
labs(
title = "Total Return vs k (Window = 20)",
subtitle = "Each line represents a different level of strategy aggressiveness (m)",
x = "k (Standard Deviation Multiplier)",
y = "Total Return (%)",
color = "m"
) +
theme_minimal(base_size = 13) +
theme(legend.position = "right")
bitcoin %>%
filter(Window == 20) %>%
ggplot(aes(x = k, y = `Total Return`, color = factor(m), group = m)) +
geom_line(linewidth = 1.2) +
geom_point(size = 1.5, alpha = 0.8) +
scale_color_manual(values = color_palette) +
labs(
title = "Total Return vs k (Window = 20)",
subtitle = "Each line represents a different level of strategy aggressiveness (m)",
x = "k (Standard Deviation Multiplier)",
y = "Total Return (%)",
color = "m"
) +
theme_minimal(base_size = 13) +
theme(legend.position = "right")
bitcoin %>%
filter(Window == 20) %>%
ggplot(aes(x = k, y = `Total Return`, color = factor(m), group = m)) +
geom_line(linewidth = 1.2) +
geom_point(size = 1.5, alpha = 0.8) +
scale_color_manual(values = color_palette) +
labs(
title = "Total Return vs k (Window = 20)",
subtitle = "Each line represents a different level of strategy aggressiveness (m)",
x = "k (Standard Deviation Multiplier)",
y = "Total Return (%)",
color = "m"
) +
theme_minimal(base_size = 13) +
theme(legend.position = "right")
bitcoin %>%
filter(Window == 20) %>%
ggplot(aes(x = k, y = `Total Return`, color = factor(m), group = m)) +
geom_line(linewidth = 1.2) +
geom_point(size = 1.5, alpha = 0.8) +
scale_color_manual(values = color_palette) +
labs(
title = "Total Return vs k (Window = 20)",
subtitle = "Each line represents a different level of strategy aggressiveness (m)",
x = "k (Standard Deviation Multiplier)",
y = "Total Return (%)",
color = "m"
) +
theme_minimal(base_size = 13) +
theme(legend.position = "right")
bitcoin %>%
filter(Window == 20) %>%
ggplot(aes(x = k, y = `Total Return`, color = factor(m), group = m)) +
geom_line(linewidth = 1.2) +
geom_point(size = 1.5, alpha = 0.8) +
scale_color_manual(values = color_palette) +
labs(
title = "Total Return vs k (Window = 20)",
subtitle = "Each line represents a different level of strategy aggressiveness (m)",
x = "k (Standard Deviation Multiplier)",
y = "Total Return (%)",
color = "m"
) +
theme_minimal(base_size = 13) +
theme(legend.position = "right")
bitcoin %>%
filter(Window == 20) %>%
ggplot(aes(x = k, y = `Total Return`, color = factor(m), group = m)) +
geom_line(linewidth = 1.2) +
geom_point(size = 1.5, alpha = 0.8) +
scale_color_manual(values = color_palette) +
labs(
title = "Total Return vs k (Window = 20)",
subtitle = "Each line represents a different level of strategy aggressiveness (m)",
x = "k (Standard Deviation Multiplier)",
y = "Total Return (%)",
color = "m"
) +
theme_minimal(base_size = 13) +
theme(legend.position = "right")
bitcoin %>%
group_by(Window, m) %>%
summarize(mean_return = mean(`Total Return`, na.rm = TRUE), .groups = "drop") %>%
ggplot(aes(x = Window, y = mean_return, color = factor(m), group = m)) +
geom_line(linewidth = 1.2) +
geom_point(size = 1.5, alpha = 0.8) +
scale_color_manual(values = color_palette) +
labs(
title = "Average Total Return vs Window",
subtitle = "Grouped by strategy aggressiveness (m), aggregated over all k",
x = "Window (Moving Average Window)",
y = "Average Total Return (%)",
color = "m"
) +
theme_minimal(base_size = 13) +
theme(legend.position = "right")
library(tidyverse)
library(ggplot2)
library(plotly)
library(dplyr)
bitcoin <- read_csv("bitcoin.csv")
ethereum <- read_csv("ethereum.csv")
dogecoin <- read_csv("dogecoin.csv")
litecoin <- read_csv("litecoin.csv")
ripple <- read_csv("ripple.csv")
#Regular Main Effects Models
b1 <- lm(`Total Return` ~ Window + k + m, data = bitcoin)
summary(b1)
e1 <- lm(`Total Return` ~ Window + k + m, data = ethereum)
summary(e1)
d1 <- lm(`Total Return` ~ Window + k + m, data = dogecoin)
summary(d1)
l1 <- lm(`Total Return` ~ Window + k + m, data = litecoin)
summary(l1)
r1 <- lm(`Total Return` ~ Window + k + m, data = ripple)
summary(r1)
#Interaction Models
b2 <- lm(`Total Return` ~ Window * k * m, data = bitcoin)
summary(b2)
e2 <- lm(`Total Return` ~ Window * k * m, data = ethereum)
summary(e2)
d2 <- lm(`Total Return` ~ Window * k * m, data = dogecoin)
summary(d2)
l2 <- lm(`Total Return` ~ Window * k * m, data = litecoin)
summary(l2)
r2 <- lm(`Total Return` ~ Window * k * m, data = ripple)
summary(r2)
#Polynomial Models
b3 <- lm(`Total Return` ~ poly(Window, 2) + poly(k, 2) + poly(m, 2), data = bitcoin)
summary(b3)
e3 <- lm(`Total Return` ~ poly(Window, 2) + poly(k, 2) + poly(m, 2), data = ethereum)
summary(e3)
d3 <- lm(`Total Return` ~ poly(Window, 2) + poly(k, 2) + poly(m, 2), data = dogecoin)
summary(d3)
l3 <- lm(`Total Return` ~ poly(Window, 2) + poly(k, 2) + poly(m, 2), data = litecoin)
summary(l3)
r3 <- lm(`Total Return` ~ poly(Window, 2) + poly(k, 2) + poly(m, 2), data = ripple)
summary(r3)
#Best Performers
bitcoin$top10 <- bitcoin$`Total Return` > quantile(bitcoin$`Total Return`, 0.9)
bbest <- glm(top10 ~ Window + k + m, data = bitcoin, family = "binomial")
summary(bbest)
ethereum$top10 <- ethereum$`Total Return` > quantile(ethereum$`Total Return`, 0.9)
ebest <- glm(top10 ~ Window + k + m, data = ethereum, family = "binomial")
summary(ebest)
dogecoin$top10 <- dogecoin$`Total Return` > quantile(dogecoin$`Total Return`, 0.9)
dbest <- glm(top10 ~ Window + k + m, data = dogecoin, family = "binomial")
summary(dbest)
litecoin$top10 <- litecoin$`Total Return` > quantile(litecoin$`Total Return`, 0.9)
lbest <- glm(top10 ~ Window + k + m, data = litecoin, family = "binomial")
summary(lbest)
ripple$top10 <- ripple$`Total Return` > quantile(ripple$`Total Return`, 0.9)
rbest <- glm(top10 ~ Window + k + m, data = ripple, family = "binomial")
summary(rbest)
#Plots
color_palette <- c(
"#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
"#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf", "#e6afaa"
)
#Returns Based on k
bitcoin %>%
filter(Window == 20) %>%
ggplot(aes(x = k, y = `Total Return`, color = factor(m), group = m)) +
geom_line(linewidth = 1.2) +
geom_point(size = 1.5, alpha = 0.8) +
scale_color_manual(values = color_palette) +
labs(
title = "Total Return vs k (Window = 20)",
subtitle = "Each line represents a different level of strategy aggressiveness (m)",
x = "k (Standard Deviation Multiplier)",
y = "Total Return (%)",
color = "m"
) +
theme_minimal(base_size = 13) +
theme(legend.position = "right")
ethereum %>%
filter(Window == 20) %>%
ggplot(aes(x = k, y = `Total Return`, color = factor(m), group = m)) +
geom_line(linewidth = 1.2) +
geom_point(size = 1.5, alpha = 0.8) +
scale_color_manual(values = color_palette) +
labs(
title = "Total Return vs k (Window = 20)",
subtitle = "Each line represents a different level of strategy aggressiveness (m)",
x = "k (Standard Deviation Multiplier)",
y = "Total Return (%)",
color = "m"
) +
theme_minimal(base_size = 13) +
theme(legend.position = "right")
dogecoin %>%
filter(Window == 20) %>%
ggplot(aes(x = k, y = `Total Return`, color = factor(m), group = m)) +
geom_line(linewidth = 1.2) +
geom_point(size = 1.5, alpha = 0.8) +
scale_color_manual(values = color_palette) +
labs(
title = "Total Return vs k (Window = 20)",
subtitle = "Each line represents a different level of strategy aggressiveness (m)",
x = "k (Standard Deviation Multiplier)",
y = "Total Return (%)",
color = "m"
) +
theme_minimal(base_size = 13) +
theme(legend.position = "right")
litecoin %>%
filter(Window == 20) %>%
ggplot(aes(x = k, y = `Total Return`, color = factor(m), group = m)) +
geom_line(linewidth = 1.2) +
geom_point(size = 1.5, alpha = 0.8) +
scale_color_manual(values = color_palette) +
labs(
title = "Total Return vs k (Window = 20)",
subtitle = "Each line represents a different level of strategy aggressiveness (m)",
x = "k (Standard Deviation Multiplier)",
y = "Total Return (%)",
color = "m"
) +
theme_minimal(base_size = 13) +
theme(legend.position = "right")
ripple %>%
filter(Window == 20) %>%
ggplot(aes(x = k, y = `Total Return`, color = factor(m), group = m)) +
geom_line(linewidth = 1.2) +
geom_point(size = 1.5, alpha = 0.8) +
scale_color_manual(values = color_palette) +
labs(
title = "Total Return vs k (Window = 20)",
subtitle = "Each line represents a different level of strategy aggressiveness (m)",
x = "k (Standard Deviation Multiplier)",
y = "Total Return (%)",
color = "m"
) +
theme_minimal(base_size = 13) +
theme(legend.position = "right")
#Returns Based on Window
bitcoin %>%
group_by(Window, m) %>%
summarize(mean_return = mean(`Total Return`, na.rm = TRUE), .groups = "drop") %>%
ggplot(aes(x = Window, y = mean_return, color = factor(m), group = m)) +
geom_line(linewidth = 1.2) +
geom_point(size = 1.5, alpha = 0.8) +
scale_color_manual(values = color_palette) +
labs(
title = "Average Total Return vs Window",
subtitle = "Grouped by strategy aggressiveness (m), aggregated over all k",
x = "Window (Moving Average Window)",
y = "Average Total Return (%)",
color = "m"
) +
theme_minimal(base_size = 13) +
theme(legend.position = "right")
ethereum %>%
group_by(Window, m) %>%
summarize(mean_return = mean(`Total Return`, na.rm = TRUE), .groups = "drop") %>%
ggplot(aes(x = Window, y = mean_return, color = factor(m), group = m)) +
geom_line(linewidth = 1.2) +
geom_point(size = 1.5, alpha = 0.8) +
scale_color_manual(values = color_palette) +
labs(
title = "Average Total Return vs Window",
subtitle = "Grouped by strategy aggressiveness (m), aggregated over all k",
x = "Window (Moving Average Window)",
y = "Average Total Return (%)",
color = "m"
) +
theme_minimal(base_size = 13) +
theme(legend.position = "right")
dogecoin %>%
group_by(Window, m) %>%
summarize(mean_return = mean(`Total Return`, na.rm = TRUE), .groups = "drop") %>%
ggplot(aes(x = Window, y = mean_return, color = factor(m), group = m)) +
geom_line(linewidth = 1.2) +
geom_point(size = 1.5, alpha = 0.8) +
scale_color_manual(values = color_palette) +
labs(
title = "Average Total Return vs Window",
subtitle = "Grouped by strategy aggressiveness (m), aggregated over all k",
x = "Window (Moving Average Window)",
y = "Average Total Return (%)",
color = "m"
) +
theme_minimal(base_size = 13) +
theme(legend.position = "right")
litecoin %>%
group_by(Window, m) %>%
summarize(mean_return = mean(`Total Return`, na.rm = TRUE), .groups = "drop") %>%
ggplot(aes(x = Window, y = mean_return, color = factor(m), group = m)) +
geom_line(linewidth = 1.2) +
geom_point(size = 1.5, alpha = 0.8) +
scale_color_manual(values = color_palette) +
labs(
title = "Average Total Return vs Window",
subtitle = "Grouped by strategy aggressiveness (m), aggregated over all k",
x = "Window (Moving Average Window)",
y = "Average Total Return (%)",
color = "m"
) +
theme_minimal(base_size = 13) +
theme(legend.position = "right")
ripple %>%
group_by(Window, m) %>%
summarize(mean_return = mean(`Total Return`, na.rm = TRUE), .groups = "drop") %>%
ggplot(aes(x = Window, y = mean_return, color = factor(m), group = m)) +
geom_line(linewidth = 1.2) +
geom_point(size = 1.5, alpha = 0.8) +
scale_color_manual(values = color_palette) +
labs(
title = "Average Total Return vs Window",
subtitle = "Grouped by strategy aggressiveness (m), aggregated over all k",
x = "Window (Moving Average Window)",
y = "Average Total Return (%)",
color = "m"
) +
theme_minimal(base_size = 13) +
theme(legend.position = "right")
bitcoin %>%
group_by(Window, m) %>%
summarize(mean_return = mean(`Total Return`, na.rm = TRUE), .groups = "drop") %>%
ggplot(aes(x = Window, y = mean_return, color = factor(m), group = m)) +
geom_line(linewidth = 1.2) +
geom_point(size = 1.5, alpha = 0.8) +
scale_color_manual(values = color_palette) +
labs(
title = "Average Total Return vs Window",
subtitle = "Grouped by strategy aggressiveness (m), aggregated over all k",
x = "Window (Moving Average Window)",
y = "Average Total Return (%)",
color = "m"
) +
theme_minimal(base_size = 13) +
theme(legend.position = "right")
bitcoin %>%
filter(Window == 20) %>%
ggplot(aes(x = k, y = `Total Return`, color = factor(m), group = m)) +
geom_line(linewidth = 1.2) +
geom_point(size = 1.5, alpha = 0.8) +
scale_color_manual(values = color_palette) +
labs(
title = "Total Return vs k (Window = 20)",
subtitle = "Each line represents a different level of strategy aggressiveness (m)",
x = "k (Standard Deviation Multiplier)",
y = "Total Return (%)",
color = "m"
) +
theme_minimal(base_size = 13) +
theme(legend.position = "right")
#Best Performers
bitcoin$top10 <- bitcoin$`Total Return` > quantile(bitcoin$`Total Return`, 0.9)
bbest <- glm(top10 ~ Window + k + m, data = bitcoin, family = "binomial")
summary(bbest)
b3 <- lm(`Total Return` ~ poly(Window, 2) + poly(k, 2) + poly(m, 2), data = bitcoin)
summary(b3)
b2 <- lm(`Total Return` ~ Window * k * m, data = bitcoin)
summary(b2)
b1 <- lm(`Total Return` ~ Window + k + m, data = bitcoin)
summary(b1)
high_performers <- bitcoin %>%
filter(`Total Return` >= 30)
avg_params <- high_performers %>%
summarise(
avg_window = mean(Window),
avg_k = mean(k),
avg_m = mean(m)
)
window <- round(avg_params$avg_window)
k <- avg_params$avg_k
m <- avg_params$avg_m
# Run cmr and cback using these values
window
avg_k
bitcoin %>%
filter(m == 1, k == 1.75, Window == 165) %>%
pull(`Total Return`)
result <- bitcoin %>%
filter(m == 1, k == 1.75, Window == 165)
print(result)
result <- bitcoin %>%
filter(m == 1, k == 1.75, Window == 165)
print(result)
result
result['Total Return']
print(result['Total Return'])
result <- bitcoin %>%
filter(m == 1, k == 1.75, Window == 165)
View(result)
result <- bitcoin %>%
filter(m == 1, k == 1.7, Window == 165)
print(result)
result <- bitcoin %>%
filter(m == 1, k == 1.8, Window == 165)
print(result)
result <- bitcoin %>%
filter(m == 0.9, k == 1.8, Window == 165)
print(result)
result <- bitcoin %>%
filter(m == 0.9, k == 1.7, Window == 165)
print(result)
dogecoin %>%
group_by(Window, m) %>%
summarize(mean_return = mean(`Total Return`, na.rm = TRUE), .groups = "drop") %>%
ggplot(aes(x = Window, y = mean_return, color = factor(m), group = m)) +
geom_line(linewidth = 1.2) +
geom_point(size = 1.5, alpha = 0.8) +
scale_color_manual(values = color_palette) +
labs(
title = "Average Total Return vs Window",
subtitle = "Grouped by strategy aggressiveness (m), aggregated over all k",
x = "Window (Moving Average Window)",
y = "Average Total Return (%)",
color = "m"
) +
theme_minimal(base_size = 13) +
theme(legend.position = "right")
ethereum %>%
group_by(Window, m) %>%
summarize(mean_return = mean(`Total Return`, na.rm = TRUE), .groups = "drop") %>%
ggplot(aes(x = Window, y = mean_return, color = factor(m), group = m)) +
geom_line(linewidth = 1.2) +
geom_point(size = 1.5, alpha = 0.8) +
scale_color_manual(values = color_palette) +
labs(
title = "Average Total Return vs Window",
subtitle = "Grouped by strategy aggressiveness (m), aggregated over all k",
x = "Window (Moving Average Window)",
y = "Average Total Return (%)",
color = "m"
) +
theme_minimal(base_size = 13) +
theme(legend.position = "right")
bitcoin %>%
group_by(Window, m) %>%
summarize(mean_return = mean(`Total Return`, na.rm = TRUE), .groups = "drop") %>%
ggplot(aes(x = Window, y = mean_return, color = factor(m), group = m)) +
geom_line(linewidth = 1.2) +
geom_point(size = 1.5, alpha = 0.8) +
scale_color_manual(values = color_palette) +
labs(
title = "Average Total Return vs Window",
subtitle = "Grouped by strategy aggressiveness (m), aggregated over all k",
x = "Window (Moving Average Window)",
y = "Average Total Return (%)",
color = "m"
) +
theme_minimal(base_size = 13) +
theme(legend.position = "right")
dogecoin %>%
filter(Window == 20) %>%
ggplot(aes(x = k, y = `Total Return`, color = factor(m), group = m)) +
geom_line(linewidth = 1.2) +
geom_point(size = 1.5, alpha = 0.8) +
scale_color_manual(values = color_palette) +
labs(
title = "Total Return vs k (Window = 20)",
subtitle = "Each line represents a different level of strategy aggressiveness (m)",
x = "k (Standard Deviation Multiplier)",
y = "Total Return (%)",
color = "m"
) +
theme_minimal(base_size = 13) +
theme(legend.position = "right")
ethereum %>%
filter(Window == 20) %>%
ggplot(aes(x = k, y = `Total Return`, color = factor(m), group = m)) +
geom_line(linewidth = 1.2) +
geom_point(size = 1.5, alpha = 0.8) +
scale_color_manual(values = color_palette) +
labs(
title = "Total Return vs k (Window = 20)",
subtitle = "Each line represents a different level of strategy aggressiveness (m)",
x = "k (Standard Deviation Multiplier)",
y = "Total Return (%)",
color = "m"
) +
theme_minimal(base_size = 13) +
theme(legend.position = "right")
bitcoin %>%
filter(Window == 20) %>%
ggplot(aes(x = k, y = `Total Return`, color = factor(m), group = m)) +
geom_line(linewidth = 1.2) +
geom_point(size = 1.5, alpha = 0.8) +
scale_color_manual(values = color_palette) +
labs(
title = "Total Return vs k (Window = 20)",
subtitle = "Each line represents a different level of strategy aggressiveness (m)",
x = "k (Standard Deviation Multiplier)",
y = "Total Return (%)",
color = "m"
) +
theme_minimal(base_size = 13) +
theme(legend.position = "right")
