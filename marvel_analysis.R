# Marvel Movie Performance Analysis
# STAT 355 Final Project - Hanush Gupta
#
# To run: open this file in the project root (the folder containing the
# `data/` directory) and source it, or run with `Rscript marvel_analysis.R`.
# Data is read from data/ using relative paths, so no manual file selection
# is needed.

library(tidyverse)

marvel_raw = read.csv("data/marvel.csv", fileEncoding = "latin1",
                      stringsAsFactors = FALSE)

reviews_raw = read.csv("data/marvel_reviews.csv", fileEncoding = "latin1",
                       stringsAsFactors = FALSE)
str(marvel_raw)
str(reviews_raw)
# 2. Clean & Merge -----------------------------
colnames(marvel_raw) = c(
  "Title",
  "Distributor",
  "ReleaseDateUS",
  "BudgetMillions_raw",
  "OpeningWeekendNA_raw",
  "NorthAmerica_raw",
  "OtherTerritories_raw",
  "Worldwide_raw"
)
colnames(reviews_raw) = c(
  "Film",
  "RottenTomatoes_raw",
  "Metacritic_raw",
  "CinemaScore_raw"
)

marvel = marvel_raw %>%
  filter(!(Title %in% c("Total", "Average")))

clean_money = function(x) {
  x = gsub(",", "", x)
  x = gsub("\\$", "", x)
  x[x == ""] = NA
  as.numeric(x)
}
clean_title = function(x) {
  x = trimws(x)
  x = gsub("\\s*\\(.*\\)", "", x)
  x = gsub("&amp;", "&", x)
  x
}
clean_rt = function(x) {
  val = sub("^(\\d+).*", "\\1", x)
  val[val == ""] = NA
  as.numeric(val)
}
clean_mc = function(x) {
  val = sub("^(\\d+).*", "\\1", x)
  val[val == ""] = NA
  as.numeric(val)
}
clean_cs = function(x) {
  val = sub("^([A-F][+-]?).*", "\\1", x)
  val[val == ""] = NA
  val
}
reviews = reviews_raw %>%
  mutate(
    Title_clean = clean_title(Film),
    RottenTomatoes = clean_rt(RottenTomatoes_raw),
    Metacritic = clean_mc(Metacritic_raw),
    CinemaScore = clean_cs(CinemaScore_raw)
  )
marvel = marvel %>%
  mutate(Title_clean = clean_title(Title))
marvel_full = marvel %>%
  left_join(
    reviews %>% select(Title_clean, RottenTomatoes, Metacritic, CinemaScore),
    by = "Title_clean"
  )
head(marvel_full)
summary(marvel_full)
marvel_full = marvel_full %>%
  mutate(
    BudgetMillions = clean_money(BudgetMillions_raw),
    OpeningWeekendNA = clean_money(OpeningWeekendNA_raw),
    NorthAmericaGross = clean_money(NorthAmerica_raw),
    OtherTerritoriesGross = clean_money(OtherTerritories_raw),
    WorldwideGross = clean_money(Worldwide_raw),
    WorldwideMillions = WorldwideGross / 1e6,
    Year = as.numeric(sub(".* (\\d{4})$", "\\1", ReleaseDateUS)),
    Era = if_else(Year < 2008, "Pre_2008", "2008_and_later")
  )
summary(marvel_full$BudgetMillions)
summary(marvel_full$WorldwideMillions)
#Plots
# Summary stats for selected numeric variables
summary(marvel_full %>%
          select(BudgetMillions, OpeningWeekendNA,
                 NorthAmericaGross, OtherTerritoriesGross,
                 WorldwideMillions, RottenTomatoes, Metacritic, Year))

# Histogram: Worldwide Box Office
ggplot(marvel_full, aes(x = WorldwideMillions)) +
  geom_histogram(binwidth = 100, color = "black") +
  labs(title = "Distribution of Worldwide Box Office (Millions USD)",
       x = "Worldwide Gross (millions USD)",
       y = "Number of Movies")

# Histogram: Budgets
ggplot(marvel_full, aes(x = BudgetMillions)) +
  geom_histogram(binwidth = 20, color = "black") +
  labs(title = "Distribution of Movie Budgets (Millions USD)",
       x = "Budget (millions USD)",
       y = "Number of Movies")
# Scatterplot: Budget vs Worldwide Gross
ggplot(marvel_full,
       aes(x = BudgetMillions, y = WorldwideMillions)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Budget vs Worldwide Gross",
       x = "Budget (millions USD)",
       y = "Worldwide Gross (millions USD)")
# Boxplot: Pre-2008 vs 2008+
ggplot(marvel_full,
       aes(x = Era, y = WorldwideMillions)) +
  geom_boxplot() +
  labs(title = "Worldwide Gross by Era",
       x = "Era",
       y = "Worldwide Gross (millions USD)")

# ================== PART 2: INFERENCE =====================
############################################################

# ---------------------------------------------------------
# RQ1 (Two-sample t-test):
# "Do Marvel movies released in or after 2008 earn
#  significantly higher worldwide box office revenue than
#  Marvel movies released before 2008?"
# ---------------------------------------------------------
rq1_data = marvel_full %>%
  filter(!is.na(WorldwideMillions),
         !is.na(Era)) %>%
  mutate(
    LogWorldwide = log10(WorldwideMillions)  # log to reduce skew
  )
# Boxplot on original scale (for report)
ggplot(rq1_data,
       aes(x = Era, y = WorldwideMillions)) +
  geom_boxplot() +
  labs(title = "Worldwide Gross by Era",
       x = "Era",
       y = "Worldwide Gross (millions USD)")
# Histograms of log revenues by group (assumption check)
par(mfrow = c(1, 2))
hist(rq1_data$LogWorldwide[rq1_data$Era == "Pre_2008"],
     main = "Log Revenue - Pre 2008", xlab = "log10(WorldwideMillions)")
hist(rq1_data$LogWorldwide[rq1_data$Era == "2008_and_later"],
     main = "Log Revenue - 2008+", xlab = "log10(WorldwideMillions)")
par(mfrow = c(1, 1))
# Two-sample t-test on log revenues
t_test_rq1 = t.test(LogWorldwide ~ Era,
                    data = rq1_data,
                    var.equal = FALSE)   # Welch t-test
t_test_rq1
# Group means on original and log scales (for interpretation)
rq1_means = rq1_data %>%
  group_by(Era) %>%
  summarise(
    mean_worldwide = mean(WorldwideMillions, na.rm = TRUE),
    mean_log = mean(LogWorldwide, na.rm = TRUE),
    n = n()
  )
rq1_means
# ---------------------------------------------------------
# RQ2 (One-way ANOVA):
# "Is the average worldwide box office revenue different
#  across major distributor groups (Disney, Fox, Sony, Other)
#  for Marvel movies?"
# ---------------------------------------------------------

# Create distributor groups
marvel_full = marvel_full %>%
  mutate(
    DistributorGroup = case_when(
      Distributor == "Walt Disney Studios Motion Pictures" ~ "Disney",
      Distributor %in% c("20th Century Fox", "20th Century Studios") ~ "Fox",
      Distributor == "Sony Pictures" ~ "Sony",
      TRUE ~ "Other"
    )
  )

rq2_data = marvel_full %>%
  filter(!is.na(WorldwideMillions),
         !is.na(DistributorGroup)) %>%
  mutate(
    LogWorldwide = log10(WorldwideMillions)
  )

# Boxplot by distributor (for report)
ggplot(rq2_data,
       aes(x = DistributorGroup, y = WorldwideMillions)) +
  geom_boxplot() +
  labs(title = "Worldwide Gross by Distributor Group",
       x = "Distributor Group",
       y = "Worldwide Gross (millions USD)")

# ANOVA on log revenues
anova_rq2 = aov(LogWorldwide ~ DistributorGroup, data = rq2_data)
summary(anova_rq2)

# Diagnostic plots (assumption checks)
par(mfrow = c(2, 2))
plot(anova_rq2)
par(mfrow = c(1, 1))

# Post-hoc pairwise comparisons if ANOVA is significant
tukey_rq2 = TukeyHSD(anova_rq2)
tukey_rq2
# ---------------------------------------------------------
# RQ3 (Multiple Linear Regression):
# "How do budget, Rotten Tomatoes score, and release year
#  together predict a Marvel movie's worldwide box office?"
# ---------------------------------------------------------
rq3_data = marvel_full %>%
  filter(!is.na(WorldwideMillions),
         !is.na(BudgetMillions),
         !is.na(RottenTomatoes),
         !is.na(Year)) %>%
  mutate(
    LogWorldwide = log10(WorldwideMillions),
    Budget_log = log10(BudgetMillions)
  )
# Scatterplots for relationships (optional, nice for report)
ggplot(rq3_data,
       aes(x = BudgetMillions, y = WorldwideMillions)) +
  geom_point() +
  labs(title = "Worldwide Gross vs Budget",
       x = "Budget (millions USD)",
       y = "Worldwide Gross (millions USD)")
ggplot(rq3_data,
       aes(x = RottenTomatoes, y = WorldwideMillions)) +
  geom_point() +
  labs(title = "Worldwide Gross vs Rotten Tomatoes Score",
       x = "Rotten Tomatoes (%)",
       y = "Worldwide Gross (millions USD)")

# Fit regression model
model_rq3 = lm(LogWorldwide ~ Budget_log + RottenTomatoes + Year,
               data = rq3_data)
summary(model_rq3)
# Diagnostic plots (assumptions: linearity, normal residuals, etc.)
par(mfrow = c(2, 2))
plot(model_rq3)
par(mfrow = c(1, 1))

# Example prediction for a hypothetical movie:
# Budget = $150M, Rotten Tomatoes = 80%, Year = 2025
new_movie = data.frame(
  Budget_log = log10(150),
  RottenTomatoes = 80,
  Year = 2025
)
pred_log = predict(model_rq3, newdata = new_movie, interval = "prediction")
pred_log
# Back-transform predicted log10 revenue to millions of USD
pred_worldwide_millions = 10^pred_log
pred_worldwide_millions
