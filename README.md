# Marvel Movie Performance Analysis

A statistical analysis of box office performance across 67 Marvel films, examining how budget, critical reception, release era, and distributor relate to worldwide revenue. Built in R for STAT 355 (Applied Statistics).

## Questions

1. Do Marvel movies released in or after 2008 (the MCU era) earn significantly more than pre-2008 films?
2. Does average worldwide revenue differ across distributor groups (Disney, Fox, Sony, Other)?
3. How do budget, Rotten Tomatoes score, and release year jointly predict worldwide gross?

## Methods

- **Data cleaning:** merged a financials table and a reviews table on a cleaned title key; stripped currency symbols, review-count noise, and footnote markers with regex; parsed release years and derived an era flag.
- **RQ1:** Welch two-sample t-test on log10 worldwide revenue, with per-group histograms checking the normality assumption first.
- **RQ2:** one-way ANOVA on log revenue across distributor groups, followed by Tukey HSD post-hoc comparisons; residual diagnostics for the ANOVA assumptions.
- **RQ3:** multiple linear regression of log revenue on log budget, Rotten Tomatoes score, and year, with the standard four diagnostic plots.

Revenue and budget are log-transformed throughout to address strong right skew.

## Key findings

- 2008+ films earn significantly more than pre-2008 films (Welch t = 3.00, p = 0.0045). Interpreted on the log scale, this is a difference in geometric mean revenue.
- Mean log revenue differs across distributors (F(3,63) = 15.44, p < 1.3e-7). Tukey: Disney outperforms Fox and Other; Sony outperforms Other; Sony and Fox do not differ.
- Budget (log) and Rotten Tomatoes score are both strong predictors; release year is not significant after controlling for the other two. The model explains about 79% of the variation in log revenue (R^2 = 0.793).

## A note on scope

This dataset covers essentially the full set of theatrically released Marvel films, not a random sample. The inferential tests here are best read as describing this set of films and quantifying effect sizes within it, rather than generalizing to all films. A stricter framing would treat the films as a sample from the broader population of comic-book/superhero movies; the tests are reported in that spirit.

## Repository

```
marvel-movie-analysis/
├── marvel_analysis.R              # full analysis, reads from data/
├── data/
│   ├── marvel.csv                 # financials (title, distributor, budget, grosses)
│   └── marvel_reviews.csv         # Rotten Tomatoes, Metacritic, CinemaScore
├── STAT_FinalProject_Hanush_.pdf  # written report with figures
└── README.md
```

## Running it

Requires R with `tidyverse`.

```r
install.packages("tidyverse")   # if not already installed
```

From the project root (the folder containing `data/`):

```bash
Rscript marvel_analysis.R
```

Or open `marvel_analysis.R` in RStudio and source it. Paths are relative to the project root, so no manual file selection is needed.

## Data

Source: [Marvel Movie Dataset on Kaggle](https://www.kaggle.com/datasets/minisam/marvel-movie-dataset).

## Tools

R, tidyverse (dplyr, ggplot2), base R stats (`t.test`, `aov`, `TukeyHSD`, `lm`).
