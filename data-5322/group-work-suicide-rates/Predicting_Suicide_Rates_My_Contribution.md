# Predicting Gender-Specific Global Suicide Rates — My Contribution

**Academic work** — DATA 5322 Statistical Machine Learning 2, Seattle
University, Spring 2025. Our group final project, with Yeabsera Gente,
Michelle Sathish, David Stanko, and me — and I contributed to most parts of
it, working through the data and the modeling with the team.

This is my write-up of the project — what we set out to answer and how we
went about it. The report and slides were our group's shared documents, so
they're not in this repo, and the project code lives in my teammates'
repositories.

## What the project was

Over 720,000 people die by suicide each year, and the rates look different
for men and women. We wanted to know how mental health conditions and a
country's wealth relate to suicide rates, and whether we could predict
gender-specific suicide rates from those factors.

We used country-level data from the Institute for Health Metrics and
Evaluation, all from 2021: suicide rates for men and women aged 15–49, the
prevalence of anxiety, depression, and bipolar disorder for each gender, and
GDP per capita for each country. Everything was aggregate, country-level
numbers — no individual records.

## What we did

- **Merged and cleaned the data.** We brought together five separate
  datasets — suicide rates, three mental-health prevalence datasets, and GDP —
  matching them on country and gender, then split everything into separate
  male and female datasets.
- **Prepared it for modeling.** We standardized the predictors, split 80/20
  into training and test sets (the same split for both genders), and separated
  the predictors from the suicide-rate response.
- **Ran PCA.** We used principal component analysis to find the directions in
  which the data varies most, and looked at the loadings to see which original
  variables drove each component.
- **Ran decision trees and ensembles.** We fit decision trees (tuned by max
  depth and minimum samples per leaf, picked by cross-validation), then
  bagging, random forests, and boosting, and compared how well they predicted
  the rates.

## What I took from it

Working with a sensitive topic like this, at the country level, taught me a
lot about handling aggregate health data carefully — the numbers describe
real people, so the analysis has to stay honest about what it can and can't
say. Technically, it was my deepest hands-on work with PCA and tree ensembles
in the program: merging messy multi-source data, standardizing properly
before PCA, and tuning trees with cross-validation instead of guessing.

## Tools

R, PCA, decision trees, random forests, boosting, cross-validation.

## What is not here

- The group's report and presentation were our shared documents, so they're
  not here — this write-up covers the project instead.
- The project code lives in my teammates' repositories, not mine.
- No raw data is included. The data was aggregate country-level data from the
  Institute for Health Metrics and Evaluation, and the topic is a sensitive
  one, so this is shared as a written account of the analysis only.
