# Global Trends in Religious Composition: Secularization and Socio-Economic Development

## Overview

This project investigates the relationship between religious secularization and socio-economic development across countries using data from the World Religion Project (WRP 2020) and the World Bank World Development Indicators (WDI).

The objective is to identify which demographic and economic factors best predict secularization, measured as the percentage of a country's population reporting no religious affiliation.

The analysis combines both **unsupervised learning** and **supervised learning** techniques, including:

* Exploratory Data Analysis (EDA)
* Correlation Analysis
* Principal Component Analysis (PCA)
* K-Means Clustering
* Hierarchical Clustering
* Ordinary Least Squares (OLS) Regression
* Ridge Regression
* Lasso Regression
* Random Forest Regression

---

## Research Question

Which socio-economic and demographic factors best explain variations in secularization across countries?

Specifically, the project evaluates the influence of:

* Fertility Rate
* Urbanization Rate
* Life Expectancy
* GDP per Capita

on the share of the population with no religious affiliation.

---

## Dataset

### World Religion Project (WRP 2020)

Provides country-level religious composition data including:

* Unaffiliated population (%)
* Christian population (%)
* Muslim population (%)
* Hindu population (%)
* Other religious groups

### World Bank World Development Indicators (WDI)

Socio-economic indicators:

* Fertility Rate
* Urbanization Rate
* Life Expectancy
* GDP per Capita

### Coverage

* 165 countries
* 1960–2010 period
* 954 country-year observations after merging datasets

---

## Data Access

The datasets are not included in this repository due to size considerations.

Download all required files from:

**OneDrive:** [WRP_2020](https://unimi2013-my.sharepoint.com/:f:/g/personal/srishti_kansra_studenti_unimi_it/IgAduAMs46ZyQ5QymggEnAkEASxOcIj5A6tgiKz_TIqRsz8?e=38lrDP)

After downloading, create the following folder structure:

```text
data/
├── WRP_national.csv
├── COW-country-codes.csv
└── WDIData.csv
```

Update the paths in `WRP_2020_analysis3.R`:

```r
wrp_path <- "data/WRP_national.csv"
cow_path <- "data/COW-country-codes.csv"
wdi_path <- "data/WDIData.csv"
```

---


## Project Structure

```text
.
├── WRP_2020_analysis3.R        # Complete analysis script
├── WRP_Report_FINAL.pdf        # Final report
├── figures/                   # Generated visualizations 
├── data/                      # Raw datasets link (data size >100MB)
└── README.md
```

---

## Methods

### Unsupervised Learning

#### Correlation Analysis

Examines relationships between secularization and development indicators.

#### Principal Component Analysis (PCA)

Reduces dimensionality and identifies major patterns in the data.

#### K-Means Clustering

Groups countries into development–religiosity profiles.

#### Hierarchical Clustering

Validates cluster structure obtained from K-Means.

---

### Supervised Learning

#### Linear Regression

* Simple OLS
* Multivariate OLS
* Interaction Models

#### Regularized Regression

* Ridge Regression
* Lasso Regression

#### Random Forest Regression

Captures non-linear relationships and evaluates predictor importance.

---

## Main Findings

### 1. Fertility Rate is the Strongest Predictor

Across all methods, fertility consistently emerges as the most important predictor of secularization.

Countries with lower fertility rates tend to exhibit substantially higher levels of secularization.

### 2. GDP per Capita is a Weak Predictor

While wealth is often assumed to drive secularization, GDP per capita shows a relatively weak and inconsistent relationship once fertility is accounted for.

### 3. Random Forest Outperforms Linear Models

Random Forest captures important non-linear relationships and achieves substantially better predictive performance than traditional OLS regression.

### 4. The Relationship is Non-Linear

The effect of fertility on secularization is strongest at high fertility levels and gradually levels off as fertility declines.

### 5. Richest Countries Are Not Necessarily the Most Secular

Cluster analysis reveals a group of middle-income countries with higher average secularization than some of the wealthiest countries.

This challenges the simplistic assumption that economic development automatically produces secularization.

---

## Software and Packages

Required R packages:

```r
install.packages(c(
  "tidyverse",
  "readr",
  "corrplot",
  "ggplot2",
  "scales",
  "randomForest",
  "pdp",
  "factoextra",
  "cluster",
  "caret",
  "knitr",
  "glmnet"
))
```

---

## Running the Project

1. Download the WRP 2020 dataset.
2. Download the World Bank WDI dataset.
3. Update file paths in `WRP_2020_analysis3.R`.
4. Install required packages.
5. Run the script in RStudio.

```r
source("WRP_2020_analysis3.R")
```

The script automatically:

* Loads and cleans data
* Merges WRP and WDI datasets
* Generates visualizations
* Performs clustering analyses
* Fits regression models
* Evaluates Random Forest performance
* Exports project figures

---

## Author

**Srishti Kansra**

MSc Data Science for Economics
University of Milan

Course: Statistical Learning
Instructor: Prof. Silvia Salini

---

## License

This repository is intended for academic and educational purposes.
