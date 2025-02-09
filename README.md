# Association Between Smoking and Mental Health: Empirical Evidence from IPUMS NHIS Data of US

Authors
Group Q:

Haoxuan Li
Jing Liu
Lingjia Kang
Qiannuo Chen
Sitong Yan
Xuyuan Jin

Date
2025.02.01

## Project Overview
This project investigates the association between smoking (CIGSDAY) and mental health (K6) using IPUMS NHIS survey data from the United States (2015-2018). The analysis employs survey-weighted regression models to account for complex sampling designs and conducts robustness checks to validate the findings.

## Description of data
In this section you should describe what data you use for your project with enough detail that anyone could download it and run your analysis.
- **IPUMS series**: IPUMS NATIONAL HEALTH INTERVIEW SERIES
- **Countries**: United States of America
- **Years**: 2015, 2016, 2017, 2018
- **How to access the data**: This analysis utilized the IPUMS NHIS data from United States (URL:https://nhis.ipums.org/nhis/), selecting individual survey information from 2015 to 2018 to ensure data availability and integrity, the variables were selected from the IPUMS include:"AHOPELESS", "ANERVOUS", "ANERVOUS", "ARESTLESS", "ASAD", "AWORTHLESS", "AEFFORT", "CIGSDAY", "AGE", "SEX", "INFAM07ON","HEALTH","NCHILD", "COHABEVMAR", "EDUCREC1","EMPSTATIMP1","INCFAM97ON2", "CIGDAYMO", "HRSLEEP","SLEEPFALL", "SLEEPSTAY". Variables include "AHOPELESS", "ANERVOUS", "ANERVOUS", "ARESTLESS", "ASAD", "AWORTHLESS", "AEFFORT", "CIGSDAY", "AGE", "SEX", "INFAM07ON"and "HEALTH","NCHILD" were applied for main regression model, while "SLEEPFALL" and "SLEEPSTAY" were used for robustness check. Due to time constraint of the analysis, all other variables were not applied but considered to be used for robustness check if time permit. 

##Dataset
The dataset used in this analysis is from the IPUMS NHIS (National Health Interview Survey). The data can be accessed at:
IPUMS NHIS Data

Key variables used in the analysis include:
Mental health indicators: AHOPELESS, ANERVOUS, ARESTLESS, ASAD, AWORTHLESS, AEFFORT
Smoking behavior: CIGSDAY
Demographics: AGE, SEX, NCHILD, INCFAM07ON, HEALTH
Sleep-related variables (for robustness checks): SLEEPFALL, SLEEPSTAY

  

## Description of how to run the code (Description of Replication Process)
This project examines the association between smoking (CIGSDAY) and mental health (K6) using IPUMS NHIS survey data from 2015 to 2018. The analysis follows a structured process, including data cleaning, feature engineering, statistical modeling, and robustness checks. Below is a step-by-step guide on how to replicate the analysis.

### 1. Setting Up the Environment
To replicate this analysis, you need R and RStudio installed on your computer. Additionally, the required R packages must be installed before running the .Rmd file.

### 2. Downloading and Preparing the Data
The dataset used in this project comes from IPUMS NHIS and must be downloaded manually from IPUMS NHIS.
Once downloaded, extract the data files (.xml and .dat.gz) and place them in your working directory.
The script loads and processes this data to extract relevant variables and clean missing or invalid values.

### 3. Structure of the Code Files
This project is structured using an R Markdown file (Main.Rmd), which integrates data analysis, visualization, and model estimation.
The key sections of the code include:

(i) Data Preprocessing and Cleaning
Loads the IPUMS NHIS dataset.
Selects relevant variables (e.g., mental health indicators, demographic information, smoking behavior).
Filters invalid values and ensures data quality.
(ii) Feature Engineering
Constructs the K6 score as a composite measure of psychological distress by summing six mental health indicators.
Adjusts survey weights for accurate population-level inference.
(iii) Exploratory Data Analysis (EDA)
Computes correlation between smoking and psychological distress.
Generates scatter plots and other visualizations.
(iv) Weighted Regression Analysis
Uses survey-weighted regression models to account for the NHIS complex survey design.
Includes demographic and socioeconomic covariates.
(v) Robustness Checks
Tests for heteroscedasticity using the Breusch-Pagan and White tests.
Performs subgroup analysis (e.g., gender-specific, age-group specific).
Conducts mediation analysis to explore the role of sleep in the smoking-mental health relationship.
(vi) Model Fit and Diagnostics
Computes weighted R-squared to assess model performance.
Generates residual plots for diagnostic checking.

### 4. Running the Code
Open the Main.Rmd file in RStudio.
Click Knit to generate an HTML report of the analysis.
Alternatively, you can run the script chunk by chunk to inspect the intermediate outputs.

### 5. Notes on Reproducibility
Ensure that the dataset is available in the correct format before running the code.
The script uses survey weights, so results may slightly differ from unweighted models.
If any package is missing, install it using install.packages("package_name").
