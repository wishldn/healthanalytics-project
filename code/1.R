packages <- c("dplyr", "ggplot2", "tidyr", "corrplot", "ggcorrplot", "car", "survey", "ipumsr")
library(lmtest)
library(sandwich)
library(lmtest)
package.check <- lapply(packages, function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})


# Load data
ddi <- read_ipums_ddi("nhis_00005.xml")
data <- read_ipums_micro(ddi)

# View first few rows of the dataset
data_clean <- data %>%
  dplyr::select(AGE, SEX, PSU, STRATA, PERWEIGHT, INCFAM07ON, INCFAM97ON2, HEALTH, EDUCREC1, NCHILD, EMPSTATIMP1, CIGDAYMO, CIGSDAY, HRSLEEP, SLEEPFALL, SLEEPSTAY, 
                AHOPELESS, ANERVOUS, ARESTLESS, ASAD, AWORTHLESS, AEFFORT) %>%
  filter(AGE >= 18, CIGSDAY <= 20, INCFAM07ON < 90, SLEEPSTAY < 90, SLEEPFALL < 90,
         AHOPELESS < 6, ANERVOUS < 6, ARESTLESS < 6, ASAD < 6, AWORTHLESS < 6, AEFFORT < 6, HEALTH<6, EMPSTATIMP1!=0, INCFAM07ON<96, INCFAM97ON2<97)


#write.csv(data_clean, file = "~/Desktop/data_clean.csv", row.names = FALSE)
# Generating K6 as new variable and embedded with weight
data_clean <- data_clean %>%
  mutate(K6 = AHOPELESS + ANERVOUS + ARESTLESS + ASAD + AWORTHLESS + AEFFORT, PERWEIGHT = PERWEIGHT/4)

# Correlation analysis
cor_test <- cor.test(data_clean$CIGSDAY, data_clean$K6, use = "complete.obs")
print(cor_test)

# Visualize the relationship between K6 and CIGSDAY
ggplot(data_clean, aes(x = CIGSDAY, y = K6)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Cigarettes Per Day vs. Psychological Distress (K6)",
       x = "Cigarettes per Day", y = "K6 Score") +
  theme_minimal()

# Weighted regression analysis (if data involves sampling weights)
design <- svydesign(ids = ~1, weights = ~PERWEIGHT, data = data_clean)

weighted_model <- svyglm(K6 ~ CIGSDAY + AGE + SEX + HEALTH + NCHILD + INCFAM07ON,
                         design = design)
summary(weighted_model)

# F-test
f_test <- regTermTest(weighted_model, ~ CIGSDAY + AGE + SEX + HEALTH + NCHILD + INCFAM07ON)
print(f_test)

# R-squared & adjusted
y_hat <- predict(weighted_model, type = "response")
y <- data_clean$K6
w <- weights(design)
sst <- sum(w * (y - weighted.mean(y, w))^2)
sse <- sum(w * (y - y_hat)^2)
R2_weighted <- 1 - (sse / sst)
n <- nrow(data_clean) 
p <- length(coef(weighted_model)) - 1  
adjusted_R2_weighted <- 1 - ((1 - R2_weighted) * (n - 1) / (n - p - 1))
print(R2_weighted)
print(adjusted_R2_weighted)

# Heteroscedasticity test,
# 1. Breusch-Pagan Test
bp_test <- bptest(weighted_model) 
print(bp_test)


# 2. White test
# Calculating Huber-White heteroscedasticity and robust standard error
robust_se <- vcovHC(weighted_model, type = "HC0")  

# Calculating the regression result with robust se
robust_model <- coeftest(weighted_model, vcov = robust_se)

#print the regression result of robust se
print(robust_model)

#the robust_mode was over-adjusted，therefore  the Robust Standar Errors: Design-Based Standard Errors was used as the final print result
summary(weighted_model, vartype = c("se", "ci"))

# 3. Residual Plot
# Calculating the residual of svyglm
data_clean$residuals_svyglm <- residuals(weighted_model)
data_clean$fitted_svyglm <- fitted(weighted_model)

# Plotting residual graph
ggplot(data_clean, aes(x = fitted_svyglm, y = residuals_svyglm)) +
  geom_point(alpha = 0.5, color = "blue") + 
  geom_smooth(method = "loess", color = "red", se = FALSE) +  
  labs(title = "Residual Plot for Survey Weighted Generalized Linear Model",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

# Extract the data from male and female separately
data_male <- subset(data_clean, SEX == 1)
data_female <- subset(data_clean, SEX == 2)

# svyglm for sample survey
# Define the samplomg design for male and femmale
design_male <- svydesign(ids = ~1, weights = ~PERWEIGHT, data = data_male)
design_female <- svydesign(ids = ~1, weights = ~PERWEIGHT, data = data_female)

# Weighted regression for male
weighted_model_male <- svyglm(K6 ~ CIGSDAY + AGE + HEALTH + NCHILD + INCFAM07ON, 
                              design = design_male)

# Weighted regression for female
weighted_model_female <- svyglm(K6 ~ CIGSDAY + AGE + HEALTH + NCHILD + INCFAM07ON, 
                                design = design_female)

# VIF check for male
vif(weighted_model_male)

# Calculate the F statistic for the entire model: male
f_test <- regTermTest(weighted_model_male, ~ CIGSDAY + AGE + SEX + HEALTH + NCHILD + INCFAM07ON)
print(f_test)

# R-squared & adjusted male
y_hat_male <- predict(weighted_model_male, type = "response")
y_male <- design_male$variables$K6  
w_male <- weights(design_male)  
sst_male <- sum(w_male * (y_male - weighted.mean(y_male, w_male))^2)
sse_male <- sum(w_male * (y_male - y_hat_male)^2)
R2_male <- 1 - (sse_male / sst_male)
n_male <- nrow(design_male$variables) 
p_male <- length(coef(weighted_model_male)) - 1  
adjusted_R2_male <- 1 - ((1 - R2_male) * (n_male - 1) / (n_male - p_male - 1))
print(R2_male)  
print(adjusted_R2_male)  

# male heteroscedasticity test
# Run BP test
bp_test <- bptest(weighted_model_male) 
print(bp_test)


# VIF check for female
vif(weighted_model_female)

# Calculating the F-Statistic for female
f_test <- regTermTest(weighted_model_female, ~ CIGSDAY + AGE + SEX + HEALTH + NCHILD + INCFAM07ON)
print(f_test)

# R-squared & adjusted female
y_hat_female <- predict(weighted_model_female, type = "response")
y_female <- design_female$variables$K6  
w_female <- weights(design_female)  
sst_female <- sum(w_female * (y_female - weighted.mean(y_female, w_female))^2)
sse_female <- sum(w_female * (y_female - y_hat_female)^2)
R2_female <- 1 - (sse_female / sst_female)
n_female <- nrow(design_female$variables) 
p_female <- length(coef(weighted_model_female)) - 1  
adjusted_R2_female <- 1 - ((1 - R2_female) * (n_female - 1) / (n_female - p_female - 1))
print(R2_female) 
print(adjusted_R2_female) 

#Female heteroscedasticity test
#Run BP test
bp_test <- bptest(weighted_model_female) 
print(bp_test)

# Standard error for calculating male band clustering (PSU)
summary(weighted_model_male, vartype = c("se", "ci"))

# Standard error for calculating female band clustering (PSU)
summary(weighted_model_female, vartype = c("se", "ci"))

# The fit values and residuals of the male model were extracted
residuals_male <- data.frame(
  Fitted_Values = fitted(weighted_model_male),
  Residuals = residuals(weighted_model_male)
)

# The fit values and residuals of the female model were extracted
residuals_female <- data.frame(
  Fitted_Values = fitted(weighted_model_female),
  Residuals = residuals(weighted_model_female)
)

# Plotting the residual graph for male
ggplot(residuals_male, aes(x = Fitted_Values, y = Residuals)) +
  geom_point(color = "blue") +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  ggtitle("Residual Plot for Male (Weighted Regression)") +
  xlab("Fitted Values") + ylab("Residuals")

# Plotting the residual graph for female
ggplot(residuals_female, aes(x = Fitted_Values, y = Residuals)) +
  geom_point(color = "blue") +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  ggtitle("Residual Plot for Female (Weighted Regression)") +
  xlab("Fitted Values") + ylab("Residuals")

#===========================================================================
#===========================================================================

#Robustness Check
# 1️: Only considering the core independent variable
model_1 <- svyglm(K6 ~ CIGSDAY, design = design)
summary(model_1, vartype = c("se", "ci"))
# Calculating the F-Statistic
f_test <- regTermTest(model_1, ~ CIGSDAY)
print(f_test) 
# R-squared & adjusted
y_hat <- predict(model_1, type = "response")
y <- data_clean$K6
w <- weights(design)
sst <- sum(w * (y - weighted.mean(y, w))^2)
sse <- sum(w * (y - y_hat)^2)
R21 <- 1 - (sse / sst)
n <- nrow(data_clean) 
p <- length(coef(model_1)) - 1  
adjusted_R21 <- 1 - ((1 - R21) * (n - 1) / (n - p - 1))
print(R21)
print(adjusted_R21)

# 2️:Adding the variables of age and sex
model_2 <- svyglm(K6 ~ CIGSDAY + AGE + SEX, design = design)
summary(model_2, vartype = c("se", "ci"))
# Calculating the F-Statistic
f_test <- regTermTest(model_2, ~ CIGSDAY+ AGE + SEX)
print(f_test) 
# R-squared & adjusted
y_hat <- predict(model_2, type = "response")
y <- data_clean$K6
w <- weights(design)
sst <- sum(w * (y - weighted.mean(y, w))^2)
sse <- sum(w * (y - y_hat)^2)
R22 <- 1 - (sse / sst)
n <- nrow(data_clean) 
p <- length(coef(model_2)) - 1  
adjusted_R22 <- 1 - ((1 - R22) * (n - 1) / (n - p - 1))
print(R22)
print(adjusted_R22)

# 3️: Adding variables about general health condition and family
model_3 <- svyglm(K6 ~ CIGSDAY + AGE + SEX + HEALTH + NCHILD, design = design)
summary(model_3, vartype = c("se", "ci"))
# Calculating the F-Statistic
f_test <- regTermTest(model_3, ~ CIGSDAY+ AGE + SEX + HEALTH + NCHILD)
print(f_test) 
# R-squared & adjusted
y_hat <- predict(model_3, type = "response")
y <- data_clean$K6
w <- weights(design)
sst <- sum(w * (y - weighted.mean(y, w))^2)
sse <- sum(w * (y - y_hat)^2)
R23 <- 1 - (sse / sst)
n <- nrow(data_clean) 
p <- length(coef(model_3)) - 1  
adjusted_R23 <- 1 - ((1 - R23) * (n - 1) / (n - p - 1))
print(R23)
print(adjusted_R23)

# 4️: Adding income related variable
model_4 <- svyglm(K6 ~ CIGSDAY + AGE + SEX + HEALTH + NCHILD + INCFAM07ON, design = design)
summary(model_4, vartype = c("se", "ci"))
# Calculating the F-Statistic
f_test <- regTermTest(model_4, ~ CIGSDAY+ AGE + SEX + HEALTH + NCHILD + INCFAM07ON)
print(f_test) 
# R-squared & adjusted
y_hat <- predict(model_4, type = "response")
y <- data_clean$K6
w <- weights(design)
sst <- sum(w * (y - weighted.mean(y, w))^2)
sse <- sum(w * (y - y_hat)^2)
R24 <- 1 - (sse / sst)
n <- nrow(data_clean) 
p <- length(coef(model_4)) - 1  
adjusted_R24 <- 1 - ((1 - R24) * (n - 1) / (n - p - 1))
print(R24)
print(adjusted_R24)

# 5️: Adding sleep related variable (entire model)
model_5 <- svyglm(K6 ~ CIGSDAY + AGE + SEX + HEALTH + NCHILD + INCFAM07ON + SLEEPFALL + SLEEPSTAY, design = design)
summary(model_5, vartype = c("se", "ci"))
# Calculating the F-Statistics for the entire model
f_test <- regTermTest(model_5, ~ CIGSDAY + AGE + SEX + HEALTH + NCHILD + INCFAM07ON + SLEEPFALL + SLEEPSTAY)
print(f_test) 
# R-squared & adjusted
y_hat <- predict(model_5, type = "response")
y <- data_clean$K6
w <- weights(design)
sst <- sum(w * (y - weighted.mean(y, w))^2)
sse <- sum(w * (y - y_hat)^2)
R25 <- 1 - (sse / sst)
n <- nrow(data_clean) 
p <- length(coef(model_5)) - 1  
adjusted_R25 <- 1 - ((1 - R25) * (n - 1) / (n - p - 1))
print(R25)
print(adjusted_R25)

#age group 
univariate_model_group1<-data_clean%>%filter(AGE<=30)
univariate_model_group2<-data_clean%>%filter(AGE>30&AGE<=50)
univariate_model_group3<-data_clean%>%filter(AGE>50)
#Weighted regression for age group 1 (18-30)
design <- svydesign(ids = ~1, weights = ~PERWEIGHT, data = univariate_model_group1)
weighted_model <- svyglm(K6 ~ CIGSDAY + SEX + HEALTH + NCHILD + INCFAM07ON, 
                         design = design)
summary(weighted_model,vartype = c("se", "ci"))

