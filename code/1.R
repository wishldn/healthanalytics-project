packages <- c("dplyr", "ggplot2", "tidyr", "corrplot", "ggcorrplot", "car", "survey", "ipumsr")
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
library(survey)
design <- svydesign(
  ids = ~PSU,         # 聚类变量 (Primary Sampling Unit)
  strata = ~STRATA,   # 分层变量 (Stratification)
  weights = ~PERWEIGHT,  # 加权变量
  data = data_clean,
  nest = TRUE  # 如果数据有嵌套抽样，使用 nest=TRUE
)

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

# Heteroscedasticity test， 
#p-value < 0.05：existing heteroscedasticity issue.
#p-value > 0.05：no obvious  heteroscedasticity issue.

library(lmtest)
bp_test <- bptest(weighted_model) 
print(bp_test)


# 2. White test，（t -value and p - value is different from the master model ），heteroscedasticity has influence on the result.
library(sandwich)
library(lmtest)

# Calculating Huber-White heteroscedasticity and robust standard error
robust_se <- vcovHC(weighted_model, type = "HC0")  

# Calculating the regression result with robust se
robust_model <- coeftest(weighted_model, vcov = robust_se)

#print the regression result of robust se
print(robust_model)

#the robust_mode was over-adjusted，therefore  the Robust Standar Errors: Design-Based Standard Errors was used as the final print result
summary(weighted_model, vartype = c("se", "ci"))

# 3. Residual Plot："If the points are uniformly distributed without a systematic pattern, it indicates that homoscedasticity holds. If the points exhibit a funnel shape or other systematic variations, it suggests the presence of heteroscedasticity.
library(ggplot2)

# Calculating the residual of WLS
data_clean$residuals_wls <- residuals(weighted_model)
data_clean$fitted_wls <- fitted(weighted_model)

# Plotting residual graph
ggplot(data_clean, aes(x = fitted_wls, y = residuals_wls)) +
  geom_point(alpha = 0.5, color = "blue") +  # 绘制残差点
  geom_smooth(method = "loess", color = "red", se = FALSE) +  # 添加 LOESS 平滑曲线
  labs(title = "Residual Plot for Weighted Least Squares (WLS)",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()


# Building the regression model again with the robust se
robust_model <- coeftest(lm_model_multi, vcov = vcovHC(lm_model_multi, type = "HC"))
print(robust_model)








# Extract the data from male and female separately
data_male <- subset(data_clean, SEX == 1)
data_female <- subset(data_clean, SEX == 2)


# Define the weight structure（using PERWEIGHT）
library(lmtest)

# Using PERWEIGHT as weighted variable
wls_male <- lm(K6 ~ CIGSDAY + AGE  + HEALTH + NCHILD + INCFAM07ON, 
               data = data_male, weights = PERWEIGHT)

wls_female <- lm(K6 ~ CIGSDAY + AGE  + HEALTH + NCHILD + INCFAM07ON, 
                 data = data_female, weights = PERWEIGHT)


summary(wls_male)
summary(wls_female)

library(ggplot2)

# Generating the result of residual
residuals_male <- data.frame(Fitted = fitted(wls_male), Residuals = residuals(wls_male))
residuals_female <- data.frame(Fitted = fitted(wls_female), Residuals = residuals(wls_female))

# Plotting the residual graph for male
ggplot(residuals_male, aes(x = Fitted, y = Residuals)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(title = "Residual Plot for Males (WLS)", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# Plotting the residual graph for female
ggplot(residuals_female, aes(x = Fitted, y = Residuals)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(title = "Residual Plot for Females (WLS)", x = "Fitted Values", y = "Residuals") +
  theme_minimal()






# svyglm for sample survey

library(survey)

# Define the samplomg design for male and femmale
design_male <- svydesign(ids = ~1, weights = ~PERWEIGHT, data = data_male)
design_female <- svydesign(ids = ~1, weights = ~PERWEIGHT, data = data_female)



# Weighted regression for male
weighted_model_male <- svyglm(K6 ~ CIGSDAY + AGE + HEALTH + NCHILD + INCFAM07ON, 
                              design = design_male)
vcov(weighted_model_male) 

# Weighted regression for female
weighted_model_female <- svyglm(K6 ~ CIGSDAY + AGE + HEALTH + NCHILD + INCFAM07ON, 
                                design = design_female)

# 🔹 VIF check: male
library(car)
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

# male异方差检验， 🔹 1. Breusch-Pagan 检验（BP 检验）， 运行 Breusch-Pagan 检验，
#p 值 < 0.05：存在异方差问题。
#p 值 > 0.05：未发现显著的异方差问题。
library(lmtest)
bp_test <- bptest(weighted_model_male) 
print(bp_test)


# 🔹 VIF 多重共线性检查 female
library(car)
vif(weighted_model_female)

# 计算整个模型的 F 统计量 female
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

# female异方差检验， 🔹 1. Breusch-Pagan 检验（BP 检验）， 运行 Breusch-Pagan 检验，
#p 值 < 0.05：存在异方差问题。
#p 值 > 0.05：未发现显著的异方差问题。
library(lmtest)
bp_test <- bptest(weighted_model_female) 
print(bp_test)

# 计算male带聚类（PSU）的标准误
summary(weighted_model_male, vartype = c("se", "ci"))

# 计算female带聚类（PSU）的标准误
summary(weighted_model_female, vartype = c("se", "ci"))

# 提取男性模型的拟合值和残差
residuals_male <- data.frame(
  Fitted_Values = fitted(weighted_model_male),
  Residuals = residuals(weighted_model_male)
)

# 提取女性模型的拟合值和残差
residuals_female <- data.frame(
  Fitted_Values = fitted(weighted_model_female),
  Residuals = residuals(weighted_model_female)
)

library(ggplot2)

# 绘制男性残差图
ggplot(residuals_male, aes(x = Fitted_Values, y = Residuals)) +
  geom_point(color = "blue") +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  ggtitle("Residual Plot for Male (Weighted Regression)") +
  xlab("Fitted Values") + ylab("Residuals")

# 绘制女性残差图
ggplot(residuals_female, aes(x = Fitted_Values, y = Residuals)) +
  geom_point(color = "blue") +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  ggtitle("Residual Plot for Female (Weighted Regression)") +
  xlab("Fitted Values") + ylab("Residuals")















#Robustness Check
# 1️仅包含核心变量
model_1 <- svyglm(K6 ~ CIGSDAY, design = design)
summary(model_1, vartype = c("se", "ci"))
# 计算整个模型的 F 统计量
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

# 2️加入基本的人口统计变量
model_2 <- svyglm(K6 ~ CIGSDAY + AGE + SEX, design = design)
summary(model_2, vartype = c("se", "ci"))
# 计算整个模型的 F 统计量
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

# 3️加入健康和家庭相关变量
model_3 <- svyglm(K6 ~ CIGSDAY + AGE + SEX + HEALTH + NCHILD, design = design)
summary(model_3, vartype = c("se", "ci"))
# 计算整个模型的 F 统计量
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

# 4️加入经济变量
model_4 <- svyglm(K6 ~ CIGSDAY + AGE + SEX + HEALTH + NCHILD + INCFAM07ON, design = design)
summary(model_4, vartype = c("se", "ci"))
# 计算整个模型的 F 统计量
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

# 5️加入睡眠相关变量（完整模型）
model_5 <- svyglm(K6 ~ CIGSDAY + AGE + SEX + HEALTH + NCHILD + INCFAM07ON + SLEEPFALL + SLEEPSTAY, design = design)
summary(model_5, vartype = c("se", "ci"))
# 计算整个模型的 F 统计量
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

