#################################### AMRM ############################################
##########The impact of wine design on the purchase decision involvement #############
######################################################################################

##This study aimed to measure the impact of visual wine design on the buyer's
##behavior. The research investigated how much the design in the wine industry
##influences the consumer's decision for purchase. The survey attempted to consider
##wine design as the general percep-tion of product design.
##The effect of the visual wine aesthetics was researched regarding the consumer's
##purchase decision involvement and the importance of the stimulus for a purchase
##decision, be that the product or the task.

#####################################################################################
##### necessary packages 
# ltm package for cronbach alpha's check
library("ltm", lib.loc="~/R/win-library/3.2")
# RcmdrMisc package for a reliability test
library("RcmdrMisc", lib.loc="~/R/win-library/3.2")
# Durbin Watson test, VIF
library(car)
# Breusch Pagan Test for Heteroskedasticity
library(lmtest)
# global validation of linear model assumptions
library(gvlma)

#####################################################################################
##### Read the data
#####################################################################################

data_raw = data.frame(read.csv(file.choose(), header = TRUE))

# display top 3 lines
head(data_raw, 3)

# fix column names
colnames(data_raw) = c("ID", "Value1", "Value2", "Value3", "Value4",
                       "Acumen1", "Acumen2", "Acumen3", "Acumen4",
                       "Response1", "Response2", "Response3",
                       "PDI1", "PDI2", "PDI3", "PDI4",
                       "CIP_PCI1", "CIP_PCI2", "CIP_PCI3",
                       "CIP_PDI1", "CIP_PDI2", "CIP_PDI3",
                       "Gender", "Age", "Education", "Income", "Employment",
                       "Marriage", "Start_Date", "Submit_Date", "Network_ID")

# display the top 3 lines to check column names
head(data_raw, 3)

# View data
View(data_raw)

# column classes
sapply(data_raw, class)
sapply(data_raw, typeof)

#####################################################################################
##### Check for duplicates based on Network ID
###################################################################################

## unique values length and all values length based on network ID
length(unique(data_raw$Network_ID))
length(data_raw$Network_ID)

# find all duplicated rows
duplicated_rows = subset(data_raw, duplicated(data_raw$Network_ID) | duplicated(data_raw$Network_ID, fromLast = TRUE))
View(duplicated_rows)

# find from how many networks the duplicates are
duplicated_network_factors = factor(duplicated_rows$Network_ID)
levels(duplicated_network_factors)

# too much duplicates from only 4 network IDs
length(duplicated_rows$ID)
summary(duplicated_rows)

# delete all network duplicated rows
unique_raw_data = data_raw[!duplicated(data_raw$Network_ID),]
length(unique_raw_data$ID)


#####################################################################################
##### Check for missing values
###################################################################################

# specify NA's
na.strings = c("?", "")

# check for missing values
dim(unique_raw_data)
dim(na.omit(unique_raw_data))

# inspect the 7 records with missing data

# vector for missing values
na_vec = apply(unique_raw_data, 1, anyNA)

# rows with missing data
missing_data = subset(unique_raw_data, na_vec == TRUE)
View(missing_data)

# all na's affect the items from the conceptual framework and not the demographics

# drop all rows with missing data
manipulated_data = na.omit(unique_raw_data)
dim(manipulated_data)

###################################################################################
##### Demographic overview
###################################################################################

#########
## Gender

# view the gender table
plot(manipulated_data$Gender, main = "Gender distribution", xlab = "Values", ylab = "Frequency", col = 3:7, density = 25)
table(manipulated_data$Gender)

# create data frame to calculate the occurances and percentages
gender_df <- data.frame(table(manipulated_data$Gender))
colnames(gender_df) <- c('Gender','Frequency')
gender_df$Percentage <- gender_df$Frequency / sum(gender_df$Frequency) * 100

# results
print(gender_df)
sum(gender_df$Frequency)

######
## Age

# view the age table
plot(manipulated_data$Age, main = "Age distribution", xlab = "Values", ylab = "Frequency", col = 3:7, density = 25)
table(manipulated_data$Age)

# create data frame to calculate the occurances and percentages
age_df <- data.frame(table(manipulated_data$Age))
colnames(age_df) <- c('Age','Frequency')
age_df$Percentage <- age_df$Frequency / sum(age_df$Frequency) * 100

# results
print(age_df)
sum(age_df$Frequency)

############
## Education

# view the Education table
plot(manipulated_data$Education, main = "Education distribution", xlab = "Values", ylab = "Frequency", col = 3:7, density = 25)
table(manipulated_data$Education)
# create data frame to calculate the occurances and percentages
educ_df <- data.frame(table(manipulated_data$Education))
colnames(educ_df) <- c('Education','Frequency')
educ_df$Percentage <- educ_df$Frequency / sum(educ_df$Frequency) * 100

# results
print(educ_df)
sum(educ_df$Frequency)

#########
## Income

# view the Income table
plot(manipulated_data$Income, main = "Income distribution", xlab = "Values", ylab = "Frequency", col = 3:7, density = 25)
table(manipulated_data$Income)
# create data frame to calculate the occurances and percentages
inc_df <- data.frame(table(manipulated_data$Income))
colnames(inc_df) <- c('Income','Frequency')
inc_df$Percentage <- inc_df$Frequency / sum(inc_df$Frequency) * 100

# results
print(inc_df)
sum(inc_df$Frequency)

#############
## Employment

# view the Employment table
plot(manipulated_data$Employment, main = "Employment distribution", xlab = "Values", ylab = "Frequency", col = 3:7, density = 25)
table(manipulated_data$Employment)
# create data frame to calculate the occurances and percentages
empl_df <- data.frame(table(manipulated_data$Employment))
colnames(empl_df) <- c('Employment','Frequency')
empl_df$Percentage <- empl_df$Frequency / sum(empl_df$Frequency) * 100

# results
print(empl_df)
sum(empl_df$Frequency)

#################
## Marital status

# view the Marriage table
plot(manipulated_data$Marriage, main = "Marriage distribution", xlab = "Values", ylab = "Frequency", col = 3:7, density = 25)
table(manipulated_data$Marriage)
# create data frame to calculate the occurances and percentages
marriage_df <- data.frame(table(manipulated_data$Marriage))
colnames(marriage_df) <- c('Marriage','Frequency')
marriage_df$Percentage <- marriage_df$Frequency / sum(marriage_df$Frequency) * 100

# results
print(marriage_df)
sum(marriage_df$Frequency)


####################################################################################
##### Cronbach alphas
####################################################################################

## calculate the coefficient alphas for all scales used in the model

## overall WineDesign alpha coefficient
cronbach.alpha(manipulated_data[,c("Value1", "Value2", "Value3", "Value4",
                                   "Acumen1", "Acumen2", "Acumen3", "Acumen4",
                                   "Response1", "Response2", "Response3")])
# 11 items, alpha 0.897

## WineDesign Value alpha coefficient
cronbach.alpha(manipulated_data[,c("Value1", "Value2", "Value3", "Value4")])
# 4 items, alpha 0.818

## WineDesign Acumen alpha coefficient
cronbach.alpha(manipulated_data[,c("Acumen1", "Acumen2", "Acumen3", "Acumen4")])
# 4 items, alpha 0.805

## WineDesign Response alpha coefficient
cronbach.alpha(manipulated_data[,c("Response1", "Response2", "Response3")])
# 4 items, alpha 0.860

## PDI alpha coefficient
cronbach.alpha(manipulated_data[,c("PDI1", "PDI2", "PDI3", "PDI4")])
# 4 items, alpha 0.762
# alpha greater than 0.7, however close
# check reliability
reliability(cov(manipulated_data[,c("PDI1", "PDI2", "PDI3", "PDI4")]))
# removing the second item would increase the scale's alpha coefficient
# a prove for the later modification of the scale

##PDI_Stimulus alpha coefficient
cronbach.alpha(manipulated_data[,c("PDI1", "PDI3", "PDI4",
                                   "CIP_PCI1", "CIP_PCI2", "CIP_PCI3",
                                   "CIP_PDI1", "CIP_PDI2", "CIP_PDI3")])
# 9 items, alpha 0.904

## modified PDI alpha coefficient
cronbach.alpha(manipulated_data[,c("PDI1", "PDI3", "PDI4")])
# 3 items, alpha 0.812

# modified CIP alpha coefficient
cronbach.alpha(manipulated_data[,c("CIP_PCI1", "CIP_PCI2", "CIP_PCI3",
                                   "CIP_PDI1", "CIP_PDI2", "CIP_PDI3")])
# 6 items, alpha 0.888

## all cronbach alphas are greater than 0.7, all items can be used in the suggested model


###################################################################################
##### Transform the data to get the means for the variables
###################################################################################


## manipulated data frame with column means from the variable's likert scales +  demographics data
model_data = data.frame(ID = manipulated_data$ID,
                              Value = rowMeans(manipulated_data[,c(2,3,4,5)]),
                              Acumen = rowMeans(manipulated_data[,c(6,7,8,9)]),
                              Response = rowMeans(manipulated_data[,c(10,11,12)]),
                              WineDesign = rowMeans(manipulated_data[,c(2,3,4,5,6,7,8,9,10,11,12)]),
                              PDI_Stimulus = rowMeans(manipulated_data[,c(13,15,16,17,18,19,20,21,22)]),
                              PDI = rowMeans(manipulated_data[,c(13,14,15,16)]),
                              PDI_Modified = rowMeans(manipulated_data[,c(13,15,16)]),
                              CIP_Modified = rowMeans(manipulated_data[,c(17,18,19,20,21,22)]),
                              CIP_PCI = rowMeans(manipulated_data[,c(17,18,19)]),
                              CIP_PDI = rowMeans(manipulated_data[,c(20, 21, 22)]),
                              Gender = manipulated_data$Gender,
                              Age = manipulated_data$Age,
                              Education = manipulated_data$Education,
                              Income = manipulated_data$Education,
                              Employment = manipulated_data$Employment,
                              Marriage = manipulated_data$Marriage
                              )

#check the resulted data frame
head(model_data[,c(2-12)], 3)

####################################################################################
####################################################################################
##### Estimate the models
####################################################################################
####################################################################################

####################################################################################
##### multiple linear regression for H1, H2, H3
####################################################################################

lin_reg_H123 = lm(PDI ~ Value + Acumen + Response, data = model_data)

# View and save the summary
summary(lin_reg_H123)
summary_lin_reg_H123 = summary(lin_reg_H123)

# Save a model summary to a data frame
H123_df_Model = data.frame(Model_No = "M1",
                            Regression = "PDI = ??0 + ??1xValue + ??2xAcumen + ??2xResponse",
                            R_squared = summary_lin_reg_H123$r.squared,
                            Adj_R_squared = summary_lin_reg_H123$adj.r.squared,
                            Sigma = summary_lin_reg_H123$sigma,
                            F_Statistic = summary_lin_reg_H123$fstatistic)

# write the model summary to a csv file for the documentation
write.csv(H123_df_Model[c(1,2),], "C:\\Users\\Vladimir\\Desktop\\H123_Model_summary.csv", row.names = FALSE)

# save a coefficient summary to a csv file for the documentatio
write.csv(summary_lin_reg_H123$coefficients, "C:\\Users\\Vladimir\\Desktop\\H123_Coeff_summary.csv", row.names = TRUE)

####################################################################################
##### OLS assumptions for the first hypothesis

par(mfrow = c(2,2))
plot(lin_reg_H123)
par(mfrow = c(1,1))

# residuals from lin regression
residuals_H123 = lin_reg_H123$residuals
standard_residuals_H123 = rstandard(lin_reg_H123)


#################
## Normality test

# graphical test

# histogram
hist(standard_residuals_H123, freq = FALSE, breaks=20, density = 20, xlab = "Residuals",
     ylim = c(0,1), main = "Standardized residuals distribution")
# add normal curve
curve(dnorm(x), add = TRUE, col = "blue")

hist(residuals_H123, freq = FALSE, breaks = 20)
# add normal curve
curve(dnorm, add = TRUE)

qqnorm(residuals_H123)
qqline(residuals_H123)
#skewness on left and right

# Shapiro-Wilk's test for normality
shapiro.test(residuals_H123)
# p value less than 0.05 
# the null hypothesis that the data came from a normally distributed population can be rejected
# the qqplot shows skewness on left and right 

##################
## Autocorrelation

# Durbin Watson Test
durbinWatsonTest(residuals_H123)
# no autocorrelation

####################
## Multicollinearity

# VIF value 
vif(lin_reg_H123)
# vif values of the independent variables are less than 2 -> no multicolinearity

#####################
## Heteroscedasticity

# Breuch Pagan test to formally check presence of heteroscedasticity
bptest(lin_reg_H123)

# p value < 0.05 -> no heteroscedasticity

# global validation of linear model assumptions
gvlma(lin_reg_H123)
# Heteroscedasticity Assumptions acceptable.

###############
## Outlier test
outlierTest(lin_reg_H123)
# Cook's Distance plot
plot(cooks.distance(lin_reg_H456))

#case 49 from the initial data set most prominent
data_raw[49,]

#outlier ID
outl_id = data_raw[49, 1]

# regression without the outlier
lin_reg_H123.no49 = lm(PDI_Stimulus ~ Value + Acumen + Response, data = subset(model_data, model_data$ID != outl_id))

# new regression summary
summary(lin_reg_H123.no49)
summary_lin_reg_H123.no49 = summary(lin_reg_H123.no49)

# save a coefficient summary to a csv file for the documentatio
write.csv(summary_lin_reg_H123.no49$coefficients, "C:\\Users\\Vladimir\\Desktop\\robust_lm_H123.no49_Coeff_summary.csv", row.names = TRUE)


# check the regression plot
par(mfrow = c(2,2))
plot(lin_reg_H123.no49)
par(mfrow = c(1,1))

# check the residuals qq plot
residuals_H123.no49 = lin_reg_H123.no49$residuals
qqnorm(residuals_H123.no49)
qqline(residuals_H123.no49)

# more outliers?
outlierTest(lin_reg_H123.no49)
plot(cooks.distance(lin_reg_H123.no49))



####################################################################################
##### multiple linear regression for H4, H5, H6
####################################################################################

# save the regression result
lin_reg_H456 = lm(PDI_Stimulus ~ Value + Acumen + Response, model_data)

# View and save the summary
summary(lin_reg_H456)
summary_lin_reg_H456 = summary(lin_reg_H456)

# Save a model summary to a data frame
H456_df_Model = data.frame(Model_No = "M2",
                           Regression = "PDI_Stimulus = ??0 + ??1xValue + ??2xAcumen + ??2xResponse",
                           R_squared = summary_lin_reg_H456$r.squared,
                           Adj_R_squared = summary_lin_reg_H456$adj.r.squared,
                           Sigma = summary_lin_reg_H456$sigma,
                           F_Statistic = summary_lin_reg_H456$fstatistic)

# write the model summary to a csv file for the documentation
write.csv(H456_df_Model[c(1,2),], "C:\\Users\\Vladimir\\Desktop\\H456_Model_summary.csv", row.names = FALSE)

# save a coefficient summary to a csv file for the documentatio
write.csv(summary_lin_reg_H456$coefficients, "C:\\Users\\Vladimir\\Desktop\\H456_Coeff_summary.csv", row.names = TRUE)



####################################################################################
##### OLS Assumptions for the second hypothesis

# plot the result of the regression
par(mfrow = c(2,2))
plot(lin_reg_H456)
par(mfrow = c(1,1))

#residuals from lin regression
residuals_H456 = lin_reg_H456$residuals
standard_residuals = rstandard(lin_reg_H456)

#################
## Normality test

# graphical test

# histogram of 
hist(standard_residuals, freq = FALSE, breaks=20, density = 20, xlab = "Residuals",
     ylim = c(0,1), main = "Standardized residuals distribution")
# add normal curve
curve(dnorm(x), add = TRUE, col = "blue")


# normal distribution of the residuals with light right skewness
qqnorm(residuals_H456)
qqline(residuals_H456)

# Shapiro-Wilk's test for normality
shapiro.test(residuals_H456)
# p value greater than alpha level of 0.05
# the null hypothesis that the data came from a normally distributed population cannot be rejected
# the qqplot, histogram and high W-value show a rather normal distribution

##################
## Autocorrelation

# Durbin Watson Test
durbinWatsonTest(residuals_H456)
# no autocorrelation, value 1.75

####################
## Multicollinearity

# VIF value 
vif(lin_reg_H456)
# vif values of the independent variables are less than 2 -> no multicolinearity
# additionally no time data

#####################
## Heteroscedasticity

# Breuch Pagan test to formally check presence of heteroscedasticity
bptest(lin_reg_H456)
# p value = 0.06 -> little above 0.05 -> heteroscedasticity possibly existent

# global validation of linear model assumptions
gvlma(lin_reg_H456)
# Heteroscedasticity Assumptions acceptable according the global validation

###############
## Outlier test
# Cook's Distance plot
plot(cooks.distance(lin_reg_H456))

#case 49 from the initial data set most promiment
data_raw[49,]

#outlier ID
outl_id = data_raw[49, 1]

# regression without the outlier
lin_reg_H456.no49 = lm(PDI_Stimulus ~ Value + Acumen + Response, data = subset(model_data, model_data$ID != outl_id))

# new regression summary
summary(lin_reg_H456.no49)
summary_lin_reg_H456.no49 = summary(lin_reg_H456.no49)
summary(lin_reg_H456)

par(mfrow = c(2,2))
plot(lin_reg_H456.no49)
par(mfrow = c(1,1))

# regression output
# Save a model summary to a data frame
H456.no49_df_Model = data.frame(Model_No = "M2",
                           Regression = "PDI_Stimulus = ??0 + ??1xValue + ??2xAcumen + ??2xResponse",
                           R_squared = summary_lin_reg_H456.no49$r.squared,
                           Adj_R_squared = summary_lin_reg_H456.no49$adj.r.squared,
                           Sigma = summary_lin_reg_H456.no49$sigma,
                           F_Statistic = summary_lin_reg_H456.no49$fstatistic)

# write the model summary to a csv file for the documentation
write.csv(H456.no49_df_Model[c(1,2),], "C:\\Users\\Vladimir\\Desktop\\H456.no49_Model_summary.csv", row.names = FALSE)

# save a coefficient summary to a csv file for the documentatio
write.csv(summary_lin_reg_H456.no49$coefficients, "C:\\Users\\Vladimir\\Desktop\\H456.no49_Coeff_summary.csv", row.names = TRUE)

########################################################################################
# perform robust regression for second hypothesis to check again outliers and heteroskedasticity
robust_lm_H456 = rlm(formula = PDI_Stimulus ~ Value + Acumen + Response, data = model_data)

# summary
summary(robust_lm_H456)
summary_robust_lm_H456 = summary(robust_lm_H456)

# save a coefficient summary to a csv file for the documentatio
write.csv(summary_robust_lm_H456$coefficients, "C:\\Users\\Vladimir\\Desktop\\robust_lm_H456_Coeff_summary.csv", row.names = TRUE)


#####################################################################################