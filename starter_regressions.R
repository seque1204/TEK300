#get packages
install.packages('aod')
install.packages('ggplot2')
install.packages('dplyr')
install.packages('readxl')

#libraries
library(aod)
library(ggplot2)
library(dplyr)
library(readxl)

#data reading (generated first, then real0
fakedata <- read_excel('C:/Users/kadem/Downloads/coup_data_with_predictions.xlsx')
head(fakedata)
colnames(fakedata)<- gsub(" ", "_", colnames(fakedata))

#logit with coup as dv and gdp + military expenditure + media_freedom
mylogit <- glm(`Coup` ~ `GDP_per_Capita` + `Military_Expenditure_%_GDP` + `Media_&_Internet_Freedom`, data = fakedata, family = 'binomial')
mylogit

#residuals
logit_residuals <- residuals(mylogit)
logit_residuals

#pearson residuals
pearson_residuals_logit <- residuals(logit_model, type = "pearson")
pearson_residuals_logit

#probit with same variables 
myprobit <- glm(`Coup` ~ `GDP_per_Capita` + `Military_Expenditure_%_GDP` + `Media_&_Internet_Freedom`, data = fakedata, family = binomial(link = 'probit'))
summary(myprobit)

#residuals
probit_residuals <- residuals(myprobit)
probit_residuals

#pearson residuals (preferred)
pearson_residuals_probit <- residuals(probit_model, type = "pearson")
pearson_residuals_probit