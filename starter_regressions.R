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
LFitted <- fitted(mylogit)
pearson_residuals_logit <- residuals(mylogit, type = "pearson")
pearson_residuals_logit

#plotted pearson residuals
ggplot(fakedata, aes(x = 1:249, y = pearson_residuals_logit)) +
  geom_point(color = "blue") +  # Scatter plot of residuals
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  # Horizontal line at y = 0
  labs(title = "Pearson Residuals from Logit Model",
       x = "Observations", y = "Pearson Residuals") +
  theme_minimal()

#probit with same variables 
myprobit <- glm(`Coup` ~ `GDP_per_Capita` + `Military_Expenditure_%_GDP` + `Media_&_Internet_Freedom`, data = fakedata, family = binomial(link = 'probit'))
summary(myprobit)

#residuals
Fitted <- fitted(myprobit)
probit_residuals <- residuals(myprobit)
probit_residuals

#pearson residuals (preferred)
pearson_residuals_probit <- residuals(myprobit, type = "pearson")
pearson_residuals_probit

#plotting pearson residuals
ggplot(fakedata, aes(x = 1:249, y = pearson_residuals_probit)) +
  geom_point(color = "blue") +  # Scatter plot of residuals
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  # Horizontal line at y = 0
  labs(title = "Pearson Residuals from Probit Model",
       x = "Observations", y = "Pearson Residuals") +
  theme_minimal()
