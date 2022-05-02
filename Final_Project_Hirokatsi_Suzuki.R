#Read data and define variables
df = read.table('house_price_data_2000.txt',header = TRUE)
price = df$price
bedrooms = df$bedrooms
bedrooms_factor = as.factor(bedrooms)
bathrooms = df$bathrooms
living = df$sqft_living
lot = df$sqft_lot
floors = df$floors
floors_factor = as.factor(floors)
waterfront = df$waterfront
waterfront_factor = as.factor(waterfront)
view = df$view
view_factor = as.factor(view)
condition = df$condition
condition_factor = as.factor(condition)
grade = df$grade
grade_factor = as.factor(grade)
above = df$sqft_above
basement = df$sqft_basement
year = df$yr_built
renov = df$yr_renovated
lat = df$lat
long = df$long
living15 = df$sqft_living15
lot15 = df$sqft_lot15

#Making full model
model.full = lm(price~bedrooms_factor+bathrooms+living+lot+floors+waterfront_factor+
                  view_factor+condition_factor+grade_factor+above+basement+year+renov+lat+long)
summary(model.full)

#Selection of variables
#install.packages('Rcmdr')
library('Rcmdr')
library('RcmdrMisc')
stepwise(model.full,direction = 'backward',criterion = 'BIC')
stepwise(model.full,direction = 'forward',criterion = 'BIC')

#Models after selection of variables
model.back = lm(price ~ bathrooms + living + waterfront_factor + 
                  view_factor + grade_factor + year + lat)
summary(model.back)
#log transformation gave a little better adj R^2 value
model.back.log = lm(log(price) ~ bathrooms + living + waterfront_factor + 
                      view_factor + grade_factor + year + lat)
summary(model.back.log)
#model.forward = lm(price ~ living + view_factor + lat + grade_factor + 
#                     year + waterfront_factor + bathrooms )
#model.forward.log = lm(log(price) ~ living + view_factor + lat + grade_factor + 
#                         year + waterfront_factor + bathrooms)
#summary(model.forward.log)

#ANOVA
anova.back.log = aov(model.back.log)
summary(anova.back.log)

#Normality Test
residuals = summary(model.back.log)$resid
qqnorm(residuals); qqline(residuals)

#Tests
#install.packages('olsrr')
library('olsrr')
ols_step_best_subset(model.back.log)

#MSE
mean(summary(model.back.log)$residuals^2)








