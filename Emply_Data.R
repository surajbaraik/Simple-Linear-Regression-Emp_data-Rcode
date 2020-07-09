######
######
##### Assignment Question no 3 , Churn out rate, Predict  the churn out rate based on salary hike
###### Y(output) is churn out rate and X(input) is Salary hike

churnRate <- read.csv(file.choose())
attach(churnRate)
remove(ChurnRate)
View(churnRate)

summary(churnRate)

plot(Salary_hike, Churn_out_rate)
### Sorting Time is X, and Delivery Time is Y

### after visualization of scatter plot, we can say it is negative in direction 
### strength is moderate

cor(Salary_hike, Churn_out_rate)
CRmodel1 <- lm(Churn_out_rate ~ Salary_hike)
CRmodel1
summary(CRmodel1)
predict(CRmodel1)
CRmodel1$residuals
confint(CRmodel1, level = 0.95)

predict(CRmodel1, interval = "confidence")
CRrmse <- sqrt(mean(CRmodel1$residuals^2))
CRrmse


##### LOG MODEL #####
### churn rate is Y, salary hike is X
plot(log(Salary_hike), Churn_out_rate)
cor(log(Salary_hike), Churn_out_rate)
CRmodel2 = lm(Churn_out_rate ~ log(Salary_hike))
summary(CRmodel2)

CRrmse2 <- sqrt(mean(CRmodel2$residuals^2))
CRrmse2


####### Exponential Model
plot(Salary_hike ,log(Churn_out_rate))
cor(Salary_hike, log(Churn_out_rate))
CRmodel3 = lm(log(Churn_out_rate) ~ Salary_hike)
summary(CRmodel3)

log_CR <- predict(CRmodel3, interval = "confidence")
log_CR
EXP_CR <- exp(log_CR)

CR_err<- Salary_hike - EXP_CR
CR_err

CRrmse3 <- sqrt(mean(CR_err^2))
CRrmse3



########
######### Polynomial Transformation

CRmodel4 <- lm(Churn_out_rate ~ Salary_hike)
summary(CRmodel4)

confint(CRmodel4, level = 0.95)
CR_logres<- predict(CRmodel4, interval = "confidence")
CRpoly <- exp(CR_logres)
CRpoly
err_CRpoly <- Salary_hike - CRpoly
err_CRpoly
CRrmse4 <- sqrt(mean(err_CRpoly^2))
CRrmse4


#############################################  