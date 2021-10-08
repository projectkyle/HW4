# HW4

#Taulant Bega, Adelia Fida, Nicholas Esposito
#Comparing how Philosophy and Religious Studies majors do in terms of income compared to high school or other college major

attach(acs2017_ny)
use_varb <- (AGE >= 25) & (AGE <= 55) & (LABFORCE == 2) & (WKSWORK2 > 4) & (UHRSWORK >= 35)
dat_use <- subset(acs2017_ny,use_varb) # 
detach()
attach(dat_use)
# Github example--------------------------------------
model_temp1 <- lm(INCWAGE ~ AGE + female + AfAm + Asian + Amindian + race_oth + Hispanic + educ_hs + educ_somecoll + educ_college + educ_advdeg)
summary(model_temp1)
plot(model_temp1)

require(stargazer)
stargazer(model_temp1, type = "text")

require(AER)

# subset in order to plot...
NNobs <- length(INCWAGE)
set.seed(12345) # just so you can replicate and get same "random" choices
graph_obs <- (runif(NNobs) < 0.1) # so something like just 1/10 as many obs
dat_graph <-subset(dat_use,graph_obs)  

plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), data = dat_graph)
# ^^ that looks like crap since Wages are soooooooo skew!  So try to find some sensible ylim = c(0, ??)
plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,150000), data = dat_graph)
# discus what you see in this plot

# change this line to fit your regression
to_be_predicted2 <- data.frame(AGE = 25:55, female = 1, AfAm = 0, Asian = 0, Amindian = 1, race_oth = 1, Hispanic = 1, educ_hs = 0, educ_somecoll = 0, educ_college = 1, educ_advdeg = 0)
to_be_predicted2$yhat <- predict(model_temp1, newdata = to_be_predicted2)

lines(yhat ~ AGE, data = to_be_predicted2)

#------------------------------------------------------

# My example-------------------------------------------
model_temp2 <- lm(INCWAGE ~ DEGFIELD + AGE + EDUC)
summary(model_temp2)
plot(model_temp2)

require(stargazer)
stargazer(model_temp2, type = "text")

#Dependent variable for Philosophy and Religious Studies INCWAGE
#53,046.870***       
  #(5,640.128) 

#AGE 
#1,293.151***        
  #(39.820)  

#EDUC_HS
#  -6,072.012***       
  #(992.301)    

#EDUC_COLLEGE
# -28,582.760***       
  #(1,096.139) 


# Observations                                                                 46,971           
# R2                                                                            0.144           
# Adjusted R2                                                                   0.143           
# Residual Std. Error                                                  77,047.600 (df = 46930)  
# F Statistic                                                        197.516*** (df = 40; 46930)
# ==============================================================================================
#  Note:                                                              *p<0.1; **p<0.05; ***p<0.01

require(AER)

# subset in order to plot...
NNobs <- length(INCWAGE)
set.seed(12345) # just so you can replicate and get same "random" choices
graph_obs <- (runif(NNobs) < 0.1) # so something like just 1/10 as many obs
dat_graph <-subset(dat_use,graph_obs)  

plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), data = dat_graph)
# ^^ that looks like crap since Wages are soooooooo skew!  So try to find some sensible ylim = c(0, ??)
plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,150000), data = dat_graph)
# discus what you see in this plot

# change this line to fit your regression
to_be_predicted2 <- data.frame(AGE = 25:55, DEGFIELD = 0, educ_hs = 1, educ_college = 0)
to_be_predicted2$yhat <- predict(model_temp2, newdata = to_be_predicted2)

lines(yhat ~ AGE, data = to_be_predicted2)

#--Notes----------------------------------------

# STD for Philosophy =  59220.927 
# STD for EDUC4 years of college = 21319.480
# STD for EDUCGrade 12 = 8664.393 

# Difference of people who majored in Philosophy and those who have a 4 year degree = 
# 59220.927 - 21319.480 = 37,901.447

# Difference of people who have a hs degree compared to those who majored in phil. = 
# 59220.927 - 8664.393 = 50,556.534

# The gap is smaller for those who majored in philosophy and have a 4 year degree 
# as opposed to those who only have a hs degree. 

getwd()
