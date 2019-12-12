## Group Project
## Author: Ting Wei Lin, tingwei@umich.edu
## Using data from NHANES. Select several dataset we need and
## analyse whether the variables have influence on sleeping hours
## Updated: Dec. 11 2019

# libraries: -------------------------------------------------------------------
library(Hmisc)
library(data.table)
library(splines)

# data: ------------------------------------------------------------------------
path = "/Users/Sabrina/Documents/2019UMICH/STATS506/group project/data/"

sleep = as.data.table(sasxport.get(paste0(path, "SLQ_D.XPT")))
alc =  as.data.table(sasxport.get(paste0(path, "ALQ_D.xpt")))
physical = as.data.table(sasxport.get(paste0(path, "PAQ_D.XPT")))
physical_indv = as.data.table(sasxport.get(paste0(path, "PAQIAF_D.XPT")))
demo = as.data.table(sasxport.get(paste0(path, "DEMO_D.XPT")))
dietary1 = as.data.table(sasxport.get(paste0(path, "DR1TOT_D.XPT")))
dietary2 = as.data.table(sasxport.get(paste0(path, "DR2TOT_D.XPT")))

# 80: --------------------------------------------------------------------------

# keep the variables we need, and filter the unreasonale data
sleep = sleep[sld010h < 77, .(seqn, sld010h)]
alc = alc[alq130 < 100,.(seqn, alq130)]
physical = physical[paq520 < 7, .(seqn, paq520)]
physical_indv = physical_indv[, .(seqn, padtimes, paddurat)]
demo = demo[indfminc < 12, .(seqn, riagendr, ridageyr, ridreth1, indfminc, ridexmon)]
dietary1 = dietary1[, .(seqn, dr1tkcal, dr1tsugr, dr1tcaff)]

# rename colnames
names(dietary1) = c("seqn", "drtkcal", "drtsugr", "drtcaff")

# merge day 1 data
data1 = merge(sleep, physical, all = TRUE)
data1 = merge(data1, alc, all = TRUE)
data1 = merge(data1, physical_indv, all = TRUE)
data1 = merge(data1, demo, all = TRUE)
data1 = merge(data1, dietary1, all = TRUE)

# rename colnames
names(data1) = c("seqn", "sleep", "pa_comp", "alcohol", "padtimes", 
                 "paddurat", "gender", "age", "race", "inc", "winter", 
                 "energy", "sugar", "caffeine", "day")

# omit the rows that have missing values
data1_naomit = data1[complete.cases(data1[])]
# compute total activity times (hr.) in past 30 days
data1_pad = data_naomit[, .(pad = paddurat*padtimes/60)]
# cbind two dataset
data1_use = cbind(data_naomit, pad)

# take mean for repeated seqn
data1_avg_value = data_use[, lapply(.SD, mean), by = .(seqn), 
                    .SDcols = c("sleep", "alcohol", "pa_comp", 
                                "padtimes", "paddurat", "age", "race", 
                                "inc", "winter", "gender", "energy",
                                "sugar", "caffeine", "pad")]
# decode the factors
data_new1 = data1_avg_value[, `:=` (Mex = 1L * {race == 1},
              Hisp = 1L * {race == 2}, NHwhite = 1L * {race == 3}, 
              NHblack = 1L * {race == 4}, pa_high = 1L * {pa_comp == 1}, 
              pa_low = 1L * {pa_comp == 2}, winter = 1L * {winter == 1})]

# fit linear model for all variables
model_lm1 = lm(sleep ~ alcohol + energy + sugar + caffeine + 
                 as.factor(gender) + age + as.factor(inc) + as.factor(winter) + 
                 as.factor(Mex) + as.factor(Hisp) + as.factor(NHwhite)+
                 as.factor(NHblack) + as.factor(pa_high) + as.factor(pa_low) + 
                 pad, data = data_new1)

summary(model_lm1)

# variable selection ( drop the variables the has large p-value)
model_lm2 = lm(sleep ~ as.factor(Mex) + as.factor(NHwhite) + as.factor(NHblack) + 
                as.factor(pa_low) + age + alcohol + pad + sugar + caffeine, 
               data = data_new1)

summary(model_lm2)

# fit model for "pad" variable
model_pad1 = lm(sleep ~ pad,  data = data_new1)
summary(model_pad1)
coef(model_pad1) # extract coefficient

# plot relationship between Activity times and sleeping hours
plot(data_new1$pad, data_new1$sleep, col="darkblue"
     ,xlab="Sleep (hr.)", ylab="Activity times (hr.)",
     main = "Relationship between Activity times and sleeping hours")
# add regression line to the plot
abline(coef(model_pad1)[1], coef(model_pad1)[2], col = "red")

# create another dataset to fit only sleep and pad
data2 = merge(sleep, physical_indv, all = TRUE)
# rename colnames
names(data2) = c("seqn", "sleep", "padtimes", "paddurat")

# omit the rows that have missing values
data2_naomit = data2[complete.cases(data2[])]
# compute total activity times (hr.) in past 30 days
data2_pad = data2_naomit[, .(pad = paddurat*padtimes/60)]
# cbind two dataset
data2_use = cbind(data2_naomit, data2_pad)

# take mean for repeated seqn
data2_avg_value = data2_use[, lapply(.SD, mean), by = .(seqn), 
                           .SDcols = c("sleep", "padtimes", "paddurat", "pad")]

# fit model for "pad" variable
model_pad2 = lm(sleep ~ pad,  data = data2_avg_value)
summary(model_pad2)
coef(model_pad2) # extract colnames

# plot relationship between Activity times and sleeping hours
plot(data_new2$pad, data_new2$sleep, col="darkblue"
     ,xlab="Sleep (hr.)", ylab="Activity times (hr.)",
     main = "Relationship between Activity times and sleeping hours")
# add regression line to the plot
abline(coef(model_pad2)[1], coef(model_pad2)[2], col = "red")

# create jitter
random = rnorm(nrow(data2_avg_value), 0, 0.1)
# add jitter to "sleep" variable
data_new2 = data2_avg_value[, .(sleep = sleep + random, pad)]

# do spline for "pad" variable
model_lm3 = lm(sleep ~ pad,  data = data_new2)
summary(model_lm3)
coef(model_lm3) # extract coefficient


# do spline for "pad" variable
spline_model = lm(sleep ~ bs(pad),  data = data_new2)

summary(spline_model)

# plot splines results
plot(data_new2$pad, data_new2$sleep, 
     col="grey", xlab="Sleep (hr.)", ylab="Activity times (hr.)", 
     main = "Splines Regression Result")
fit1 = smooth.spline(data_new2$pad, data_new2$sleep, df=3) 
abline(coef(model_lm3)[1], coef(model_lm3)[2], col = "yellow")
lines(fit1, col="red",lwd=2)




# fit logit regression
data_logit = avg_value[, sleep := 1L * {sld010h > 7}]

model_logit = glm(sleep ~ alq130 + ridageyr + as.factor(ridreth1) + 
                    indfminc + as.factor(ridexmon) +  drtsugr +  
                    drtcaff + pad, 
                  data_logit, family = (binomial))

summary(model_logit)

# compute the odds ration
exp(coef(model_logit))






