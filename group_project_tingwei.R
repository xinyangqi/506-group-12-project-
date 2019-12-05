## Group Project Draft
## Author: Ting Wei Lin, tingwei@umich.edu
## Updated: Dec. 5 2019

# libraries: -------------------------------------------------------------------
library(Hmisc)
library(data.table)
library(splines)

# data: ------------------------------------------------------------------------
path = "/Users/Sabrina/Documents/2019UMICH/STATS506/group project/data/"

sleep = as.data.table(sasxport.get(paste0(path, "SLQ_D.XPT")))

physical = as.data.table(sasxport.get(paste0(path, "PAQ_D.XPT")))

physical_indv = as.data.table(sasxport.get(paste0(path, "PAQIAF_D.XPT")))

demo = as.data.table(sasxport.get(paste0(path, "DEMO_D.XPT")))

dietary1 = as.data.table(sasxport.get(paste0(path, "DR1TOT_D.XPT")))

dietary2 = as.data.table(sasxport.get(paste0(path, "DR2TOT_D.XPT")))

# 80: --------------------------------------------------------------------------

# keep the variables we need
sleep = sleep[, .(seqn, sld010h)]
physical = physical[, .(seqn, pad080, paq520)]
physical_indv = physical_indv[, .(seqn, padtimes, paddurat)]
demo = demo[, .(seqn, riagendr, ridageyr, ridreth1, indfminc, ridexmon)]
dietary1 = dietary1[, .(seqn, dr1tkcal, dr1tsugr, dr1tcaff, day = 1)]
dietary2 = dietary2[, .(seqn, dr2tkcal, dr2tsugr, dr2tcaff, day = 2)]

# rename colnames
names(dietary1) = c("seqn", "drtkcal", "drtsugr", "drtcaff", "day")
names(dietary2) = c("seqn", "drtkcal", "drtsugr", "drtcaff", "day")

# merge day 1 data
data1 = merge(sleep, physical, all = TRUE)
data1 = merge(data1, physical_indv, all = TRUE)
data1 = merge(data1, demo, all = TRUE)
data1 = merge(data1, dietary1, all = TRUE)

# merge day 2 data
data2 = merge(sleep, physical, all = TRUE)
data2 = merge(data2, physical_indv, all = TRUE)
data2 = merge(data2, demo, all = TRUE)
data2 = merge(data2, dietary2, all = TRUE)

# row bind day 1 and day 2 data
data = rbind(data1, data2)

# omit the rows that have missing values
data_naomit = data[complete.cases(data[])]

# take mean for repeated seqn
avg_value = data_naomit[, lapply(.SD, mean), by = .(seqn), 
                    .SDcols = c("sld010h", "pad080", "paq520", "padtimes",
                        "paddurat", "ridageyr", "ridreth1", "indfminc",
                        "ridexmon", "drtkcal",  "drtsugr",  "drtcaff")]

# fit linear model 
model = lm(sld010h ~ pad080 + padtimes + ridageyr + as.factor(ridreth1) + 
             indfminc + as.factor(ridexmon) + drtkcal + drtsugr + drtcaff, 
           data = avg_value)
summary(model)

# do spline for "padtimes" variable
spline_model = lm(sld010h ~ pad080 + bs(padtimes, df = 3) + ridageyr + 
                    as.factor(ridreth1) + indfminc + as.factor(ridexmon) + 
                    drtkcal + drtsugr + drtcaff, 
                  data = avg_value)
summary(spline_model)

# plot relationship between Activity times and sleeping hours
plot(avg_value$sld010h, avg_value$pad080, col="black"
     ,xlab="Sleep (hr.)", ylab="Activity times (min.)",
     main = "Relationship between Activity times and sleeping hours")

# plot splines results
plot(avg_value$sld010h, avg_value$padtimes, col="grey",xlab="Sleep (hr.)"
     , ylab="Padtimes (# of times did activity in past 30 days)"
     , main = "Splines Results")
fit1 = smooth.spline(avg_value$sld010h, avg_value$padtimes, df=3) 
lines(fit1,col="red",lwd=2)