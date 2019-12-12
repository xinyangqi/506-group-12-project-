# Group Project 

# Data read and process.
library(SASxport)
library(dplyr)
library(mgcv)
alc = read.xport("C:/Users/Surface-pc/Downloads/ALQ_D.xpt")
phy = read.xport("C:/Users/Surface-pc/Downloads/PAQ_D.xpt")
phy_id = read.xport("C:/Users/Surface-pc/Downloads/PAQIAF_D.xpt")
slp = read.xport("C:/Users/Surface-pc/Downloads/SLQ_D.xpt")
demo = read.xport("C:/Users/Surface-pc/Downloads/DEMO_D.xpt")
total_d1 = read.xport("C:/Users/Surface-pc/Downloads/DR1TOT_D.xpt")

alc = alc %>% transmute(seqn = SEQN, alcohol = ALQ130)
phy = phy %>% transmute(seqn = SEQN, pa_comp = PAQ520)
phy_id = phy_id %>% transmute(seqn = SEQN, pa_time = PADTIMES, pa_dur = PADDURAT)
slp = slp %>% transmute(seqn = SEQN, sleep = SLD010H)
demo = demo %>% transmute(seqn = SEQN, gender = RIAGENDR, age = RIDAGEYR, 
                          race = RIDRETH1, inc = INDFMINC, winter = RIDEXMON)
total_d1 = total_d1 %>% transmute(seqn = SEQN, energy_1 = DR1TKCAL, 
                                  sugar_1 = DR1TSUGR, caffe_1 = DR1TCAFF)
data = left_join(demo, alc, by="seqn") %>% left_join(phy_id, by="seqn") %>% 
       left_join(phy, by = "seqn") %>% left_join(total_d1, "seqn") %>% 
       left_join(slp, "seqn") 

# Quick check: how many missing values.
colSums(apply(data, 2, is.na))

# Data cleaning.
data = data%>% filter(!is.na(sleep), pa_comp%in%c(1, 2, 3), 
         alcohol!=999, sleep<77, inc<12) %>% mutate(gender = ifelse(gender==1, 1, 0), 
         Mex = ifelse(race == 1, 1, 0), Hisp = ifelse(race == 2, 1, 0), 
         NHwhite = ifelse(race == 3, 1, 0), NHblack = ifelse(race == 4, 1, 0), 
         pa_high = ifelse(pa_comp == 1, 1, 0), pa_low = ifelse(pa_comp == 2, 1, 0),
         winter = ifelse(winter == 1, 1, 0), pad = pa_time*pa_dur/60) %>% 
       select(-c("race","pa_comp", "pa_time", "pa_dur"))

# A version without encoding factor variables.
data1 = left_join(demo, alc, by="seqn") %>% left_join(phy_id, by="seqn") %>% 
  left_join(phy, by = "seqn") %>% left_join(total_d1, "seqn") %>% 
  left_join(slp, "seqn") %>% filter(!is.na(sleep), pa_comp%in%c(1, 2, 3), 
            alcohol!=999, sleep<77, inc<12) %>% mutate(gender = ifelse(gender==1, 1, 0), 
            winter = ifelse(winter == 1, 1, 0), pad = pa_time*pa_dur/60) %>% 
  select(-c("pa_time", "pa_dur"))

# Alcohol data is not available for respondents age under 20.
# Collecting data for age 20-150 (those under 20 will be filtered for missing value.)
dat = data%>%filter(rowSums(apply(data,2,is.na))==0) %>% group_by(seqn) %>% 
      mutate(pad = mean(pad)) %>% distinct()
dat1 = data1%>%filter(rowSums(apply(data,2,is.na))==0) %>% group_by(seqn) %>% 
       mutate(pad = mean(pad)) %>% distinct()

# Simple OLS. (Rsq~0.0575)
result0=lm(sleep~as.factor(gender) + age + inc + as.factor(winter) +
             alcohol + energy_1 + sugar_1 + caffe_1 + as.factor(Mex) + as.factor(Hisp) +
             as.factor(NHwhite) + as.factor(NHblack) + as.factor(pa_high) +
             as.factor(pa_low) + pad, data=dat)
summary(result0)

# GAM with some variables discarded. (Adj Rsq~0.0711, explained deviance~8.26% when k is not set.)
result=gam(sleep~as.factor(Mex)+as.factor(NHwhite)+as.factor(NHblack)+
           as.factor(pa_low)+s(age)+s(alcohol)+s(pad)+
           s(energy_1)+s(sugar_1)+s(caffe_1),data=dat)
summary(result)

# Adj Rsq dropped a little (~0.07) when factor variables are not recoded. 
# The recoded version will be kept in later versions.
result1=gam(sleep~as.factor(race)+as.factor(pa_comp)+
            s(age)+s(alcohol)+s(pad)+s(energy_1)+s(sugar_1)+
            s(caffe_1),data=dat1)
summary(result1)

# Fit only the pad term.
res_pad = gam(sleep ~ s(pad), data = dat)
df = data.frame(x = dat$pad, y=dat$sleep, z=fitted(res_pad))
ggplot(df) + geom_point(aes(x = x, y = y), col="cornflowerblue") + 
  geom_line(aes(x = x, y = z),col = "gray") + 
  labs(x = "Monthly Physical Activity(hour)", y = "Sleep")
# A straight line. Now cut off the largest two examples.

dat2 = as.data.frame(dat[dat$pad<200, ])
res_pad1 = gam(sleep~s(pad), data = dat2)
df1 = data.frame(x = dat2$pad, y = dat2$sleep, z = fitted(res_pad1))
ggplot(df1) + geom_point(aes(x = x, y = y), col = "cornflowerblue") + 
  geom_line(aes(x = x, y = z), col = "gray") + 
  labs(x = "Monthly Physical Activity(hour)", y = "Sleep")
# Somehow have a curvature, but not very much convincing.

# Now use the full dataset, only dropping obs with missing values on PA and sleep.
data_pad = left_join(slp, phy, by="seqn") %>% 
  left_join(phy_id, by="seqn") %>% filter(!is.na(sleep), sleep<77) %>% 
  mutate(pad = pa_time*pa_dur/60) %>% select(-c("pa_time", "pa_dur"))

data_pad = data_pad%>%filter(rowSums(apply(data_pad,2,is.na))==0) %>% select(-"pa_comp")%>%
  group_by(seqn) %>% mutate(pad = mean(pad)) %>% distinct()

res_pad2 = gam(sleep~s(pad, k=5), data = data_pad)
df2 = data.frame(x = data_pad$pad, y = data_pad$sleep, z = fitted(res_pad2))
ggplot(df2) + geom_point(aes(x = x, y = y), col = "cornflowerblue") + 
  geom_line(aes(x = x, y =z ), col = "gray") + 
  labs(x = "Monthly Physical Activity(hour)", y = "Sleep")
# A slight downward trend. Seems that the response itself is somewhat random.

# Since truncated data should be used, return to dat2.
# Reported sleep should be an approximate value. Add a jitter so that 
# more than 99% of the respondents get the right approximated value,
# that is, sigma~0.2 (This also converts "sleep" to continuous.)
eps=rnorm(nrow(dat2), 0, 0.04)
dat_eps = as.data.frame(dat[dat$pad<200, ])
dat_eps = dat_eps %>% mutate(sleep = as.numeric(sleep)+eps)
res_eps_pad = gam(sleep~s(pad), data = dat_eps)
df_eps = data.frame(x=dat_eps$pad,y=dat_eps$sleep, z=fitted(res_eps_pad))
ggplot(df_eps) + geom_point(aes(x=x,y=y),col="cornflowerblue") + 
  geom_line(aes(x=x,y=z),col="gray") + 
  labs(x = "Monthly Physical Activity(hour)", y = "Sleep")
# Adj Rsq=0.00173, 0.398% of deviance explained.

# Compare with 2nd, 3rd polynomial and OLS.
ols_eps = lm(sleep~pad, dat_eps)
summary(ols_eps) # Rsq=0.0007, Adj rsq=0.00016
poly_eps2 = lm(sleep~poly(pad, 2),dat_eps)
summary(poly_eps2) # Rsq=0.0008, Adj Rsq<0
poly_eps3 = lm(sleep~poly(pad,3),dat_eps) 
summary(poly_eps3) # Rsq=0.0008, Adj Rsq<0

# Perform 10 fold cross-validation.
N = nrow(dat_eps)
ind = rep(1:10, N%/%10+1)[1:N]
r_ind=sample(ind, N, F)
mat_rmse=matrix(0,10,4)
for (i in 1:10){
  train = dat_eps[r_ind!=i, ]
  test = dat_eps[r_ind==i, ]
  test_x=test%>%select(-"sleep")
  test_y=test[,"sleep"]   
  ols_train = lm(sleep~pad, data = train)
  ols_test=predict(ols_train,test_x)
  p2_train = lm(sleep~poly(pad,2), data = train)
  p2_test=predict(p2_train,test_x)
  p3_train = lm(sleep~poly(pad,3), data = train)
  p3_test=predict(p3_train,test_x)
  gam_train = gam(sleep~s(pad), data = train)
  gam_test=predict(gam_train,test_x)
  mat_rmse[i,1]=sqrt(mean((test_y-ols_test)^2))
  mat_rmse[i,2]=sqrt(mean((test_y-p2_test)^2))
  mat_rmse[i,3]=sqrt(mean((test_y-p3_test)^2))
  mat_rmse[i,4]=sqrt(mean((test_y-gam_test)^2))
}
colMeans(mat_rmse) # 1.255465 1.255958 1.257100 1.256845
# The discrepancy of data under different conditions is probably not large 
# enough to perform CV.

# Use RMSE from the full dataset instead.
rmse = function(method){
 round(sqrt(mean(method$residual^2)),5)
}
rmse_ols = rmse(ols_eps)
rmse_poly2 = rmse(poly_eps2)
rmse_poly3 = rmse(poly_eps3)
rmse_gam = rmse(res_eps_pad)
rmse_gam;rmse_ols;rmse_poly2;rmse_poly3
# 1.25488  1.25692  1.25686  1.25686
# Not a large difference, but slightly better.
