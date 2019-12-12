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
total_d2 = read.xport("C:/Users/Surface-pc/Downloads/DR2TOT_D.xpt")


alc = alc %>% transmute(seqn = SEQN, alcohol = ALQ130)
phy = phy %>% transmute(seqn = SEQN, pa_comp = PAQ520)
phy_id = phy_id %>% transmute(seqn = SEQN, pa_time = PADTIMES, pa_dur = PADDURAT)
slp = slp %>% transmute(seqn = SEQN, sleep = SLD010H)
demo = demo %>% transmute(seqn = SEQN, gender = RIAGENDR, age = RIDAGEYR, 
                          race = RIDRETH1, inc = INDFMINC, winter = RIDEXMON)
total_d1 = total_d1 %>% transmute(seqn = SEQN, energy_1 = DR1TKCAL, 
                                  sugar_1 = DR1TSUGR, caffe_1 = DR1TCAFF)
total_d2 = total_d2 %>% transmute(seqn = SEQN, energy_2 = DR2TKCAL, 
                                  sugar_2 = DR2TSUGR, caffe_2 = DR2TCAFF)
data = left_join(demo, alc, by="seqn") %>% left_join(phy_id, by="seqn") %>% 
       left_join(phy, by = "seqn") %>% left_join(total_d1, "seqn") %>% 
       left_join(slp, "seqn") %>% filter(!is.na(sleep), pa_comp%in%c(1, 2, 3), 
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

# Quick check: how many missing values.
colSums(apply(data, 2, is.na))

dat = data%>%filter(rowSums(apply(data,2,is.na))==0) %>% group_by(seqn) %>% 
      mutate(pad = mean(pad)) %>% distinct()
dat1 = data1%>%filter(rowSums(apply(data,2,is.na))==0) %>% group_by(seqn) %>% 
  mutate(pad = mean(pad)) %>% distinct()

# Simple OLS. (Rsq~0.06)
result0=lm(sleep~.,data=dat)
summary(result0)

# GAM with some variables discarded. (Rsq~0.07)
result=gam(sleep~as.factor(Mex)+as.factor(NHwhite)+as.factor(NHblack)+as.factor(pa_low)+
           s(age,k=3)+s(alcohol,k=3)+s(pad,k=3)+
           s(energy_1,k=3)+s(sugar_1,k=3)+s(caffe_1,k=3),data=dat)
summary(result)

result1=gam(sleep~as.factor(race)+as.factor(pa_comp)+
            s(age,k=3)+s(alcohol,k=3)+s(pad,k=3)+s(energy_1,k=3)+s(sugar_1,k=3)+
            s(caffe_1,k=3),data=dat1)
summary(result1)

# Fit only the pad term.
res_pad = gam(sleep ~ s(pad, bs = "tp"), data = dat)
df = data.frame(x = dat$pad, y=dat$sleep, z=fitted(res_pad))
ggplot(df) + geom_point(aes(x=x,y=y),col="cornflowerblue") + 
  geom_line(aes(x=x,y=z),col="gray") + 
  labs(x = "Monthly Physical Activity(hour)", y = "Sleep")
# A straight line. Now cut off the largest two examples.

dat2 = dat[dat$pad<200,]
res_pad1 = gam(sleep~s(pad,bs="tp"), data=dat2)
df1 = data.frame(x=dat2$pad,y=dat2$sleep, z=fitted(res_pad1))
ggplot(df1) + geom_point(aes(x=x,y=y),col="cornflowerblue") + 
  geom_line(aes(x=x,y=z),col="gray") + 
  labs(x = "Monthly Physical Activity(hour)", y = "Sleep")
# Somehow have a curvature, but not very much convincing.

# Now use the full dataset, only dropping obs with missing values on PA and sleep.
data_pad = left_join(slp, phy, by="seqn") %>% 
  left_join(phy_id, by="seqn") %>% filter(!is.na(sleep), sleep<77) %>% 
  mutate(pad = pa_time*pa_dur/60) %>% select(-c("pa_time", "pa_dur"))

data_pad = data_pad%>%filter(rowSums(apply(data_pad,2,is.na))==0) %>% select(-"pa_comp")%>%
  group_by(seqn) %>% mutate(pad = mean(pad)) %>% distinct()

res_pad2 = gam(sleep~s(pad, bs = "tp"), data = data_pad)
df2 = data.frame(x = data_pad$pad, y=data_pad$sleep, z=fitted(res_pad2))
ggplot(df2) + geom_point(aes(x=x,y=y),col="cornflowerblue") + 
  geom_line(aes(x=x,y=z),col="gray") + 
  labs(x = "Monthly Physical Activity(hour)", y = "Sleep")
# A slight downward trend. Perhaps the response itself is somewhat random?
