# Group Project 

# Data read and process.
library(SASxport)
library(dplyr)
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
         winter = ifelse(winter == 1, 1, 0), pad = pa_time*pa_dur) %>% 
       select(-c("race","pa_comp", "pa_time", "pa_dur"))

# A version without encoding factor variables.
data1 = left_join(demo, alc, by="seqn") %>% left_join(phy_id, by="seqn") %>% 
  left_join(phy, by = "seqn") %>% left_join(total_d1, "seqn") %>% 
  left_join(slp, "seqn") %>% filter(!is.na(sleep), pa_comp%in%c(1, 2, 3), 
            alcohol!=999, sleep<77, inc<12) %>% mutate(gender = ifelse(gender==1, 1, 0), 
            winter = ifelse(winter == 1, 1, 0), pad = pa_time*pa_dur) %>% 
  select(-c("pa_time", "pa_dur"))

# Quick check: how many missing values.
colSums(apply(data, 2, is.na))

dat = data%>%filter(rowSums(apply(data,2,is.na))==0) %>% group_by(seqn) %>% 
      mutate(pad = mean(pad)) %>% distinct()
dat1 = data1%>%filter(rowSums(apply(data,2,is.na))==0) %>% group_by(seqn) %>% 
  mutate(pad = mean(pad)) %>% distinct()

# Rsq~0.07
result=gam(sleep~as.factor(Mex)+as.factor(NHwhite)+as.factor(NHblack)+as.factor(pa_low)+
           s(age,k=3)+s(alcohol,k=3)+s(pad,k=3)+
           s(energy_1,k=3)+s(sugar_1,k=3)+s(caffe_1,k=3),data=dat)

result1=gam(sleep~as.factor(race)+as.factor(pa_comp)+
            s(age,k=3)+s(alcohol,k=3)+s(pad,k=3)+s(energy_1,k=3)+s(sugar_1,k=3)+
            s(caffe_1,k=3),data=dat1)

# Discarded variables. Do not run.
#gender+winter+s(inc,k=3)+pa_high+Hisp+NHblack+s(seqn,bs="re")