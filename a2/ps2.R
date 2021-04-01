library(bayesm)
library(tidyverse)
setwd("~/Desktop")

# Problem 1

data("margarine")

#View(margarine$choicePrice)
#View(margarine$demos)


# Descriptive statistics

means <- map_dbl(margarine$choicePrice, function(x) mean (x))
mean(margarine$choicePrice$PPk_Stk)
View(means)

mean_price_hhd <- margarine$choicePrice %>%
  group_by(hhid) %>%
  summarize(across(everything(),sd))

View(mean_price_hhd)


# Joining the two datasets

merged_data <- left_join(margarine$choicePrice, margarine$demos, by = "hhid")

merged_data %>%
  group_by(retired) %>%
  summarize(across(everything(),mean))

merged_data %>%
  group_by(Fam_Size) %>%
  summarize(across(everything(),mean)) %>%
  ggplot(aes(x = Fam_Size, y = PBB_Stk)) +
  geom_line() + 
  theme_bw()

# Family sizes greater than or equal to 5 have more college students and it drives their reservation price for margarine lower
margarine$demos %>%
  group_by(Fam_Size) %>%
  summarize(mean(college))

margarine$demos %>%
  ggplot(aes(college, Fam_Size)) + 
  geom_jitter(width = 0.1, height = 0.3) +
  theme_bw()


# Market share

brand_names <- colnames(margarine$choicePrice)[3:12]

merged_data %>%
  mutate(choice_name = brand_names[choice]) %>%
  group_by(choice_name) %>%
  count() %>%
  ungroup() %>%
  mutate(pct = n/sum(n))


# Question 2
library(nnet)
merged_data$type = merged_data$choice

for (i in 1:NROW(merged_data)){
  merged_data$price[i]=merged_data[i,3:12][merged_data$choice[i]]
}
merged_data$price= unlist(merged_data$price)


myfun = function(theta){
  merged_data$constant = 0
  merged_data$constant[merged_data$type=='1'] = theta[1]
  merged_data$constant[merged_data$type=='2'] = theta[2]
  merged_data$constant[merged_data$type=='3'] = theta[3]
  merged_data$constant[merged_data$type=='4'] = theta[4]
  merged_data$constant[merged_data$type=='5'] = theta[5]
  merged_data$constant[merged_data$type=='6'] = theta[6]
  merged_data$constant[merged_data$type=='7'] = theta[7]
  merged_data$constant[merged_data$type=='8'] = theta[8]
  merged_data$constant[merged_data$type=='9'] = theta[9]
  
  choice_probability = exp(merged_data$constant + theta[10]*merged_data$price) / 
    (exp(theta[1]+theta[10]*merged_data$PPk_Stk)+exp(theta[2]+theta[10]*merged_data$PBB_Stk)                                                                        +exp(theta[3]+theta[10]*merged_data$PFl_Stk)
     +exp(theta[4]+theta[10]*merged_data$PHse_Stk)+exp(theta[5]+theta[10]*merged_data$PGen_Stk)
     +exp(theta[6]+theta[10]*merged_data$PImp_Stk)+exp(theta[7]+theta[10]*merged_data$PSS_Tub)
     +exp(theta[8]+theta[10]*merged_data$PPk_Tub)+exp(theta[9]+theta[10]*merged_data$PFl_Tub)
     +exp(theta[10]*merged_data$PHse_Tub))
  return(-sum(log(choice_probability)))
}

estimation = optim(runif(10), myfun, method = 'BFGS')


# Question 3
q3data=merged_data[,2:13]
names(q3data) <- c("choice",paste0(1:10),"Income")

data_long <- gather(q3data, type, price, `1`:`10`, factor_key=TRUE)

like_fun= function(param,data_long )
{
  price =  data_long$price
  type       =  data_long$type
  Income    =  data_long$Income
  ni = nrow(data_long)
  nj = length(unique(data_long[,'type']))
  ut = mat.or.vec(ni,nj)
  choice = data_long$choice
  for (j in 1:nj)
  {
    # conditional logit
    ut[,j] = param[1] + param[2]*price[j]+ param[3]*Income[j] 
  }
  prob   = exp(ut)            # exp(XB)
  #sprob  = rowsums(prob)      # sum_j exp(XB) denominator
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob)) # an example of how to construct
  # match prob to actual choices
  probc = NULL
  for (i in 1:ni)
  {
    probc[i] = prob[i,choice[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc))
  return(-like)
}

estimation = optim(runif(3), like_fun, method = 'BFGS', data_long= data_long)

estimation

# [1] 0.4107993 4.5926646 0.1211126
# This means price has a positive effect on choice. If the price increases 1 unit, the log odds 
# with increase  0.1211126

# try different data input
q3_try = merged_data[,c('choice', 'type', 'Income', 'price')]
est2 = optim(runif(3), like_fun, method = 'BFGS', data_long= q3_try)
est2
#
# 0.8579925 5.2046348 0.1208143



# Question 4

# By definition, beta*exp is our marginal effect, 



# Question 5






