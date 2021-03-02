library(tidyverse)
datstu = read.csv("~/Desktop/Econ 613/datstu.csv")
datjss = read.csv("~/Desktop/Econ 613/datjss.csv")%>%
  select(-X)
datsss = read.csv("~/Desktop/Econ 613/datsss.csv") %>%
  select(-c(X, schoolname)) %>%
  filter(!is.na(ssslong)) %>%
  unique()

#Exercise 1

# Number of students
nrow(datstu)

# Number of schools
datstu[,5:10] %>%
  unlist() %>%
  unique() %>%
  length()

# Number of programs
length(unique(unlist(datstu[,11:16])))

# Number of choices

# mutate() - create a new column
# select() - grabs a given column
# filter() - grabs specific rows

datstu %>%
  mutate(choice_1 = paste(schoolcode1, choicepgm1),
         choice_2 = paste(schoolcode2, choicepgm2),
         choice_3 = paste(schoolcode3, choicepgm3),
         choice_4 = paste(schoolcode4, choicepgm4),
         choice_5 = paste(schoolcode5, choicepgm5),
         choice_6 = paste(schoolcode6, choicepgm6)) %>%
  select(choice_1, choice_2, choice_3, choice_4, choice_5, choice_6) %>%
  unlist() %>%
  unique() %>%
  length()

# Missing test score, how many students are missing the test score

datstu %>%
  filter(is.na(score)) %>%
  nrow()

# How many students applied to different programs at the same school

test <- datstu %>%
  pivot_longer(cols = schoolcode1:schoolcode6, values_to = "schoolcode") %>%
  select(X, agey, male, schoolcode) %>%
  group_by(X, schoolcode) %>%
  count()

multiple_apps <- test %>%
  filter(n>1)

multiple_apps

multiple_apps$X %>%
  unique() %>%
  length ()

# How many students applied to fewer than 6 schools

test2 <- datstu %>%
  pivot_longer(cols = choicepgm1:choicepgm6, values_to = "choiceprogram") %>%
  select(X, agey, male, choiceprogram) %>%
  group_by(X, choiceprogram) %>%
  count()

multiple_apps2 <- test2 %>%
  filter(n>1)

multiple_apps2$X %>%
  unique() %>%
  length ()



# Exercise 2

# datstu %>%
  #filter(!is.na(rankplace)) %>%
  #View()

admitted_students <- datstu %>%
  pivot_longer(cols = schoolcode1:schoolcode6, values_to = "schoolcode") %>%
  group_by(X) %>%
  filter(row_number() == rankplace) %>%
  ungroup()


admitted_students %>%
  left_join(datsss, by = c("schoolcode" = "schoolcode")) %>%
  group_by(schoolcode) %>%
  summarize(sssdistrict = last(sssdistrict),
            ssslat = last(ssslat),
            ssslong = last(ssslong),
            cutoff = min(score),
            size = n(),
            ssslong, ssslat,
            cutoff = min(score),
            size = n(),
            quality = mean(score)) %>%
  head(n=20)



# Exercise 3

admitted_students %>%
  left_join(datjss, by = c("jssdistrict" = "jssdistrict")) %>%
  left_join(datsss, by = c("schoolcode" = "schoolcode")) %>%
  mutate(distance = sqrt((69.172*(ssslong - point_x)*cos(point_y/57.3))^2 + 69.172*(ssslat - point_y)^2))%>%
  head(n=20)



# Exercise 4

summary_table <- admitted_students %>%
  left_join(datjss, by = c("jssdistrict" = "jssdistrict")) %>%
  left_join(datsss, by = c("schoolcode" = "schoolcode")) %>%
  mutate(distance = sqrt((69.172*(ssslong - point_x)*cos(point_y/57.3))^2 + 69.172*(ssslat - point_y)^2)) %>%
  group_by(schoolcode) %>%
  summarize(avg_distance = mean(distance, na.rm = T),
            sssdistrict = last(sssdistrict),
            ssslat = last(ssslat),
            ssslong = last(ssslong),
            cutoff = min(score),
            size = n(),
            quality = mean(score)) %>%
  summarize(across(c(avg_distance, cutoff, quality), c(mean, sd)))

colnames(summary_table) <- c("avg_distance", "sd_distance", "avg_cutoff",
                             "sd_cutoff", "avg_quality", "sd_quality")
  



# Exercise 5

set.seed(198)

x_1 <- runif(10000, min = 1, max = 3)
x_2 <- rgamma(10000, shape = 3, scale =2)
x_3 <- rbinom(10000, size = 1, prob = 0.3)
epsilon <- rnorm(10000, mean=2, sd = 1)

Y = 0.5 + 1.2*x_1 - 0.9*x_2 + 0.1*x_3 + epsilon

head(Y)

y_dum <- ifelse(Y > mean(Y),1,0)

head(y_dum)



# Exercise 6

r <- sum( (x_1 - mean(x_1)) * (Y - mean(Y)) ) /
  sqrt( sum( (x_1 - mean(x_1))^2 ) * (sum( (Y - mean(Y))^2 )) )

beta <- function(predictor){
  ols_coefficient <- sum( (predictor - mean(predictor)) * (Y - mean(Y)) )/
    sum( (predictor - mean(predictor))^2 )
  
  return(ols_coefficient)
}


se <- function(predictor){
  var_coefficient <- var(Y)/sum( (predictor - mean(predictor))^2 )
  se_coefficient <- sqrt(var_coefficient)
  return(se_coefficient)
}

se(x_1)
beta(x_1)
# Exercise 7

linear_model <- lm(y_dum ~ x_1 + x_2 + x_3)

broom::tidy(linear_model)
broom::glance(linear_model)

logit_model <- glm(y_dum ~ x_1 + x_2 + x_3, family = "binomial")
broom::tidy(logit_model)
broom::glance(logit_model)

probit_model <- glm(y_dum ~ x_1 + x_2 + x_3, family = binomial(link = "probit"))
broom::tidy(probit_model)
broom::glance(probit_model)

# Examine the p-values and notice that they are < 0.05. Thus, they are significant. We have not investigated if the assumptions
# of linear regression, thus our model may be off. 


# Exercise 8

# Please see Exercise 7 and look at the output of the logit model, that is, a 1 unit increase in x_1 corresponds
# to a 2.5 increase in the log odds of the response

