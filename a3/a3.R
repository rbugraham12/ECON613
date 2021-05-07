setwd("~/Desktop")
# import csv data
df <- read.csv("crime_long.csv")
View(df)
str(df)
table(df$district)

# get the total crime per month

sumdata <- aggregate(crimes ~ crime_month, data=df, sum)
View(sumdata)

# data goes from 2002-Jan to 2019-Dec
#plot(sumdata$crime_month, sumdata$crimes)
# create a time series object and time series plot

crimedta <- ts(sumdata[,c("crimes")], start=c(2002,1), frequency=12)
plot(crimedta, type="l")

# import csv data
df2 <- read.csv("population.csv")
View(df2)
table(df2$district)
table(df2$period)
df2$crime_month <- df2$month

# merge the population data with crime data using same district and month
library(dplyr)
mergedta <- left_join(df2, df, by=c("district","crime_month"))
View(mergedta)

# Panel data by month and by district

# Total crimes per resident

dfmerge <- mergedta %>% group_by(district, crime_month) %>% mutate(totalcrime = sum(crimes))

View(dfmerge)
str(dfmerge)

dfmerge$crime_per_resident <- dfmerge$totalcrime/dfmerge$tot_pop

#Share of black, Hispanic, and white residents

dfmerge$share_black_resident <- dfmerge$tot_black/dfmerge$tot_pop

dfmerge$share_white_resident <- dfmerge$tot_white/dfmerge$tot_pop

dfmerge$share_hisp_resident <- dfmerge$tot_hisp/dfmerge$tot_pop



dfmerge2 <- dfmerge %>% group_by(district, crime_month, crime_type) %>% mutate(totalcrime_type = sum(crimes))
View(dfmerge2)

table(dfmerge2$crime_type)

#Property crimes per resident

# keep property only for totalcrime_type
dfmerge2pp <- dfmerge2[dfmerge2$crime_type == "property", ]
View(dfmerge2pp)

# rename the variable
dfmerge2pp$ppcrime_per_resident <- dfmerge2pp$totalcrime_type/dfmerge2pp$tot_pop

dfmerge2pp1 <- dfmerge2pp[, c(1,3,21)]
dfmerge2pp1$crime_month <- dfmerge2pp1$month
dfmerge2pp2 <- dfmerge2pp1[, c(2:4)]

View(dfmerge2pp2)

# merge back to original data

dfmerge3 <- left_join(dfmerge, dfmerge2pp2, by=c("district","crime_month"))
View(dfmerge3)

#Median income
#p50_inc

#Violent crimes per resident

# keep violent crimes only for totalcrime_type
dfmerge2vi <- dfmerge2[dfmerge2$crime_type == "violent", ]

# rename the variable
dfmerge2vi$vicrime_per_resident <- dfmerge2vi$totalcrime_type/dfmerge2vi$tot_pop
View(dfmerge2vi)
dfmerge2vi1 <- dfmerge2vi[, c(1,3,21)]
dfmerge2vi1$crime_month <- dfmerge2vi1$month
dfmerge2vi2 <- dfmerge2vi1[, c(2:4)]

View(dfmerge2vi2)

# merge back to original data

dfmerge4 <- left_join(dfmerge3, dfmerge2vi2, by=c("district","crime_month"))
View(dfmerge4)

#dfmerge4 is the final dataset in exercise 2

########################################################################
df3 <- read.csv("officers.csv")
View(df3)

df3$crime_month <- df3$month
df3$district <- df3$unit

dfsubset <- dfmerge4[,c(1:9, 12:21)]

mergedta3 <- left_join(df3, dfsubset, by=c("district","crime_month"))
View(mergedta3)

unique_mergedta3 <- mergedta3[!duplicated(mergedta3$totalcrime),]
View(unique_mergedta3)

str(mergedta3)
#unique_mergedta3 is the final dataset in exercise 3
################################
# Exercise 3

model <- lm(arrest ~ tenure + crime_per_resident  + share_black_resident + share_white_resident + share_hisp_resident + p50_inc, data= unique_mergedta3)
summary(model)


########################################################################

# Exercise 4

table(unique_mergedta3$district)

unique_mergedta3$year <- substr(unique_mergedta3$crime_month,1,4)
unique_mergedta3$month <- substr(unique_mergedta3$crime_month,6,7)

## Running model with year, month and district fixed effects - dummy variables
model4 <- lm(arrest ~ tenure + crime_per_resident  + share_black_resident + share_white_resident + share_hisp_resident + p50_inc + factor(district) + factor(year) + factor(month), data= unique_mergedta3)
summary(model4)

########################################################################

# Exercise 5

# Fixed effects model
library(plm)

#within estimators (FE)
model5 <- plm(arrest ~ tenure + crime_per_resident  + share_black_resident + share_white_resident + share_hisp_resident + p50_inc + factor(year) + factor(month), data= unique_mergedta3, index = c("district", "crime_month"), model = "within")
summary(model5)

# between estimators (RE)
model6 <- plm(arrest ~ tenure + crime_per_resident  + share_black_resident + share_white_resident + share_hisp_resident + p50_inc, data= unique_mergedta3, index = c("district", "crime_month"), model = "random")
summary(model6)











