## install.packages("pwr")
library(tidyverse) ## readr and dplyr
library(pwr) ## to find minimum n require for stat test

dataset <- read_csv("หวย_R.csv")

## LAST TWO
last_two <- data.frame(Catagories = dataset$last_two)
last_two_count <- table(last_two$Catagories) ## for count each value
last_two_result <- chisq.test(last_two_count) ## chi square test
last_two_result$statistic
ggplot(last_two, mapping = aes(x=last_two$Catagories))+
  geom_bar()
nrow(last_two_count[last_two_count>=5])

w = sqrt(last_two_result$statistic/nrow(last_two)) ## effect size sqrt(x2/N)
df = last_two_result$parameter
sig.level = 0.05
power = 0.8 ## most common value
?pwr.chisq.test
pwr.chisq.test(w=w,df=df,power=power)

## LAST THREE
dataset_new <- dataset %>%
  pivot_longer(cols = "last_three1":"last_three4",
               names_to = "order_last_three",
               values_to = "last_three") 
last_three <- data.frame(Catagories = na.omit(dataset_new$last_three))
ggplot(last_three, mapping = aes(x=last_three$Catagories))+
  geom_bar()
last_three_count <- table(last_three$Catagories) 
last_three_result <- chisq.test(last_three_count) 
nrow(last_three_count[last_three_count>=5])
nrow(last_three_count[last_three_count>=0])

w = sqrt(last_three_result$statistic/nrow(last_three))
df = last_three_result$parameter
sig.level = 0.05
power = 0.8 ## most common value
pwr.chisq.test(w=w,df=df,power=power)

w = sqrt(1012.22/nrow(last_three))
df = 999
sig.level = 0.05
power = 0.8 ## most common value
pwr.chisq.test(w=w,df=df,power=power)

## FIRST THREE
dataset_new <- dataset %>%
  pivot_longer(cols = "first_three1":"first_three2",
               names_to = "order_first_three",
               values_to = "first_three") 
first_three <- data.frame(Catagories = na.omit(dataset_new$first_three))
ggplot(first_three, mapping = aes(x=first_three$Catagories))+
  geom_bar()
first_three_count <- table(first_three$Catagories) 
first_three_result <- chisq.test(first_three_count) 
nrow(first_three_count[first_three_count>=5])
nrow(first_three_count[first_three_count>=0])

w = sqrt(first_three_result$statistic/nrow(first_three))
df = first_three_result$parameter
sig.level = 0.05
power = 0.8 ## most common value
pwr.chisq.test(w=w,df=df,power=power)

w = sqrt(1007.36/nrow(first_three))
df = 999
sig.level = 0.05
power = 0.8 ## most common value
pwr.chisq.test(w=w,df=df,power=power)
