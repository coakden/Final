library(tidyverse)
library(readr)

dddata <- read_csv("data_raw/2016_dddata.csv")                #import data
  
head(dddata)                                                  #take a peek


ddclean <- dddata %>%                                         #spring cleaning
  select(., - CountType) %>%                                  #not sure why this column exists
  gather(., January, February, March, April, May, June,       
         July, August, September, October, November, December,#get all the months in a single column 
         key = "month", value = "count") %>%                  
  spread(., key = "Category", value = "count") %>%               #Category column actually 3 variables
filter(., ABV != "unspecified")

ddclean <- rename(ddclean, timeofday = TimeOfDay, abv = ABV)  #don't like mix of upper/lowercase

head(ddclean)                                                 #much better!
tail(ddclean)                                                  

ddnoUn <- ddclean %>%
  filter(., abv != "unspecified")

ddnoUn <- ddclean %>%
  filter(., abv == ">0.35%")

median(ddclean$cases, na.rm = TRUE)

write_csv(ddclean, "data_tidy/ddclean.csv") #write tidy data to new .csv

mean(ddclean$cases, na.rm = TRUE, trim = .1)

ggplot(ddclean, aes(x = month, y = log(cases)))+
  geom_boxplot(na.rm = TRUE, color = "black", fill = "lightsteelblue3")+
  geom_hline(yintercept = 3, lty = 2, size = .5, color = "orangered") +
  coord_flip()

glm(cases ~ timeofday, 
    data = ddnoUn, 
    family = poisson(link = "log"))
  
ggplot(ddclean, aes(x = injuries, y = cases), na.rm = T) +
  geom_point() +
  geom_smooth(method = lm)




as.factor(ddclean$timeofday)

is.integer(ddclean$cases)

is.ordered(ddclean$timeofday)

factime <- as.factor(ddclean$timeofday)  

d2 <- gather(ddclean, timeofday, cases, day:night, factor_key=T)

str(ddclean)

glm(timeofday ~ as.numeric(factor((cases))), 
    data = ddclean, 
    family = binomial(link = "logit"))

ddclean %>% 
  group_by(., timeofday) %>% 
  summarize(CasesMean = mean(cases, na.rm = T), CasesSd = sd(cases, na.rm = T), 
            InjuriesMean = mean(injuries, na.rm = T), InjuriesSd = sd(injuries, na.rm = T)) %>% 
  knitr::kable(., format = 'markdown')

mean(ddclean$cases, na.rm = T)

ddclean %>%
  mutate(., timeofday_n = as.numeric(as.factor(timeofday)) - 1) %>%
  ggplot(., aes(x = cases, y = timeofday_n)) + 
  geom_point() + 
  geom_smooth(method = "glm", method.args = list(family = "binomial"), na.rm = T)

ddclean %>%
  mutate(., timeofday_n = as.numeric(as.factor(timeofday)) -1) %>%
  ggplot(., aes(x = cases, y = timeofday_n)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), na.rm = T)

    
highabv <- ddclean %>%
  filter(., abv == ">0.35%") 
otherabv <- ddclean %>%
  filter(., abv != ">0.35%" & abv != "unmeasured" & abv != "noncompliant")

sum(highabv$injuries, na.rm = T)/sum(highabv$cases, na.rm = T)

sum(otherabv$injuries, na.rm = T)/sum(otherabv$cases, na.rm = T)

sum(otherabv$injuries, na.rm = T)

sum(otherabv$cases, na.rm = T)

  (try$injuries/try$cases)

highsum <- sum(highabv$injuries, na.rm = T)/sum(highabv$cases, na.rm = T)
lowsum <- sum(otherabv$injuries, na.rm = T)/sum(otherabv$cases, na.rm = T) 


higherabv <- ddclean %>%
  filter(., abv == ">0.35%" | abv == "0.30%~0.34%") 
lowerabv <- ddclean %>%
  filter(., abv != ">0.35%" & abv != "unmeasured" & abv != "noncompliant" & abv != "0.30%~0.34%")

sum(higherabv$injuries, na.rm = T)/sum(higherabv$cases, na.rm = T)
sum(lowerabv$injuries, na.rm = T)/sum(lowerabv$cases, na.rm = T)

order(ddclean$month)

onlynum <- ddclean %>%
  filter(., abv != "unmeasured" & abv != "noncompliant")

ggplot(onlynum, aes(x = abv, y = log((injuries/cases)))) +
  geom_boxplot(na.rm = TRUE, color = "black", fill = "lightsteelblue3")+
  geom_hline(yintercept = 3, lty = 2, size = .5, color = "orangered")+
  coord_flip() 

a <- as.numeric(as.factor(ddclean$abv))

splitAt <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos)))

splitAt(a, c(25))

ddclean %>%
  mutate(., abv_n = as.numeric(as.factor(abv)) -1) %>%
    ggplot(., aes(x = log(injuries), y = abv_n)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), na.rm = T)

as.numeric(as.factor(factor(ddclean$abv, c(">0.35%", "0.05%~0.09%", "0.10%~0.14%", "0.15%~0.19%", "0.20%~0.24%
", "0.25%~0.29%", "0.30%~0.34%", "noncompliant", "unmeasured"), labels = c(1, 0, 0, 0, 0, 0, 0, 0, 0))))


ggplot(onlynum, aes(x = abv, y = log(injuries)))+
  geom_point(na.rm = TRUE, color = "black", fill = "lightsteelblue3")

