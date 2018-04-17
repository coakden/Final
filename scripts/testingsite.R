library(tidyverse)
library(readr)

dddata <- read_csv("data_raw/2016_dddata.csv")

head(dddata)


ddclean <- dddata %>%
  select(., - CountType) %>%
  gather(., January, February, March, April, May, June, July, August, September,
         October, November, December, key = "Month", value = "Count") %>%
  spread(., key = "Category", value = "Count") %>%
  select(ddclean, Deaths = deaths, Cases = cases, Injuries = injuries)

ddclean <- rename(ddclean, Deaths = deaths, Cases = cases, Injuries = injuries)

head(ddclean)
tail(ddclean)


write_csv(ddclean, "data_tidy/ddclean.csv")
