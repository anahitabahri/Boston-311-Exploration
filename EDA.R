library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)

data <- read_csv("../data.csv")
data <- data.frame(data)
data <- unique(data) # remove duplicates, no duplicates, we're fine


# distinct(data, SUBJECT)
# View(data)

ggplot(data, aes(year)) + geom_bar() + facet_wrap(~SUBJECT)

# don't care about 2017, there's not much info there. we may go back and compare jan to jan.
data1 = filter(data,year!=2017, SUBJECT == "Transportation - Traffic Division")

# View(data1)
distinct(data1,CASE_TITLE) # 209 distinct vals!
distinct(data1,REASON) # 9 vals!


ggplot(data1, aes(year)) + geom_bar() + facet_wrap(~REASON) + 
  labs(title="Count of Transportation Reasons / Year", x ="Year", y = "Count")
# Let's focus on enforcement & abandoned vehicles, and potentially signs & signals!

data2 = filter(data1, REASON %in% c("Enforcement & Abandoned Vehicles","Signs & Signals"))

distinct(data2,CASE_TITLE) # 153 distinct vals!
# View(data2)

library(ggmap)

boston_map <- get_map(location = "Boston", zoom = 13)

ggmap(boston_map, extent = "device") + geom_density2d(data = data2, aes(x = LONGITUDE, y = LATITUDE), size = 0.3) + 
  stat_density2d(data = data2, 
                 aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)

# all in a particular area, not very helpful location data here. 

data3 = filter(data2, OnTime_Status != "NA")
qplot(year, data=data3, geom="bar", fill=OnTime_Status)  + 
  labs(title="Count of Enforcement & Abandoned Vehicles, Signs & Signals", 
       x ="Year", y = "Count")


library(reshape)
pivot_table <- cast(data3, year ~ OnTime_Status)
pt <- data.frame(pivot_table)
pt

pt <- mutate(pt,TOTAL=OVERDUE + ONTIME)
pt <- mutate(pt,PCT_OVERDUE=OVERDUE / TOTAL)

View(pt)

(44278 - 22278)/44278

# although 2016 has lowest percentage of overdue cases, 
# total # of cases has grown by 50% since 2015! That's insane!

pt2 <- cast(data3, year ~ REASON)
pt2 <- data.frame(pt2)
pt2 <- mutate(pt2,TOTAL=Enforcement...Abandoned.Vehicles + Signs...Signals)
pt2 <- mutate(pt2,pct_enf=Enforcement...Abandoned.Vehicles / TOTAL)
pt2
View(pt2)

(0.6732237 - 0.4898106)/0.6732237
# pct grown of enforcement/abandoned vehicles is close to 30%!

# let's look at just enforcement & abandoned vehicles
data4 = filter(data3,REASON == "Enforcement & Abandoned Vehicles")

# View(data4)
distinct(data4,TYPE)

qplot(year, data=data4, geom="bar", fill=TYPE) + 
  labs(title="Count of Enforcement & Abandoned Vehicles", 
       x ="Year", y = "Count")

# ok well how is this going in jan 2017?

data_2017 <- filter(data,year == 2017)

ggplot(data_2017, aes(SUBJECT)) + geom_bar() + coord_flip()  + 
  labs(title="Count of Subjects in Jan 2017", 
       x ="Subject", y = "Count")

data_2017 <- filter(data_2017,SUBJECT == "Transportation - Traffic Division")


ggplot(data_2017, aes(REASON)) + geom_bar() + coord_flip()  + 
  labs(title="Count of Transporation Reasons in Jan 2017", 
       x ="Transporation Reason", y = "Count")

data_2017 = filter(data_2017, REASON %in% c("Enforcement & Abandoned Vehicles","Signs & Signals"))
ggplot(data_2017, aes(TYPE)) + geom_bar() + coord_flip()  + 
  labs(title="Count of Transporation Types in Jan 2017", 
       x ="Transporation Types", y = "Count")

# PARKING ENFORCEMENT ONCE AGAIN TAKES THE LEAD!


