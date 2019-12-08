title: "Introduction on Market Basket Analysis - Association Rules"

#Introduction

Market Basket Analysis is one of the key techniques used by the large retailers that uncovers associations between items by looking for combinations of items that occur together frequently in transactions. In other words, it allows the retailers to identify relationships between the items that people buy.

Association Rules is widely used to analyze retail basket or transaction data, is intended to identify strong rules discovered in transaction data using some measures of interestingness, based on the concept of strong rules.

## An Example of Association Rules

* Assume there are 100 customers
* 10 out of them bought milk, 8 bought butter and 6 bought both of them. 
* bought milk => bought butter
* Support = P(Milk & Butter) = 6/100 = 0.06
* confidence = support/P(Butter) = 0.06/0.08 = 0.75
* lift = confidence/P(Milk) = 0.75/0.10 = 7.5

## Load the packages 

# install.packages('readxl')
# install.packages('lubridate')
# install.packages('tidyverse')
# install.packages('knitr')
# install.packages('arules')
# install.packages('arulesViz')


library(arulesViz)
library(arules)
library(knitr)
library(readxl)
library(lubridate)
library(ggplot2)
library(dplyr)

## Data preprocessing and exploring

retail<-read_excel('retail.xlsx')
retail<-read_excel(file.choose())
retail2<-retail
table(is.na(retail))

retail<-retail[complete.cases(retail),]
table(is.na(retail))

str(retail)
summary(retail)


retail$Description<-as.factor(retail$Description)
retail$Date<-as.Date(retail$InvoiceDate)
retail$Time <- format(retail$InvoiceDate,"%H:%M:%S")
retail$Country<-as.factor(retail$Country)

summary(retail)

## What time do people often purchase online

we need to extract "hour" from the time column

retail$Time<-as.factor(retail$Time)
a<-hms(retail$Time)
retail$Time<-hour(a)

retail$month<-format(retail$Date,"%m")
table(retail$month)

ggplot(retail,aes(x=month))+
  geom_bar(fill="indianred")+
  ggtitle("Transactions across the year")+
  xlab("Month")+
  ylab("No. of transactions")


ggplot(retail,aes(x=Time))+
  geom_bar(fill="indianred")+
  ggtitle("Transcations across the day")+
  xlab("Time")+
  ylab("No. of transactions")

There is a clear effect of hour of day on order volume. Most orders happened between 11:00-15:00.

items<-retail %>%
  group_by(InvoiceNo) %>%
  summarise(total=n())

ggplot(items,aes(x=total))+
  geom_histogram(fill="indianred", binwidth = 1)+
  geom_rug()+
  coord_cartesian(xlim=c(0,80))+
  ggtitle("No. of transactions with different basket sizes")+
  xlab("Basket size")+
  ylab("No of transactions ")

## Top 10 best sellers

top_items<-retail %>%
  group_by(Description) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

summary(retail)
top_items<-head(top_items,10)

ggplot(top_items,aes(x=reorder(Description,count), y=count))+
  geom_bar(stat="identity",fill="indian red")+
  coord_flip()+
  scale_y_continuous(limits = c(0,3000))+
  ggtitle("Frequency plot of top 10 Items")+
  xlab("Description of item")+
  ylab("Count")

## Association rules for online retailer

Before using any rule mining algorithm, we need to transform data from the data frame format into transactions such that we have all the items bought together in one row. For example, this is the format we need:
  
retail_sorted <- retail[order(retail$CustomerID),]
library(plyr)
itemList <- ddply(retail,c("CustomerID","Date"), 
                  function(df1)paste(df1$Description, 
                                     collapse = ","))

The function ddply() accepts a data frame, splits it into pieces based on one or more factors, computes on the pieces, then returns the results as a data frame. We use "," to separate different items. 

We only need item transactions, so, remove customerID and Date columns.


itemList$CustomerID <- NULL
itemList$Date <- NULL
colnames(itemList) <- c("items")

#Write the data from to a csv file

write.csv(itemList,"market_basket.csv", quote = FALSE, row.names = TRUE)

Now we have our transaction dataset shows the matrix of items being bought together. We don't actually see how often they are bought together, we don't see rules either. But we are going to find out. 

tr <- read.transactions('market_basket.csv', format = 'basket', sep=',',skip=1)
tr
summary(tr)

Let's have a look item freqnency plot

itemFrequencyPlot(tr, topN=20, type='absolute')

#Create some rules


* We use the Apriori algorithm in arules library to mine frequent itemsets and association rules. The algorithm employs level-wise search for frequent itemsets.

* We pass supp=0.001 and conf=0.8 to return all the rules have a support of at least 0.1% and confidence of at least 80%. 

* We sort the rules by decreasing confidence. 

* Have a look the summary of the rules. 


rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8, target='rules'))
df_basket <- as(rules,"data.frame")
rules <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules)

I don't want to print them all, let's inspect top 10.

inspect(rules[1:10])

* 100% customers who bought "WOBBLY CHICKEN" end up bought "DECORATION" as well. 

* 100% customers who bought "BLACK TEA" end up bought "SUGAR JAR" as well. 

And plot these top 10 rules.

topRules <- rules[1:10]
plot(topRules)

plot(topRules, method="graph")

plot(topRules, method = "grouped")