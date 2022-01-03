# Form a new dataframe only with rows containing data from the year 2019
census_2019 = census_2019_1999[census_2019_1999$year == "2019",]

# A summary of each variable including the minimum and maximum values,
# the quartiles, the mean and the standard deviation

summary(census_2019$total.fertility.rate.per.woman)
sd(census_2019$total.fertility.rate.per.woman)

summary(census_2019$life.expectancy.both.sexes)
sd(census_2019$life.expectancy.both.sexes)

summary(census_2019$life.expectancy.male)
sd(census_2019$life.expectancy.male)

summary(census_2019$life.expectancy.female)
sd(census_2019$life.expectancy.female)

#Load library ggplot2 for plotting graphs

library(ggplot2)

# Manually Order the regions, grouping the continents together

census_2019$region <- factor(census_2019$region,
                             levels = c("Australia/New Zealand", 
                                        "Melanesia", 
                                        "Micronesia", 
                                        "Polynesia",
                                        "Caribbean",
                                        "Central America",
                                        "Northern America",
                                        "South America",
                                        "Eastern Africa",
                                        "Middle Africa",
                                        "Northern Africa",
                                        "Southern Africa",
                                        "Western Africa",
                                        "Eastern Asia",
                                        "South-Central Asia",
                                        "South-Eastern Asia",
                                        "Western Asia",
                                        "Eastern Europe",
                                        "Northern Europe",
                                        "Southern Europe",
                                        "Western Europe"))

median(count(census_2019$region))
#Plot the frequency distributions for each variable

ggplot(data = census_2019) +
  aes(census_2019$region, fill = region) +
  geom_bar() + 
  theme(axis.text.x = element_text(angle = -90, vjust = 0.4, hjust=0)) +
  xlab("Geographical Region")  + 
  ylab("Number of Countries") +
  scale_fill_manual(values = c("Australia/New Zealand" = "#FFFF00", 
                               "Melanesia" = "#FFFF00", 
                               "Micronesia" = "#FFFF00", 
                               "Polynesia" = "#FFFF00",
                               "Caribbean" = "#FF0000",
                               "Central America" = "#FF0000",
                               "Northern America" = "#FF0000",
                               "South America" = "#FF0000",
                               "Eastern Africa" = "#008000",
                               "Middle Africa" = "#008000",
                               "Northern Africa" = "#008000",
                               "Southern Africa" = "#008000",
                               "Western Africa" = "#008000",
                               "Eastern Asia" = "#FF00FF",
                               "South-Central Asia" = "#FF00FF",
                               "South-Eastern Asia" = "#FF00FF",
                               "Western Asia" = "#FF00FF",
                               "Eastern Europe" = "#0000FF",
                               "Northern Europe" = "#0000FF",
                               "Southern Europe" = "#0000FF",
                               "Western Europe" = "#0000FF"))

# The green line shows the median and the blue line shows the mean

ggplot(data = census_2019) +
  aes(total.fertility.rate.per.woman) +
  geom_histogram(breaks = seq(0, 8, by=0.25), col = "red") +
  geom_vline(aes(xintercept=mean(total.fertility.rate.per.woman)), color="blue", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(total.fertility.rate.per.woman)), color="green", linetype="dashed", size=1) +
  xlab("Total Fertility Rate per Woman") + 
  ylab("Number of Countries")

ggplot(data = census_2019) +
  aes(life.expectancy.both.sexes) +
  geom_histogram(breaks = seq(45, 95, by=5), col = "red") +
  geom_vline(aes(xintercept=mean(life.expectancy.both.sexes)), color="blue", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(life.expectancy.both.sexes)), color="green", linetype="dashed", size=1) +
  xlab("Life Expectancy Both Sexes") + 
  ylab("Number of Countries")

ggplot(data = census_2019) +
  aes(life.expectancy.male) +
  geom_histogram(breaks = seq(45, 95, by=5), col = "red") +
  geom_vline(aes(xintercept=mean(life.expectancy.male)), color="blue", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(life.expectancy.male)), color="green", linetype="dashed", size=1) +
  xlab("Life Expectancy Male") + 
  ylab("Number of Countries")

ggplot(data = census_2019) +
  aes(life.expectancy.female) +
  geom_histogram(breaks = seq(45, 95, by=5), col = "red") +
  geom_vline(aes(xintercept=mean(life.expectancy.female)), color="blue", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=median(life.expectancy.female)), color="green", linetype="dashed", size=1) +
  xlab("Life Expectancy Female") + 
  ylab("Number of Countries")


#User defined functions

get_r_squared <- function(x,y){
  correlation <- round(cor(x, y),3)
  string <- paste("r^2 = ",toString(correlation))
  return(string)
}

get_x_label <- function(x){
  return(max(x)-(max(x)*.05) )
}

get_y_label <- function(y){
  return(max(y))
}


plot_correlation <- function(x, y, color, x_label, y_label, title){
  qplot(x,
        y,
        xlab = x_label,
        ylab = y_label,
        method = "lm",
        se = FALSE,
        main = title,
        colour = color,  
        data = census) + annotate("text", x= get_x_label(x), 
                                  y = get_y_label(y), 
                                  label = get_r_squared(x,y))
  
} 

#Include ggplot2 library

library(ggplot2)

# Rearrange regions and group them into continents

unique(census_2019_1999$country)
nrow(census_2019_1999)

census_2019_1999$region <- factor(census_2019_1999$region,
                                  levels = c("Australia/New Zealand", 
                                             "Melanesia", 
                                             "Micronesia", 
                                             "Polynesia",
                                             
                                             "Northern America",
                                             "Central America",
                                             "South America",
                                             "Caribbean",
                                             
                                             "Southern Africa",
                                             "Northern Africa",
                                             "Middle Africa",
                                             "Eastern Africa",
                                             "Western Africa",
                                             
                                             "Eastern Asia",
                                             "South-Eastern Asia",
                                             "South-Central Asia",
                                             "Western Asia",
                                             
                                             "Western Europe",
                                             "Eastern Europe",
                                             "Northern Europe",
                                             "Southern Europe"
                                  ))



# Continents
census_2019_1999$continent <-ifelse( census_2019_1999$region == "Australia/New Zealand",   'Oceania', 
                                     ifelse( census_2019_1999$region ==       "Caribbean" , 'America',
                                             ifelse( census_2019_1999$region == "Middle Africa" , 'Africa', 
                                                     ifelse( census_2019_1999$region ==  "Eastern Africa", "Africa",
                                                             ifelse( census_2019_1999$region ==     "Melanesia" , "Oceania",
                                                                     ifelse( census_2019_1999$region ==      "South America" , "America",
                                                                             ifelse( census_2019_1999$region ==      "Western Africa" , "Africa",
                                                                                     ifelse( census_2019_1999$region ==       "Northern America" , "America", 
                                                                                             ifelse( census_2019_1999$region ==     "South-Central Asia" , "Asia", 
                                                                                                     ifelse( census_2019_1999$region ==   "Southern Africa" , "Africa",
                                                                                                             ifelse( census_2019_1999$region ==   "Western Asia" , "Asia", 
                                                                                                                     ifelse( census_2019_1999$region ==    "Central America" , "America", 
                                                                                                                             ifelse( census_2019_1999$region ==    "Eastern Asia" , "Asia",
                                                                                                                                     ifelse( census_2019_1999$region ==     "Eastern Europe" , "Europe",
                                                                                                                                             ifelse( census_2019_1999$region ==     "Micronesia" , "Oceania",
                                                                                                                                                     ifelse( census_2019_1999$region ==      "Northern Africa" , "Africa",
                                                                                                                                                             ifelse( census_2019_1999$region ==      "Northern Europe" ,"Europe", 
                                                                                                                                                                     ifelse( census_2019_1999$region ==  "Polynesia" ,"Oceania",
                                                                                                                                                                             ifelse( census_2019_1999$region ==    "South-Eastern Asia" , "Asia",
                                                                                                                                                                                     ifelse( census_2019_1999$region ==   "Southern Europe" , "Europe", 
                                                                                                                                                                                             ifelse( census_2019_1999$region == "Western Europe" ,"Europe"
                                                                                                                                                                                                     ,"NA")))))))
                                                                                                                                     ))))))))))))))    




census_2019_1999$region <- factor(census_2019_1999$region,
                                  levels = c("Australia/New Zealand", 
                                             "Melanesia", 
                                             "Micronesia", 
                                             "Polynesia",
                                             "Caribbean",
                                             "Central America",
                                             "Northern America",
                                             "South America",
                                             "Eastern Africa",
                                             "Middle Africa",
                                             "Northern Africa",
                                             "Southern Africa",
                                             "Western Africa",
                                             "Eastern Asia",
                                             "South-Central Asia",
                                             "South-Eastern Asia",
                                             "Western Asia",
                                             "Eastern Europe",
                                             "Northern Europe",
                                             "Southern Europe",
                                             "Western Europe"))


census <- census_2019_1999[census_2019_1999$year == "2019",]
census_99  <- census_2019_1999[census_2019_1999$year == "1999",]

ggplot(data = census) +
  aes(region) +
  geom_bar(aes(fill=continent)) + 
  theme(axis.text.x = element_text(angle = -90, vjust = 0.4, hjust=0)) +
  xlab("Geographical Region")  + 
  ylab("Number of Countries")

unique(census_2019_1999[c("Continent", "region")])


nrow(census_2019_1999)
nrow(census)
nrow(census_99)

#check missing data from 1999
apply(is.na(census),2,sum)
summary(census)

head(census)
nrow(census)
#get rid of cat variables
census_numeric <- census[c(-1,-2,-3)]
head(census_numeric)

#check data type
str(census_numeric)

#heatmap created to get an overview of correlatin behaviour
cormat <-cor(x = as.matrix(census_numeric), method = "spearman", use = "pairwise.complete.obs")
s <- signif(cormat)
col<- colorRampPalette(c("blue", "white", "red", "green"))(800)
heatmap(s)


#correlations graphs
corr_g1 <-pairs(census_numeric, pch = 19)
corr_g1

corr_g2 <- ggpairs(census_numeric, stars =FALSE)
corr_g2


#individual correlation graphs

# fertility rate ~ female life expectancy


plot_correlation(census$total.fertility.rate.per.woman,
                 census$life.expectancy.female, 
                 census$continent,
                 "Fertility Rate", 
                 "Life Expectancy Female", 
                 "")

# fertility rate ~ male expectancy
plot_correlation(census$total.fertility.rate.per.woman,
                 census$life.expectancy.male, 
                 census$continent,
                 "Fertility Rate", 
                 "Life Expectancy Male", 
                 "")

# fertility rate ~  life expectancy
plot_correlation(census$total.fertility.rate.per.woman,
                 census$life.expectancy.both.sexes, 
                 census$continent,
                 "Fertility Rate", 
                 "Life Expectancy Both Sexes", 
                 "")

# life expectancy ~  life expectancy female
plot_correlation(census$life.expectancy.both.sexes,
                 census$life.expectancy.female, 
                 census$continent,
                 "Life Expectancy Both Sexes", 
                 "Life Expectancy Female", 
                 "")

# life expectancy ~  life expectancy male
plot_correlation(census$life.expectancy.both.sexes,
                 census$life.expectancy.male, 
                 census$continent,
                 "Life Expectancy Both Sexes", 
                 "Life Expectancy Male", 
                 "")

# life expectancy female ~  life expectancy male
plot_correlation(census$life.expectancy.female,
                 census$life.expectancy.male, 
                 census$continent,
                 "Life Expectancy Female", 
                 "Life Expectancy Male", 
                 "")




life.expectancy.both.sexes_boxplot = ggplot(census, aes(x = life.expectancy.both.sexes, y = continent, fill=continent)) + geom_boxplot()
life.expectancy.both.sexes_boxplot +
  xlab("Life Expectancy Both Sexes") + ylab("Continents")

total.fertility.rate.per.woman_boxplot = ggplot(census, aes(x = total.fertility.rate.per.woman, y = region, fill=continent)) + geom_boxplot()
total.fertility.rate.per.woman_boxplot + 
  xlab("Total Fertility Rate per Woman") + ylab("Continents")

census_numeric <- census[c(-1,-2,-3)]
census_numeric_99 <- census_99[c(-1,-2,-3)]
census_numeric_99_19  <-census_2019_1999[c(-1,-2,-3)]
summary(census_numeric)
summary(census_numeric_99)
summary(census_numeric_99_19)


ggplot(census) + 
  aes(x=census$life.expectancy.both.sexes,
      y=census$total.fertility.rate.per.woman) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  ylab("Total Fertility Rate per woman") + 
  xlab("Life Expectancy both sexes")

ggplot(census) + 
  aes(x=census$life.expectancy.female,
      y=census$life.expectancy.male) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  xlab("Life Expectancy Female") + 
  ylab("Life Expectancy Male")

cor(census$life.expectancy.female,census$life.expectancy.male)
cor(census$life.expectancy.both.sexes,census$total.fertility.rate.per.woman)


#install.packages("plyr") # This package is useful as a data manipulator 
#install.packages("ggplot2") #This package is useful for visualizing data
library(plyr)    
library(ggplot2)

census_2019_1999 = read.csv('./census_2019_1999.csv')
census_2019 = census_2019_1999[census_2019_1999$year == "2019",]

census_2019$region<-as.factor(census_2019$region)

str(census_2019)

with(census_2019, summary(region))

with(census_2019, summary(life.expectancy.both.sexes))

with(census_2019, summary(life.expectancy.male))

with(census_2019, summary(life.expectancy.female))

with(census_2019, summary(total.fertility.rate.per.woman))

life.expectancy.both.sexes_means = with(census_2019, by(life.expectancy.both.sexes, region, mean))

life.expectancy.both.sexes_boxplot = ggplot(census_2019, aes(x = life.expectancy.both.sexes, y = region)) + geom_boxplot()
life.expectancy.both.sexes_boxplot +
  xlab("Life Expectancy Both Sexes") + ylab("Regions")

life.expectancy.male_boxplot = ggplot(census_2019, aes(x = life.expectancy.male, y = region)) + geom_boxplot()
life.expectancy.male_boxplot + 
  xlab("Male Life Expectancy") + ylab("Regions")

life.expectancy.female_boxplot = ggplot(census_2019, aes(x = life.expectancy.female, y = region)) + geom_boxplot()
life.expectancy.female_boxplot + 
  xlab("Female Life Expectancy") + ylab("Regions")

total.fertility.rate.per.woman_boxplot = ggplot(census_2019, aes(x = total.fertility.rate.per.woman, y = region)) + geom_boxplot()
total.fertility.rate.per.woman_boxplot +
  xlab("Total Fertility Rate per Woman") + ylab("Regions")




# A summary of each variable including the minimum and maximum values,
# the quartiles, the mean and the standard deviation

summary(census_2019$total.fertility.rate.per.woman)
sd(census_2019$total.fertility.rate.per.woman)

summary(census_2019$life.expectancy.both.sexes)
sd(census_2019$life.expectancy.both.sexes)

summary(census_2019$life.expectancy.male)
sd(census_2019$life.expectancy.male)

summary(census_2019$life.expectancy.female)
sd(census_2019$life.expectancy.female)

#Load library ggplot2 for plotting graphs

library(ggplot2)

#Plot the frequency distributions for each variable

ggplot(data = census_2019) +
  aes(census_2019$region) +
  geom_bar() + 
  theme(axis.text.x = element_text(angle = -90, vjust = 0.4, hjust=0)) +
  xlab("Geographical Region")  + 
  ylab("Number of Countries")

ggplot(data = census_2019) +
  aes(total.fertility.rate.per.woman) +
  geom_histogram(breaks = seq(0, 8, by=0.25), col = "red") +
  xlab("Total Fertility Rate per Woman") + 
  ylab("Number of Countries")

ggplot(data = census_2019) +
  aes(life.expectancy.both.sexes) +
  geom_histogram(breaks = seq(45, 95, by=5), col = "red") +
  xlab("Life Expectancy Both Sexes") + 
  ylab("Number of Countries")

ggplot(data = census_2019) +
  aes(life.expectancy.male) +
  geom_histogram(breaks = seq(45, 95, by=5), col = "red") +
  xlab("Life Expectancy Male") + 
  ylab("Number of Countries")

ggplot(data = census_2019) +
  aes(life.expectancy.female) +
  geom_histogram(breaks = seq(45, 95, by=5), col = "red") +
  xlab("Life Expectancy Female") + 
  ylab("Number of Countries")


##### TASK 4 ###### By Moaz
# How have the values of the variables changed over the last 20 years, i.e. comparing
# 1999 with 2019?
ggplot(data=census_2019_1999, 
       aes(x=factor(year), y=total.fertility.rate.per.woman)) +
  geom_boxplot(outlier.colour = 'red') +
  xlab("Year")+
  ylab("Fertility rate per woman")

ggplot(data=census_2019_1999, 
       aes(x=factor(year), y=life.expectancy.both.sexes)) +
  geom_boxplot(outlier.colour = 'red') +
  xlab("Year")+
  ylab("Life expectancy of both sexes")

ggplot(data=census_2019_1999, 
       aes(x=factor(year), y=life.expectancy.male)) +
  geom_boxplot(outlier.colour = 'red') +
  xlab("Year")+
  ylab("Life expectancy of males")

ggplot(data=census_2019_1999, 
       aes(x=factor(year), y=life.expectancy.female)) +
  geom_boxplot(outlier.colour = 'red') +
  xlab("Year")+
  ylab("Life expectancy of females")

census_1999 = census_2019_1999[census_2019_1999$year == "1999",]

summary(census_1999$life.expectancy.both.sexes)
summary(census_2019$life.expectancy.both.sexes)

