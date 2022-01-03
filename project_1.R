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

 