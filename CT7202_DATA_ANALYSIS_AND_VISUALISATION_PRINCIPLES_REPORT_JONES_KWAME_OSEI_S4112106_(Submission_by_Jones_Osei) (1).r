############################################################# Install Packages ##############################################################################

install.packages("TH.data") 
install.packages("factoextra")
install.packages("tidyverse") 
install.packages("party")
install.packages("ggplot2") 
install.packages("gganimate")
install.packages("gifski")
install.packages("av")
install.packages("ggmap")  # cite it if used 
install.packages("lubridate") 
install.packages("Hmisc") 
install.packages("devtools") 
install.packages("skimr") 
install.packages("purrr") 
install.packages("fs") 
install.packages("DataExplorer") 
install.packages("GGally") 
install.packages("inspectdf") 
install.packages("GGally") 
install.packages("hrbrthemes")
install.packages("devtools")
install.packages("caret")
install.packages("zoo")
install.packages("Metrics")
install.packages("plotly")
devtools::install_github("ropensci/visdat") 
install.packages("rpart")
install.packages("class")
install.packages("tree")
install.packages("rattle")

############################################################# Load Packages ###############################################################

library(lubridate)
library(party)
library(factoextra)
library(pastecs)
library(Hmisc)
library(skimr)
library(devtools)
library(visdat)
library(ggplot2)
library(DataExplorer)
library(inspectdf)
library(GGally)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(hrbrthemes)
library(corrplot)
library(broom)
library(ggpubr)
library(caret)
library(plotly)
library(ggmap)
library(rstudioapi)
library(grid)
library(mvtnorm)
library(modeltools)
library(stats4)
library(strucchange)
library(zoo)
library(Metrics)
library(rpart)
library(class)
library(plyr)
library(tree)
library(rattle)
library(sjmisc)
library(sjPlot)




options(dplyr.summarise.inform = FALSE)


############################################################# Data importation ##############################################################################


data <- list.files(path="./AssaignmentData", full.names = TRUE, recursive = TRUE,
                   pattern = ".csv$")
view(data)

############################################################# Data Cleaning ##################################################################################


emptydf = matrix(ncol = 0, nrow = 0)

for(i in 1:length(data)){
  df1 <- read.csv(data[i])
  year_df <- as.data.frame(str_split(data[i],'/'))
  year<- year_df[3,]
  
  monthpath <- year_df[4,]
  month_df <- as.data.frame(str_split(monthpath,'_'))
  month <- month_df[4,]
  
  date <- paste(month,year)
  df1$date <- date
  class(df1$date)
  df1$date <- my(date)
  
  #year
  df1$year <- year
  df1$month <- month.abb[month(as.POSIXct(df1$date, formart = "Y-%m-%d"))]
  emptydf <- rbind(emptydf,df1)
}


All_Crimes <- emptydf


## Data Structure before cleaning
str(All_Crimes)


# 1. Remove percent
percent_cleaner <- function(x){
  gsub("%","",x)
}
All_Crimes <- as.data.frame(lapply(All_Crimes,percent_cleaner))


# 2. Dash replace with 0
dash_replace <- function(x){
  gsub("-","0.0",x)
}
All_Crimes <- as.data.frame(lapply(All_Crimes,dash_replace))

# 3. Remove comma and empty spaces
comma_replace <- function(x){
  gsub(",","",x)
}
All_Crimes <- as.data.frame(lapply(All_Crimes,comma_replace))

# replace dataset date
All_Crimes$date<- emptydf$date


# rename county
names(All_Crimes)[names(All_Crimes)=="X"] <- "County"

num_df <- All_Crimes[2:51]

All_Crimes[2:51] <- as.data.frame(lapply(num_df, as.numeric))

str(All_Crimes)

# Removing Percentage columns
All_Crimes <- All_Crimes %>%
  select(County, year, month, starts_with("Number"), date)

# Rename Columns with longer names
names(All_Crimes)

names(All_Crimes)[4:28] <- c("Homicide_convict", "Homicide_unsuc", "OffencesAgt_Person_convict", "OffencesAgt_Person_unsuc",
                             "Sexual_Offences_convict", "Sexual_Offences_unsuc", "Burglary_convict", "Burglury_unsuc", "Robbery_Convict",
                             "Robbery_unsuc", "Theft_Handling_Convict", "Theft_Handling_unsuc", "Fraud_Forgery_Convict",
                             "Fraud_Forgery_unsuc", "Criminal_Damage_convict", "Criminal_Damage_unsuc", "Drugs_Offences_convict", 
                             "Drugs_Offences_unsuc", "Public_Order_Offences_convict", "Public_Order_Offences_unsuc",
                             "All_Other_Offences_convict_ex_motor_offences",  "All_Other_Offences_unsuc_ex_motor_offences",
                             "Motor_Offences_convict", "Motor_Offences_unsuc", "Admin_Finalised_unsuc")


colnames(All_Crimes)

# Remove National from the county row
All_Crimes <- All_Crimes %>%
  filter(County != "National")


############################################################# Descriptive Data Analysis #####################################################################

######################################################## Basic Exploratory Data Analysis ####################################################################

# Head of our dataset
head(All_Crimes, 10)

# Tail of our dataset
tail(All_Crimes, 10)

# Data structure after cleaning
str(All_Crimes)

# Dimension of Dataframe after cleaning
dim(All_Crimes)

# let's display the type and a preview of all columns as a row 
glimpse(All_Crimes)

# Summary of the dataset
summary(All_Crimes)

# The distinct and frequency of each value and proportion of that values in each cloumn
describe(All_Crimes)

# Having a deeper a deeper look at our dataset
skim(All_Crimes)

# checking for missing values
colSums(is.na(All_Crimes))

# Visualise missing values
vis_miss(All_Crimes)

vis_dat(All_Crimes)

# Plotting the dataset structure
plot_str(All_Crimes)

# Plotting the bar chart for each discrete feature
plot_bar(All_Crimes)

# The charts can also be grouped by a discrete variable, e.g. the presence of a Robbery
plot_bar(All_Crimes, by="County")

plot_bar(All_Crimes, by="County")

# Plotting quantile-quantile for each continuous feature
plot_qq(All_Crimes)



# from quantile plots, we can observe that the charts are showing the data are not normally distributed because the plots are skewed from the straightlines.
# It can be inferred that there are outliers in the dataset. Hence, the distribution are skewed.

# Plots density estimates for each continues feature
plot_density(All_Crimes)

# Visualise correlation heatmap
plot_correlation(All_Crimes)

# Visualising the principal components
plot_prcomp(All_Crimes$County)

## This is a principal component analysis (PCA) of each principal component within each county


# By observing the plots, one may immediately notice that Metropolitan and city present higher values of crimes distributed by county and maybe induced to further investigate the risk of crimes represented by counties through statistical testing

# change plot size (optional)
options(repr.plot.width = 20, repr.plot.height = 30)

All_Crimes %>% 
  select("Homicide_convict", "Homicide_unsuc", "Drugs_Offences_convict") %>%
  ggpairs(mapping = aes(color = All_Crimes$month, alpha = 0.5))

# Inspect the dataset
inspect_types(All_Crimes)

inspect_types(All_Crimes) %>%
  show_plot()

inspect_num(All_Crimes) %>%
  show_plot()


############################################## Advance Exploratory Data Analysis ###########################################

# Creating a variable called total_crime to input all crimes together for each county.
total_crime <- apply(All_Crimes[4:28],1,sum)
All_Crimes$Total_crime <- total_crime 

colnames(All_Crimes)


# Creating variables called unsuccessful crime conviction (Unsuc_convict) to compute all unsuccessful cases
unsuc_df <- All_Crimes %>%
  select(contains('unsuc'))

All_Crimes$Unsuc_crime_convict <- apply(unsuc_df,1,sum)

#creating variables called unsuccessful crime conviction (Unsuc_convict)
# successful total
suc_df <- All_Crimes %>%
  select(contains('convict'))

All_Crimes$Suc_crime_convict <- apply(suc_df,1,sum)

colnames(All_Crimes)

## Visualising All crimes
suc_df %>%
  gather(key='convit_type', value = 'crime_values') %>%
  group_by(convit_type) %>%
  dplyr::summarise(total=sum(crime_values)) %>%
  ggplot(aes(x=reorder(convit_type,+total),y=total)) +
  geom_col(aes(fill=convit_type)) +
  coord_flip() + theme(legend.position = 'none')


suc_df %>%
  gather(key='convit_type', value = 'crime_values') %>%
  group_by(convit_type) %>%
  dplyr::summarise(total=sum(crime_values)) %>%
  ggplot(aes(x=reorder(convit_type,+total),y=total)) +
  geom_bar(stat = "identity")
  
# Visualising Successful crime conviction Distribution 
# Successful crime conviction in 2014
All_Crimes %>%
  filter(year == 2014) %>%
  select(month,Suc_crime_convict) %>%
  group_by(month) %>%
  dplyr::summarize(total.convict=sum(Suc_crime_convict)) %>%
  ggplot(aes(x=reorder(month,+total.convict),y=total.convict,fill=month)) + 
  geom_col() +
  labs(x = 'month', y='Total successful conviction',
       title = 'Sucessful conviction per month in 2014')+
  coord_flip()

All_Crimes %>%
  filter(year == 2014) %>%
  select(month,Suc_crime_convict) %>%
  group_by(month) %>%
  dplyr::summarize(total.convict=sum(Suc_crime_convict)) %>%
  ggplot(aes(x=reorder(month,+total.convict),y=total.convict)) + 
  geom_col()

# Successful crime conviction in 2015
All_Crimes %>%
  filter(year == 2015) %>%
  select(month,Suc_crime_convict) %>%
  group_by(month) %>%
  dplyr::summarize(total.convict=sum(Suc_crime_convict)) %>%
  ggplot(aes(x=reorder(month,+total.convict),y=total.convict,fill=month)) + 
  geom_col() +
  labs(x = 'month', y='Total successful conviction',
       title = 'Successful per month in 2015')+
  coord_flip()

#Alternative with geom bar
All_Crimes %>%
  filter(year == 2015) %>%
  select(month,Suc_crime_convict) %>%
  group_by(month) %>%
  dplyr::summarize(total.convict=sum(Suc_crime_convict)) %>%
  ggplot(aes(x=reorder(month,+total.convict),y=total.convict,fill=month)) + 
  geom_bar(stat='identity') +
  theme_bw() +
  labs(x = 'month', y='Total successful conviction',
       title = 'Successful per month in 2015')

# Successful crime conviction in 2016
All_Crimes %>%
  filter(year == 2016) %>%
  select(month,Suc_crime_convict) %>%
  group_by(month) %>%
  dplyr::summarize(total.convict=sum(Suc_crime_convict)) %>%
  ggplot(aes(x=reorder(month,+total.convict),y=total.convict,fill=month)) + 
  geom_col() +
  labs(x = 'month', y='Total successful conviction',
       title = 'Successful per month in 2016')+
  coord_flip()

# Successful crime conviction in 2017
All_Crimes %>%
  filter(year == 2017) %>%
  select(month,Suc_crime_convict) %>%
  group_by(month) %>%
  dplyr::summarize(total.convict=sum(Suc_crime_convict)) %>%
  ggplot(aes(x=reorder(month,+total.convict),y=total.convict,fill=month)) + 
  geom_col() +
  labs(x = 'month', y='Total successful conviction',
       title = 'Successful per month in 2017')

# Visualising unsuccessful crime distribution
All_Crimes %>%
  select(month,year,Unsuc_crime_convict) %>%
  group_by(month,year) %>%
  dplyr::summarize(total.unsc=sum(Unsuc_crime_convict)) %>%
  ggplot(aes(x=reorder(month,+total.unsc),y=total.unsc,fill=year)) + 
  geom_col() +
  labs(x = 'month', y='Total Unsuccessful conviction',
       title = 'Unsucessful per month in years')+
  coord_flip()

# unsuccessful crime distribution for 2014
All_Crimes %>%
  filter(year == 2014) %>%
  select(month,Unsuc_crime_convict) %>%
  group_by(month) %>%
  dplyr::summarize(total.unsc=sum(Unsuc_crime_convict)) %>%
  ggplot(aes(x=reorder(month,+total.unsc),y=total.unsc,fill=month)) + 
  geom_col() +
  labs(x = 'month', y='Total Unsuccessful conviction',
       title = 'Unsucessful per month in 2014')+
  coord_flip()

# unsuccessful crime distribution for 2015
All_Crimes %>%
  filter(year == 2015) %>%
  select(month,Unsuc_crime_convict) %>%
  group_by(month) %>%
  dplyr::summarize(total.unsc=sum(Unsuc_crime_convict)) %>%
  ggplot(aes(x=reorder(month,+total.unsc),y=total.unsc,fill=month)) + 
  geom_col() +
  labs(x = 'month', y='Total Unsuccessful conviction',
       title = 'Unsucessful per month in 2015') +
  coord_flip()

# unsuccessful crime distribution for 2016
All_Crimes %>%
  filter(year == 2016) %>%
  select(month,Unsuc_crime_convict) %>%
  group_by(month) %>%
  dplyr::summarize(total.unsc=sum(Unsuc_crime_convict)) %>%
  ggplot(aes(x=reorder(month,+total.unsc),y=total.unsc,fill=month)) + 
  geom_col() +
  labs(x = 'month', y='Total Unsuccessful conviction',
       title = 'Unsucessful per month in 2016')+
  coord_flip()

# unsuccessful crime distribution for 2017
All_Crimes %>%
  filter(year == 2017) %>%
  select(month,Unsuc_crime_convict) %>%
  group_by(month) %>%
  dplyr::summarize(total.unsc=sum(Unsuc_crime_convict)) %>%
  ggplot(aes(x=reorder(month,+total.unsc),y=total.unsc,fill=month)) + 
  geom_col() +
  labs(x = 'month', y='Total Unsuccessful conviction',
       title = 'Unsucessful per month in 2017')+
  coord_flip()

# Find top counties per total crime
top_county <- All_Crimes %>%
  group_by(County) %>%
  dplyr::summarise(Total_crime = sum(Total_crime)) %>%
  arrange(desc(Total_crime)) %>%
  top_n(5) %>%
  pull(County)
  


All_Crimes %>%
  filter(County %in% top_county) %>%
  group_by(County, year) %>%
  dplyr::summarise(Total_crime = sum(Total_crime)) %>%
  ggplot(aes(x=County, y=Total_crime, fill=year)) + 
  geom_bar(position="dodge", stat="identity") +
  coord_flip()

# Counties with most Crimes from 2014 to 2017 
All_Crimes %>%
  filter(County %in% top_county) %>%
  group_by(year, County) %>%
  dplyr::summarise(Total_crime = sum(Total_crime)) %>%
  ggplot(aes(x=year, y=Total_crime, fill=County)) + 
  geom_bar(position="dodge", stat="identity")

# Most Crimes from 2014 to 2017 with a voilin plot
ggplot(All_Crimes, aes(x = Total_crime, y = year, fill = year)) +
  geom_violin()


#Average crime distribution for each year
All_Crimes %>%
  group_by(year) %>%
  dplyr::summarise(average.crime = mean(Total_crime)) %>%
  ggplot(aes(x=year, y=average.crime, fill= year)) + 
  geom_col(position="dodge") + 
  ggtitle("Average crime distribution by Years 2014, 2015, 2016 and 2017") + 
  coord_flip() + theme(legend.position = 'none')

# Total crime Per Month Within Each Year
All_Crimes %>%
  filter(year==2014) %>%
  group_by(month) %>%
  dplyr::summarise(Total_crime) %>%
  ggplot(aes(x=month, y=Total_crime, fill= month)) + 
  geom_col(position="dodge") + 
  ggtitle("Total crime distributed by month") + 
  coord_flip() + theme(legend.position = 'none')


All_Crimes %>%
  filter(year==2014) %>%
  group_by(month) %>%
  dplyr::summarise(avg.total.homocide = mean(Total_crime)) %>%
  arrange(desc(avg.total.homocide)) %>%
  ggplot(aes(x=month, y=avg.total.homocide, fill=month)) + 
  geom_bar(position="dodge", stat="identity") + 
  ggtitle("Average homicide crime type distributed by month for 2014") + 
  coord_flip() + theme(legend.position = 'none')

# Visualising Robbery Per each year
# Average Robbery for 2014

All_Crimes %>%
  filter(year==2014) %>%
  group_by(month) %>%
  dplyr::summarise(avg.total.robbery = mean(Total_crime)) %>%
  arrange(desc(avg.total.robbery)) %>%
  ggplot(aes(x=month, y=avg.total.robbery, fill=month)) + 
  geom_bar(position="dodge", stat="identity") + 
  ggtitle("Average Robbery crime type distributed by month for 2014") + 
  coord_flip() + theme(legend.position = 'none')

# Average Robbery for 2015
All_Crimes %>%
  filter(year==2015) %>%
  group_by(month) %>%
  dplyr::summarise(avg.total.robbery = mean(Total_crime)) %>%
  arrange(desc(avg.total.robbery)) %>%
  ggplot(aes(x=month, y=avg.total.robbery, fill=month)) + 
  geom_bar(position="dodge", stat="identity") + 
  ggtitle("Average Robbery crime type distributed by month for 2015") + 
  coord_flip() + theme(legend.position = 'none')

# Average Robbery for 2016
All_Crimes %>%
  filter(year==2016) %>%
  group_by(month) %>%
  dplyr::summarise(avg.total.robbery = mean(Total_crime)) %>%
  arrange(desc(avg.total.robbery)) %>%
  ggplot(aes(x=month, y=avg.total.robbery, fill=month)) + 
  geom_bar(position="dodge", stat="identity") + 
  ggtitle("Average Robbery crime type distributed by month for 2016") + 
  coord_flip() + theme(legend.position = 'none')

# Average Robbery for 2017
All_Crimes %>%
  filter(year==2017) %>%
  group_by(month) %>%
  dplyr::summarise(avg.total.robbery = mean(Total_crime)) %>%
  arrange(desc(avg.total.robbery)) %>%
  ggplot(aes(x=month, y=avg.total.robbery, fill=month)) + 
  geom_bar(position="dodge", stat="identity") + 
  ggtitle("Average Robbery crime type distributed by month for 2017") + 
  coord_flip() + theme(legend.position = 'none')

All_Crimes3 <- All_Crimes %>% 
  filter(year==2017) %>%
  group_by(month) %>%   
  dplyr::summarise(Ave_Homicide_unsuc = mean(Homicide_unsuc), 
                   Ave_Offences_Agt_Person_unsuc = mean(OffencesAgt_Person_unsuc),
                   Ave_Sexual_Offences_unsuc = mean(Sexual_Offences_unsuc), 
                   Ave_Burglury_unsuc = mean(Burglury_unsuc),
                   Ave_Robbery_unsuc = mean(Robbery_unsuc), 
                   Ave_Theft_Handling_unsuc = mean(Theft_Handling_unsuc),
                   Ave_Fraud_Forgery_unsuc = mean(Fraud_Forgery_unsuc), 
                   Ave_Criminal_Damage_unsuc = mean(Criminal_Damage_unsuc), 
                   Ave_Drugs_Offences_unsuc = mean(Drugs_Offences_unsuc),
                   Ave_Public_Order_Offences_unsuc = mean(Public_Order_Offences_unsuc),
                   Ave_All_Other_Offences_unsuc_ex_motor_offences= mean(All_Other_Offences_unsuc_ex_motor_offences),
                   Ave_Motor_Offences_unsuc = mean(Motor_Offences_unsuc)) %>% 
  gather(key="crime_type", value = "values", -month) 


All_Crimes3 %>%
  group_by(crime_type) %>%
  dplyr::summarise(total_conv = sum(values)) %>%
  arrange(desc(total_conv)) %>%
  top_n(10) %>%
  ggplot(aes(x=reorder(crime_type,+total_conv),y=total_conv,fill=crime_type)) + 
  geom_col(position="dodge", stat="identity") + 
  ggtitle("Average crime type distributed by County for 2017")+
  coord_flip() + theme(legend.position = 'none')



############################################################# Predictive Analysis ############################################################################

# Correlation and Covariance
CrimeData <- All_Crimes 

colnames(CrimeData)
plot(CrimeData[4:12])
CrimeCor <- cor(CrimeData[6:12])

corrplot(CrimeCor)

# print the first 6 rows
head(CrimeData, 6)

# Compute covariance and correlation matrix
CrimeData2 <- cov(CrimeData[6:12], method = "pearson")
CrimeData2
CrimeData3 <- cor(CrimeData2)
CrimeData3
round(CrimeData3, 2)
CrimeData3


# Correlation matrix with significance levels (p-value)
rcorr(CrimeData3, type = "pearson")


myCrimeData <- rcorr(as.matrix(CrimeData2))


# correlation matrix
CrimeData3

#significance level
myCrimeData


##Extracting The Correlation coefficients
myCrimeData$r

##Extracting the p-values
myCrimeData$P


# formatting the correlation matrix

# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor =(cormat)[ut],
    p = pmat[ut]
  )
}

myCrimeData <- rcorr(as.matrix(CrimeData[,6:12]))

flattenCorrMatrix(myCrimeData$r, myCrimeData$P)


### Visualize correlation matrix

corrplot(CrimeData3, type = "upper", order = "hclust", tl.col = "black", tl.srt = 10)


########################################################################## Regression ##########################################################################
                                                                             
#                                                                           Linear

####################################################################### Simple Linear Regression ##############################################################
length.of.data <- 1:nrow(All_Crimes)
trainlm <- sample(length.of.data,0.8*nrow(All_Crimes))
train.data.set <- All_Crimes[trainlm,]
test.data.set <- All_Crimes[-trainlm,]
nrow(test.data.set)


# Plotting our variables for Model One 
qplot(Burglary_convict, Robbery_Convict,data=All_Crimes, geom = "point") +
  geom_smooth(method = "lm", se = FALSE)

# Regression Analysis and Summary for Model one
model1 <- lm(Robbery_Convict~Burglary_convict,data = train.data.set)
summary(model1)


##### Compute p-value
pt(78.95, df = 1409, lower.tail = FALSE)

# Prediction for Model One
predictions.robbery <- predict(model1,test.data.set)

actual.values <- test.data.set$Robbery_Convict

mse(actual.values,predictions.robbery)

# Plotting Simple Linear Model One Results
model1<-ggplot(All_Crimes, aes(x=Burglary_convict, y=Robbery_Convict))+ geom_point()

model1

#Adding the linear regression line to the above plotted graph
model1 <- model1 + geom_smooth(method="lm", col="black")

model1

#Adding the equation for the regression line
model1 <- model1 + stat_regline_equation(label.x = 3, label.y = 7)

model1


model1 + theme_bw() + labs(title = "Convicted cases on Robbery as a Results of Burglary ", 
                           x = "Convicted Burglary Cases", y = "Convicted Robbery Cases")


model1 + theme_bw() + labs(title = "Convicted cases on Robbery as a Results of Burglary ", 
                           x = "Convicted Burglary Cases", y = "Convicted Robbery Cases")



##### Linear Model Two <br>

## Null Hypothesis = H0=There is no linear relationship between robbery and burglury conviction, in other words, the  increase of robbery is not 
##.. dependent on burglary conviction.
#### Alternative hypothesis = H1 = There is a statistical linear relationship between robbery and burglary, in other words,the increase in robbery
####...comes as a result of an increase in burglary conviction.




# Linear model 2
length.of.data <- 1:nrow(All_Crimes)
trainlm <- sample(length.of.data,0.8*nrow(All_Crimes))
train.data.set <- All_Crimes[trainlm,]
test.data.set <- All_Crimes[-trainlm,]
nrow(test.data.set)

# Plotting our variables for Model Two 
qplot(Drugs_Offences_convict, Robbery_Convict,data=All_Crimes, geom = "point") +
  geom_smooth(method = "lm", se = FALSE)


# Regression Analysis and Summary for Model Two
model2 <- lm(Robbery_Convict~Drugs_Offences_convict,data = train.data.set)
summary(model2)


# Prediction for Model Two
predictions.drug <- predict(model2,test.data.set)

actual.values <- test.data.set$Robbery_Convict


mse(actual.values,predictions.drug)


# Plotting Simple Linear Model Two Results

model2<-ggplot(All_Crimes, aes(x=Burglary_convict, y=Robbery_Convict))+ geom_point()

model2

#Adding the linear regression line to the above plotted graph
model2 <- model2 + geom_smooth(method="lm", col="black")

model2

#Adding the euquation for the regression line
model2 <- model2 + stat_regline_equation(label.x = 3, label.y = 7)

model2


model2 + theme_bw() + labs(title = "Convicted cases on Robbery as a Results of Drug Cases ", 
                           x = "Convicted Drug Cases", y = "Convicted Robbery Cases")



################################################################## Multiple Linear Regression ##################################################################


length.of.data <- 1:nrow(All_Crimes)
trainlm <- sample(length.of.data,0.8*nrow(All_Crimes))
train.data.set <- All_Crimes[trainlm,]
test.data.set <- All_Crimes[-trainlm,]
nrow(test.data.set)


# Regression Analysis and Summary for Mutltiple Regression
multReg_model <- lm(Robbery_Convict~Drugs_Offences_convict + Burglary_convict, data = All_Crimes)


summary(multReg_model)

#####Prediction for Multiple Regression
predictions.drug <- predict(multReg_model,test.data.set)

actual.values <- test.data.set$Robbery_Convict

mse(actual.values,predictions.drug)


# 1. Create a new dataframe with the information needed to plot the model

Plot.multReg_model <- expand.grid( Drugs_Offences_convict = seq(min(All_Crimes$Drugs_Offences_convict),
                                                                max(All_Crimes$Drugs_Offences_convict), length.out=30), Burglary_convict=c(min(All_Crimes$Burglary_convict),
                                                                                                                                           mean(All_Crimes$Burglary_convict), max(All_Crimes$Burglary_convict)))

# Predict the values of Robbery cases based on your linear model
Plot.multReg_model$predicted.y <- predict.lm(multReg_model, newdata=Plot.multReg_model)

# Round the smoking numbers to two decimals

Plot.multReg_model$Burglary_convict <- round(Plot.multReg_model$Burglary_convict, digits = 2)

# Change the ‘smoking’ variable into a factor
Plot.multReg_model$Burglary_convict <- as.factor(Plot.multReg_model$Burglary_convict)

# Plot the original data
Robbery.plot <- ggplot(All_Crimes, aes(x=Drugs_Offences_convict, y=Robbery_Convict)) + geom_point()
Robbery.plot


Robbery.plot <- Robbery.plot + 
  geom_line(data=Plot.multReg_model, aes(x=Drugs_Offences_convict, y=predicted.y,color=Burglary_convict),   size=1.25)

Robbery.plot

Robbery.plot <- Robbery.plot + theme_bw() +
  labs(title = "Rates of Robbery Cases (% of population) \n as a function of
Drug Cases and Burglary Cases",
       x = "Drugs Cases (% of population)",
       y = "Robbery Cases (% of population)",
       color = "Burglary \n (% of population)")


Robbery.plot

########################################################################### Clustering #######################################################################

numerical_df <- as.data.frame(scale(unsuc_df))

set.seed(7)
kcluster <- kmeans(numerical_df,2)
kcluster
summary(kcluster)
fviz_cluster(kcluster, data = numerical_df, ellipse.type = 'convex')

All_Crimes$Cluster <- as.character(kcluster$cluster)

All_Crimes %>%
  group_by(Cluster) %>% 
  dplyr::summarise(avg.crime = mean(Total_crime))


All_Crimes %>%
  filter(Cluster==1)

All_Crimes %>%
  filter(Cluster==2)

cluster_2 <- as.factor(All_Crimes$Cluster)

# All_Crimes labels
set.seed(7)
All_Crimes.labels <- All_Crimes$County
table(All_Crimes.labels)
All_Crimes_data <- All_Crimes[4:28]
view(All_Crimes_data)
colnames(All_Crimes_data)


#Scale the data
## Scaling the data is incredibly important because it makes the distance unwaded and also have more of a balanced dataset when it is scaled down
## 

All_Crimes_data_scale <- scale(All_Crimes_data)

#Distance Function
All_Crimes_data <- dist(All_Crimes_data_scale)


fviz_nbclust(All_Crimes_data_scale, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")


# K-Means
set.seed(7)
km.out <- kmeans(All_Crimes_data_scale, centers = 3, nstart = 100)
print(km.out)

# Visualise the clustering algorithm results
km.clusters <- km.out$cluster

rownames(All_Crimes_data_scale) <- paste(All_Crimes$County, 1:dim(All_Crimes)[1], sep = "_")

fviz_cluster(list(data=All_Crimes_data_scale, cluster= km.clusters))

All_Crimes %>%
  group_by(Cluster) %>%
  dplyr::summarise(avg.crime = mean(Total_crime))

All_Crimes %>%
  filter(Cluster==1)

All_Crimes %>%
  filter(Cluster==2)

All_Crimes %>%
  filter(Cluster==3)

cluster_3 <- as.factor(km.out$cluster)

set.seed(7)
All_Crimes.labels <- All_Crimes$County
table(All_Crimes.labels)
All_Crimes_data <- All_Crimes[4:14]
view(All_Crimes_data)
colnames(All_Crimes_data)



All_Crimes_data_std <- scale(All_Crimes_data) #standardising our dataset

#Distance Function
All_Crimes.dist <- dist(All_Crimes_data_std)

# Hierarchical Clustering Algorithm
hc.out_All_crime <- hclust(All_Crimes.dist, method = "complete")
hc.out_All_crime

#Dendrogram
plot(hc.out_All_crime)
rect.hclust(hc.out_All_crime, k = 3, border = 2:5)

# Clusters
All_Crimes.clusters <- cutree(hc.out_All_crime, k = 2)
All_Crimes.clusters
length(All_Crimes.clusters)

# Visualise the Cluster
rownames(All_Crimes_data_std) <- paste(All_Crimes$County, 1:dim(All_Crimes)[1], sep = "_")
All_Crimes_data_std
fviz_cluster(list(data = All_Crimes_data_std, cluster = All_Crimes.clusters))

table(All_Crimes.clusters, All_Crimes$County)

All_Crimes$Cluster <-  as.factor(All_Crimes.clusters)
All_Crimes %>%
  group_by(Cluster) %>%
  dplyr::summarise(avg.crime = mean(Total_crime))

All_Crimes %>%
  filter(Cluster==1)

All_Crimes %>%
  filter(Cluster==2)

All_Crimes %>%
  filter(Cluster==3)


# Using k = 2

plot(hc.out_All_crime)
rect.hclust(hc.out_All_crime, k = 2, border = 2:5)

# Clusters
All_Crimes.clusters <- cutree(hc.out_All_crime, k = 2)
All_Crimes.clusters
length(All_Crimes.clusters)

All_Crimes$Cluster <- as.factor(All_Crimes.clusters)

# Visualise the Cluster
rownames(All_Crimes_data_std) <- paste(All_Crimes$County, 1:dim(All_Crimes)[1], sep = "_")
All_Crimes_data_std
fviz_cluster(list(data = All_Crimes_data_std, cluster = All_Crimes.clusters))

table(All_Crimes.clusters, All_Crimes$County)

All_Crimes %>%
  group_by(Cluster) %>%
  dplyr::summarise(avg.crime = mean(Total_crime))

All_Crimes %>%
  filter(Cluster==1)

All_Crimes %>%
  filter(Cluster==2)




######################################################################### Classification ##########################################################################

### data partition (2 clusters)

# using 2 clusters
All_Crimes


###Generate a random number that is 80% of the total number of rows in dataset.
set.seed(7)
ran <- sample(1:nrow(All_Crimes), 0.8 * nrow(All_Crimes))


# normalization 
All_Crimes_norm_func <-function(x) { (x -min(x))/(max(x)-min(x)) }
All_Crimes_norm <- as.data.frame(lapply(All_Crimes[,c(4:28)], All_Crimes_norm_func))
All_Crimes_norm$Cluster <- cluster_2

# Dataset has been normalised before spliting
##extracting training set
All_Crimes_train <- All_Crimes_norm[ran,]

##extracting testing set
All_Crimes_test <- All_Crimes_norm[-ran,]



##extracting 5th column of train dataset because it will be used as 'cl' argument in knn function.
All_Crimes_target_category <- All_Crimes_train$Cluster



##running knn function
set.seed(7)
knn_prediction <- knn(All_Crimes_train, All_Crimes_test, cl=All_Crimes_target_category, k=13)


##creating confusion matrix
conf_mat <- table(knn_prediction,All_Crimes_test$Cluster)
confusionMatrix(knn_prediction,All_Crimes_test$Cluster)

## The function above divides the correct predictions by total number of predictions that tell us
# how accurate the model is.

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(conf_mat)



### Decision Tree 


#building the classification tree

#All_Crimes$County <- as.factor(All_Crimes$County)

#class(All_Crimes$County)

xtree <- tree(Cluster ~., data = All_Crimes_train)

summary(xtree)

plot(xtree)

text(xtree)




### data partition (For 3 clusters)

# using 3 clusters
All_Crimes


###Generate a random number that is 80% of the total number of rows in dataset.
set.seed(7)
ran <- sample(1:nrow(All_Crimes), 0.8 * nrow(All_Crimes))


# normalization 
All_Crimes_norm_func <-function(x) { (x -min(x))/(max(x)-min(x)) }
All_Crimes_norm <- as.data.frame(lapply(All_Crimes[,c(4:28)], All_Crimes_norm_func))
All_Crimes_norm$Cluster <- cluster_3

# Dataset has been normalised before splitting
##extracting training set
All_Crimes_train <- All_Crimes_norm[ran,]

##extracting testing set
All_Crimes_test <- All_Crimes_norm[-ran,]



##extracting 5th column of train dataset because it will be used as 'cl' argument in knn function.
All_Crimes_target_category <- All_Crimes_train$Cluster


##extract 5th column if test dataset to measure the accuracy
#All_Crimes_test_category <- All_Crimes[-ran,5]

##running knn function
set.seed(7)
knn_prediction <- knn(All_Crimes_train, All_Crimes_test, cl=All_Crimes_target_category, k=13)


##creating confusion matrix
conf_mat <- table(knn_prediction,All_Crimes_test$Cluster)
confusionMatrix(knn_prediction,All_Crimes_test$Cluster)

## The function above divides the correct predictions by total number of predictions that tell us
# how accurate the model is.

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(conf_mat)


### Decision Tree 


#building the classification tree

#All_Crimes$County <- as.factor(All_Crimes$County)

#class(All_Crimes$County)

xtree <- tree(Cluster ~., data = All_Crimes_train)

summary(xtree)

plot(xtree)



text(xtree)

xtree_pred <- predict(xtree, All_Crimes_test, type = 'class')
confusionMatrix(xtree_pred,All_Crimes_test$Cluster)



# Compare the settlement pattern of the areas of Metropolitan and city to Gloucester
myLocation <- 'London'

register_google(key = 'AIzaSyDINC5fe45WyMuEGICXS4eLAAFOKv1eKAQ')
register_google(key = 'AIzaSyAA0EnsCzDeWBlUxRn8gJJ36fEeurRF7Ec')
register_google(key = 'AIzaSyCAKNrvt6QTLkdKlsf-Cv_dWQ3z2N5na3E')

crime_map <- All_Crimes

mymap <- get_map(location = myLocation, zoom = 12)

ggmap(mymap)



myLocation <- 'Gloucestershire'
mymap <- get_map(location = myLocation, zoom = 12)

ggmap(mymap)






