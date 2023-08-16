#Step1: Initial Exploratory Analysis

# 1. Name has been assigned to 'hollywood' for dataset

View(hollywood)

# 2. Import Library
library(tidyverse)

# 3. Check Data types

str(hollywood)

#Step 2: Cleaning Data

# 4. Check for Missing Values
colSums(is.na(hollywood))

dim(hollywood)

# 5. Drop Missing Value from Rows

hollywood <- na.omit(hollywood)

dim(hollywood)

# 6. Check for duplicates and remove them

hollywood <- hollywood %>% distinct(Film, .keep_all = TRUE)

dim(hollywood)

# 7. Round off values to 2 places

hollywood$Profitability <- round(hollywood$Profitability ,digit=2)
View(hollywood)

hollywood$Worldwide.Gross <- round(hollywood$Worldwide.Gross ,digit=2)
View(hollywood)

# Step 2.1: Outlier Removal

#Check for outliers using a boxplot

library(ggplot2)

#Create a boxplot that labels the outliers 
ggplot(hollywood,aes(x=Profitability, y=Worldwide.Gross)) +geom_boxplot(outlier.colour= "red",outlier.shape= 1)+scale_x_continuous(labels = scales::comma)+coord_cartesian(ylim= c(0,1000))

dim(hollywood)

#Remove outliers in 'Profitability'
Q1 <- quantile(hollywood$Profitability, .25)
Q3 <- quantile(hollywood$Profitability, .75)
IQR <- IQR(hollywood$Profitability)

no_outliers <- subset(hollywood, hollywood$Profitability> (Q1 - 1.5*IQR) & hollywood$Profitability< (Q3 + 1.5*IQR))

dim(no_outliers)

# Remove outliers in 'Worldwide.Gross'
Q1 <- quantile(no_outliers$Worldwide.Gross, .25)
Q3 <- quantile(no_outliers$Worldwide.Gross, .75)
IQR <- IQR(no_outliers$Worldwide.Gross)

df1 <- subset(no_outliers, no_outliers$Worldwide.Gross> (Q1 - 1.5*IQR) & no_outliers$Worldwide.Gross< (Q3 + 1.5*IQR))

dim(df1)

#Outlier checking
ggplot(df1,aes(x=Profitability, y=Worldwide.Gross)) +geom_boxplot(outlier.colour= "red",outlier.shape= 1)+scale_x_continuous(labels = scales::comma)+coord_cartesian(ylim= c(0,1000))


#Step 3: Exploratory Data Analysis

#Do a Summary Statistics/Univariate Analysis using appropriate code

#Summary Statistics:
summary(df1)


#Bivariate Analysis

#scatterplot
ggplot(df1, aes(x=Lead.Studio, y=Rotten.Tomatoes..)) + geom_point()+ scale_y_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 110))+theme(axis.text.x = element_text(angle = 90))

#bar chart
ggplot(df1, aes(x=Year)) + geom_bar()


#Histogram
hist(df1$Worldwide.Gross, main="Histogram", xlab=" Worldwide.Gross", ylab="Profit", col='green')

#Export data

write.csv(no_outliers, "D:clean_df1.csv")
















