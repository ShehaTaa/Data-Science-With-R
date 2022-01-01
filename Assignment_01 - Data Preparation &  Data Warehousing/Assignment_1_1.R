# import libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(plyr)
library(dplyr)
library(modeest)

################################### 1 #########################################
# Read Dataset
bank_df <-read.csv("/media/shehata/Data/R_Task/bank-additional-full.csv", header =TRUE, sep =";")
View(bank_df)

# reduce the dataset to only four predictors [age, education, previous, and pdays]
# and the target [response].
reduced_df = subset(bank_df, select = c(age, education, previous, pdays,y))
View(head(reduced_df))

################################### 2 #########################################
# The field pdays is a count of the number of days since the client was last contacted
# from a previous campaign. The code 999 in the value represents customers who had not been
# contacted previously. Change the field value 999 to “NA” to represent missing values.

reduced_copy<-data.frame(reduced_df)

table(reduced_df$pdays)

# change 999 to NA
reduced_copy <- reduced_copy %>% replace(.== 999, NA)

table(reduced_copy$pdays)

################################### 3 #########################################
 
hist(reduced_df$pdays,
     col= 'blue',
     xlim = c(0,1000),
     main= 'pdays Frequency',
     xlab = 'pdays',
     ylab = 'Frequency')

################################### 4 #########################################
#Create a histogram of the pdays variable showing the missing value excluded.
hist(reduced_copy$pdays,
     col= 'blue',
     xlim = c(0,30),
     main= 'pdays Frequency',
     xlab = 'pdays',
     ylab = 'Frequency')

################################### 5 #########################################
# Transform the data values of the education field into numeric values using the chart in
# Table 1 below

#get unique values of education before changing it
unique(reduced_copy$education)

# change the values
reduced_copy$education <- revalue(x = reduced_copy$education,
               replace= c("illiterate" = 0,"basic.4y" = 4, "basic.6y" = 6, "basic.9y" = 9,
                          "high.school" = 12, "professional.course" = 14,
                          "university.degree" = 16, "unknown" = NA))

# convert education type to numeric
reduced_copy$education <- as.numeric(reduced_copy$education)

################################### 6 #########################################
# Compute the mean, median & mode of the age variable. Using a boxplot, give the five-
# number summary of the data. Plot the quantile information.

#mean
mean_age <- mean(reduced_copy$age, na.rm = TRUE)
mean_age

#meadian
median_age <- median(reduced_copy$age, na.rm = TRUE)
median_age

#mode
mode_age <- mlv(reduced_copy$age, method=mfv)
mode_age

# summary
summary(reduced_copy$age)

quantile(reduced_copy$age, c(0.25,0.5,0.75))

boxplot(reduced_copy$age,
        main = "Age",
        xlab = "Data points",
        ylab = "Info",
        col = "orange",
        horizontal = TRUE,
        notch = TRUE, 
        axes = TRUE,
        staplewex = 2)

text(x=fivenum(reduced_copy$age), labels =fivenum(reduced_copy$age), y=1.25)

# plot the quantile information
qplot(sample = age, data = reduced_copy)

################################### 7 #########################################
# Some machine learning algorithms perform better when the numeric fields are
# standardized. Standardize the age variable and save it as a new variable, age_z.

age_z <- scale(x = reduced_copy$age)
# add the standard age as a new column to dataframe
reduced_copy$age_z <- age_z
head(reduced_copy)
################################### 8 #########################################
# Obtain a listing of all records that are outliers according to the field age_z.
boxplot(reduced_copy$age_z,
        main = "Age Z",
        xlab = "Data points",
        ylab = "Info",
        col = "orange",
        horizontal = TRUE,
        notch = TRUE, 
        axes = TRUE,
        staplewex = 1)

# list of all outlires in age_z
outlires <- boxplot.stats(reduced_copy$age_z)$out
head(outlires)
