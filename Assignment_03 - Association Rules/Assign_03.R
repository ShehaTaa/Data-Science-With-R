# Load libraries
library(arules)
library(arulesViz)

# Read Dataset
trans_df <-read.transactions("/home/shehata/Assignment_03/Dataset/transactions.csv",
                             format = 'basket',
                             header = TRUE,
                             sep = ',')
class(trans_df)
inspect(trans_df)

summary(trans_df)

############################### 1  ############################### 

# Generate a plot of the top 10 transactions
itemFrequencyPlot(trans_df, support = 0.1)
itemFrequencyPlot(trans_df, topN = 10)

############################### 2  ############################### 

# Generate association rules using minimum support of 0.002, minimum confidence of
# 0.20, and maximum length of 3. Display the rules, sorted by descending lift value.

# set parameters support, confidence, and maxlen levels.
trans_rules_1 <- apriori(trans_df, parameter = list(support = 0.002,
                                                    confidence =0.20,
                                                    maxlen = 3))

trans_rules_1
# Display the rules, sorted by descending lift value
l <- inspect(sort(trans_rules_1, by = "lift"))
# Display the rules, sorted by descending support value
s <- inspect(sort(trans_rules_1, by = "support"))
############################### 3. ############################### 

# Select the rule from QII-b with the greatest lift. Compare this rule with the highest lift
# rule for maximum length of 2.
# i) Which rule has the better lift?
#   ii) Which rule has the greater support?
#   iii) If you were a marketing manager, and could fund only one of these rules, which
# would it be, and why? 

trans_rules_2 <- apriori(trans_df, parameter = list(support = 0.002,
                                                  confidence =0.20,
                                                  maxlen = 2))

# Display the rules, sorted by descending lift value
l <- inspect(sort(trans_rules_2, by = "lift"))
# Display the rules, sorted by descending support value
s <- inspect(sort(trans_rules_2, by = "support"))

############################### Part B  ############################### 
library(lsa)    # Latent Semantic Analysis

ratings = read.csv("/home/shehata/Assignment_03/Dataset/ratings.csv")
ratings_transpose = read.csv("/home/shehata/Assignment_03/Dataset/ratings_transposed.csv")

# View(ratings)

# convert NA to 0
# ratings[is.na(ratings)] <- 0
ratings_transpose[is.na(ratings_transpose)] <- 0

# convert dataframe to matrix and drop traget col
ratings_mx <- as.matrix(ratings[, -1])
ratings_tra_mx <- as.matrix(ratings_transpose[, -1])

# Use R to compute the cosine similarity between users.
corr <- cosine(ratings_tra_mx) 
corr
# correlation with EN
corr[,4]

###################### 5 ######################
# install.packages("recommenderlab", dependencies=TRUE)
library(recommenderlab)
library(dplyr)

# Convert ratings matrix to real rating matrix which makes it dense
ratings_tra_mx = as(ratings_mx, "realRatingMatrix")

# Create Recommender Model. The parameters are UBCF and Cosine similarity.
# We take 1 nearest neighbours
rec_mod = Recommender(ratings_tra_mx, method = "IBCF", param=list(method="Cosine")) 

# Obtain top 5 recommendations for 1st user entry in dataset
Top_3_pred = predict(rec_mod, ratings_tra_mx[4], n=3)

#Convert the recommendations to a list
Top_3_List = as(Top_3_pred, "list")
Top_3_List



