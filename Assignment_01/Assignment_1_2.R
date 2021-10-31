######################################## 2 #############################################
# step 2.1 Read files
library(readxl)

location_table <- read_excel("Dataset/location.xlsx")
city_table <- read_excel("Dataset/city.xlsx")

dough_table <- read_excel("Dataset/dough.xlsx")
cheese_table <- read_excel("Dataset/cheese.xlsx")
topping_table <- read_excel("Dataset/topping.xlsx")

day_table <- read_excel("Dataset/day.xlsx")
month_table <- read_excel("Dataset/month.xlsx")
year_table <- read_excel("Dataset/year.xlsx")

pizza_Size_table <-
  data.frame(pizza_size_id = c(1, 2, 3, 4, 5),
             size=c("personal", "small", "medium", "large", "xlarge"),
             pizza_price=c(20, 30, 40, 50, 70))
View(dough_table)
# step 2.2 
# Function to generate the date table
gen_rondom_date <- function(no_of_data) {
  # Generate transaction data randomly
  date <- expand.grid(1:no_of_data)
  day <- sample(day_table$day_id, no_of_data, replace=T, prob=c(3,2,2,1,4,1,2))
  month <- sample(month_table$month_id, no_of_data, replace=T)
  year <- sample(year_table$year, no_of_data, replace=T, prob=c(3,2,2))

  date_orders <- data.frame(date_id = date,
                            day_id = day,
                            month_id = month,
                            year_id = year)
  return(date_orders)
}
  
date_fact <- gen_rondom_date(50)
names(date_fact)[1] <- 'date_id'
View(date_fact)

# step 2.3 
# Function to generate order table
gen_rondom_orders <- function(no_of_data) {
  # Generate transaction data randomly
  loc <- sample(location_table$location_id, no_of_data, replace=T, prob=c(3,2,1))
  city <- sample(city_table$city_id, no_of_data, replace=T, prob = c(2,4,1))
  dough <- sample(dough_table$dough_id, no_of_data, replace=T, prob = c(2,4,1))
  cheese <- sample(cheese_table$cheese_id, no_of_data, replace=T, prob=c(3,2,2))
  pizza <- sample(pizza_Size_table$pizza_size_id, no_of_data, replace=T, prob=c(1,2,4,9,12))
  topping <- sample(topping_table$topping_id, no_of_data, replace=T, prob=c(3,2,2,5))
  ddate <- date_fact$date_id
  quantitty <- sample(c(1:10), no_of_data, replace=T, prob = c(1:10))
  profitt <- quantitty * pizza_Size_table[as.factor(pizza),]$pizza_price
  orders <- data.frame(location_id = loc,
                       city_id = city,
                       date_id = ddate,
                       pizza_size_id = pizza,
                       dough_id = dough,
                       cheese_id = cheese,
                       topping_id = topping,
                       quantity = quantitty,
                       revenue = profitt)
  return(orders)
}
order_fact <- gen_rondom_orders(2000)
View(order_fact)

# step 2.4
# Build up a cube of revenue based on order_fact and date_fact
order_by_date <- merge(order_fact, date_fact, by.x = "date_id", by.y = "date_id")
order_by_date_month <- merge(order_by_date, month_table, by.x = "month_id", by.y = "month_id")
View(order_by_date_month)

revenue_cube_date <- 
  tapply(order_by_date_month$revenue, 
         order_by_date_month[,c("location_id", "city_id", "pizza_size_id","quarter",
                                 "quantity", "month_name", "year_id")], 
         FUN=function(x){return(sum(x))})

# Showing the cells of a subset of the cells
dimnames(revenue_cube_date)

# Quantity and months
apply(revenue_cube_date, c("quantity", "month_name"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})

# Pizza size and quarter
apply(revenue_cube_date, c("quarter", "pizza_size_id"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})
######################################## 3 #############################################
order_by_date <- merge(order_fact, date_fact, by.x = "date_id", by.y = "date_id")
components1 <- merge(order_by_date, pizza_Size_table, by.x = "pizza_size_id", by.y = "pizza_size_id" )
components2 <- merge(components1, topping_table, by.x = "topping_id", by.y = "topping_id" )
components3 <- merge(components2, dough_table, by.x = "dough_id", by.y = "dough_id" )
components4 <- merge(components3, cheese_table, by.x = "cheese_id", by.y = "cheese_id" )
componenet_with_month <- merge(components4, month_table, by.x = "month_id", by.y = "month_id" )


View(componenet_with_month)
# step 3.1
quantity_cube <- 
  tapply(componenet_with_month$quantity, 
         componenet_with_month[,c("size", "dough_type", "cheese_type", "topping_name", "month_name", "quarter")], 
         FUN=function(x){return(sum(x))})

# cube based on components4 df aggregated with revenue
revenue_cube2 <- 
  tapply(components4$revenue, 
         components4[,c("quantity", "date_id", "size", "dough_type", "cheese_type", "topping_name")], 
         FUN=function(x){return(sum(x))})


# find trends and thus to predict which Pizza components the store should order more of
pizza_size <- apply(quantity_cube, c("size"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})
dough <- apply(quantity_cube, c("dough_type"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})
cheese <- apply(quantity_cube, c("cheese_type"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})
topping <- apply(quantity_cube, c("topping_name"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})

pizza_size
dough
cheese
topping

# step 3.2
# Rollup
apply(revenue_cube2, c("size"),
      FUN=function(x) {return(sum(x, na.rm=TRUE))})

apply(quantity_cube, c("size"),
      FUN=function(x) {return(sum(x, na.rm=TRUE))})

# DrillDown
apply(quantity_cube, c("size","quarter"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})
