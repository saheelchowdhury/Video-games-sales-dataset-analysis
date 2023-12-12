library("readxl")
games_sales <- read.csv("video_games_sales.csv")

#Lets filter for years 2010 to 2020
games_sales <- games_sales[games_sales$year >= 2010 & games_sales$year <= 2020,]

#This code creates a new dataframe total_sales that 
#contains the total North American and European sales for each publisher, genre
total_sales <- aggregate(cbind(na_sales, eu_sales) ~ publisher + genre, 
                         data = games_sales, FUN = sum)


#Now, the total_sales dataframe has an additional column 
#'total_profit' that represents the total profit for each publisher.

total_sales$total_profit <- total_sales$na_sales + total_sales$eu_sales

#The top_publishers dataframe now contains the top 10 
#publishers based on their combined North American and European sales.

top_publishers <- total_sales[order(total_sales$total_profit, decreasing = TRUE),][1:10,]

# Companies with total highest selling genres combined

# Activision = 164.21 
# EA = 212.95
# Ubisoft = 135.36
# Warner Bros. Interactive Entertainment = 79.67
# Take Two = 114.55
# Nintendo = 97.62


total_sales$percentage_increase <- (total_sales$na_sales + total_sales$eu_sales) / (total_sales$na_sales[total_sales$year == 2010] + total_sales$eu_sales[total_sales$year == 2010]) - 1




