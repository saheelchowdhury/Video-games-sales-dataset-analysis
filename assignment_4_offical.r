# ===================================
# GBA464: Assignment 4
# Twitch and Steam: Part I and II
# Instructor: Yufeng Huang
# ===================================

# Instructions:
#   1. This is a team assignment. Your team hands in one copy of R code by email to r.programming.simon@gmail.com
#   2. Please attempt all questions. Please make sure the code runs. If you have attempted all questions and the
#       code runs, you will get full marks.
#   3. You will need to make assumptions when you process the data. Be critical and convince yourselves that your
#       choices make sense. These choices do not affect your grades. But this is a good place to learn when to stand
#       behind your decisions and when to acknowledge that something do not make sense.
#   4. You can use ChatGPT or other language models to help you code.

# Specific coding instructions
#   5. Load your libraries here. Do not load them later in the code. Do not install.packages in the code (install them separately)
#   6. Datasets are large. So consider using data.table. Also, the data are large, so try to avoid looping over many observations.
#   7. Have fun!


#Assignment group members 4 
# Srinivas Vivek Vardhan - Svardhan@simon.rochester.edu
# Moemedi Wazzza Rakhudu - mrakhudu@simon.rochester.edu
# Saheel Chowdhary       - schowd14@ur.rochester.edu
# Joseph Pellumbi        - jpellumb@simon.rochester.edu 

#load packages
rm(list = ls())
# Load necessary libraries
#install.packages('data.table')
library('data.table')

#install.packages("tidyverse")
library('tidyverse')

#install.packages('dplyr')
library('dplyr')

#install.packages('stringr')
library('stringr')

#############################
# Step 1: clean and understand Steam data
#############################

# We start with two data sets: Twitch and Steam. In this step we first work on cleaning Steam data
# There are three files. game_attributes, game_players, and game_price_changes
#   The primary key for game_attributes is $app_id. This is app-level data on their attributes.
#   The primary key for game_players is $app_id and $date. This is daily data for the number of players in the game
#   The primary key for game_price_changes is $app_id and $price_change_date. This is a record of the price-changing
#   occasions and the new prices at those occasions.


# add data to data tables
dt_ga = fread("game_attributes.csv")

dt_gp = fread("game_players.csv")

dt_gpc = fread("game_price_changes.csv")

# converting to data tables  

dt_ga = as.data.table(dt_ga)
dt_gp = as.data.table(dt_gp)
dt_gpc = as.data.table(dt_gpc)

# We should clean game_attributes data. $release_date is in some weird format. Make it a date variable.
# Discard the time aspect
  

dt_ga$release_date  = sapply(strsplit(as.character(dt_ga$release_date), ' – '), `[`, 1)
dt_ga$release_date <- as.Date(dt_ga$release_date, format = "%d %B %Y")



#$rating_text contains useful information. We want the percentage of positive ratings and the total number of
#number in two separate variables.

# Example 83.46% of the 3,773 user reviews are positive. SteamDB Rating


dt_ga$rating =  as.numeric(sapply(strsplit(dt_ga$rating_text, '%'), `[`, 1))
split_data <-
    sapply(strsplit(dt_ga$rating_text, ' of the '), `[`, 2)
dt_ga$review_count  <-
    sapply(strsplit(split_data, ' user reviews'), `[`, 1)
dt_ga$review_count = as.numeric(gsub(",", "", dt_ga$review_count))



#For all data, keep games with the type "game" and with non-missing release
#date, rating,and reviews.

#getting rid of the na

dt_ga = subset(dt_ga,
               dt_ga$rating_text != "" &
                   !is.na(dt_ga$release_date) & dt_ga$app_type == "Game")


# We need to clean the price change data to get daily prices.
#For a game-date combination, if the game_price_changes
#   data has an observation on that day, the $price variable in that data measures the price on that day. If there is no
#   price data on a day, we know that price did not change on that day, so it takes the previous value. For example, for
#   game 730 (Counter-Strike: Global Offensive). The price was changed to $9.99 on 2012-11-01. Then it was changed again
#   to $11.24 on 2012-11-22. This means that before the change on 2012-11-22, the price stayed at $9.99.
#   By the end of this step, you should have a price data at the level of game-date. There should be no gap in dates unless
#   for specific reasons that lead to missing data. You should then be able to merge the players data with the price data.

#order

dt_gpc = dt_gpc[order(dt_gpc$app_id, dt_gpc$price_change_date)]
n = nrow(dt_gpc) 

#when you encounter 

for (i in 2:n) {
    if (dt_gpc$app_id[i] == dt_gpc$app_id[i-1]){
        if (dt_gpc$price[i] == 0) {
            dt_gpc$price[i] =  dt_gpc$price[i - 1]
        }
    }
}

#merge game players data with price data


dt_gpc$date= dt_gpc$price_change_date              # adding a date column to match player data
merged_df = merge(dt_gpc, dt_gp, by = c("date", "app_id"))
merged_df = as.data.table(merged_df)
dt_gpc$date = NULL                                 # bringing the player dataset to original form after merging
merged_df$price_change_date= NULL

# Finally, produce summary statistics at the game level: for each game, first compute the average daily number of players,
#   average price, total number of price changes, number of ratings and the fraction of positive reviews,

# compute the average daily number of players 
dt_sum = aggregate(player_count ~ app_id + date ,
                   data = merged_df, 
                   FUN =  mean)
dt_sum = as.data.table(dt_sum)


#average price of each game
avg_price = aggregate(price ~ app_id ,
                      data = merged_df, 
                      FUN =  mean)
avg_price = as.data.table(avg_price)


#total number of price changes
total_price_changes = aggregate(price_change_date ~ app_id ,
                                data = dt_gpc, 
                                FUN =  length)
total_price_changes = as.data.table(total_price_changes)


#number of ratings,
num_ratings = aggregate(review_count ~ app_id ,
                        data = dt_ga, 
                        FUN =  sum)
num_ratings = as.data.table(num_ratings)

#fraction of positive reviews
dt_ga$pos_reviews = dt_ga$rating/100


#  Then summarize the mean and standard deviation of these variables across games. 

mean_daily_players <- mean(dt_sum$player_count, na.rm = TRUE)
sd_daily_players <- sd(dt_sum$player_count, na.rm = TRUE)

mean_avg_price <- mean(avg_price$price, na.rm = TRUE)
sd_avg_price <- sd(avg_price$price, na.rm = TRUE)

mean_price_changes <- mean(total_price_changes$price_change_date, na.rm = TRUE)
sd_price_changes <- sd(total_price_changes$price_change_date, na.rm = TRUE)

mean_num_ratings <- mean(num_ratings$review_count, na.rm = TRUE)
sd_num_ratings <- sd(num_ratings$review_count, na.rm = TRUE)

mean_pos_reviews = mean (dt_ga$pos_reviews , na.rm = TRUE)
sd_pos_reviews <- sd(dt_ga$pos_reviews, na.rm = TRUE)

summary_df <- data.frame(
    Data = c("Daily Players", "Average Price", "Price Changes", "Number of Ratings", "Positive Reviews"),
    Mean = c(mean_daily_players, mean_avg_price, mean_price_changes, mean_num_ratings, mean_pos_reviews),
    SD = c(sd_daily_players, sd_avg_price, sd_price_changes, sd_num_ratings, sd_pos_reviews)
)
summary_df=as.data.table(summary_df)
#  Also, find all paid games (i.e., games with an average price above zero), and among those paid games, 
#  find 9 games with the highest average daily players. Produce summary statistics
#  for each of these 9 games. 

game_above_zero = subset(merged_df, merged_df$price > 0)
game_above_zero = as.data.table(game_above_zero)

merged_game_above_zero = merge(game_above_zero, dt_sum, by = c("app_id","date"))
merged_game_above_zero = as.data.table(merged_game_above_zero)
merged_game_above_zero$player_count.y = NULL
setnames(merged_game_above_zero, old = c('player_count.x'),
         new = c('player_count'))

#agg = aggregate(player_count ~ app_id ~ date, data = merged_game_above_zero , FUN = sum)

#top_9_games = head(agg[order(agg$player_count, decreasing = TRUE), ], 9)
top_9_games = head(merged_game_above_zero[order(merged_game_above_zero$player_count, decreasing = TRUE), ], 9)

top_9_games = as.data.table(top_9_games)

summary_stats <- top_9_games %>%
    group_by(date) %>%
    summarise(
        average_price = mean(price, na.rm = TRUE),
        std_dev = sd(price,na.rm = TRUE)
    )

summary_stats <- top_9_games %>%
    group_by(app_id) %>%
    summarise(
        average_price = mean(price),
        std_dev = sd(price)
    )


print(summary_stats)

#Print these summary stats in the console.Optionally export the summary stats into tables 9 games highest


#############################
# Step 2: how well associated between prices and the number of players?
#############################

# When there is a price discount, the number of players might increase if people buy the game and play it. We will look into
#   this hypothesis.
#   We start by looking at the 9 games. For each of these 9 games, plot the trajectory of prices and the number of players
#   over time. Use log scale for the number of players but not for price (put them on two different axes). Do you see a correlation?
#   Then, calculate the correlation coefficient between the log number of players and price. Then, in the graph title, mark the game
#   and the correlation coefficient between the log number of players and price. Do you see a negative correlation, in that the higher
#   the price, the lower the number of players?
#   Finally, compute the correlation between log players and price for each game, and summarize the average and the distribution of this
#   correlation. Or you can plot a histogram. Do you still find negative correlation?

top_game_data = top_9_games

# Loop through each of the 9 top games
for(game in unique(top_game_data$app_id)) {
    
   
    # Subset data for the specific game
    data <- merged_game_above_zero[merged_game_above_zero$app_id == game, ]
    
    # Calculate correlation coefficient for log player count and price
    cor_coefficient <- cor(log( data$player_count),data$price, use = "complete.obs")
    
    # Create an empty plot for dual y-axes
    plot(data$date, data$price, type="l", xlab="Date", ylab="Price", col="brown", 
         main=paste("Game:", game, "Correlation:", round(cor_coefficient, 2)))
    
    # Add a price line to the plot
   # lines(data$date, data$price, col="blue", lwd=2)
    
    # Add a secondary axis for log-transformed player count
    axis(4, at=axTicks(4), col.axis="brown")
   mtext("Log Players", side=4, line=3, col="green")
    
    # Add a line for log-transformed player count
   par(new=TRUE)
   plot(data$date, log(data$player_count), type="l", xaxt="n", yaxt="n", 
        xlab="", ylab="", col="green", lwd=2)
    
    legend("topright", legend=c("Price", "Log Player Count"), 
           col=c("brown", "green"), lwd=2, bty="n")
 
}



#   Now, perform a related (but different) exercise on the number of ratings and the average number of players.
#   Note that we do not see ratings on each day for each game. We simply see an average rating. So we cannot look at 
#.  how rating changes and how that's associated with the number of players. But at least we can still look at the 
#.  scatter plot between the average rating and the log(average number of players) for each game. In the scatter plot, 
#.  one observation is a game.
#.  Fit a line or curve between the two variables. Also write down the correlation coefficient in the graph title. 
#.  What can you conclude here?

#X-axis: This will have the average rating of each game.
#Y-axis: This will have the logarithm (natural log, or base e) of the average number of players for each game. 
#This log transformation is commonly done in data analysis to make certain patterns more visible or to
#linearize relationships.

#Each Point on the Scatter Plot: Each dot or point on this scatter plot represents one game.

#prepare data


avg_player_ratings = aggregate(player_count ~ app_id, data= dt_gp, FUN = mean )
player_ratings = merge (dt_ga, avg_player_ratings, by = c("app_id"))
# Perform linear regression
linear_model <- lm(log(player_ratings$player_count) ~ player_ratings$rating)

cor_coefficient <- cor(player_ratings$rating,log(player_ratings$player_count), use = "complete.obs")


plot (player_ratings$rating, log(player_ratings$player_count),
      xlab = " average rating", ylab="log average number of players",
      main=paste("Correlation:", round(cor_coefficient, 2))
      )
#Fit a line or curve between the two variables. 
# Add the regression line to the plot
abline(linear_model, col="blue")


#############################
# Step 3: now load the Twitch streamer data
#############################


# add data to data tables
dt_profiles = fread("twitch_profiles.csv")
dt_streams = fread("twitch_streams.csv")




# now read Twitch data. There are two files. 
#   Data twitch_profiles has the key $streamer. This is a sample of about 10,000 streamers. 
#   Data twitch_streams' key should be each observation, which is a stream. 

#   A stream is broadcasted by $streamer on a $date, but notice that a streamer on a date can have multiple streams. 
#   Variable $viewers records how many viewers are simultaneously present in the stream. 
#.  $duration is the number of hours of the stream. 
#   $unique_viewers is the number of viewers who ever appeared. $followers is the number of followers of the streamer at 
#the time of the stream. $stream_title is the title of the stream. And $games is a list of all games that are
#broadcasted in the same stream. 




# Now, let us organize the data such that we count the total amount of viewing time for each game on each day.
#To do this, we have to process the data in a few steps:
#   1. restructure the data into stream (the original level of observation)-game level. ]
#.  That is, we should put different 
#   games in different observations for each stream. 
#   2. for each stream, multiply the number of viewers (not the unique viewer) 
#.  by the stream duration to get the viewing time (unit is viewer-hour). 
#   Then divide viewing time by the number of games. 
#.  Here, we're assuming that each game gets equal amount of exposure. 
#   3. sum up the total viewing time for each game on each day, across streams. 
#Also count how many streamers broadcasted the game on each day.


#1 expand the rows by splitting the games into unique rows

#dt_total_view_time = dt_streams %>% separate_rows(games, sep = ", ")

dt_total_view_time = as.data.table(dt_streams %>% separate_rows(games, sep = ", "))

#2 viewing time


game_exposure <- function(i){
    # count the number of games in each stream
    count_games <- str_count(dt_streams$games[i], ",") + 1
    # calculate the exposure of each game
    exposure <- (dt_streams$viewers[i] * dt_streams$duration[i]) / count_games
    return(exposure)
}
#compute all the viewing times
result = as.data.table(sapply(1:nrow(dt_streams), game_exposure))
dt_streams$viewingtime = result
# Compute viewing_time --> 
dt_total_view_time = merge(dt_total_view_time, 
                           dt_streams, 
                           by = c("streamer","stream_title","date","time") )

dt_total_view_time$duration.y = NULL
dt_total_view_time$viewers.y = NULL
dt_total_view_time$unique_viewers.y = NULL
dt_total_view_time$followers.y = NULL
dt_total_view_time$games.y = NULL

#rename columns of the dataFrame

setnames(dt_total_view_time, old = c("duration.x", "viewers.x","unique_viewers.x","followers.x","games.x"),
         new = c("duration", "viewers","unique_viewers","followers","games"))

setnames(dt_total_view_time, old = c('viewingtime'),
         new = c('viewing_time'))



#3 sum total viewing times by stream title and date

dt_total_view_time_aggr= aggregate(viewing_time ~ stream_title + date, 
                                   data = dt_total_view_time, FUN = sum)
dt_total_view_time_aggr= as.data.table(dt_total_view_time_aggr);

#what is the most streamed game
mostStreamed = head(dt_total_view_time_aggr$stream_title[dt_total_view_time_aggr$viewing_time], 1)

#count streamers per day
streamer_count = aggregate(streamer ~ date + games, data = dt_total_view_time, FUN = length )
streamer_count = as.data.table(streamer_count)
#   Finally, because some streamers are "big" in that they attract many viewers, whereas other 
#   streamers are small. Compute a follower-weighted 
#   measure of the number of streamers by summing up all streamers who broadcast the game by
#   their followers (sum followers for those who broadcast game j)


# Compute the follower-weighted measure for each game
follower_weighted = aggregate(followers ~ stream_title + streamer,
                              data = dt_total_view_time,
                              FUN = sum)



# Prepare summary stats table 

result <- dt_total_view_time %>%
    group_by(games) %>%
    mutate(total_followers_for_game = sum(followers),
           streamer_weight = followers / total_followers_for_game) %>%
    ungroup()
print(result)


#############################
# Step 4: Is there an association between Twitch streaming and video game playing?
#############################

# Now we're ready to examine whether Twitch broadcasts are associated with the number of players in games
#   To begin with, merge the Twitch data on the daily total viewing hours and the number of streamers. 
#   Drop games that do not exist in both data, but keep all dates for games that exist in both data even if 
#   that date does not have streams. Finally, keep observations after 2017-01-01. 

#prepare our final data table
dt_final_data = NULL

#cooverting to data table
dt_final_data = as.data.table(dt_final_data)
#filter streamer count data starting at 2017-01-01
streamer_count = subset(streamer_count, date >= as.Date("2017-01-01"))
streamer_count = as.data.table(streamer_count)

dt_final_data <- streamer_count %>%
    left_join(select(dt_ga, app_id, title), by = c("games" = "title"))
dt_final_data = as.data.table(dt_final_data)

dt_final_data = subset(dt_final_data, !is.na(dt_final_data$app_id))


dt_final_data <- merge(dt_final_data, dt_sum[, c("app_id", "date","player_count")], 
                          by = c("app_id","date"))

#get total viewer tines
dt_total_view_time = as.data.table(dt_total_view_time)

dt_viewer_time_aggr = aggregate(viewing_time ~ games + date, data= dt_total_view_time,
                                FUN = sum)


dt_final_data <- merge(dt_final_data, dt_viewer_time_aggr[, c("games", "date","viewing_time")], 
                          by = c("games","date"))


#   For days when there is no stream, replace viewing hour, number of streamers,
#and sum of streamers' followers to zero.Drop observations without data on the number of players
#   Now examine the correlation between the number of streamers who broadcast the game and the number of players 
#   First, for each of the top-9 paid games we obtain from step 2,
#plot the number of players and the number of streamers. 


# Subset the merged_data_2017 for these top 9 games

subset_data <- dt_final_data %>% 
    filter(app_id %in% top_9_games$app_id)


# Extract player count and viewers data

number_players <- subset_data$player_count
number_streamer <- subset_data$streamer

# Calculate correlation coefficient for log player count and price

cor_coefficient <- cor(log(number_players), number_streamer, use = "complete.obs")

plot(x = number_players,y= number_streamer, xlab="Num. of Players", ylab="Number of Streamers" ,
     main = paste("Corelation coefficient: ",cor_coefficient))



#   Separately, plot the number of players against the log viewing hours + 1 (so if viewing_hours = 0
#log(viewing_hours + 1) is not NaN). 
#   In both cases, title the graph with the correlation coefficient between the two covariates. 
#   Alternatively, you can plot viewing hours, players, and number of streamers against time,
#just like what we did in step 2.GRAPH
#   Calculate correlation coefficient for log player count and price
#   Compute the correlation coefficient

cor_coefficient <- cor(number_players, log(subset_data$viewing_time + 1))


plot(number_players,log(subset_data$viewing_time+1), 
xlab="Number of Players", ylab="Viewing Time", main = paste("Corelation coefficient: ", cor_coefficient),
)




#CHATGpt history
#https://chat.openai.com/share/3b9ef630-e674-4afc-8418-812238a1d032
#https://chat.openai.com/share/214bdbd9-f172-4621-aadd-d47f7e7bbbf9
#https://chat.openai.com/share/303f9e45-b639-4ec4-ac94-43e1c51199bd
#https://chat.openai.com/share/aacdff0a-8002-4876-a922-c57a823ee99c