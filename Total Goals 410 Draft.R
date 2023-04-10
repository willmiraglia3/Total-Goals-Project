
library(caret)
library(purrr)
library(stringr)
library(lubridate)
library(dplyr)
library(gdata)
library(ggplot2)


# Set the file path to your CSV file
game_path <- "~/Desktop/archive/understat_per_game.csv"
team_path <- "~/Desktop/archive/understat.com.csv"
matches_path <- "~/Desktop/archive/results.csv"
odds_2019_path <- "~/Desktop/archive/2019.csv"
odds_2018_path <- "~/Desktop/archive/2018.csv"
odds_2017_path <- "~/Desktop/archive/2017.csv"
odds_2016_path <- "~/Desktop/archive/2016.csv"
odds_2015_path <- "~/Desktop/archive/2015.csv"
odds_2014_path <- "~/Desktop/archive/2014.csv"


# Use read.csv() to read the file into a data frame
game <- read.csv(game_path)
matches <- read.csv(matches_path)
odds_2019 <- read.csv(odds_2019_path)
odds_2018 <- read.csv(odds_2018_path)
odds_2017 <- read.csv(odds_2017_path)
odds_2016 <- read.csv(odds_2016_path)
odds_2015 <- read.csv(odds_2015_path)
odds_2014 <- read.csv(odds_2014_path)

# clean each odds so that the dataframes match and are interpretable
clean_odds <- function(df, yr) {
  df$year <- yr
  df$Date <- gsub("/", "-", df$Date)
  if (yr == '2019') {
    df <- df %>% 
      rename(BbAv.2.5 = Avg.2.5, BbAv.2.5.1 = Avg.2.5.1)
  }
  df <- subset(df, select = c("Date", "HomeTeam", "AwayTeam", "year", "BbAv.2.5", "BbAv.2.5.1"))
  return(df)
}

# Clean the odds
odds_2019 <- clean_odds(odds_2019, '2019')
odds_2018 <- clean_odds(odds_2018, '2018')
odds_2017 <- clean_odds(odds_2017, '2017')
odds_2016 <- clean_odds(odds_2016, '2016')
odds_2015 <- clean_odds(odds_2015, '2015')
odds_2014 <- clean_odds(odds_2014, '2014')

# Join the odds data from each year into 1 dataframe
odds <- rbind(odds_2019, odds_2018, odds_2017, odds_2016, odds_2015, odds_2014)


# Clean the matches data so that I have the schedule of each game from 2014-2019
# Also, this dataset has basic stats from each game
matches$Season <- substr(matches$Season, 1, 4)
matches <- matches[as.numeric(substr(matches$Season, 1, 4)) %in% 2014:2019, ]
matches$DateTime <- substr(matches$DateTime, 1, 10)
matches$DateTime <- format(as.Date(matches$DateTime), "%d-%m-%Y")
matches <- rename(matches, Date = DateTime)

# Merge the matches and odds so that I have the odds for each respective game
merged_df <- merge(matches, odds, by = c("Date", "HomeTeam", "AwayTeam"))

# Subset the odds so that there is just Over/Under Odds for 2.5 Goals
subset_df <- subset(merged_df, select = c("Date", "HomeTeam", "AwayTeam", "Season", "FTHG", "FTAG", "BbAv.2.5", "BbAv.2.5.1"))
# clean for future use
names(subset_df)[names(subset_df) == "Season"] <- "year"

# Get the Premier League Data from our Expected Goals Dataframe
colnames(game)[which(colnames(game) == "missed")] <- "allowed"
premier <- subset(game, league == 'EPL')

## All teams and years
teams <- unique(premier$team)
years <-c(2014, 2015, 2016, 2017, 2018, 2019)


## This function finds the average statistics for each team up till March 1
## We will then use this data for the rest of the season.
team_avgs <- function(team_name) {
  premier <- subset(game, league == 'EPL' & team == team_name)
  year_data <- list()
  year_regs <- list()
  year_avgs <- list()
  
  for (year in years) {
    ################## EXPECTED GOALS DATA ################## 
    ## Subsets By Year
    year_data[[year]] <- premier[premier$year == as.character(year), ]
    ## Sorts the games by date
    year_data[[year]] <- year_data[[year]][order(year_data[[year]]$date), ]
    year_data[[year]]$date <- as.Date(year_data[[year]]$date, format = "%Y-%m-%d")
    final_date <- paste0(as.numeric(year) + 1, "-03-01")
    ## Gets all games before March 1 of the next year
    year_data[[year]] <- year_data[[year]] %>%
      group_by(team) %>%
      filter(date < as.Date(final_date))
    
    ## Finds the variables we want averages of
    year_regs <- select(year_data[[year]], -c(league, h_a, xpts,
                                              xpts, result, date, wins,
                                              draws, loses, pts, team,
                                              xpts_diff))
    
    
    ## Computes the averages and adds a row to year_avgs
    year_avgs_row <- colMeans(year_regs[, -1])
    year_avgs_row <- c(as.character(year_regs$team[1]), year_avgs_row)
    year_avgs <- rbind(year_avgs, year_avgs_row)
    rownames(year_avgs) <- NULL
  }
  
  team_data <- subset(new_df, team == team_name)
  year_data_2 <- list()
  year_regs_2 <- list()
  year_avgs_2 <- list()
  
  for (year in years) {
    ################## GAME DATA ################## 
    ## Subsets By Year
    year_data_2[[year]] <- team_data[team_data$year == as.character(year), ]
    ## Sorts the games by date
    year_data_2[[year]] <- year_data_2[[year]][order(year_data_2[[year]]$date), ]
    year_data_2[[year]]$date <- as.Date(year_data_2[[year]]$date, format = "%Y-%m-%d")
    final_date <- paste0(as.numeric(year) + 1, "-03-01")
    ## Gets all games before March 1 of the next year
    year_data_2[[year]] <- year_data_2[[year]] %>%
      group_by(team) %>%
      filter(date < as.Date(final_date))
    
    ## Finds the variables we want averages of
    year_regs_2 <- select(year_data_2[[year]], -c(date, team))
    year_regs_2$year <- as.numeric(year_regs_2$year)
    
    ## Computes the averages and adds a row to year_avgs_2
    year_avgs_2_row <- colMeans(year_regs_2[, -1])
    year_avgs_2_row <- c(as.character(year_regs_2$team[1]), year_avgs_2_row)
    year_avgs_2 <- rbind(year_avgs_2, year_avgs_2_row)
    rownames(year_avgs_2) <- NULL
  }
  # rename the columns to join them
  colnames(year_avgs)[1] <- "team"
  colnames(year_avgs_2)[1] <- "team"
  
  # merge the dataframes
  ovr_year_avgs <- merge(year_avgs, year_avgs_2, by = c("team", "year"))
  return(ovr_year_avgs)
}

## Get the average data for each team
tab1 <- c()
for (name in teams) {
  temp <- team_avgs(name)
  tab1 <- rbind(tab1, temp)
}

## Clean the averages dataframe
tab1_df <- as.data.frame(tab1)
colnames(tab1_df)[1] <- "team"
averages_data <- subset(tab1_df, team != 'NANA')

## Match team names from the matches and averages data 
averages_data$team <- ifelse(averages_data$team == "West Bromwich Albion", "West Brom", averages_data$team)
averages_data$team <- ifelse(averages_data$team == "Manchester United", "Man United", averages_data$team)
averages_data$team <- ifelse(averages_data$team == "Manchester City", "Man City", averages_data$team)
averages_data$team <- ifelse(averages_data$team == "Wolverhampton Wonderers", "Wolves", averages_data$team)
averages_data$team <- ifelse(averages_data$team == "Queens Park Rangers", "QPR", averages_data$team)
averages_data$team <- ifelse(averages_data$team == "Newcastle United", "Newcastle", averages_data$team)

## Clean the averages dataframe
averages_data <- averages_data %>% mutate(year = as.character(year))
averages_data <- averages_data %>% mutate(team = as.character(team))
averages_data <- averages_data %>% mutate(xG = as.numeric(xG))
averages_data <- averages_data %>% mutate(xGA = as.numeric(xGA))
averages_data <- averages_data %>% mutate(scored = as.numeric(scored))
averages_data <- averages_data %>% mutate(allowed = as.numeric(allowed))

## Make copies of the averages data so that the averages have columns with suffix
## _home or _away. For example, scored_home and scored_away
home_df <- averages_data %>% rename_with(~paste0(., "_home"))
away_df <- averages_data %>% rename_with(~paste0(., "_away"))

## Merge the home and away averages data onto the schedules
## Now, each row has data for each team based on the year the game happened
just_home <- inner_join(subset_df, home_df, by = c("year" = "year_home", "HomeTeam" = "team_home"))
full_merged <- inner_join(just_home, away_df, by = c("year" = "year_away", "AwayTeam" = "team_away"))

## Clean our new fully merged dataframe
full_merged$Date <- as.Date(str_trim(full_merged$Date), format = "%d-%m-%Y")
full_merged$Date <- as.Date(full_merged$Date)
full_merged$year <- as.numeric(full_merged$year)

## Since we took averages from the games before March 1st in the season,
## this code takes the games for the rest of the season for us to use the data on
full_merged_subset <- list()
for (yr in years) {
  year_end_date <-paste0(as.numeric(yr)+1, "-03-01")
  year_subset <- subset(full_merged, year == yr & Date > as.Date(year_end_date))
  full_merged_subset[[yr]] <- year_subset
}
# Add all rows years to one dataframe
full_merged <- bind_rows(full_merged_subset)

# Get our regressors and make it interpretable
regressors <- select(full_merged, 5:ncol(full_merged))
regressors <- sapply(regressors, as.numeric)
regressors <- as.data.frame(regressors)

# Get our total dataframe (which is what we will predict)
totals <- data.frame(total = regressors$FTHG + regressors$FTAG)
regressors <- select(regressors, 3:ncol(regressors))

# Run the model
model <- lm(totals$total ~ BbAv.2.5 + xG_home + npxG_home + npxGA_home + 
              deep_home + deep_allowed_home + oppda_att_home + xG_away + 
              xGA_away + npxG_away + scored_away + allowed_away + oppda_def_away + 
              F_home + Y_home + ST_away + F_away, data = regressors)
summary(model)

## Get the predictions from our model
predictions <- predict(model)
totals$predicted <- predictions

## Find if the bet would have won simply
totals$win <- ifelse((totals$total > 2.5 & totals$predicted > 2.5),  "Win", ifelse(totals$total < 2.5 & totals$predicted < 2.5, "Win", "Loss"))
## Counts the wins and losses
table(totals$win)
## Use z-score to make predictions
totals$z_score <- (totals$predicted - mean(totals$predicted)) / sd(totals$predicted)
totals$z_score_win <- ifelse(totals$z_score > 0 & totals$total > 2.5, "Win", 
                             ifelse(totals$z_score < 0 & totals$total < 2.5, "Win", "Loss"))
## Counts wins and losses
table(totals$z_score_win)

### Find Profit
totals$bet_amt <- 100
totals$over_odds <- regressors$BbAv.2.5
totals$under_odds <- regressors$BbAv.2.5.1
## Find What we would have bet
totals$bet <- ifelse(totals$predicted > 2.5, "OVER", "UNDER")
## Calculate the profit from the bet
totals$profit <- ifelse(totals$win == "Win", ifelse(totals$bet == "OVER", totals$bet_amt*totals$over_odds - totals$bet_amt, 
                                                    totals$bet_amt*totals$under_odds - totals$bet_amt), -totals$bet_amt)

## Winnings
total_profit <- sum(totals$profit)
return_percent <- sum(totals$profit)/sum(totals$bet_amt)

## Plot the actual vs. predicted
ggplot(aes(y = predicted, x = total, color = win), data = totals) +
  geom_point() +
  geom_smooth(col = "goldenrod") +
  geom_jitter(width = 0.1) +
  theme_minimal() +
  geom_hline(yintercept = 2.5, lty = 16) +
  labs(x = "Total", y = "Predicted Goals", color = "Result") +
  ggtitle("Actual Goals vs. Predicted") +
  scale_color_manual(values = c("maroon", "forest green"))


#### Exploring Feature Selection ####

## Forward AIC
mint <- lm(totals$total~1,data=regressors)
forwardAIC <- step(mint,scope=list(lower=~1, 
                                   upper=~BbAv.2.5 + BbAv.2.5.1 + xG_home + xGA_home + npxG_home + npxGA_home + 
                                     deep_home + deep_allowed_home + scored_home + allowed_home + 
                                     npxGD_home + ppda_coef_home + ppda_att_home + ppda_def_home +
                                     oppda_coef_home + oppda_coef_home + oppda_att_home + oppda_def_home + 
                                     xG_diff_home + xGA_diff_home +
                                     xG_away + xGA_away + npxG_away + npxGA_away + 
                                     deep_away + deep_allowed_away + scored_away + allowed_away + 
                                     npxGD_away + ppda_coef_away + ppda_att_away + ppda_def_away +
                                     oppda_coef_away + oppda_coef_away + oppda_att_away + oppda_def_away + 
                                     xG_diff_away + xGA_diff_away + 
                                     S_home + ST_home + C_home + F_home + Y_home + R_home +
                                     S_away + ST_away + C_away + F_away + Y_away + R_away),
                   direction="forward", data=regressors)


## Backward AIC
m1 <- lm(totals$total ~ BbAv.2.5 + BbAv.2.5.1 + xG_home + xGA_home + npxG_home + npxGA_home + 
           deep_home + deep_allowed_home + scored_home + allowed_home + 
           npxGD_home + ppda_coef_home + ppda_att_home + ppda_def_home +
           oppda_coef_home + oppda_coef_home + oppda_att_home + oppda_def_home + 
           xG_diff_home + xGA_diff_home +
           xG_away + xGA_away + npxG_away + npxGA_away + 
           deep_away + deep_allowed_away + scored_away + allowed_away + 
           npxGD_away + ppda_coef_away + ppda_att_away + ppda_def_away +
           oppda_coef_away + oppda_coef_away + oppda_att_away + oppda_def_away + 
           xG_diff_away + xGA_diff_away + 
           S_home + ST_home + C_home + F_home + Y_home + R_home +
           S_away + ST_away + C_away + F_away + Y_away + R_away, data = regressors)
backAIC <- step(m1,direction="backward", data=regressors)


install.packages('glmnet')
library(glmnet)

## Ridge
x <- model.matrix(totals$total ~ ., data = regressors)
y <- totals$total
ridge_model <- cv.glmnet(x, y, alpha = 0.5)

coef(ridge_model, s = "lambda.min")



## Other tests for linear regression models

install.packages('lmtest')
library(lmtest)
library(car)

dw <- dwtest(model)
dw$statistic ## no autocorrelation as value = 2

cooksd <- cooks.distance(model)
plot(cooksd, pch = 20, main = "Cook's distance plot")
abline(h = 4/length(totals$total), col = "red")

vif(model)
alias(model)


#########################################