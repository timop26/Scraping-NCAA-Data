# Installing package to web scrape,  only needs to be ran once
# install.packages("rvest") 
library(rvest)
library(caret)
classes <- c("Fr", "So", "Jr", "Sr")
position <- c("PG", "SG", "SF", "PF", "C")
conference <- c("America-East-Conference/18", "American-Athletic-Conference/4", "Atlantic-10-Conference/10", 
                "Atlantic-Coast-Conference/1", "Atlantic-Sun-Conference/19", "Big-12-Conference/3", "
                Big-East-Conference/59", "Big-Sky-Conference/20", "Big-South-Conference/21", 
                "Big-Ten-Conference/2", "Big-West-Conference/22", "Colonial-Athletic-Association/15", 
                "Conference-USA/9", "Great-West-Conference/24", "Horizon-League/12", "Ivy-League/14", 
                "Metro-Atlantic-Athletic-Conference/17", "Mid-American-Conference/25", 
                "Mid-Eastern-Athletic-Conference/26", "Missouri-Valley-Conference/5", "Mountain-West-Conference/6", 
                "Northeast-Conference/27", "Ohio-Valley-Conference/16", "Pacific-12-Conference/7", 
                "Patriot-League/28", "Southeastern-Conference/8", "Southern-Conference/29", "Southland-Conference/30", 
                "Southwestern-Athletic-Conference/31", "Sun-Belt-Conference/33", "The-Summit-League/32", 
                "West-Coast-Conference/11", "Western-Athletic-Conference/13")
# Creating data frame with every possible combination of class, position, and conference
all_combos <- expand.grid(classes, position, conference, stringsAsFactors=FALSE)

## Scraping data from this website: https://basketball.realgm.com/ncaa/stats/2019/Averages/All/All/Season/All/points/desc/1/"
url <- "https://basketball.realgm.com/ncaa/stats/2019/Averages/All/All/Season/All/points/desc/1/"
test <- html_table(html_nodes(read_html(url), "table"))[[1]]

# Function to read in player data from a vector with class, position, and conference and a year and stat_type
read.micro.data <- function(x, year, stat_type){
  # Taking the inputed vector of class, position, and conference, the year, and the stat_type to create url and scrape it
  stats <- html_table(html_nodes(read_html(
    paste0("https://basketball.realgm.com/ncaa/conferences/", x[3], "/stats/", year, "/", 
           stat_type, "/Qualified/", x[1], "/Season/", x[2], "/per/desc/1/")), "table"))[[1]]
  # Outputing data frame with the scraped stats and the class, position, and conference of each player
  data.frame(stats, class=x[1], position=x[2], conference=x[3], row.names=NULL)
}

# Testing that the read.micro.data function works. Grabbing Averages for Freshmen PGs in AAC in 2018
test <- read.micro.data(c("Fr",  "PG",  conference[2]),  2018,  "Averages")

read.ncaa.stats <- function(year, stat_type){
  # stat_type options: Advanced_Stats, Averages, Misc_Stats, Per_36, Totals
  data_in_lists <- lapply(1:nrow(all_combos), function(x) 
    tryCatch(expr={read.micro.data(all_combos[x, ], year, stat_type)}, error=function(e){"none"}))
  list.condition <- sapply(data_in_lists, function(x) class(x) == "data.frame")
  data_in_lists  <- data_in_lists[list.condition]
  output <- do.call(rbind, data_in_lists)
  colnames(output)[colnames(output) == "Var1"] <- "class"
  colnames(output)[colnames(output) == "Var2"] <- "position"
  colnames(output)[colnames(output) == "Var3"] <- "conference"
  return(output[, -1])
}

# Testing that the read.ncaa.stats function works. Grabbing Averages for entire 2018 season
start <- Sys.time()
test <- read.ncaa.stats(2018, "Averages")
end <- Sys.time()
end - start # Takes about 18 minutes on my computer

# Function to merge different types of stats for each player for a specific year
merge_data <- function(year){
  advanced <- read.ncaa.stats(year, "Advanced_Stats")
  averages <- read.ncaa.stats(year, "Averages")
  return(merge(advanced, averages, by=c("Player", "class", "position", "conference", "Team")))
}
# Grabbing all statistics for NCAA players in 2017-2019 (takes awhile to run)
ally17 <- data.frame(merge_data(2017), year=2017) 
ally18 <- data.frame(merge_data(2018), year=2018)
ally19 <- data.frame(merge_data(2019), year=2019)
# Adding all players from 2017-2019 to one data frame
ncaa17_19 <- rbind(ally17, ally18, ally19)

# Writing data frame to a csv file so the web scraping only has to be done once
#write.csv(ncaa17_19, "NCAA17-19.csv")
# Reading in the data if data has been written to a csv
# ncaa17_19 <- read.csv("NCAA17-19.csv")

# Seeing how accurately we can predict a player's sophomore year PPG based on their freshman year stats
players <-ncaa17_19$Player[ncaa17_19$class == "Fr"][ncaa17_19$Player[ncaa17_19$class == "Fr"] %in% 
                                                      ncaa17_19$Player[ncaa17_19$class == "So"]]
# Selecting only players with their freshman and sophomore season
underclass <- ncaa17_19[ncaa17_19$Player %in% players & ncaa17_19$class %in% c("Fr", "So"), ]
# Fixing data for multiple players with the same name
sort(table(underclass$Player))
# Removing seasons for players with duplicate names but not a freshman and sophomore season
underclass <- underclass[!(underclass$Player == "A.J. Lawson" & underclass$position == "SG"), ]
underclass <- underclass[!(underclass$Player == "Jalen Harris" & underclass$position == "SG"), ]
underclass <- underclass[!(underclass$Player == "Jordan Davis" & underclass$position == "SF"), ]
underclass <- underclass[!(underclass$Player == "Terrell Brown" & underclass$position == "SG"), ]
# Saving sophomore scoring
sophomore_scoring <- underclass[underclass$class == "So", c("Player", "Team", "PPG")]
# Subsetting freshman data
freshman <- underclass[underclass$class == "Fr", ]
# Sorting data frames to make joins easier
sophomore_scoring <- sophomore_scoring[order(sophomore_scoring$Player, sophomore_scoring$Team), ]
freshman <- freshman[order(freshman$Player, freshman$Team), ]
# Adding the PPG from sophomore seasons to the freshman data set
freshman$PPG_soph <- sophomore_scoring$PPG

# Exploratory Data Analysis
# response: PPG_soph
# explanatory variables: any of the freshman year statistics
# Plotting freshman PPG against sophomore PPG
plot(freshman$PPG, freshman$PPG_soph)
abline(c(0, 1))
# Players generally score more points as sophomores than as freshman
# Plotting freshman PPG against sophomore PPG
plot(freshman$PER, freshman$PPG_soph)
# The better the freshman PER, the more likely to score more the second year

# Using backward selection and prediction accuracy to determine best combination 
### of variables to use
# Set up repeated k-fold cross-validation
train.control <- trainControl(method="cv", number=30)
# Train the model on set of variables that could predict sophomore PPG
step.model <- train(PPG_soph ~ TS. * position + eFG. + TRB. * position + AST. * position + 
                      TOV. + STL. + USG. * position + PPS + PER + GP + MPG + FGA + X3PM + 
                      X3PA + X3P. + FTM + FTA + FT. + TOV + RPG + APG + SPG + BPG + PPG, 
                    data=freshman, method = "leapBackward", tuneGrid=data.frame(nvmax=1:20),
                    trControl=train.control)
summary(step.model$finalModel)
# Most simple model that accurately predicts sophomore PPG uses USG%, TOV, APG, and PPG
best_model <- lm(PPG_soph ~ USG. + TOV + APG + PPG, data=freshman)
# MAE of model
step.model$results[4, 4] # Model is 2.38 PPG off on average

# Plotting predicted sophomore PPG against actual sophomore PPG
plot(best_model$fitted.values, freshman$PPG_soph, xlab="Predicted", ylab="Sophomore PPG")
abline(c(0, 1))

# Top 10 players that scored much better than expected
top10 <- freshman[order(best_model$residuals, decreasing=TRUE)[1:10], ]
top10$predicted <- best_model$fitted.values[order(best_model$residuals, decreasing=TRUE)[1:10]]
top10[, c("Player", "PER", "MPG", "RPG", "APG", "PPG", "PPG_soph", "predicted")]
# Labeling top 10 players on the plot
points(top10$predicted, top10$PPG_soph, col="red", pch=19)
text(top10$PPG_soph ~ top10$predicted, labels=top10$Player, cex=0.75, font=1, pos=2)

## Predicting sophomore PPG for 2020 freshman
ally20 <- data.frame(merge_data(2020), year=2020)
freshman20 <- ally20[ally20$class == "Fr", ]
predict20 <- predict(best_model, newdata=freshman20, interval="prediction")
colnames(predict20) <- c("Prediction", "Lower PI", "Upper PI")
freshman20 <- cbind(freshman20, predict20)
# Players with the highest predicted PPG
freshman20[order(freshman20$Prediction, decreasing=TRUE)[1:20], 
           c("Player", "Team", "PER", "MPG", "RPG", "APG", "PPG", "Prediction", "Lower PI", "Upper PI")]
# The prediction intervals are more wide than I'd like, the predictions don't
### have a lot of variety. Both show there is definitely room for improvement 
# Players with the largest predicted jump
freshman20[order(freshman20$Prediction - freshman20$PPG, decreasing=TRUE)[1:20], 
           c("Player", "Team", "PER", "MPG", "RPG", "APG", "PPG", "Prediction", "Lower PI", "Upper PI")]


