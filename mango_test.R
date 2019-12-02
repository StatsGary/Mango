
#=============================Mango coding test ==================================#
#==============================Candidate - Gary Hutson ===========================#

install_or_load_pack <- function(pack){
  create.pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
  if (length(create.pkg))
    install.packages(create.pkg, dependencies = TRUE)
  sapply(pack, require, character.only = TRUE)
  
}
packages <- c("data.table", "tidyverse", "caret", "rlang", "gganimate", "R6", "sloop", "e1071")
install_or_load_pack(packages)
theme_set(theme_classic())
###################### ---------------------- Question 1 -------------------------------------##############

#path <- "C:\\Users\\GaryHutson\\Desktop\\Mango_Test"
setwd("~/D & D - Gary Personal/NBA2")
getwd()

###################### ----------------------READ IN DATA-------------------------------------##############

nba_df <- read_csv("free_throws.csv",
                   col_types =  cols(end_result = col_character(),
                                     game = col_character(),
                                     game_id = col_double(),
                                     period = col_double(),
                                     play = col_character(),
                                     player = col_factor(),
                                     playoffs = col_character(),
                                     score = col_character(),
                                     season = col_number(),
                                     shot_made = col_double(),
                                     time = col_time(format = "")),
                   col_names = T,
                   skip_empty_rows = F)

glimpse(nba_df)
head(nba_df, 20)

store_df <- function()

# For big data this could be changed to a DT (data.table) format



###################### ----------------------VISUALISE SOME ASPECT OF DATA------------------------------##############

nba_df %<>%
  mutate(period_one = case_when(period == 1 ~ 1,
                                TRUE ~ 0)) %>%
  mutate(period_two = case_when(period == 2 ~ 1,
                                TRUE ~ 0)) %>%
  mutate(period_three = case_when(period == 3 ~ 1,
                                  TRUE ~ 0)) %>%
  mutate(period_four = case_when(period == 4 ~ 1,
                                 TRUE ~ 0)) %>%
  mutate(regular_match = case_when(playoffs == "regular" ~ 1,
                                   TRUE ~ 0)) %>%
  mutate(playoff_match = case_when(playoffs == "playoff"  ~ 1,
                                   TRUE ~ 0)) %>%
  select(-c("period"))


#Adds additional flags for performing other types of analysis

summary(nba_df)

# A bit of DPLYR to get the data into a summarised fashion
# Tidyr was considered here as an alternative approach to how I have approached it


player_agg <- nba_df %>%
  group_by(player, season, game, playoffs) %>%
  summarise(sum_shot_made_player = sum(shot_made),
            mean_rate = mean(shot_made),
            shot_opportunities=n(),
            sum_shot_made_q1 = sum(period_one),
            sum_shot_made_q2 = sum(period_two),
            sum_shot_made_q3 = sum(period_three),
            sum_shot_made_q4 = sum(period_four)) %>%
  ungroup() %>%
  mutate(conversion_rate = sum_shot_made_player / shot_opportunities) %>%
  mutate(shots_missed = shot_opportunities - sum_shot_made_player) %>%
  mutate(shots_q1_to_total = sum_shot_made_q1 / sum_shot_made_player) %>%
  mutate(shots_q2_to_total = sum_shot_made_q2 / sum_shot_made_player) %>%
  mutate(shots_q2_to_total = sum_shot_made_q2 / sum_shot_made_player) %>%
  mutate(made_to_missed_ratio = sum_shot_made_player / shots_missed) %>%
  arrange(season, desc(sum_shot_made_player))


#Use ggplot to make the visualisation of this - I prefer not to chain plots into DPLYR, although I know this is trendy

# I want the season and game type to be combined - I will use tidyR here to combine them

agg_player <- player_agg %>%
  tidyr::unite(col = game_type, c("game","playoffs"), sep = " ")

agg_player$game_type <- factor(agg_player$game_type)

#Convert to factor to give implicit ordering
glimpse(agg_player)

agg_player <- cbind(agg_player, playoff_game=player_agg$playoffs)

#Make a cool bubble chart to store these values and do a facet to wrap

test <- agg_player[agg_player$sum_shot_made_player > 30,]

plot <- ggplot(agg_player,
               aes(x=sum_shot_made_player, y=shots_missed, size=conversion_rate,
                   colour= factor(playoff_game))) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  geom_text(aes(sum_shot_made_player, shots_missed, label = player), data = agg_player[agg_player$sum_shot_made_player > 30,],
            position=position_dodge(width = 0.9), color="black") +
  scale_color_manual(values = c("#ff0000", "#ff4d00")) +
  #scale_size(range = c(2, 12)) +
  labs(title = 'Season: {frame_time}',
       x = 'Free Throws Made',
       y= 'Free Throws Missed') #+ theme(legend.position = 'none')

#geom_text(position = position_dodge(width = 1), vjust = 0.4, color = "white", size = 4, 
#fontface = "bold", show.legend = FALSE)

plot <- plot + transition_time(season)
image <- animate(plot)
anim_save("NBA_Player_Comparisons.gif", image)


###################### ---------------------- Question 2 -------------------------------------##############


random_sampleR <- function(x, dist, ...){
  
  if (dist == "norm"){
    message("Generating Gaussian Distribution")
    result <- rnorm(x,...)
    
  } else if(dist == "pois"){
      message("Generating Poisson Distribution")
      result <- rpois(x,...)
    
  } else if (dist == "binom"){
      message("Generating Biomial Distribution")
      result <- rbinom(x, ...)
  } else{
    stop("Please use dist = binom, pois, norm to use this function")
  }
  
  # Return the result of the function
  attr(result, "class") <- "myClass"
  # Attributes the result of the function to a class
  return(result)
  
}


# Use function
size <- 5000
random_sampleR(size, "norm", mean = 100)
# Calls the function passing the norm argument
random_sampleR(size, "pois", lambda = 10)
# Calls the function passing the poisson (pois) argument
random_sampleR(size, "binom", size = 100, prob = 0.5)
# Calls the function passing the bionial (binom) argument
random_sampleR(size, "error")
# Add value to raise an error


###################### ---------s----------- Question 3 - OOP -----------------------------------##############

# Create method to get a custom summary of my function

x <- random_sampleR(size, "norm", mean = 100)
x

unname(quantile(x, 0.10))

summary.myClass <- function(x){
  
  results <- data.frame(Mean = mean(x),
                      Standard_Dev = sd(x),
                      Median = median(x),
                      Min = min(x),
                      Max = max(x),
                      IQR = IQR(x),
                      Kurtosis = e1071::kurtosis(x),
                      Skew = e1071::skewness(x),
                      Quantile_25th = unname(quantile(x, 0.25)),
                      Quantile_75th = unname(quantile(x, 0.75)),
                      Quantile_90th = unname(quantile(x, 0.9)),
                      Quantile_95th = unname(quantile(x, 0.95)),
                      Quantile_99th = unname(quantile(x, 0.99)),
                      Size = length(x))
  
  
  return(results)
}

# Check that the new method is now an S3 method

sloop::ftype(summary.myClass)

summary(x)







