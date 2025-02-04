# Read in data 
game_data <- readRDS("~/Desktop/Sports Analytics/data.rds")
head(game_data)

## View unique teams
# Note: Home and visiting teams are the same
sort(unique(game_data$Home_Team))
sort(unique(game_data$Visiting_Team))

# View unique years 
unique(game_data$season)

################## 1 Probability Transition Matrix #############################
### 1.1: Construct the matrix 
# Note: Finding win/loss probability over ALL years 

# Create a vector of all teams 
teams <- sort(unique(c(game_data$Home_Team, game_data$Visiting_Team)))

#Initialize empty matrix 
n_teams <- length(teams)
transition_matrix <- matrix(0, nrow = n_teams, ncol = n_teams, dimnames = list(teams, teams ))

## Populate matrix with wins/losses 
for (i in 1:nrow(game_data)) { # Loop through data
  home_team <- game_data$Home_Team[i] # Index through each home and visiting team
  visiting_team <- game_data$Visiting_Team[i]
  
  home_score <- game_data$Home_Score[i] # Index through each home and visiting score
  visiting_score <- game_data$Visiting_Score[i]
  
  # Calculate wins/losses 
  if (home_score > visiting_score) { 
    transition_matrix[visiting_team, home_team] <- transition_matrix[visiting_team, home_team] + 1 # Adds 1 to appropriate matrx position
  } else if (visiting_score > home_score) {
    transition_matrix[home_team, visiting_team] <- transition_matrix[home_team, visiting_team] + 1
  }
}

# Normalize matrix by dividing each probability by the number of games played
transition_matrix <- sweep(transition_matrix, 1, rowSums(transition_matrix), FUN = "/") # sweep() function applies operation across rows
transition_matrix[is.na(transition_matrix)] <- 0 # Add 0s where they were no losses 

steady_state <- transition_matrix # Larger probabilities --> more wins 
sort(transition_matrix)

### 1.2: Establish a ranking using Markov chains 

# Initialize matrix with Uniform probability distribution 
state_vector <- rep(1 / nrow(transition_matrix), nrow(transition_matrix))

# Applies the transition matrix to state_vector 10,000 times 
for (i in 1:10000) {
  state_vector <- state_vector %*% transition_matrix
}
state_vector

# Agjust State Vector to Sort It

state_vectorT<-t(state_vector)

state_vector_df<-data.frame(state_vectorT)

library(tibble)

state_vector_with_rownames <- rownames_to_column(state_vector_df, var = "RowID")

#Sorted State Vector as a Data Frame

sorted_state<-state_vector_with_rownames[order(state_vector_with_rownames[, 2],decreasing=T), ]

sorted_state

sorted_state$ranking<-seq_along(sorted_state$RowID)

#-------------------------------------------------------------------------------------------------------------
# Question 2: Which teams appear in the yearly top five the most?


library(tidyverse)

# Function for creating a sorted state for just one specified year

yearly_matrix<-function(year){
  
  game_datayr<-game_data %>% filter(season==year)
  
  unique(game_datayr$season)
  
  ################## 1 Probability Transition Matrix #############################
  ### 1.1: Construct the matrix 
  # Note: Finding win/loss probability over ALL years 
  
  # Create a vector of all teams 
  teams <- sort(unique(c(game_datayr$Home_Team, game_datayr$Visiting_Team)))
  
  #Initialize empty matrix 
  n_teams <- length(teams)
  transition_matrix <- matrix(0, nrow = n_teams, ncol = n_teams, dimnames = list(teams, teams ))
  
  ## Populate matrix with wins/losses 
  for (i in 1:nrow(game_datayr)) { # Loop through data
    home_team <- game_datayr$Home_Team[i] # Index through each home and visiting team
    visiting_team <- game_datayr$Visiting_Team[i]
    
    home_score <- game_datayr$Home_Score[i] # Index through each home and visiting score
    visiting_score <- game_datayr$Visiting_Score[i]
    
    # Calculate wins/losses 
    if (home_score > visiting_score) { 
      transition_matrix[visiting_team, home_team] <- transition_matrix[visiting_team, home_team] + 1 # Adds 1 to appropriate matrx position
    } else if (visiting_score > home_score) {
      transition_matrix[home_team, visiting_team] <- transition_matrix[home_team, visiting_team] + 1
    }
  }
  
  # Normalize matrix by dividing each probability by the number of games played
  transition_matrix <- sweep(transition_matrix, 1, rowSums(transition_matrix), FUN = "/") # sweep() function applies operation across rows
  transition_matrix[is.na(transition_matrix)] <- 0 # Add 0s where they were no losses 
  
  steady_state <- transition_matrix # Larger probabilities --> more wins 
  sort(transition_matrix)
  
  ### 1.2: Establish a ranking using Markov chains 
  
  # Initialize matrix with Uniform probability distribution 
  state_vector <- rep(1 / nrow(transition_matrix), nrow(transition_matrix))
  
  # Applies the transition matrix to state_vector 10,000 times 
  for (i in 1:10000) {
    state_vector <- state_vector %*% transition_matrix
  }
  state_vector
  
  # Agjust State Vector to Sort It
  
  state_vectorT<-t(state_vector)
  
  state_vector_df<-data.frame(state_vectorT)
  
  state_vector_with_rownames <- rownames_to_column(state_vector_df, var = "RowID")
  
  #Sorted State Vector as a Data Frame
  
  sorted_state<-state_vector_with_rownames[order(state_vector_with_rownames[, 2],decreasing=T), ]
  
  sorted_state$ranking<-seq_along(sorted_state$RowID)
  sorted_state$Year<-year
  
  return(sorted_state)
  
}


#This for loop creates a ranking for every year in the data set and combines it into one df.

all_years<-unique(game_data$season)

combined_years<-matrix(ncol = 3, nrow = 0)
for (i in all_years){
  combined_years<-rbind(combined_years,yearly_matrix(i))
}


# The df filtered to have only top five for each year.

top5<-combined_years %>% filter(ranking<=5)

# The counts for each team for the number of times they are in the top five.

table(top5$RowID)

# The teams that appeared the most in the top five across the years were LAN with 7 appearances,
# HOU with 5 appearances,BOS and ATL with four appearances.
