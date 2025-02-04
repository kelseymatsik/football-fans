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

state_vector <- c(
  ANA = 0.03219857, 
  ARI = 0.03200487, 
  ATL = 0.03445328, 
  BAL = 0.03316139, 
  BOS = 0.03584793,
  CHA = 0.02930756, 
  CHN = 0.0348977, 
  CIN = 0.03064543, 
  CLE = 0.03454301, 
  COL = 0.0304538,
  DET = 0.02959835, 
  HOU = 0.03721329, 
  KCA = 0.02999425, 
  LAN = 0.03948072, 
  MIA = 0.02969201,
  MIL = 0.03498321, 
  MIN = 0.03187847, 
  NYA = 0.03830918, 
  NYN = 0.03341753, 
  OAK = 0.03197763,
  PHI = 0.03243858, 
  PIT = 0.03138499, 
  SDN = 0.03235099, 
  SEA = 0.03394242, 
  SFN = 0.03400499,
  SLN = 0.03536689, 
  TBA = 0.0366062, 
  TEX = 0.03184672, 
  TOR = 0.03532205, 
  WAS = 0.032678
)
sorted_state_vector <- sort(state_vector, decreasing = TRUE)
sorted_state <- as.data.frame(sorted_state_vector)
sorted_state
