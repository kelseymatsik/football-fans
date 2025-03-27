###  Red zone: last 20 years before the end zone 
## Teams adjust their strategies in the red zone because they're close to scoring 
## Plays can't gain more than (100 - field_position) yards 

### 1. Split Yards Gained into Red Zone and Non-Red Zone 
# Define red zone as starting position ≥ 80 yards (within 20 yards of the end zone)
## Q: Bring in run_play (or another function) here? Not sure what play_data is for us. 
red_zone_data <- play_data[play_data$start_position >= 80, "yards_gained"]
non_red_zone_data <- play_data[play_data$start_position < 80, "yards_gained"]

### Fit mixture models 
mix_red_zone <- normalmixEM(red_zone_data, k = 2) # What is k=2?
mix_non_red_zone <- normalmixEM(non_red_zone_data, k = 2)

### Visualize
# plot(mix_red_zone, density = TRUE, main = "Red Zone Yardage Distribution")
# plot(mix_non_red_zone, density = TRUE, main = "Non-Red Zone Yardage Distribution")

### 2. Modify Yards Gained Sampling based on Field Position
sample_yards <- function(FP) { 
  if (FP >= 80) {
    component <- sample(1:length(mix_red_zone$lambda), size = 1, prob = mix_red_zone$lambda)
    yards <- rnorm(1, mean = mix_red_zone$mu[component], sd = mix_red_zone$sigma[component])
  } else {
    component <- sample(1:length(mix_non_red_zone$lambda), size = 1, prob = mix_non_red_zone$lambda)
    yards <- rnorm(1, mean = mix_non_red_zone$mu[component], sd = mix_non_red_zone$sigma[component])
  }
  
  # Ensure yards gained does not exceed the remaining field space
  max_yards <- 100 - field_position
  yards_gained <- min(yards, max_yards)
  yards_gained # Q: Not sure if this is yards gained 
}

### 3. Update Simulation to Incoporate Field Position 
## This is our NEW yards gained function
## References helper down functions 
yards_gained <- function(fp) {
  play <- run_play() # Calls run_play function to get the down, yards to go, and field position 
  if (play$play_result == "successful") {
    yards <- sample_yards(field_position)
    # Q: Does 'yards' and 'field_position' need to match our variable names in helper_down.R?
    return(list(type = play$play_type, result = "successful", yards = yards, new_position = field_position + yards))
  } else {
    play
  }
}

## Test -- try later 
set.seed(123)
simulated_play1 <- yards_gained(fp = 50)  # Non-red zone
simulated_play2 <- yards_gained(fp = 85)  # Red zone

print(simulated_play1)
print(simulated_play2)

### FINISH LATER 
## Implement "decision tree" logic in sample_yards