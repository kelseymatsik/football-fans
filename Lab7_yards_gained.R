library(mixtools) # mixture models 
library(mclust)

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
## Decision Tree Logic 
simulate_play <- function(field_position) {
  play_type <- sample(c("run", "pass"), size = 1, prob = c(0.5, 0.5))
  
  if (play_type == "pass") {
    interception <- rbinom(1, 1, 0.05)  
    if (interception == 1) return(list(type = "pass", result = "interception", yards = -sample(0:20, 1)))
    
    incompletion <- rbinom(1, 1, 0.3)
    if (incompletion == 1) return(list(type = "pass", result = "incomplete", yards = 0))
  }
  
  if (play_type == "run") {
    fumble <- rbinom(1, 1, 0.02)
    if (fumble == 1) return(list(type = "run", result = "fumble", yards = -sample(0:10, 1)))
  }
  
  # If play is successful, move to yards gained step
  yards <- sample_yards(field_position)
  list(type = play_type, result = "success", yards = yards)
}


## Test -- try later 
set.seed(123)
simulated_play1 <- yards_gained(fp = 50)  # Non-red zone
simulated_play2 <- yards_gained(fp = 85)  # Red zone

print(simulated_play1)
print(simulated_play2)

### FINISH LATER 
## Implement "decision tree" logic in sample_yards
