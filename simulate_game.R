# Top-Level Function (simulates game)
simulate_game <- function(down, ytg, fp, n = 1000) {
  results <- numeric(n)
  for (i in 1:n) {
    results[i] <- simulate_epoch(down, ytg, fp)
  }
  mean(results)
}