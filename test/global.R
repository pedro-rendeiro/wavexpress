# global.R

# Sample data
sample_data <- data.frame(
  Name = c("Alice", "Bob", "Carol"),
  Age = c(25, 30, 28),
  Score = c(85, 92, 78)
)

# Function to calculate average score
calculate_average <- function(data) {
  return(mean(data$Score))
}
