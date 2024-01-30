# Load the Cars data set
data("cars")
View(cars)

# Define variables
speed <- cars$speed
distance <- cars$dist

# Calculate descriptive statistics for Speed
speed_stats <- c(
  Min = min(speed),
  Max = max(speed),
  Q1 = quantile(speed, 0.25),
  Q3 = quantile(speed, 0.75),
  Mean = mean(speed),
  Median = median(speed),
  Standard_Deviation = sd(speed),
  Variance = var(speed),
  IQR = IQR(speed),
  MAD = mad(speed)
)

print(speed_stats)

# Calculate descriptive statistics for Distance
distance_stats <- c(
  Min = min(distance),
  Max = max(distance),
  Q1 = quantile(distance, 0.25),
  Q3 = quantile(distance, 0.75),
  Mean = mean(distance),
  Median = median(distance),
  Standard_Deviation = sd(distance),
  Variance = var(distance),
  IQR = IQR(distance),
  MAD = mad(distance)
)

print(distance_stats)

# Combine statistics into a data frame
stats_df <- data.frame(
  Variables = c("Speed", "Distance"),
  t(speed_stats),
  t(distance_stats)
)


# Print the table
print(stats_df)


# Load necessary libraries
library(ggplot2)


# Scatter plot Y~X
ggplot(cars, aes(x = speed, y = distance, )) +
  geom_point(color= "orange") +
  labs(title = "Scatter Plot of Stopping Distance vs. Speed",
       x = "Speed (mph)",
       y = "Stopping Distance (ft)")

# Boxplot of Y (Stopping Distance)
ggplot(cars, aes(y = distance)) +
  geom_boxplot(color= "seagreen") +
  labs(title = "Boxplot of Stopping Distance",
       y = "Stopping Distance (ft)")

# Boxplot of X (Speed)
ggplot(cars, aes(y = speed)) +
  geom_boxplot(color= "magenta") +
  labs(title = "Boxplot of Speed",
       y = "Speed (mph)")

# Histogram of Y (Stopping Distance)
ggplot(cars, aes(x = distance)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Stopping Distance",
       x = "Stopping Distance (ft)",
       y = "Frequency")

# Histogram of X (Speed)
ggplot(cars, aes(x = speed)) +
  geom_histogram(binwidth = 2, fill = "lightgreen", color = "black") +
  labs(title = "Histogram of Speed",
       x = "Speed (mph)",
       y = "Frequency")

