#Red finch advantage

# Install ggplot2 if you haven't already
# install.packages("ggplot2")

library(ggplot2)
# Clear all variables in the environment
rm(list = ls())

# Create the groups
long_birds <- rep("Red", 10)   # 5 Red individuals
short_birds <- rep("Blue", 10)  # 5 Blue individuals

# Combine both groups into one vector
group <- c(short_birds, long_birds)

# Create a data frame for plotting
group_df <- data.frame(ID = 1:20, Group = group)

# Assign probabilities: higher probability for Blue to be killed
kill_probs <- ifelse(group_df$Group == "Blue", 0.75, 0.25)  # 75% chance for Blue, 30% for Red

# Shuffle the group and randomly select 5 individuals to "kill" based on probabilities
killed_indices <- sample(1:20, 5, prob = kill_probs)  # Randomly select 5 indices to kill with unequal probabilities

# Mark Killed Individuals in the data frame
group_df$Killed <- FALSE  # Initialize the Killed column
group_df$Killed[killed_indices] <- TRUE  # Mark the killed individuals

# Print the killed individuals
cat("Killed individuals:\n")
print(group_df[killed_indices, ])

# Plot the groups and mark the killed individuals with "X"
p <- ggplot(group_df, aes(x = ID, y = 1, color = Group)) +   # Set y to a constant value (1)
  geom_point(size = 5) +  # Plot the group members
  geom_point(data = subset(group_df, Killed == TRUE),               
             aes(x = ID, y = 1),  # Keep y constant for killed individuals
             shape = 4, size = 8, stroke = 2, color = "black") +  # Mark killed with X
  scale_color_manual(values = c("Red" = "red", "Blue" = "blue")) +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),  # Optionally remove y-axis ticks
        axis.text.y = element_blank()) +  # Optionally remove y-axis text
  labs(title = "Group Members with 'Killed' Marked")

p
