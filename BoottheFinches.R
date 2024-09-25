# Install ggplot2 if you haven't already
# install.packages("ggplot2")

library(ggplot2)
library(dplyr)
# Clear all variables in the environment
rm(list = ls())

# Create the groups
long_birds <- rep("Red", 5)   # 5 Red individuals
short_birds <- rep("Blue", 5)  # 5 Blue individuals

# Combine both groups into one vector
group <- c(short_birds, long_birds)

# Create a data frame for plotting
group_df <- data.frame(ID = 1:10, Group = group)

# Shuffle the group and randomly select 5 individuals to "kill"
#set.seed(123)  # Set seed for reproducibility
killed_indices <- sample(1:10, 5)  # Randomly select 5 indices to kill

# Mark Killed Individuals in the data frame
group_df$Killed <- FALSE  # Initialize the Killed column
group_df$Killed[killed_indices] <- TRUE  # Mark the killed individuals

# Number of bootstrap samples
n_bootstrap <- 1000 #do this a number of times
bootstrap_results <- numeric(n_bootstrap)

# sample from your bootstrapped population 
for (i in 1:n_bootstrap) {
  # Sample with replacement from the original data, respecting killed status
  bootstrap_sample <- sample(1:10, 10, replace = TRUE)
  
  # Get the Groups and Killed status for the sampled indices
  sampled_groups <- group_df$Group[bootstrap_sample]
  sampled_killed <- group_df$Killed[bootstrap_sample]
  
  # Calculate the proportion of surviving Red birds
  bootstrap_results[i] <- mean(sampled_groups[sampled_killed == FALSE] == "Red")
}

# Create a data frame for bootstrap results
bootstrap_df <- data.frame(Proportion_Surviving_Red = bootstrap_results)

# Plot the distribution of bootstrap results
p<-ggplot(bootstrap_df, aes(x = Proportion_Surviving_Red)) +
  geom_histogram(binwidth = 0.05, fill = "lightpink", color = "black", alpha = 0.7) +
  labs(title = "Bootstrapped Proportion of Surviving Red Birds",
       x = "Proportion of Surviving Red Birds",
       y = "Frequency") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.title.x = element_blank(), 
        axis.title.y = element_blank()) 
p

# Assume p_null is the null hypothesis proportion (e.g., 50% red survival)
observed_proportion <- 0.5977786

# Perform a one-sample t-test
t_test_result <- t.test(bootstrap_results, mu = observed_proportion)
# Extract the p-value from the t-test result
p_value <- t_test_result$p.value
# Print the p-value
t_test_result
