# Set knitr chunk options. 'echo = FALSE' prevents code from being shown in the output.
knitr::opts_chunk$set(echo = FALSE)
# Load the dataset for the first quarter of 2019.
divvy_data <- read.csv("Divvy_Trips_2019_Q1.csv", header = TRUE)

# Display the first 6 rows of the data frame to get an overview.
head(divvy_data)

# Print the column names to understand the variables.
colnames(divvy_data)

# Display the structure of the data frame, including data types.
str(divvy_data)

# Get a statistical summary of the dataset.
summary(divvy_data)

# Identify the unique user types in the 'usertype' column.
unique(divvy_data$usertype)

# Count the number of trips for each user type.
table(divvy_data$usertype)

# Load the datasets for the remaining quarters of 2019.
divvy_data_q2 <- read.csv("Divvy_Trips_2019_Q2.csv", header = TRUE)
divvy_data_q3 <- read.csv("Divvy_Trips_2019_Q3.csv", header = TRUE)
divvy_data_q4 <- read.csv("Divvy_Trips_2019_Q4.csv", header = TRUE)
# Display the first 6 rows of each new quarterly data frame.
head(divvy_data_q2)
head(divvy_data_q3)
head(divvy_data_q4)

# Add a 'quarter' column to each data frame to identify the quarter.
divvy_data$quarter <- "Q1"
divvy_data_q2$quarter <- "Q2"
divvy_data_q3$quarter <- "Q3"
divvy_data_q4$quarter <- "Q4"

# Check column names to ensure consistency across data frames before merging.
names(divvy_data)
names(divvy_data_q2)
names(divvy_data_q3)
names(divvy_data_q4)

# Rename columns of the Q2 data frame to match the others for consistency.
target_names <- names(divvy_data)
names(divvy_data_q2) <- target_names
names(divvy_data_q2)

# Combine all four quarterly data frames into a single data frame.
divvy_data_all <- rbind(divvy_data, divvy_data_q2, divvy_data_q3, divvy_data_q4)
# Display the first 6 rows of the combined data frame.
head(divvy_data_all)

# Save the combined data frame to a new CSV file.
write.csv(divvy_data_all,
          "Divvy_Trips_2019_All_Quarters.csv",
          row.names = FALSE)

# Install and load the 'janitor' package for data cleaning.
install.packages("janitor")
library(janitor)
# Clean the column names to a consistent format (e.g., snake_case).
divvy_data_all <- clean_names(divvy_data_all)

# Check for and count missing values (NA) in each column.
colSums(is.na(divvy_data_all))

# Count the number of blank entries in the 'gender' column.
sum(divvy_data_all$gender == "")
# Install and load the 'dplyr' package for data manipulation.
install.packages("dplyr")
library(dplyr)
# Group data by quarter and count the total number of trips in each.
trips_by_quarter <- divvy_data_all |>
  group_by(quarter) |>
  summarize(total_trips = n())

# Convert the 'tripduration' column from character to numeric.
divvy_data_all$tripduration <- as.numeric(divvy_data_all$tripduration)
# Check the structure of the 'tripduration' column to confirm the change.
str(divvy_data_all$tripduration)

# Count the number of trips for each user type in the combined dataset.
table(divvy_data_all$usertype)

# Convert 'start_time' and 'end_time' columns to POSIXct date-time format.
divvy_data_all$start_time <- as.POSIXct(divvy_data_all$start_time, format="%Y-%m-%d %H:%M:%S")
divvy_data_all$end_time <- as.POSIXct(divvy_data_all$end_time, format="%Y-%m-%d %H:%M:%S")
# Check the structure of the time columns to confirm the conversion.
str(divvy_data_all$start_time)
str(divvy_data_all$end_time)

# Calculate the average trip duration for each user type.
average_duration_by_usertype <- divvy_data_all |>
  group_by(usertype) |>
  summarize(average_duration = mean(tripduration, na.rm = TRUE))
average_duration_by_usertype

# Install and load the 'ggplot2' package for data visualization.
install.packages("ggplot2")
library(ggplot2)

# Create a bar chart showing the total number of trips per quarter.
ggplot(trips_by_quarter, aes(x = quarter, y = total_trips)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Number of Trips by Quarter",
       x = "Quarter",
       y = "Number of Trips") +
  theme_minimal()

# Group data by usertype and count the total number of trips for each.
trips_by_usertype <- divvy_data_all |>
  group_by(usertype) |>
  summarize(total_trips = n())

# Create a bar chart showing the total number of trips by usertype.
ggplot(trips_by_usertype, aes(x = usertype, y = total_trips)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Number of Trips by Usertype",
       x = "Usertype",
       y = "Number of Trips") +
  theme_minimal()

# Group data by gender and count the total number of trips for each.
trips_by_gender <- divvy_data_all |>
  group_by(gender) |>
  summarize(total_trips = n())

# Create a bar chart showing the total number of trips by gender.
ggplot(trips_by_gender, aes(x = gender, y = total_trips)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Number of Trips by Gender",
       x = "Gender",
       y = "Number of Trips") +
  theme_minimal()

# Group data by both gender and usertype to see the distribution.
trips_by_gender_usertype <- divvy_data_all |>
  group_by(gender, usertype) |>
  summarize(total_trips = n())

# Create a stacked bar chart for trips by gender, filled by usertype.
ggplot(trips_by_gender_usertype,
       aes(x = gender, y = total_trips, fill = usertype)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Trips by Gender for Subscribers vs Customers",
       x = "Gender",
       y = "Number of Trips") +
  theme_minimal()

# Group data by quarter and usertype to see how ridership varies.
trips_by_usertype_by_quarter <- divvy_data_all |>
  group_by(quarter, usertype) |>
  summarize(total_trips = n())

# Create a dodged bar chart to compare usertypes across quarters.
ggplot(trips_by_usertype_by_quarter, 
       aes(x = quarter, y = total_trips, fill = usertype)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Trips by Usertype for Each Quarter",
       x = "Quarter",
       y = "Number of Trips") +
  theme_minimal()

# Calculate rider age based on birth year.
current_year <- 2019
divvy_data_all$age <- current_year - divvy_data_all$birthyear
# Group data by usertype and age, filtering for ages between 10 and 75.
trips_by_usertype_by_age <- divvy_data_all |>
  filter(age >= 10 & age <= 75) |> # Filter ages between 10 and 75
  group_by(usertype, age) |>
  summarize(total_trips = n())

# Create a line chart showing the number of trips by age for each usertype.
ggplot(trips_by_usertype_by_age,
       aes(x = age, y = total_trips, color = usertype)) +
  geom_line() +
  labs(title = "Number of Trips by Usertype for Each Age",
       x = "Age",
       y = "Number of Trips") +
  theme_minimal()
