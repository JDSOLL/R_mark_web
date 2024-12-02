# Load required libraries
library(dplyr)
library(ggplot2)
library(lubridate)

# 1. Load the data
df <- read.csv('/Users/JoshSoll/Desktop/Glider_Lab/Glider_Data/UVI1_ScienceData_PR_E_Southside_1.csv')

# 2. Split the "time" column into separate "date" and "time_only" columns
df <- df %>%
  mutate(
    datetime = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%S"),  # Adjust format as needed
    date = as.Date(datetime),
    time_only = format(datetime, "%H:%M:%S")
  )

# Display the result to verify
head(df[, c("time", "date", "time_only")])

# 3. Interpolate missing values in the "depth" column
df <- df %>%
  mutate(depth = zoo::na.approx(depth, na.rm = FALSE))  # Linear interpolation for NA values

# Display the result to verify
head(df$depth, 10)

# 4. Plot depth over time
ggplot(df, aes(x = datetime, y = depth)) +
  geom_line(color = "blue") +
  scale_y_reverse() +  # Invert y-axis
  labs(
    title = "Time Series of Depth",
    x = "Time",
    y = "Depth (m)"
  ) +
  theme_minimal()

# 5. Fill remaining blank spaces with "NaN"
df[is.na(df)] <- "NaN"

# 6. Create a new dataframe with daily time bins and average values
# Ensure datetime is set as a proper column
df <- df %>%
  mutate(datetime = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%S")) %>%
  arrange(datetime)

# Exclude unnecessary columns and calculate daily averages
daily_avg_df <- df %>%
  select(-c(time, date, time_only, datetime)) %>%
  group_by(date) %>%
  summarize(across(everything(), mean, na.rm = TRUE))

# Display the result to verify
head(daily_avg_df)

# 7. Export the data to CSV files for GIS
write.csv(df, '/Users/JoshSoll/Desktop/GIS_CLASS/final_proj_data/FULL_UVI1_PR_E_South_1.csv', row.names = FALSE)
write.csv(daily_avg_df, '/Users/JoshSoll/Desktop/GIS_CLASS/final_proj_data/AVG_UVI1_PR_W_North.csv', row.names = FALSE)

cat("Files exported: 'FULL_UVI1_PR_E_South_1.csv' and 'AVG_UVI1_PR_W_North.csv'")
