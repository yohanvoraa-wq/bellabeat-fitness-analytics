library(tidyverse)
library(lubridate)

daily_activity <- read_csv("C:/Users/DELL/OneDrive/Desktop/dailyActivity_merged.csv")
sleep_day <- read_csv("C:/Users/DELL/OneDrive/Desktop/sleepDay_merged.csv")
weight_log <- read_csv("C:/Users/DELL/OneDrive/Desktop/weightLogInfo_merged.csv")

# glimpse(daily_activity)
# glimpse(sleep_day)
# glimpse(weight_log)

daily_activity <- daily_activity %>%
  rename_with(tolower)

sleep_day <- sleep_day %>%
  rename_with(tolower)

weight_log <- weight_log %>%
  rename_with(tolower)

daily_activity <- daily_activity %>%
  mutate(activitydate = mdy(activitydate))

sleep_day <- sleep_day %>%
  mutate(sleepday = mdy_hms(sleepday))

daily_activity %>% 
  count(id, activitydate) %>% 
  filter(n > 1)

sleep_day %>% 
  count(id, sleepday) %>% 
  filter(n > 1)

daily_activity <- daily_activity %>%
  distinct()

sleep_day <- sleep_day %>%
  distinct()


colSums(is.na(daily_activity))
colSums(is.na(sleep_day))
colSums(is.na(weight_log))

daily_activity <- daily_activity %>%
  drop_na(totalsteps, calories)

sleep_day <- sleep_day %>%
  mutate(sleepdate = as.Date(sleepday)) %>%
  select(-sleepday)
activity_sleep <- daily_activity %>%
  left_join(sleep_day, 
            by = c("id" = "id", "activitydate" = "sleepdate"))

glimpse(activity_sleep)
summary(activity_sleep)

activity_sleep %>%
  select(totalsteps, calories, totalminutesasleep) %>%
  summary()

steps_by_user <- activity_sleep %>%
  group_by(id) %>%
  summarise(
    avg_steps = mean(totalsteps, na.rm = TRUE),
    avg_calories = mean(calories, na.rm = TRUE)
  )

summary(steps_by_user)


activity_sleep <- activity_sleep %>%
  mutate(activity_level = case_when(
    totalsteps < 5000 ~ "Low Activity",
    totalsteps >= 5000 & totalsteps < 10000 ~ "Moderate Activity",
    totalsteps >= 10000 ~ "High Activity"
  ))
activity_sleep %>%
  count(activity_level)

activity_sleep %>%
  mutate(sleep_hours = totalminutesasleep / 60) %>%
  summarise(
    avg_sleep_hours = mean(sleep_hours, na.rm = TRUE)
  )

activity_sleep %>%
  group_by(activity_level) %>%
  summarise(
    avg_sleep_hours = mean(totalminutesasleep / 60, na.rm = TRUE)
  )

sleep_day <- sleep_day %>%
  mutate(
    sleepdate = as.Date(mdy_hms(sleepday))
  ) %>%
  select(-sleepday)
str(sleep_day$sleepdate)
str(daily_activity$activitydate)
activity_sleep <- daily_activity %>%
  left_join(
    sleep_day,
    by = c("id" = "id", "activitydate" = "sleepdate")
  )
summary(activity_sleep$totalminutesasleep)
colnames(sleep_day)

str(daily_activity$activitydate)

sleep_day <- sleep_day %>%
  mutate(
    sleepdate = as.Date(substr(as.character(sleepdate), 1, 10))
  )
str(sleep_day$sleepdate)
str(daily_activity$activitydate)



sleep_day <- read_csv("C:/Users/DELL/OneDrive/Desktop/sleepDay_merged.csv") %>%
  rename_with(tolower)
colnames(sleep_day)
sleep_day <- sleep_day %>%
  mutate(
    sleepdate = as.Date(substr(sleepday, 1, 10))
  ) %>%
  select(-sleepday)
str(sleep_day$sleepdate)

library(lubridate)

sleep_day <- sleep_day %>%
  mutate(
    sleepdate = as.Date(mdy_hms(sleepday))
  ) %>%
  select(-sleepday)

sleep_day <- read_csv("C:/Users/DELL/OneDrive/Desktop/sleepDay_merged.csv") %>%
  rename_with(tolower)
library(lubridate)

sleep_day <- sleep_day %>%
  mutate(
    sleepdate = as.Date(mdy_hms(sleepday))
  ) %>%
  select(-sleepday)
str(sleep_day$sleepdate)
str(daily_activity$activitydate)
activity_sleep <- daily_activity %>%
  left_join(
    sleep_day,
    by = c("id" = "id", "activitydate" = "sleepdate")
  )
summary(activity_sleep$totalminutesasleep)

activity_sleep %>%
  summarise(
    avg_sleep_hours = mean(totalminutesasleep / 60, na.rm = TRUE)
  )

activity_sleep %>%
  group_by(activity_level) %>%
  summarise(
    avg_sleep_hours = mean(totalminutesasleep / 60, na.rm = TRUE)
  )
activity_sleep <- activity_sleep %>%
  mutate(
    activity_level = case_when(
      totalsteps < 5000 ~ "Low Activity",
      totalsteps >= 5000 & totalsteps < 10000 ~ "Moderate Activity",
      totalsteps >= 10000 ~ "High Activity"
    )
  )
colnames(activity_sleep)
activity_sleep %>%
  group_by(activity_level) %>%
  summarise(
    avg_sleep_hours = mean(totalminutesasleep / 60, na.rm = TRUE)
  )

cor(activity_sleep$totalsteps,
    activity_sleep$calories,
    use = "complete.obs")

cor(activity_sleep$totalsteps,
    activity_sleep$totalminutesasleep,
    use = "complete.obs")

library(ggplot2)

ggplot(activity_sleep, aes(x = totalsteps)) +
  geom_histogram(binwidth = 1000, fill = "#4C72B0", color = "white") +
  labs(
    title = "Distribution of Daily Steps",
    subtitle = "Most users fall in the moderate activity range",
    x = "Total Daily Steps",
    y = "Number of Days"
  ) +
  theme_minimal()

activity_counts <- activity_sleep %>%
  count(activity_level)

activity_counts <- activity_sleep %>%
  count(activity_level)
activity_counts


ggplot(activity_counts, aes(x = activity_level, y = n, fill = activity_level)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "User Activity Levels",
    subtitle = "Moderate activity is most common",
    x = "Activity Level",
    y = "Number of Days"
  ) +
  theme_minimal()

ggplot(activity_sleep, aes(x = totalsteps, y = calories)) +
  geom_point(alpha = 0.4, color = "#DD8452") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Relationship Between Steps and Calories Burned",
    subtitle = "Higher activity is associated with increased calorie burn",
    x = "Total Daily Steps",
    y = "Calories Burned"
  ) +
  theme_minimal()

sleep_by_activity <- activity_sleep %>%
  group_by(activity_level) %>%
  summarise(avg_sleep_hours = mean(totalminutesasleep / 60, na.rm = TRUE))

sleep_by_activity <- activity_sleep %>%
  group_by(activity_level) %>%
  summarise(
    avg_sleep_hours = mean(totalminutesasleep / 60, na.rm = TRUE)
  )


ggplot(sleep_by_activity, aes(x = activity_level, y = avg_sleep_hours, fill = activity_level)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Average Sleep Duration by Activity Level",
    subtitle = "Higher activity is associated with slightly longer sleep",
    x = "Activity Level",
    y = "Average Sleep (Hours)"
  ) +
  theme_minimal()

