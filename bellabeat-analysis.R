install.packages('tidyverse')
install.packages('dplyr')
install.packages('ggplot2')
install.packages('plotly')
install.packages('readr')
install.packages('lubridate')
install.packages('skimr')

library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(skimr)
library(plotly)

daily_activity <- read.csv("dailyActivity_merged.csv")
hourly_steps <- read.csv("hourlySteps_merged.csv")
daily_sleep <- read.csv("sleepDay_merged.csv")
weight <- read.csv("weightLogInfo_merged.csv")

#change the format of the date from chr to date
daily_activity$ActivityDate <- mdy(daily_activity$ActivityDate)
hourly_steps$ActivityHour <- mdy_hms(hourly_steps$ActivityHour)
daily_sleep$SleepDay <- mdy_hms(daily_sleep$SleepDay)
weight$Date <- mdy_hms(weight$Date)

#convert IsManualReport to Bool
weight$IsManualReport <- as.logical(weight$IsManualReport)

#drop the Fat column as it has too many NA values that are irrelevant to the analysis
weight <- weight %>% 
  select(-c(Fat))

#remove logs where user isn't wearing the fitbit - calories are 0
daily_activity <- daily_activity[!(daily_activity$Calories <= 0), ]

#add column for days of the week in the daily_activity file
daily_activity$dayOfWeek <- weekdays(daily_activity$ActivityDate)
#add column in weight file for BMI if healthy, underweight, or overweight
weight <- weight %>% mutate(
  bmiStatus = case_when(
    BMI < 18.5 ~ "Underweight",
    BMI > 25.0 ~ "Overweight",
    TRUE ~ "Healthy"
    )
)
str(weight)
#order data starting from Monday through Sunday
merge1 <- merge(daily_activity, daily_sleep, by = c("Id"), all = TRUE)
merged_data <- merge(merge1, weight, by = c("Id"), all = TRUE)

merged_data$dayOfWeek <- factor(merged_data$dayOfWeek, levels = c(
  "Monday",
  "Tuesday",
  "Wednesday",
  "Thursday",
  "Friday",
  "Saturday",
  "Sunday"
))
merged_data[order(merged_data$dayOfWeek), ]
str(merged_data)
#save into csv file for Tableau presentation
write_csv(merged_data, "Final_FitBit_merged_data_24_09_2023.csv")

#recheck file for unique IDs
n_distinct(daily_activity$Id)
n_distinct(daily_sleep$Id)
n_distinct(weight$Id)

#so the data in these files vary. daily_activity has 33, daily_sleep has 24, and weight only has 8. Let's see how weight has been tracked
weight %>%
  filter(IsManualReport == 'TRUE') %>%
  group_by(Id) %>%
  summarise("Number of users who reported weight manually" = n()) %>%
  distinct()

#find out days with the most recorded data
ggplot(data = merged_data) +
  aes(x = dayOfWeek) +
  geom_bar(fill = "purple") +
  labs(title = "User data over a week")
ggsave

#before we get into the relationships, it is best if we provide our stakeholders with a summary of the data we've collected. So we should make a weekly and hourly summary of the data
#Steps in a Week
ggplot(data = merged_data) +
  aes(x = dayOfWeek, y = TotalSteps, fill = dayOfWeek) +
  geom_col(fill = 'brown3') +
  labs(x = 'Day of Week', y = 'Total Steps', title = 'Total Steps in a Week')

#Calories burned during the Week
ggplot(data = merged_data) +
  aes(x = dayOfWeek, y = Calories, fill = dayOfWeek) +
  geom_col(fill = 'chartreuse3') +
  labs(x = 'Day of Week', y = 'Total Calories', title = 'Total Calories Burned')

#plot for percentage of avg sedantary, light, moderate, and very active minutes 
total_minutes <- sum(daily_activity$VeryActiveMinutes, 
                     daily_activity$FairlyActiveMinutes, 
                     daily_activity$LightlyActiveMinutes,
                     daily_activity$SedentaryMinutes)

sedantary_minute_percent <- sum(daily_activity$SedentaryMinutes)/total_minutes * 100
very_active_minute_percent <- sum(daily_activity$VeryActiveMinutes)/total_minutes * 100
fairly_active_minute_percent <- sum(daily_activity$FairlyActiveMinutes)/total_minutes * 100
lightly_active_minute_percent <- sum(daily_activity$LightlyActiveMinutes)/total_minutes * 100

#to generate a pie chart
percent_active_minutes <- data.frame(
  level = c("Sedentary", "Lightly Active", "Fairly Active", "Very Active"),
  minutes = c(sedantary_minute_percent, lightly_active_minute_percent, 
              fairly_active_minute_percent, very_active_minute_percent)
)

plot_ly(percent_active_minutes, labels = ~level, values = ~minutes,
        type = 'pie', textposition = 'outside',
        textinfo = 'label+percent') %>%
  layout(title = 'Average Minutes Per Activity',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
         )

#plot for number of very active, moderately active, lightly active, and sedantary active minutes in a day
ggplot(data = merged_data) +
  aes(x = dayOfWeek, y = total_minutes) +
  geom_col(fill = 'darkgoldenrod2') +
  labs(x = 'Day of the Week', y = 'Total Minutes', title = 'Average Minutes Spent per Day')

#plot for total minutes in bed per day - all minutes spent in bed
ggplot(data = merged_data) +
  aes(x = dayOfWeek, y = TotalTimeInBed) +
  geom_col(fill = 'coral') +
  labs(x = 'Day of the Week', y = 'Time in Minutes', title = 'Total Time Spent in Bed')

#plot for active distance in the Week
ggplot(data = merged_data) +
  aes(x = dayOfWeek, y = TotalDistance) +
  geom_col(fill = 'deepskyblue2') +
  labs(x = 'Day of the Week', y = 'Active Distance', title = 'Total Active Distance per Day')

#plot for number of sedantary minutes per day of the Week
ggplot(data = merged_data) +
  aes(x = dayOfWeek, y = SedentaryMinutes) +
  geom_col(fill = 'darkslategray3') +
  labs(x = 'Day of the Week', y = 'Sedantary Minutes', title = 'Total Inactive Time')

#plot for hourly steps in a Day
ggplot(data = hourly_steps) +
  aes(x = ActivityHour, y = StepTotal) +
  geom_col(fill = 'darkseagreen') +
  labs(x = 'Hours', y = 'Total Steps', title = 'Hourly Steps in a Month')

hourly_steps$Hour <- format(hourly_steps$ActivityHour, format = "%H")
head(hourly_steps)


ggplot(data=hourly_steps, aes(x = Hour, y = StepTotal, fill=Hour)) +
  geom_bar(stat = "identity")+
  labs(title = "Steps taken in 24 Hours")

View(hourly_steps)
write_csv(hourly_steps, "updated_hourly_steps.csv")
#merging the hourly_step file with the existing merged data
merged_data_hour <- full_join(merged_data, hourly_steps)
merged_data_hour <- merge(x = merged_data, y = hourly_steps, by = c("Id"), 
                          all = TRUE)
merged_data_hour <- merge(merged_data, hourly_steps, by = c("Id"), all = TRUE)
write_csv(merged_data_hour, "merged_data_with_hours.csv")

#obtain statistics using summary's mean, median, min, max
daily_activity %>%
  dplyr::select(
    TotalSteps,
    TotalDistance,
    VeryActiveMinutes,
    FairlyActiveMinutes,
    LightlyActiveMinutes,
    SedentaryMinutes,
    Calories
  ) %>% summary()

daily_sleep %>%
  dplyr:: select(
    TotalSleepRecords,
    TotalMinutesAsleep,
    TotalTimeInBed
  ) %>% summary()

weight %>% 
  dplyr::select(
    WeightKg,
    BMI
  ) %>% summary()

#with the available daily active minutes, we can analyse the health records and check whether it matches the requirements of the NHS
#the records of the NHS state that we at least need 150 minutes of moderate active minutes, 75 minutes of high-intensity workout or a combination of both each week
#we need to see how many consumers achieve a daily goal of 21.4 fairly active minutes or 10.7 very active minutes
#active users
active_users <- daily_activity %>%
  filter(FairlyActiveMinutes >= 21.4 | VeryActiveMinutes >= 10.7) %>%
  group_by(Id) %>%
  count(Id)

str(active_users)
sleep_users <- daily_sleep %>%
  filter(TotalMinutesAsleep >= 420) %>%
  group_by(Id) %>%
  count(Id)

#how active are users and how much sleep do they get, studied using boxplot!
active_minutes <- daily_activity %>%
  gather(key = Activity, value = active_minutes, ends_with("minutes")) %>%
  select(Activity, active_minutes)

in_bed <- daily_sleep %>%
  gather(key = Minutes_in_Bed, value = in_bed, 4:5) %>%
  select(Minutes_in_Bed, in_bed)

ggplot(data = active_minutes, aes(x = Activity, y = active_minutes)) +
  geom_boxplot(aes(fill = Activity)) +
  scale_x_discrete(limits = c("SedentaryMinutes",
                            "LightlyActiveMinutes",
                            "FairlyActiveMinutes",
                            "VeryActiveMinutes")) + ylab("Minutes")

ggplot(data = in_bed, aes(x = Minutes_in_Bed, y = in_bed)) +
  geom_boxplot(aes(fill = Minutes_in_Bed)) + xlab("Minutes in Bed") +
  scale_x_discrete(limits = c("TotalMinutesAsleep",
                              "TotalTimeInBed")) + ylab("Minutes")

#studying user steps and their burned calories through sedantary minutes as users who are more sedantary take lesser steps, therefore burning fewer calories
par(mfrow = c(2, 2))
ggplot(data=daily_activity, aes(x=TotalSteps, y=SedentaryMinutes, 
                                color=Calories)) + 
  geom_point()+
  stat_smooth(method=lm)+
  scale_color_gradient(low="blue", high="yellow")

ggplot(data=daily_activity, aes(x=TotalSteps, y=SedentaryMinutes, color=TotalDistance))+ 
  geom_point()+
  stat_smooth(method=lm)+
  scale_color_gradient(low="blue", high="yellow")

#let's try further analysing the available data through regression models using the lm() function. For the regression function, we will look at R-squared
total_step_vs_sedentary_minutes.mod <- lm(SedentaryMinutes ~ TotalSteps, data = merged_data)
summary(total_step_vs_sedentary_minutes.mod)

user_bmi_vs_steps.mod <- lm(BMI ~ TotalSteps, data = merged_data)
summary(user_bmi_vs_steps.mod)

calories_vs_steps.mod <- lm(Calories ~ TotalSteps, data = merged_data)
summary(calories_vs_steps.mod)

sedentary_vs_sleep_time.mod <- lm(SedentaryMinutes ~ TotalMinutesAsleep, data = merged_data)
summary(sedentary_vs_sleep_time.mod)

#as seen from the above, we notice that very few consumers achieve the fairly and very active minutes
active_minutes_vs_calories <- ggplot(data = merged_data) +
  geom_point(mapping = aes(x = Calories, y = FairlyActiveMinutes), 
             color = 'darkolivegreen3', alpha = 1/3) +
  geom_smooth(method = loess, formula = y ~ x,
              mapping = aes(x = Calories, y = FairlyActiveMinutes,
                            color = FairlyActiveMinutes),
              color = 'darkolivegreen3', se = FALSE) +
  
  geom_point(mapping = aes(x = Calories, y = VeryActiveMinutes),
             color = 'red3', alpha = 1/3) +
  geom_smooth(method = loess, formula = y ~ x, mapping = aes(
    x=Calories, y = VeryActiveMinutes, color = VeryActiveMinutes),
    color = 'red3', se = FALSE) +
  
  geom_point(mapping = aes(x = Calories, y = LightlyActiveMinutes), 
             color = "orange", alpha = 1/3) +
  geom_smooth(method = loess, formula = y ~ x, mapping = aes(
    x = Calories, y = LightlyActiveMinutes, 
    color=LightlyActiveMinutes), color = "orange", se = FALSE) +
  
  geom_point(mapping = aes(x = Calories, y = SedentaryMinutes), 
             color = "steelblue", alpha = 1/3) +
  geom_smooth(method = loess, formula = y ~ x, mapping = aes(
    x = Calories, y = SedentaryMinutes, color = SedentaryeMinutes), 
    color = "steelblue", se = FALSE) +
  
  annotate("text", x=4800, y=160, label="Very Active", color="black", size=3)+
  annotate("text", x=4800, y=0, label="Fairly Active", color="black", size=3)+
  annotate("text", x=4800, y=500, label="Sedentary", color="black", size=3)+
  annotate("text", x=4800, y=250, label="Lightly  Active", color="black", size=3)+
  labs(x = "Calories", y = "Active Minutes", title="Calories vs Active Minutes")

active_minutes_vs_calories

active_minutes_vs_steps <- ggplot(data = merged_data) + 
  geom_point(mapping=aes(x=TotalSteps, y=FairlyActiveMinutes), color = "maroon", alpha = 1/3) +
  geom_smooth(method = loess,formula =y ~ x, mapping=aes(x=TotalSteps, y=FairlyActiveMinutes, color=FairlyActiveMinutes), color = "maroon", se = FALSE) +
  
  geom_point(mapping=aes(x=TotalSteps, y=VeryActiveMinutes), color = "forestgreen", alpha = 1/3) +
  geom_smooth(method = loess,formula =y ~ x,mapping=aes(x=TotalSteps, y=VeryActiveMinutes, color=VeryActiveMinutes), color = "forestgreen", se = FALSE) +
  
  geom_point(mapping=aes(x=TotalSteps, y=LightlyActiveMinutes), color = "orange", alpha = 1/3) +
  geom_smooth(method = loess,formula =y ~ x,mapping=aes(x=TotalSteps, y=LightlyActiveMinutes, color=LightlyActiveMinutes), color = "orange", se = FALSE) +
  
  geom_point(mapping=aes(x=TotalSteps, y=SedentaryMinutes), color = "steelblue", alpha = 1/3) +
  geom_smooth(method = loess,formula =y ~ x,mapping=aes(x=TotalSteps, y=SedentaryMinutes, color=SedentaryMinutes), color = "steelblue", se = FALSE) +
  
  annotate("text", x=35000, y=150, label="Very Active", color="black", size=3)+
  annotate("text", x=35000, y=50, label="Fairly Active", color="black", size=3)+
  annotate("text", x=35000, y=1350, label="Sedentary", color="black", size=3)+
  annotate("text", x=35000, y=380, label="Lightly  Active", color="black", size=3)+
  labs(x = "Total Steps", y = "Active Minutes", title="Steps vs Active Minutes")

active_minutes_vs_steps

active_minutes_vs_distance <- ggplot(data = merged_data) + 
  geom_point(mapping=aes(x=TotalDistance, y=FairlyActiveMinutes), color = "steelblue", alpha = 1/3) +
  geom_smooth(method = loess,formula =y ~ x, mapping=aes(x=TotalDistance, y=FairlyActiveMinutes, color=FairlyActiveMinutes), color = "steelblue", se = FALSE) +
  
  geom_point(mapping=aes(x=TotalDistance, y=VeryActiveMinutes), color = "gold", alpha = 1/3) +
  geom_smooth(method = loess,formula =y ~ x,mapping=aes(x=TotalDistance, y=VeryActiveMinutes, color=VeryActiveMinutes), color = "gold", se = FALSE) +
  
  geom_point(mapping=aes(x=TotalDistance, y=LightlyActiveMinutes), color = "coral", alpha = 1/3) +
  geom_smooth(method = loess,formula =y ~ x,mapping=aes(x=TotalDistance, y=LightlyActiveMinutes, color=LightlyActiveMinutes), color = "coral", se = FALSE) +
  
  geom_point(mapping=aes(x=TotalDistance, y=SedentaryMinutes), color = "forestgreen", alpha = 1/3) +
  geom_smooth(method = loess,formula =y ~ x,mapping=aes(x=TotalDistance, y=SedentaryMinutes, color=SedentaryMinutes), color = "forestgreen", se = FALSE) +
  
  scale_x_continuous(limits = c(0, 30))+
  
  annotate("text", x=28, y=150, label="Very Active", color="black", size=3)+
  annotate("text", x=28, y=50, label="Fairly Active", color="black", size=3)+
  annotate("text", x=28, y=1250, label="Sedentary", color="black", size=3)+
  annotate("text", x=28, y=280, label="Lightly  Active", color="black", size=3)+
  labs(x = "Distance", y = "Active Minutes")

active_minutes_vs_distance

#Sleep time in hours instead of minutes
daily_sleep_in_hour <- daily_sleep
daily_sleep_in_hour$TotalMinutesAsleep <- daily_sleep_in_hour$TotalMinutesAsleep/60
daily_sleep_in_hour$TotalTimeInBed <- daily_sleep_in_hour$TotalTimeInBed/60
head(daily_sleep_in_hour)

#Check for any sleep outliers. # of times user sleep more than 10 hours or less than 1  
sum(daily_sleep_in_hour$TotalMinutesAsleep > 9)
sum(daily_sleep_in_hour$TotalTimeInBed > 9)
sum(daily_sleep_in_hour$TotalMinutesAsleep < 2)
sum(daily_sleep_in_hour$TotalTimeInBed < 2)

#55 minutes are spend awake in bed before going to sleep. Let see how many users in our study is according to the FitBit data
awake_in_bed <- mutate(daily_sleep, AwakeTime = TotalTimeInBed - TotalMinutesAsleep)
awake_in_bed <- awake_in_bed %>% 
  filter(AwakeTime >= 55) %>% 
  group_by(Id) %>% 
  arrange(AwakeTime, desc=TRUE) 

n_distinct(awake_in_bed$Id) #13 users spend more than 55 minutes in bed before falling alseep
#How many minutes an user sleep may not correlate well with how actively they are, but sedentary time account for about 80% of during the day

# Majority of the users sleep between 5 to 10 hours burns around 1500 to 4500 calories a day.
ggplot(data = merged_data, aes(x = TotalMinutesAsleep/60, y=Calories, 
                               color=TotalSteps)) + 
  xlab("Total Minutes Asleep") +
  geom_point()+
  scale_color_gradient(low="blue", high="yellow")

# Majority of the users sleep between 5 to 10 hours spend 7 to 24 hours in sedentary and only 0 to 2 hours in very active mode. 
ggplot(data=merged_data, aes(x=TotalMinutesAsleep/60 ,y=SedentaryMinutes/60, color=TotalSteps))+ 
  geom_point()+
  scale_color_gradient(low="steelblue", high="orange") +
  ylab("sedentary hours")+
  xlab("total sleep hours")

ggplot(data=merged_data, aes(x=TotalMinutesAsleep/60 ,y=VeryActiveMinutes/60, color=TotalSteps))+ 
  geom_point()+
  scale_color_gradient(low="steelblue", high="orange")+
  ylab("very active hours")+
  xlab("total sleep hours")

ggplot(data=merged_data, aes(x=TotalMinutesAsleep, y = Calories, 
                             color=TotalMinutesAsleep))+ 
  geom_point()+ 
  labs(title="Total Minutes Asleep vs Calories")+
  xlab("Total Minutes Alseep")+
  stat_smooth(method=lm)+
  scale_color_gradient(low="orange", high="steelblue")