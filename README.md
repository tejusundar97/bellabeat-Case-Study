<b><i>Author:</i></b> Tejaswini Sundar

<b><i>Date:</i></b> September 26, 2023.

# Google Data Analytics Capstone: Bellabeat Case Study Analysis
Analysing smart device fitness data for a fictional company, Bellabeat, from the [Google Data Analytics Course Capstone](https://www.coursera.org/learn/google-data-analytics-capstone). As an acting Junior Data Analyst within the marketing analyst team for the wellness company, which is a high-tech manufacturer of health-focused products for women, I am tasked with analysing current smart device fitness data to unlock new growth opportunities through these trends for the firm. 
The public data set that I will be using is from a specific data set, [FitBit Fitness Tracker Data](https://www.kaggle.com/arashnic/fitbit), made available through [Mobius](https://www.kaggle.com/arashnic).
Therefore, as mentioned in the case study, my objective is to provide the company with high-level recommendations of how the studied trends can better the marketing strategy.

### Goals:
The goals for the case study can be summarised as below:
1. Observe trends through customer data on other similar smart fitness devices.
2. Note down which of these observed trends can apply to Bellabeat users.
3. Utilise relevant data to assist Bellabeat in their marketing strategy using their smart devices.

## Introduction - About Bellabeat
Bellabeat is a high-tech manufacturer of health-focused smart products for women that informs and inspires its global customer base by collecting and channeling data from daily tasks, such as daily activities, sleeping schedules, stress levels, and reproductive health. The company's products, which have been extensively marketed through traditional and digital media, has also opened offices internationally and launched multiple products. Bellabeat has heavily-invested in traditional and digital media channels, including and not limited to radios, billboards, print and TV, year-round investments in Google Search with active Facebook, Twitter and Instagram pages.
Therefore, by using the information from available consumer data, the founders of the company are confident in identifying trends to implement in their marketing strategies.

For this case study, I will follow the steps of the data analysis process, namely, [Ask](#ask), [Prepare](#prepare), [Process](#process), [Analyse](#analyse), [Share](#share), and [Act](#act)

## Ask
### Business Task
Analysing smart device usage data to gain insight into how consumers use non-bellabeat smart fitness devices to help the process of the marketing strategy for Bellabeat to grow as a global player.

The key stakeholders for the above business tasks are:
1. <b>Primary Stakeholders</b>: Urška Sršen and Sando Mur
2. <b>Secondary Stakeholders</b>: The marketing analytics team

## Prepare
The data set used for this case study contains the personal data of 30 eligible FitBit users, who have all consented to the submission of their personal Tracker Data. As mentioned above, the downloaded zipped public data set, [FitBit Fitness Tracker Data](https://www.kaggle.com/arashnic/fitbit), was stored in a separate folder, labelled Bellabeat - Case Study 2 under the Google Data Analytics Capstone folder on my local computer.

![Screenshot of the organised data within the Google-Data-Analytics-Capstone folder, containing subfolder Bellabeat_Case_Study_Mar-to-May-2016](/Screenshots/Bellabeat-Folder-Screenshot.png?raw=true "Bellabeat Folder")

The available data is in wide format, and partially follows the ROCCC approach:
1. Reliability: The data set is from 30 FitBit users who have all consented to the submission of their personal Tracker Data, including minute-level output data for their physical activity.
2. Original: This data set was generated by customer responses from a distributed survey via Amazon Mechanical Turk between March 2016 to May 2016.
3. Comprehensive: 30 eligible consumers' fitness data covers details such as minute-level output for physical activity, heart rate, and sleep monitoring. It also includes
information about daily activity, steps, and heart rate.
4. Current: The data set is not current as of this day, which poses as an additional issue.
5. Cited: Other than the known generator of this data, namely Amazon Mechanical Turk, the citation of the data remains unknown.

Furthermore, we make note of other issues:
- As there are only 30 users from whom data has been extracted, the marketing analytics team will not be able to make accurate decisions for the company's strategy. For instance, the central limit theorem states that we would require a sample size that is n &ge; 30, which would allow us to implement the t-test for statistical analysis. For the purposes of better recommendations, it is always preferred to work with a larger data set.
- Skimming through the files before analysing the available data, I found that there were differing number of unique IDs of consumers using non-bellabeat smart products. A few files contained unique consumer IDs that were either &gt; or &lt; 30 (such as the dailyActivity_merged.csv file in this folder). This was measured using the ```n_distinct()``` function on RStudio.
- Some of the data that was collected was also observed from Tuesday to Thursday, which would not produce accurate enough results for the analysis.

### Process
After downloading the data set, I loaded and installed the necessary packages on RStudio using Posit Cloud. As mentioned in the case study, which mainly examines daily activity, sleep, and weight, I will focus on:
- dailyActivity_merged.csv
- sleepDay_merged.csv
- weightLogInfo_merged.csv
- hourlySteps_merged.csv

I also altered the filenames for ease of recognition and working:

``` 
daily_activity <- read.csv("dailyActivity_merged.csv")
hourly_steps <- read.csv("hourlySteps_merged.csv")
daily_sleep <- read.csv("sleepDay_merged.csv")
weight <- read.csv("weightLogInfo_merged.csv")
```
Before beginning to analyse the files, I examined the available data using the ```str()``` function and found the following:
1. The datatypes of the dates in all of the above files were in character, which I needed to transform to the date datatype format. To do this, I used the ```mdy()``` function from the ```lubridate``` library, as the dates were in m/d/y format.
2. Additionally, the dates also contained the time of the day the data was recorded, in "HH:mm:ss", so if the above function was used to parse such dates, the values would've returned as NA. Therefore, I used ```mdy_hms()``` function from the ```lubridate``` library like so:
```
  daily_sleep$SleepDay <- mdy_hms(daily_sleep$SleepDay)
   ```
3. In the weightLogInfo.csv file, it was observed that some of the users manually entered their weight into bellabeat's app, and this was recorded in a column that had boolean values but was not assigned a boolean datatype. This was transformed using the built-in ```as.logical()```
4. Also, the weightLogInfo file contained a column that recorded Body Fat, however due to numerous records containing NA inputs, this column was discarded as it could not be used for analysis.
```
  weight <- weight %>% 
  select(-c(Fat))
```
After solving these issues, I checked through the data once more and found logs where the calories burnt by the wearer were 0. Assuming that this was data recorded when Bellabeat's products were not worn (as most of them are wearables), I removed logs where calories burnt were 0.
```
daily_activity <- daily_activity[!(daily_activity$Calories <= 0), ]
```
Once I was done, I added a column to the daily_activity.csv file to indicate the day of the week for ease of analysis and communication and a column in the weight.csv to indicate the consumer's BMI status according to guidelines from the [NHS](https://www.nhs.uk/live-well/healthy-weight/bmi-calculator/), which specifies a healthy weight range between 18.5 to 24.9 for average adults.
```
weight <- weight %>% mutate(
  bmiStatus = case_when(
    BMI < 18.5 ~ "Underweight",
    BMI > 25.0 ~ "Overweight",
    TRUE ~ "Healthy"
    )
)
```
After creating a merged CSV file with the above modified files,  I ordered the days of the week to begin from Monday like so:
```
merged_data$dayOfWeek <- factor(merged_data$dayOfWeek, levels = c(
  "Monday",
  "Tuesday",
  "Wednesday",
  "Thursday",
  "Friday",
  "Saturday",
  "Sunday"
))
```
This merged CSV file, namely ```merged_data.csv``` was written into another file, namely "Final_FitBit_merged_data_24_09_2023.csv", following the naming conventions within the Google Data Analytics course.

### Analyse
This is where our data gets interesting!

Wanting to recheck the number of distinct users in each of the selected files, I used the ```n_distinct()``` function and found that:
1. daily_activity has 33 users, which was 3 more than what the original dataset claimed to have.
2. daily_sleep had 24 users, which was short by 6 users, and
3. weight.csv had the least number of users, with only 8 users actively recording their weight.

As seen above, the users within the weight.csv file mostly tracked their weight manually. Wanting to investigate more, I observed the number of users who tracked their weight manually with the following:

```
weight %>%
  filter(IsManualReport == 'TRUE') %>%
  group_by(Id) %>%
  summarise("Number of users who reported weight manually" = n()) %>%
  distinct()
```
This was the obtained output:

![Screenshot of the distinct number of users who manually recorded their weight](/Screenshots/Manual_weight_tracking.png?raw=true "Manual Weight Tracking")

Additionally, I analysed the following with the help of a few attached graphs:
1. The day of the week containing the most number of recorded data was Tuesday, with the lowest being Sunday. It could be suggested that since energy levels at the beginning of the week were relatively high, more number of consumers would be likely to use Bellabeat's products to continue remaining healthy. As the week progresses, not too many consumers feel the need to stick to a healthy lifestyle.
2. This graph was also similar to the plot for the average number of minutes spent per activity in a week - Tuesday showed the most minutes spent in activity, while Sunday showed the least.
3. To further this possible correlation, the most number of collective steps recorded in the week was also on Tuesday, and the least was on Sunday, which also resulted in the most number of calories burnt on Tuesday, and the least on Sunday.

![Screenshot of the most weekly steps](/Screenshots/Most_recorded_weekly_steps.png?raw=true "Most Weekly Steps")

4. Wanting to plot for the average active minutes spent, I observed that the number of active/resting minutes spent by users was in Sedentary, which was a staggering 81.2% of the total minutes spent in working out and rest. In second place stood Lightly Active minutes, at 15.9%, followed by Very Active minutes (1.75%) and Fairly Active minutes (1.12%)

![Screenshot of the average minutes spent per activity](/Screenshots/Average_minutes_per_activity.png?raw=true "Most Weekly Steps")

5. Surprisingly, the total number of minutes spent in bed, irrespective of the consumer sleeping or staying sedentary, was the highest on Tuesdays and lowest on Sunday. Additionally, the number of steps in a day, if we go by the hour, is a lot more around the evening, especially 5-7 PM, with the hours from 12 PM - 2 PM.

![Screenshot of the maximum hourly steps](/Screenshots/Hourly_steps.png?raw=true "Hourly Steps")

Before merging the ```hourly_steps.csv``` file with the ```merged_data.csv file```, I added a new column to the hourly_steps file that contained the number of hours parsed from the date/time format for further analysis.

Then I checked for mean, median, min, max, and any outliers in the three files with the following:

```
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
```
From the above, we obtain the following statistics:
- The average weight of the consumers (in Kgs) is 72.04, which pushes the average BMI to overweight (25.19). The maximum weight of all consumers is a whopping 133.50 Kg
- The average number of minutes spent from high-intensity workouts is only 21.26 minutes, with many users lying in the category of lightly active to sedentary active minutes.
- If we want to have a further look at the numbers, we see that the average Fairly active minutes is 13.62, Lightly active and Sedentary minutes leave the others in the dust, with time on Light-intensive activity being 193.6 minutes and Sedentary activity being 989.3.
- The average calories burned by these consumers is 2313 kcal.
- The average amount of time spent in bed asleep was 419.5 minutes, which is nearly 7 hours, while the average amount of time spent in bed was found to be 458.6 minutes

From the above, we can deduce that the average consumer not only lags behind in physical activity, but also lags behind in sleep. According to the [Sleep Health Foundation](https://www.sleephealthfoundation.org.au/pdfs/HowMuchSleep-0716.pdf), the average adult requires 7 to 9 hours. Additionally, according to another article by the [NHS](https://www.nhs.uk/live-well/exercise/exercise-guidelines/physical-activity-guidelines-for-adults-aged-19-to-64/#:~:text=do%20at%20least%20150%20minutes,a%20week%2C%20or%20every%20day), the average adult should exercise at least 150 minutes of moderate intensity activity or 75 minutes of rigorous activity per week. Wanting to investigate these matters, I wanted to count the number of users who achieved at least 7-9 hours of sleep, and the average weekly active minutes.

<b>Healthy Sleep Minutes:</b>

![Screenshot of healthy sleep minutes](/Screenshots/Users_healthy_sleep_records.png?raw=true "Healthy Sleep Minutes")

<b>Healthy Active Minutes:</b>

![Screenshot of healthy active minutes](/Screenshots/Users_healthy_active_minutes.png?raw=true "Healthy Active Minutes")

To study the above statistical data to understand the range and medians of the activity and sleep data, I used the following code to plot a box and whisker graph using the following code:

```
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
```
From the above code, I obtained the below screenshots of the boxplots:

<b>Healthy Sleep Minutes:</b>

![Screenshot of boxplots for healthy sleep minutes](/Screenshots/Minutes_in_bed_boxplot.png?raw=true "Healthy Sleep Minutes Boxplot")

<b>Healthy Active Minutes:</b>

![Screenshot of boxplots for active minutes](/Screenshots/Activity_minutes_boxplot.png?raw=true "Healthy Active Minutes Boxplot")

From the above data, we can also deduce that:
- The more active a user was, the more steps they would take, which would result in more calories being burnt.
- Comparing the four activity levels to the total steps, we notice the data concentrated on users who take about 5000 to 15000 steps a day. These users spent an average between 8 to 13 hours in sedentary, 5 hours in lightly active, and 1 to 2 hour for fairly and very active. Additionally, we see that the sedentary line is leveling off toward the end while fairly + very active line is curing back up. This indicate that the users who burn more calories spend less time in sedentary, more time in fairly + active.

![Screenshot of distance_vs_active minutes](/Screenshots/Distance_ActiveMinutes.png?raw=true "Distance vs Active Minutes")

- I also noticed that 13 users spent 55 minutes awake in bed before actually sleeping

```
awake_in_bed <- mutate(daily_sleep, AwakeTime = TotalTimeInBed - TotalMinutesAsleep)
awake_in_bed <- awake_in_bed %>% 
  filter(AwakeTime >= 55) %>% 
  group_by(Id) %>% 
  arrange(AwakeTime, desc=TRUE)

n_distinct(awake_in_bed$Id)
```

- What we can also notice is that users burn calories even when they sleep. To find out more about this, I used the following code:

![Screenshot of total minutes asleep vs calories](/Screenshots/Total_minutes_asleep_vs_calories.png?raw=true "Minutes Asleep vs Calories")

```
ggplot(data=merged_data, aes(x=TotalMinutesAsleep, y = Calories, 
                             color=TotalMinutesAsleep))+ 
  geom_point()+ 
  labs(title="Total Minutes Asleep vs Calories")+
  xlab("Total Minutes Alseep")+
  stat_smooth(method=lm)+
  scale_color_gradient(low="orange", high="steelblue")
```

- To further analyse the above data, I performed a simple regression analysis to look at the value of R-squared. For R-squared, 0% shows no explanation of the variation around its mean from the resulting data. The mean, in this instance, is used to predict behaviour with both known and unknown data values. A higher % indicates that the model explains more of the variation to the resulting data around its mean. To conduct regression analysis in ```r```, I used the ```lm()``` function to check for independent and dependent variables.

```
total_step_vs_sedentary_minutes.mod <- lm(SedentaryMinutes ~ TotalSteps, data = merged_data)
summary(total_step_vs_sedentary_minutes.mod)

user_bmi_vs_steps.mod <- lm(BMI ~ TotalSteps, data = merged_data)
summary(user_bmi_vs_steps.mod)

calories_vs_steps.mod <- lm(Calories ~ TotalSteps, data = merged_data)
summary(calories_vs_steps.mod)

sedentary_vs_sleep_time.mod <- lm(SedentaryMinutes ~ TotalMinutesAsleep, data = merged_data)
summary(sedentary_vs_sleep_time.mod)
```

From the above, we can analyse the following to find interesting observations:
- The dependency between sedentary minutes vs total steps taken is very weak, as indicated by the adjusted R-squared value.
- There is a moderate dependency between BMI and the total number of steps taken. It could indicate that the higher the BMI of the consumer, the lesser steps per week they take.
- Surprisingly, there isn't as strong of a dependence when we compare calories burnt and steps taken. This could be that a user would walk for a lot longer, but it would not be enough to burn the appropriate amount of calories depending on their weight and BMI.
- There is a moderate dependency between the number of minutes spent sedentary and the total minutes asleep for consumers. As seen above, nearly 13 users spent around 55 minutes lying awake in bed before sleeping.

![Screenshot of bmi vs steps](/Screenshots/Summary_user_bmi_vs_steps.png?raw=true "BMI vs Total Steps")

### Share
To share my ideas with the primary and secondary stakeholders, I created a [dashboard using Tableau](https://public.tableau.com/views/GoogleCapstoneProject-Bellabeat_16956511579190/Dashboard1?:language=en-US&:display_count=n&:origin=viz_share_link) that contains the highlighted visuals for easier understanding, some of which I would also be using in my [presentation](url).

### Act
Onto the final step of the data analysis process! From the above analysis, I would like to restate the objectives of the analysis once again before I dive into my proposed high-level solutions to the founders and the marketing analytics team. As stated above, the objectives of the analysis were:

1. Observe trends through customer data on other similar smart fitness devices.
2. Note down which of these observed trends can apply to Bellabeat users.
3. Utilise relevant data to assist Bellabeat in their marketing strategy using their smart devices.

Through the observed trends, I came up with the following conclusions:
* Users wearing FitBits spent most of their time staying sedentary, with number of minutes spent on light physical activity coming to a close second.
* Most users are invigorated to continue exercise on Tuesdays, as it is the beginning of the week, while "rest days" mostly fall on Sunday's.
* At least 13 users of the 22 (54%) who recorded their daily sleep schedules and habits stayed awake in bed for around 55 minutes.
* Users take the most steps from 5-7 PM.
* Users who take minimal steps and stay sedentary burn around 1500-2500 calories compared to the more active users in the consumer base.

As requested by the founders, here are the following marketing strategies I recommend:
* Develop a feature on wearables, such as Leaf and Time, that alerts the user if it is observed that they stay sedentary for too long. The alerts can either be in the form of the wearable vibrating or a combination of the product pinging and vibrating.
* The wearable and Bellabeat app can alert the user at specific time to wind down to get ready for bed 55 minutes before the user actually sleeps. This gives the user the time to complete any activity and slowly start conditioning the body to obtain a healthier sleeping cycle.
* Another suggestion that can go along with alerting the user for the number of sedentary minutes spent is having a reward-based system in check, where the user will be able to redeem points at any affordable fashion or lifestyle brand through Amazon giftcards through the number of minutes spent when it reaches the Fairly active to the Very Active physical activity phase.
  * To back this suggestion, we could also have a Leaderboard, temporarily dubbed "Exercise Challenge" Leaderboard, in place that can be viewed from the App. The higher number of points accumulated by a user, the more number of rewards they will be eligible for.
  * Additionally, if a user continues to stay on top of the "Exercise Challenge" leaderboard, they would receive more than the subscription-based membership that they initially signed up for.
  * We can divide the subscription-based membership into various levels so that a user in a silver-tier membership, if among the top 5 users on the Leaderboard, will be eligible to experience gold or platinum-tier membership guidance.
  * They should then be notified to provide feedback on the upgraded services they receive, notifying us of more possible pathways to improve.
 
This concludes my analysis of Bellabeat! Hope this helps!
