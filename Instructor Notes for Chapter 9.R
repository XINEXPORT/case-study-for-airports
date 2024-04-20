##################################
# The starting packages
##################################

library("tidyverse")

##################################
# Get and examine the data. Make sure the polls.csv file is in your Working Directory
##################################

# set working directory to the location of this script file
polls <- read_csv("polls.csv")

# view the problems with the import to verify that you can ignore them. This is a  new function that we haven't used before. But we received a "parsing issues" error when we read the csv file into a tibble called polls. 
problems()
# the problems() function shows that 36 rows of the data in column 22. We won't use this column for the analysis so we can ignore these. Click on the polls data in the environment pane. This will open a new window that displays the file, so that you can see what the data columns contain. In this case if we apply a filter to the data we can see that all of the data is for the 2016 presidential election. And that a score is assigned to each poll, the generator of the poll varies. And preference percentiles are assigned to each of 4 presidential candidates. 
# view tibble
polls

# view tibble in R
view(polls)

# Further examination of the data shows that the cycle column only contains one value - 2016. And that  the branch column only contains one value, and that type and matchup only contain one value as well. For this analysis we won't need to use these columns. Page 300 of the book, as well as the PPT Slides 8 and 9 do a good job of showing the decision making process that goes into deciding which columns to use. To view the counts  of unique values per column. This was demonstrated on page 168-169 of the text book. We are applying apply() the function of unique  values in the columns, (MARGIN = 2) using the data from the tibble "polls". The pipe to the lapply function strips out some information that we don't need for this data revie.  

polls %>% apply(MARGIN = 2, FUN = unique) %>% lapply(FUN = length) %>% str()
# There are four columns with only one value in them and one with 3 values, and one with four. Check the values for the one with 3 unique values - the "type" column.
# view the value counts for the type column
table(polls$type)
# if you go back to the data source you will see that the values "now-cast", "polls-only", and "polls-plus" refer to methods that the polling agency applied to adjust the raw polling data for factors such as undercounts of certain populations, etc. So in this case we decide to use the raw polling data, instead of the adjusted polling data, so we can filter by any one of these methods and still get 4208 non repeating rows of raw data. But to further check this....
# compare the type column with the poll data columns - this will prove our hypothesis that the 4208 rows for each of the 3 different types repeat the column information for columns we will probably use for the analysis. In this case we are testing against one candidate, "clinton" and comparing the rawpoll percentages to make sure they are the same for each type of poll, that the end dates are the same for each type, and that the states are the same for each type. 
polls %>% 
  select(type, state, enddate, rawpoll_clinton, adjpoll_clinton) %>%
  arrange(state, enddate, type)

# view the unique values for the state column, next recall that we have 57 unique values in the state column. Since there are only 50 states plus DC we need to understand what the dataset is storing in the state column. 
unique(polls$state)
# We have national polls, "US", and in the data some polling was done by congressional district in Nebraska and and Maine. We decide to omit all of those and just use the 50 states plus DC plus the "US" rows in the data.  
# use a plot to examine the population column. This will help us determine, along with the polling web site methodology what these values stand for, and how significant they are. 
ggplot(polls, aes(x = population, fill = population)) +
  geom_bar()
# By examining the above bar chart we can see that population codes of "v" and "a" are not statistically significant, and so we decide to remove them from the analysis. 
# use a scatter plot to examine the samplesize, grade, and poll_weight columns, again this is to determine if we should use these columns in our analysis. 
ggplot(polls, aes(x = poll_wt, y = samplesize, color = grade)) +
  geom_point(size = 3)
# The above plot shows us that there are not many outliers in the data, almost all of the poll_wt values appear at the low end of the samplesize. We have already decided to use the raw polling data not, not the adjusted polling data, so poll_wt, which is a measure used to calculate the adjusted polling data, will not be used. Finally there is no relationship between the grade column, assigned by the polling web site and the sample size. Therefor we decide that all three of these columns do not need to be used in the analysis. 

# use a bar plot to examine the grade column
# (not shown in chapter), this could additionally prove or disprove that the data by grade is meaningful or not. In this case we see a pretty evenly distributed grade spread, reinforcing the idea that the grade column is not necessary for this analysis. 
ggplot(polls, aes(x = grade, fill = grade)) +
  geom_bar()

# so at this point we have decided that. 
# we want to analyze by 51 states, plus DC, plus nationwide polls = "US"
# we will only use the enddate of the poll as shown in each roll
# we only want rows of a specific type, and that the type doesn't matter
# additionally the two third party presidential candidates are not necessary for this study (Johnson and McMullin)
# we only want populations of lv and rv, which a review of the methodology from the source website stands for likely voters and registered voters.


##################################
# Clean the data
##################################

# select and rename columns
# first get the data. 
polls <- polls %>% 
  select(type, state, enddate, population, 
         rawpoll_clinton, rawpoll_trump)
# now rename each of the headers.
polls <- polls %>% rename(
  Type = type, State = state, EndDate = enddate, 
  Population = population, 
  Clinton = rawpoll_clinton, Trump = rawpoll_trump)

# sort the rows by state and enddate
polls <- polls %>% arrange(State, EndDate)
# display the tibble
polls

# select only "now-cast" rows and drop the Type column, we have proven that we can filter the data by any one of three types but we chose type == "now-cast", also we are selecting all of the columns for this analysis except the Type column, since we won't be using it. 
polls <- polls %>% 
  filter(Type == "now-cast") %>% 
  select(-Type)

# Filter out the "a" and "v" columns and select only rows with "lv" or "rv" in the Population column, remember that lv stands for likely voters and rv stands for registered voters. 
polls <- polls %>% filter(Population %in% c("lv", "rv"))

# Remember that we have some rows in Nebraska and Maine that are done at the congressional level all of the "state" values start with "CD-". Remove the rows for congressional districts. We can remove those rows by filtering out any state value that starts with "CD-". 
polls <- polls %>% filter(!str_detect(State, "CD-"))

polls
# check the number of states
length(unique(polls$State))

# improve data in Population column - lv and rv are not descriptive, therefore we will use the str_replace() function to change the value "lv" to "Likely" and "rv" to "Registered".  We are also going to rename the column. 
polls <- polls %>% 
  mutate(Population = str_replace(Population,"lv", "Likely"), 
         Population = str_replace(Population, "rv", "Registered")) %>% 
  rename(VoterType = Population)

# fix the EndDate data type to make it a Date type column and format the date in a user friendly style. 
polls <- polls %>% mutate(
  EndDate = as.Date(EndDate, format="%m/%d/%Y"))

polls

# save the tibble as an rds file into your working directory. This will allow the file to be shared easily when you complete your analysis and share your work with others. 
saveRDS(polls, file = "polls_clean.rds")



##################################
# Prepare the data
##################################

# get the saved data frame if starting from here
polls <- readRDS("polls_clean.rds") 

# create two gap columns, the first is a calculated column that subracts Trump percentage from Clinton percentage
polls <- polls %>% mutate(Gap = Clinton - Trump)
polls
# create a column that groups each of the polls by state, and then calculates the average for each poll mean(Gap). Finally it ungroups the result so that we can see the individual polls for each state, and the StateGap column will repeat. This will only be used to determine if a state is a "swing" state. Meaning that the polling is close enough between Trump and Clinton that the result is in question.  
polls <- polls %>% group_by(State) %>% 
  mutate(StateGap = mean(Gap)) %>% 
  ungroup()
polls
# create a column for swing states. In this case we are omitting the nationwide polls State != "U.S" and only flagging a state as a swing state if the absolute value of the StateGap column is <7. abs means not to consider weather the gap is negative or positive, remember we are calulating gap by Clinton percentage - Trump percentage. So if a state is polling better for Trump the StateGap column will be negative, hence using the abs function. Finally you will just populate the column with either "True" is a swing state, or "False" is not a swing state.  
polls <- polls %>% mutate(
  Swing = ifelse(State != "U.S." & (abs(StateGap) < 7), 
                 TRUE, FALSE))
polls
# finally we no longer need the StateGap column because it was only used to determine if a state was a Swing state or not. Remove this column. 
polls <- select(polls, -StateGap)

# Create a long version of the data. We have decided for this analysis that each row should pivot or combine the Clinton and Trump columns into one column called Candidate and we also want to combine the percentage values in the same way into one column. That way we can compare Clinton to Trump for each poll in seperate rows. To refresh your memory the pivot_longer() function is explained in detail on page 78 of the text book. 
polls <- pivot_longer(polls, cols = c("Clinton", "Trump"), 
                      names_to = "Candidate", values_to = "Percent")

polls

# Resave the  data frame as an rds file
saveRDS(polls, file = "polls_prepared.rds")



##################################
# Analyze the data
##################################

# get the saved data frame if starting from here
polls <- readRDS("prepared.rds")

# the national polls with a line plot (page 86-87), the x axis will be EndDAte, the y will be percent, and the color of the lines will be set to Blue for Clinton, Red for Trump, we will also insert a title, name the x axis "Date" and remove the name for the y axis, finally we will position the legend at the bottom. geom_line() function is on  pages 86-87 and 104-107. 
ggplot(filter(polls, State == "U.S."), 
       aes(x = EndDate, y = Percent, color = Candidate)) +
  geom_line() +
  scale_color_manual(values = c("blue", "red")) +
  labs(title = "Polls for the U.S.", x = "Date", y = "") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "bottom")

# the national polls with a scatter plot and a smooth line. Create a scatter plot of the data, using X as EndDate, Y as percent, and again the color by candidate. geom_point() pages 104-105 and 107-108 geom_smooth() 253-254. This plot will superimpose a smooth line in the middle of the scatter plot data points for each poll. 
ggplot(filter(polls, State == "U.S."), 
       aes(x = EndDate, y = Percent, color = Candidate)) +
  geom_point() +
  geom_smooth(se = FALSE, size = 2) +
  scale_color_manual(values = c("blue", "red")) +
  labs(title = "Polls for the U.S.", x = "Date", y = "") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "bottom")

# the national polls with a smooth line (no scatter plot)
ggplot(filter(polls, State == "U.S."), 
       aes(x = EndDate, y = Percent, color = Candidate)) +
  geom_smooth(se = FALSE) +
  scale_color_manual(values = c("blue", "red")) +
  labs(title = "Polls for the U.S.", x = "Date", y = "") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "bottom")

# the swing state polls with a smooth line, this plot is using the filter() function to only display rows for the "Swing" column, and only if the row value for Swing = TRUE
ggplot(polls %>% filter(Swing == TRUE), 
       aes(x = EndDate, y = Percent, color = Candidate)) +
  geom_smooth(se = FALSE) + 
  scale_color_manual(values = c("blue", "red")) +
  labs(title = "Polls for the Swing States", x = "Date", y = "") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "bottom")

# likely voters in swing states three months prior to the election, filter by rows of "Likely" not "Registered" voters, and also only get the polls nearest to the election date. If the EndDate is greater than August 1st of 2016. 
ggplot(polls %>% filter(Swing == TRUE &
                          VoterType == "Likely" & 
                          EndDate > "2016-08-01"), 
       aes(x = EndDate, y = Percent, color = Candidate)) +
  geom_smooth(se = FALSE) +
  scale_color_manual(values = c("blue", "red")) +
  labs(title = "Likely Voters in Swing States", x = "Date", y = "") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "bottom")


# plot two swing states, create a vector holding two state names. 
states <- c("Arizona","Wisconsin") # add more if you want

# filter for the vector values by using an %in% parameter in the filter() function. 
ggplot(polls %>% filter(State %in% states & EndDate > "2016-08-01"),
       aes(x = EndDate, y = Percent, color = Candidate)) + 
  geom_smooth(se = FALSE) +
  scale_color_manual(values = c("blue", "red")) +
  facet_wrap(~ State, ncol = 1) +
  theme(legend.position = "bottom")

# plot the gap data for selected states
start_date <- as.Date("2016-09-01")

states <- c("Florida","Michigan","Ohio")
# create a scatter plot for the three states contained in the vector states
ggplot(polls %>% filter(State %in% states & EndDate > start_date),
       aes(x = EndDate, y = Gap)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  geom_hline(yintercept = 0, size = 1) + 
  facet_grid(vars(State))

# summarize the data by voter type and candidate, by creating a tibble called voter type for all the swing states, and calculate by using the summarize() function. pages 214-215. in this case you are calculating the average percent in swing states for each candidate by Voter Type, (likely or registered)
voter_types <- polls %>% 
  filter(Swing == TRUE) %>%
  group_by(VoterType, Candidate) %>% 
  summarize(MeanPercent = mean(Percent), SD = sd(Percent))

# view the summarized data
voter_types

# plot the summarized data, including a geom(errorbar) pages 248-249 for the standard deviation 
ggplot(voter_types, 
       aes(x = Candidate, y = MeanPercent, fill = VoterType)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = MeanPercent - SD/2, ymax = MeanPercent + SD/2), 
                width = 0.1, position = position_dodge(0.9)) +
  labs(y = "", x = "")



##################################
# More preparation and analysis
##################################

# get the mean gap for key swing states in week before election, the election took place on November 8th. First create a vector of the swing states. 
states <- c("Arizona","Florida","Iowa","Nevada","North Carolina",
            "Ohio","Pennsylvania","Wisconsin")
# now create a tibble called polls_nov that filters by Swing states as stored in the vector "states" and filtering by EndDate > the first of November. you then group this by swing state getting the average (mean) of the Gap between the two candidates.  
polls_nov <- polls %>% 
  filter(State %in% states & EndDate > as.Date("2016-11-01")) %>% 
  group_by(State) %>% 
  summarize(MeanStateGap = mean(Gap))

# add an Advantage column and get the absolute value for the mean gap
polls_nov <- polls_nov %>% mutate(
  Advantage = ifelse(MeanStateGap >= 0, "Clinton","Trump"),
  MeanStateGap = abs(round(MeanStateGap, 3)))

polls_nov

#plot the data, using a column chart.
ggplot(polls_nov) +
  geom_col(aes(x = State, y = MeanStateGap, fill = Advantage)) +
  scale_fill_manual(labels = c("Clinton","Trump"), 
                    values = c("blue","red")) +
  labs(title = "Results for Final Week of Election", 
       x = "", y = "Mean Percent") +
  theme(plot.title = element_text(hjust = 0.5))


# stopped at page 323. The rest is outside the scope of our class. 




