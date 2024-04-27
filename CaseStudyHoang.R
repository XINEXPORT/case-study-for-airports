#My goal for taking this class was to learn more about data engineering, querying techniques, and making reports for my personal benefit.

library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)

tsa_claims1 <- read_csv("tsa_claims1.csv")
tsa_claims1

# What is the most common type of insurance claim?
# Which claim site within the airport are claims most commonly filed for?
# What type of claim is made most at each claim site? Hint: You can group by multiple columns.
# What is the median claim amount?
# What is the overall claim approval rate for the entire U.S.? Hint: You can get the number of claims for each status add then add a column that uses the sum() function to calculate the total number of claims.




