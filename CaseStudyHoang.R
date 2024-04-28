#My goal for taking this class was to learn more about data engineering, querying techniques, and making reports for my personal benefit.

library(readr)
install.packages("tidyverse")
library(tidyverse)
install.packages("")
library(ggplot2)
install.packages("dplyr")
library(dplyr)

tsa_claims <- read_csv("tsa_claims1.csv")
tsa_claims

str(tsa_claims)
head(tsa_claims)

# Lower case and Snake Case the column headers
#I wanted to lower case all of the column headers because R has case sensitivities for upper casing.
#I also wanted to snake case all of the column headers to prevent my queries from having back ticks due to spaces between words.

names(tsa_claims) <- tolower(names(tsa_claims))
tsa_claims

tsa_claims %>%
  rename(
    claim_number = `claim number`,
    date_received = `date received`,
    incident_date = `incident date`,
    airport_code = `airport code`,
    airline_name =  `airline name`,
    claim_type =  `claim type`,
    claim_site = `claim site`,
    claim_amount = `claim amount`,
    close_amount =  `close amount` 
    )

# What is the most common type of insurance claim?




# Which claim site within the airport are claims most commonly filed for?

# What type of claim is made most at each claim site? Hint: You can group by multiple columns.

# What is the median claim amount?

# What is the overall claim approval rate for the entire U.S.? Hint: You can get the number of claims for each status add then add a column that uses the sum() function to calculate the total number of claims.




