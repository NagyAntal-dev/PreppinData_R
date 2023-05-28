library(tidyverse)
library(dplyr)

### Read CSV ###
df <- read.csv("INPUT HERE",
               sep=',')

head(df$Transaction.Code)

### Get Bank Name ###
df$Transaction.Code <- sub("-([0-9]{3}-){2}[0-9]{3}","", df$Transaction.Code)

### Online or In-Person ###
df[df == 1] <- "Online"
df[df == 2] <- "In-Person"

### Add weekdays ###
df$Transaction.Date <- wday(df$Transaction.Date, label = TRUE)

### Task 1 - Total Values of Transactions by each bank ###
BANKS <- df %>%
  group_by(Transaction.Code) %>%
  summarise(TotalValue = sum(Value))

ggplot(data=BANKS, aes(y=TotalValue, x=Transaction.Code, fill = Transaction.Code)) + 
  geom_bar(stat="identity")

### Task 2 - Total Values by Bank, Day of the Week and Type of Transaction (NOT RIGHT) ###
BANKS_GROUPED <- df %>%
  group_by(Transaction.Code, Transaction.Date, Online.or.In.Person) %>%
  summarise(TotalValue = sum(Value), .groups = 'drop')

### Task 3 - Total Values by Bank and Customer Code ###
BANKS_CUSTOMER <- df %>%
  group_by(Transaction.Code, Customer.Code) %>%
  summarise(TotalValue = sum(Value), .groups = 'drop')
