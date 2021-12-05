library("readr")


# Day 1: Problem 1 --------------------------------------------------------

day1 <- read_delim("~/Documents/Advent of Code/Day1.csv", ";", escape_double = FALSE, trim_ws = TRUE, skip = 1)
# Q: How many times x[i+1] was bigger than x[i]?

day1 <- day1[1:2] %>% 
  rename(state = X2)

for (i in 1:length(day1)){
  if (day1$Levels[i] <= day1$Levels[i+1]) {
    day1$state[i+1] <- "Increased"
  } else {
    day1$state[i+1] <- "Decreased"
  }
}

state.tab <- table(day1$state)
state.tab # "Increased" 1564 times


# Day 1: Problem 2 --------------------------------------------------------


