library("readr")


# Day 1: Problem 1 --------------------------------------------------------

day1 <- read_delim("~/Documents/Advent of Code/Day1.csv", ";", escape_double = FALSE, trim_ws = TRUE, skip = 1)
# Q: How many times x[i+1] was bigger than x[i]?

day1 <- day1[1:2] %>% 
  rename(state = X2)

for (i in 1:length(day1)){
  if (day1$Levels[i] < day1$Levels[i+1]) {
    day1$state[i+1] <- "Increased"
  } else {
    day1$state[i+1] <- "Decreased"
  }
}

state.tab <- table(day1$state)
state.tab # "Increased" 1564 times


# Day 1: Problem 2 --------------------------------------------------------

# Q: Same as above, but with rolling sums of 3-row windows

# Re-run import for a clean slate, and...
day1 <- day1[1:3] %>% 
  rename(total = X2) %>% 
  rename(state = X3)

for (i in 1:nrow(day1)){
  day1$total[i] <- sum(day1$Levels[i:(i+2)])
}

for (i in 1:nrow(day1)){
  if (day1$total[i] < day1$total[i+1]) {
    day1$state[i+1] <- "Increased"
  } else if (day1$total[i] > day1$total[i+1]){
    day1$state[i+1] <- "Decreased"
  } else {
    day1$state[i+1] <- "No change"
  }
}

state.tab <- table(day1$state)
state.tab # "Increased" 1611 times


# Day 2: Problem 1 ---------------------------------------------------------

# Q: a) Forward sum with N+ numbers and b) Depth sum with N numbers

Day2 <- read_delim("~/Documents/Advent of Code/Day2.csv", ";", trim_ws = TRUE)

day2 <- Day2[2:5] %>% 
  rename(forward = X4, depth = X5)

for (i in 1:nrow(day2)){
  if (day2$direction[i] == "forward") {
    day2$forward[i] <- day2$steps[i] 
  } else if (day2$direction[i] == "down") {
    day2$depth[i] <- day2$steps[i]
  } else {
    day2$depth[i] <- day2$steps[i]*(-1)
  }
}

sumforward <- sum(day2$forward, na.rm = TRUE)
sumdepth <- sum(day2$depth, na.rm = TRUE)
sumforward*sumdepth # Answer: 1660158


# Day 2: Problem 2 ---------------------------------------------------------

# Q: a) Forward sum unchanged, b) Depth sum dependent on current value of forward -vector

day2 <- day2 %>% 
  add_column(aim = 0) %>% 
  add_column(depthsum = 0)


for (i in 1:nrow(day2)){
  if (day2$direction[i] == "forward"){
    day2$depth[i] <- 0
  } 
}

# Preparing the aim -vector
for (i in 2:nrow(day2)){
  if (day2$direction[i] == "forward"){
    day2$aim[i] <- day2$aim[i-1]
  } else {
    day2$aim[i] <- day2$aim[i-1] + day2$depth[i]
  }
} 

# Calculating cumulative depth
for (i in 2:nrow(day2)){
  if (day2$direction[i] == "forward"){
    day2$depthsum[i] <- day2$depthsum[i-1] + day2$aim[i] * day2$forward[i]
  } else {
    day2$depthsum[i] <- day2$depthsum[i-1]
  }
} 

forwardsum <- sum(day2$forward, na.rm = TRUE)
forwardsum * day2$depthsum[1000] # Answer: 1604592846


# Day 3: Problem 1 --------------------------------------------------------



