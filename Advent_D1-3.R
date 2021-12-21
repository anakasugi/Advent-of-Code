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

# Q: Divide single element values (e.g. 12345) into separate elements for each number (1, 2, 3, 4, 5) & find mode

Day3 <- read_delim("~/Documents/Advent of Code/Day3.rtf", "\\", escape_double = FALSE, 
                   col_names = FALSE, 
                   col_types = cols_only(X1 = col_character()), 
                   trim_ws = TRUE, skip = 9)

# TEST: Use substr() to extract the 1st element of each data point and find mode:
# substr() takes vectors as input, which is why Day3[1] (= tibble) messes things up. Day3$X1 = vector.
a <- substr(Day3$X1, 1, 1)
table(a) # This works :)

# break down each 12-character string in the original data into 12 separate variables

a <- as.data.frame(a)

for (i in 1:12){
  a[,i] <- substr(Day3$X1, i, i)
} 

# tabulate number of 0s and 1s and gather them into a new temp df "d"
for (i in 1:12){
  c <- as.data.frame(table(a[i]))
  d[i,] <- pivot_wider(c[2:3,], names_from = Var1, values_from = Freq)
} 

# Answer: 011111101100 & 100000010011 --> 2028 * 2067 = 4191876


# Day 3: Problem 2 --------------------------------------------------------

# First iteration
# d = 2 columns ("0", "1") & 12 rows (bits #1 - #12)

for (i in 1:1000){
  if (a$a[i] == "0"){
    oxygen_common$X1[i] <- Day3$X1[i]
  } else if (a$a[i] == "1"){
    CO_rare$X1[i] <- Day3$X1[i]
  }
}

for (j in 2:11){
  oxygen_common <- as.data.frame(na.omit(oxygen_common[1]))
  b <- as.factor(substr(oxygen_common$X1, j, j))
  counts <- fct_count(b)
  print(length(b))
  for (i in 1:length(oxygen_common)){
    if (counts$n[1] <= counts$n[2]){     # if there's equal or more 1s than 0s
      if (b[i] == 0){
        oxygen_common$X1[i] <- NA
      } 
    } else if (counts$n[1] > counts$n[2]) {     # if there's more 0s than 1s
      if (b[i] == 1){
        oxygen_common$X1[i] <- NA
      }
    }
  } 
} # Oxygen: 010101101111 = 1391

CO <- data.frame(matrix(NA, nrow = 1000, ncol = 5))

for (j in 2:9){
  CO_rare <- as.data.frame(na.omit(CO_rare[1]))
  b <- as.factor(substr(CO_rare$X1, j, j))
  counts <- fct_count(b)
  print(length(b))
  for (i in 1:length(CO_rare$X1)){
    if (counts$n[1] <= counts$n[2]){     # if there's more 1s than 0s
      if (b[i] == 1){
        CO_rare$X1[i] <- NA
      } 
    } else if (counts$n[1] > counts$n[2]) {     # if there's more 0s than 1s
      if (b[i] == 0){
        CO_rare$X1[i] <- NA
      }
    }
  } 
} # CO = 100110010111 = 2455

# Answer: 1391 * 2455 = 3414905



# ANOTHER WAY (by bcrossman)

oxygen <- a

Mode <- function(x) {
  ux <- unique(x)       # get unique values (in each column, when run for oxygen[,i])
  tab <- tabulate(match(x, ux))   # tabulate returns a vector of just the ncount of values (x,y,z) w/in a vector
  if (all(tab == tab[1])){  # if all values are the same as in position #1
    "1"                     
    } else {
    ux[which.max(tab)] 
  }
}

for(i in 1:ncol(oxygen)){
  if(nrow(oxygen)==1){
    next
    }
  oxygen <- oxygen[oxygen[,i] == Mode(oxygen[,i]),]
}

# oxygen[,i] == Mode(oxygen[,i])  returns logical values TRUE/FALSE
# oxygen <- oxygen[oxygen[,i] == Mode(oxygen[,i]),] only keeps values that are TRUE

oxygen_rating <- oxygen %>% 
  unlist(., use.names=FALSE) %>% # changes result into a single vector with 12 values (from 12 vars with 1 value each)
  paste0(collapse = "") %>% # collapses 12 values "0", "1", "0"... into a single value "010.."
  strtoi(base = 2) # string to integer --> base = 2 transforming binary to decimal

# oxygen_rating = 1391

CO <- a

for(i in 1:ncol(CO)){
  if(nrow(CO) == 1) {
    next
    }
  CO <- CO[CO[,i] == as.character(1-as.numeric(Mode(CO[,i]))),]
}

CO_rating <- CO %>% 
  unlist(., use.names=FALSE) %>%  
  paste0(collapse = "") %>% 
  strtoi(base = 2)

# CO_rating = 2455

oxygen_rating*CO_rating







