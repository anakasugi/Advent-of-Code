library("readr")

# Day 3: Problem 1 --------------------------------------------------------

# Q: Divide single element values (e.g. 12345) into separate elements for each number (1, 2, 3, 4, 5) & find mode

Day3 <- read_delim("~/Documents/Advent of Code/Day3.rtf", "\\", escape_double = FALSE, 
                   col_names = FALSE, 
                   col_types = cols_only(X1 = col_character()), 
                   trim_ws = TRUE, skip = 9)

# TEST: Use substr() to extract the 1st element of each data point and find mode:
a <- substr(Day3$X1, 1, 1)
table(a) # This works :)

# break down each 12-character string in the original data into 12 separate variables
a <- as.data.frame(a)

for (i in 1:12){
  a[,i] <- substr(Day3$X1, i, i)
} 

# tabulate number of 0s and 1s and gather them into a new df "d"
for (i in 1:12){
  c <- as.data.frame(table(a[i]))
  d[i,] <- pivot_wider(c[2:3,], names_from = Var1, values_from = Freq)
} 

# Answer: 011111101100 & 100000010011 --> 2028 * 2067 = 4191876


# Day 3: Problem 2 --------------------------------------------------------

# First iteration

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

