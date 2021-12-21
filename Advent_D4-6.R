# Advent of Code 2021
# Day 4: Problem 1 --------------------------------------------------------

Day4_draws <- read.csv("~/Documents/Advent of Code/Day4_draws.rtf", header=FALSE, comment.char="#")
Day4_draws <- Day4_draws[9:108,]
Day4_draws[1] <- 46

Day4_boards <- read_delim("~/Documents/Advent of Code/Day4_boards.rtf",  "\\", 
                          escape_double = FALSE, 
                          col_names = FALSE, 
                          col_types = cols_only(X1 = col_guess()), 
                          trim_ws = TRUE, skip = 9)

Day4_boards$X1[1] <- "84 94 24 52 44"

# Replace douple-scaped gaps between numbers to single-spaced ones --> split single chr_strings into 5
Day4_boards$X1 <- str_replace(Day4_boards$X1, "  ", " ") #run twice
boards <- str_split_fixed(Day4_boards$X1, " ", 5)