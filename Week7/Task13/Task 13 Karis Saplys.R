library(tidyverse)
library(readr)
library(stringr)

RandomLetters <- read_lines("https://github.com/WJC-Data-Science/DTS350/raw/master/randomletters.txt")
RandomLetter_Num <- read_lines("https://raw.githubusercontent.com/WJC-Data-Science/DTS350/master/randomletters_wnumbers.txt")

letterslist <- c(str_sub(RandomLetters, start = 1, end = 1))
for (i in seq(0, str_length(RandomLetters)/1700)) {
  letterslist <- str_c(letterslist, str_sub(RandomLetters, start = i*1700, end = i*1700))
}

letterslist

hidden_num <- unlist(str_extract_all(RandomLetter_Num, ("(\\d)+")))
message <- c()

for (i in seq(1,length(hidden_num))) {
  message <- append(message,letters[as.integer(hidden_num[i])])
}
message <- paste(message,collapse = " ")
message

str_extract_all(RandomLetters, "(.)(.)(.)(.)\\4\\3\\2\\1")

count <- c("0")

No_Spaces_Periods <- RandomLetters %>%
  str_remove_all(" ") %>%
  str_remove_all("\\.")

vowels <- unlist(str_extract_all(No_Spaces_Periods,"([aeiou])+"))

for (i in seq(1,length(vowels))) {
  if (str_length(vowels[i]) > str_length(count[length(count)])) {
    count <- vowels[i]
  }
}

count
