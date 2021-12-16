# Modulo 6 Harvard - Wrangling
# Seccion 3 - String Processing
library(tidyverse)
library(rvest)

# ----- 3.1.1. String Parsing  -------------------
# read raw data from wikipedia

url <- "https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167"
murders_raw <- read_html(url) %>% html_nodes("table") %>% html_table() %>% .[[1]] %>% setNames(c("state", "population", "total", "murder_rate"))
head(murders_raw)

# ----- 3.1.4. String Processing 1-----------------
# detect commas
commas <- function(x) any(str_detect(x, ","))
murders_raw %>% summarise_all(funs(commas))
# replace commas with empty string an convert to numeric
test_1 <- str_replace_all(murders_raw$population, ",", "")
test_1 <- as.numeric(test_1)
# parse_number also removes commas and convert to numeric
test_2 <- parse_number(murders_raw$population)
identical(test_1, test_2)

murders_new <- murders_raw %>% mutate_at(2:3, parse_number)
murders_new %>% head

# ----- 3.2.1. String Processing 2-----------------
llibrary(dslabs)
data(reported_heights)
class(reported_heights$height)

# convert to numeric, inspect, count NAs
x <- as.numeric(reported_heights$height)
head(x)
sum(is.na(x))

# keep only entries that result in NAs
reported_heights %>% mutate(new_height = as.numeric(height)) %>% filter(is.na(new_height)) %>% head(n=10)

not_inches <- function(x, smallest= 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest # | = logical OR, combines elements of 1st vector with 2nd and gives output TRUE if one of the 2 is TRUE
  ind
}
problems <- reported_heights %>% filter(not_inches(height)) %>% .$height

pattern <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$" # x'y or x'y" or x'y\"
str_subset(problems, pattern) %>% head(n=10) %>% cat

# ----- 3.2.2. Regex -----------------
library(tidyverse)
pattern <- ","
str_detect(murders_raw$total, pattern) #detect wheter there is a comma

str_subset(reported_heights$height, "cm") # show subset where is "cm"

# use | operator inside a regen (OR)
yes <- c("180 cm", "70 inches")
no <- c("180", "70''")
s <- c(yes,no)
str_detect(s,"cm") | str_detect(s,"inches")
str_detect(s,"cm|inches") #equivalent to last one but more efficient
str_view(s, "cm")

# ----- 3.2.4. Search and replace with Regex -----------------
# number of entries matching our desired pattern
pattern <- "^[4-7]'\\d{1,2}\"$"
sum(str_detect(problems, pattern))

# inspect examples of entries with problems
problems[c(2, 10, 11, 12, 15)] %>% str_view(pattern)
str_subset(problems, "inches")
str_subset(problems, "''")

# replace or remove feet/inches words before matching
pattern <- "^[4-7]'\\d{1,2}$"
problems %>% 
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>% 
  sum()

# R does not ignore whitespace
identical("Hi", "Hi ")

# \\s represents whitespace
pattern_2 <- "^[4-7]'\\s\\d{1,2}\"$"
str_subset(problems, pattern_2)

# * means 0 or more instances of a character (previous to * symbol)
yes <- c("AB", "A1B", "A11B", "A111B", "A1111B")
no <- c("A2B", "A21B")
str_detect(yes, "A1*B")
str_detect(no, "A1*B")

# test how *, ? and + differ
data.frame(string = c("AB", "A1B", "A11B", "A111B", "A1111B"),
           none_or_more = str_detect(yes, "A1*B"),
           nore_or_once = str_detect(yes, "A1?B"),
           once_or_more = str_detect(yes, "A1+B"))

# update pattern by adding optional spaces before and after feet symbol
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
problems %>% 
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>% 
  sum()

# ----- 3.2.5. Groups with Regex -----------------
# define regex with and without groups
pattern_without_groups <- "^[4-7],\\d*$"
pattern_with_groups <- "^([4-7]),(\\d*)$"
# create examples
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes,no)
# demonstrate the effect of groups
str_detect(s, pattern_without_groups)
str_detect(s, pattern_with_groups) # <-- groups don't affects str_detects
# demonstrate difference str_match and str_extract
str_match(s, pattern_with_groups) # <-- match gives info about groups so can be extracted later
str_extract(s, pattern_with_groups) # <-- extract what was detected
# improve pattern to detect more
pattern_with_groups <- "^([4-7]),(\\d*)$"
s <- c(yes, no)
str_replace(s, pattern_with_groups, "\\1'\\2") # <<- \\1 \\2 calls to extracted 1st and 2nd groups
# final pattern
pattern_with_groups <- "^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"

str_subset(problems, pattern_with_groups) %>% head
str_subset(problems, pattern_with_groups) %>% str_replace(pattern_with_groups, "\\1'\\2") %>% head

# ----- 3.2.6. Testing and Improving -----------------
# function to detect issues
not_inches_or_cm <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- !is.na(inches) & 
    ((inches >= smallest & inches <= tallest) |
       (inches/2.54 >= smallest & inches/2.54 <= tallest))
  !ind
}
# identify entries with issues
problems <- reported_heights %>% filter(not_inches_or_cm(height)) %>% .$height
length(problems)

converted <- problems %>%
  str_replace("feet|foot|ft", "'") %>%
  str_replace("inches|in|''|\"", "") %>%
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")

# find proportion of entries that fit the pattern after formatting
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
mean(index)
converted[!index] # show remaining problems

# ----- 3.3.2. Using Groups and Quantifiers  -----------------
# case1 (only one digit --> feets)
yes <- c("5", "6", "5")
no <- c("5'", "5''", "5'4")
s <- c(yes,no)
str_replace(s, "^([4-7])$", "\\1'0")

# cases 2 and 4 (only one digit --> feets')
str_replace(s, "^([56])'?$", "\\1'0")

# case 3 (inches with decimal point)
pattern <- "^[4-7]\\s*'\\s*(\\d+\\.?\\d*)$"

# case 5 (entries in meters and some with comma as decimal point)
yes <- c("1,7", "1,8", "2, ")
no <- c("5,8", "5,3,2", "1.7")
s <- c(yes, no)
str_replace(s, "^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2")

str_trim("5 ' 9 ") #trim trailing white spaces

# writing a function that covers all issues
convert_format <- function(s){
  s %>%
    str_replace("feet|foot|ft", "'") %>%
    str_replace("inches|in|''|\"|cm|and", "") %>%
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>%
    str_replace("^([56])'?$", "\\1'0") %>%
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>%
    str_trim()
}
words_to_numbers <- function(s){
  str_to_lower(s) %>%  
    str_replace_all("zero", "0") %>%
    str_replace_all("one", "1") %>%
    str_replace_all("two", "2") %>%
    str_replace_all("three", "3") %>%
    str_replace_all("four", "4") %>%
    str_replace_all("five", "5") %>%
    str_replace_all("six", "6") %>%
    str_replace_all("seven", "7") %>%
    str_replace_all("eight", "8") %>%
    str_replace_all("nine", "9") %>%
    str_replace_all("ten", "10") %>%
    str_replace_all("eleven", "11")
}
# seeing entries that still remains unsolved
converted <- problems %>% words_to_numbers %>% convert_format
remaining_problems <- converted[not_inches_or_cm(converted)]
pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"
index <- str_detect(remaining_problems, pattern)
remaining_problems[!index]

# ----- 3.3.3. Putting all together  -----------------
pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"
smallest <- 50
tallest <- 84
new_heights <- reported_heights %>% 
  mutate(original = height, 
         height = words_to_numbers(height) %>% convert_format()) %>%
  extract(height, c("feet", "inches"), regex = pattern, remove = FALSE) %>% 
  mutate_at(c("height", "feet", "inches"), as.numeric) %>%
  mutate(guess = 12*feet + inches) %>%
  mutate(height = case_when(
    !is.na(height) & between(height, smallest, tallest) ~ height, #inches 
    !is.na(height) & between(height/2.54, smallest, tallest) ~ height/2.54, #centimeters
    !is.na(height) & between(height*100/2.54, smallest, tallest) ~ height*100/2.54, #meters
    !is.na(guess) & inches < 12 & between(guess, smallest, tallest) ~ guess, #feet'inches
    TRUE ~ as.numeric(NA))) %>%
  select(-guess)

# check all the entries that were converted
new_heights %>% filter(not_inches(original)) %>% select(original, height) %>%
  arrange(height) %>% View()

# check the shortest height in dataset
new_heights %>% arrange(height) %>% head(n=7)

# ----- 3.3.4. String Splitting  -----------------
# importing murders file with R base function (reading line by line)
filename <- system.file("extdata/murders.csv", package = "dslabs")
lines <- readLines(filename)
lines %>% head()
# split at commas str_split, then removes column name
x <- str_split(lines, ",")
x %>% head()
col_names <- x[[1]]
x <- x[-1]
# extract first element of each list entry
library(purrr)
map(x, function(y) y[1]) %>% head() # map applies the same function to each element of a list
map(x, 1) %>% head() # same as above but with shortcut, map see the integer and knows one wants what's in that position

# extract column 1-5 as character then convert to proper format (different from video)
dat <- data.frame(parse_guess(map_chr(x,1)), 
                  parse_guess(map_chr(x,2)), 
                  parse_guess(map_chr(x,3)), 
                  parse_guess(map_chr(x,4)), 
                  parse_guess(map_chr(x,5))) %>% 
  setNames(col_names)  
dat %>% head

# now a more efficient code using purrr functions
dat <- x %>% 
  transpose() %>% 
  map( ~ parse_guess(unlist(.))) %>% 
  setNames(col_names) %>% 
  as.data.frame()

# even so, the simplify argument makes str_split return a matrix instead of a list, so another way
x <- str_split(lines, ",", simplify = TRUE)
col_names <- x[1,]
x <- x[-1,]
x %>% as_data_frame() %>% 
  setNames(col_names) %>% 
  mutate_all(parse_guess) %>% head()

# ----- 3.3.5. Case Study: Extracting a Table from a PDF  -----------------
library(dslabs)
data("research_funding_rates")
# downloading the pdf and passing it to a txt object
library("pdftools")
temp_file <- tempfile()
url <- "http://www.pnas.org/content/suppl/2015/09/16/1510159112.DCSupplemental/pnas.201510159SI.pdf"
download.file(url, temp_file)
txt <- pdf_text(temp_file)
file.remove(temp_file)

# txt is a chr vector with entry for each page. Keeping with the one we want
raw_data_research_funding_rates <- txt[2]
data("raw_data_research_funding_rates")
raw_data_research_funding_rates %>% head
# now we've a 1 element vector, where \n indicates a line break in pdf, even true for table. Creating a list
tab <- str_split(raw_data_research_funding_rates, "\n")
tab <- tab[[1]] # keeping the one and only element of the list
tab %>% head

# extracting the column names
the_names_1 <- tab[3]
the_names_2 <- tab[4]

the_names_1 <- the_names_1 %>% 
  str_trim() %>% # removes white spaces from start and end of string
  str_replace_all(",\\s.", "") %>% 
  str_split("\\s{2,}", simplify = TRUE) # breaks when there're 2 or more white spaces and return a chr matrix

the_names_2 <- the_names_2 %>% 
  str_trim() %>% 
  str_split("\\s+", simplify = TRUE)

tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
the_names <- c(the_names_2[1], tmp_names) %>%
  str_to_lower() %>% 
  str_replace_all("\\s", "_")

# done with column names, now go to extract the data
new_research_funding_rates <- tab[6:14] %>% 
  str_trim() %>% 
  str_split("\\s{2,}", simplify = TRUE) %>% 
  data.frame(stringsAsFactors = FALSE) %>% 
  setNames(the_names) %>% 
  mutate_at(-1, parse_number) # parse to number every column except the 1st one.

# ----- 3.3.6. Recoding  -----------------
library(dslabs)
data("gapminder")

# life expectancy for caribbean countries
gapminder %>% filter(region == "Caribbean") %>% 
  ggplot(aes(year, life_expectancy, color = country)) + geom_line()

# display long country names
gapminder %>% filter(region == "Caribbean") %>% 
  filter(str_length(country) >= 12) %>% distinct(country)

# recode long country names and remake plot
gapminder %>% filter(region == "Caribbean") %>%
  mutate(country = recode(country, 
                          'Antigua and Barbuda' = "Barbuda", 
                          'Dominican Republic' = "DR", 
                          'St. Vincent and the Grenadines' = "St. Vincent", 
                          'Trinidad and Tobago' = "Trinidad")) %>% 
  ggplot(aes(year, life_expectancy, color = country)) + geom_line()
