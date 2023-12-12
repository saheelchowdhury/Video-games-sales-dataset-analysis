#the sep and space in the middle with separate stuff 
paste("football", 'is', 'my drug', sep = " ")

# can use some separator other than the default " "
paste("football", 'is', 'my', 'drug', sep = "-")


#recycling -- This will be printed 10 times

weather <- paste ("yo ney sunny innit", 1:10, sep = "-")


# Pasting col. names 
library(reshape)

long_dat <- data.frame(id = c(1, 1, 2, 2),
                       year = c(2019, 2020, 2020, 2021),
                       inc = c(100, 110, 110, 120))

wide_dat <- cast(long_dat, id~year, value = "inc")

colnames(wide_dat) #initial check 

#Changing the col. names with paste 
colnames(wide_dat)[2:4] <- paste("inc", colnames(wide_dat)[2:4], sep = "_" ) 

colnames(wide_dat)

# counting characters using nchar 

friends <- c("eshan", "rahat", "tortilla")

nchar(friends)  # counting how many letters there are in each characters 
length(friends) # counting the length of the character vector 

#conversion of ALL to lower 
tolower(c("iS", "THIs", "hoW", "yoU", "dRRibbL3"))

#conversion of ALl to upper
toupper(c("iS", "THIs", "hoW", "yoU", "dRRibbL3"))

#Just converting initals to upper 
install.packages("Hmisc")
library(Hmisc)

#Beautify 
haphazard <- c("yoU can", 
               "dRRibbL3 reaLLy fast")
cat(haphazard)

#capitalise the first letter of each element 
capitalize(tolower(c("iS", "THIs", "hoW", "yoU", "dRRibbL3")))



#----- substr---subsplt-----

# this is how number will be extracted
#the first 0's are going to be ignored 

numero_uno <- 05767689894


substr(numero_uno, 1, 5)


# substr can be done inside the function itself 
substr("abcdef", start = 1, stop = 3)
## [1] "abc"

# can substr a vector
# each element of each vector will be affected 

substr(c("abc", "def"), start = 1, stop = 2)
## [1] "ab" "de"

#another example
substr(c("rahat", "eshan", "saheel"), 3, 5)

#phone number extraction. NOTE : ' ( ) ' are also counted. 
substr("(585) 345 7890", 2, 4)

#substring replacements 

phone_number <- "(585) 345 7890"

#replacing to bd code 

substr(phone_number, 2, 4) <- "880"

phone_number

#now lets just get all the area codes from a vector of phone numbers (IMPORTANT)

phone_numbers_together <- c("(585) 345 7890", "(880) 345 7890")

substr(phone_numbers_together, 2, 4)

#Substring function 

substring("abcdef", first = 1:3, last = 3)

#in Substring if there is no end specified # default is to stop at end of string
#complicated example
#always blank " if start > stop
substring("abcdef", first = 3:6, last = 5)        

#if the start and end are the same range, it returns each letter as output 
substring("abcdef", first = 1:3, last = 1:3)        
#even more complicated example
#here it means every odd index will end at 5 and ever other (even) will end at 6
substring("abcdef", first = 1:6, last = 5:6)

#lets make an even more complicated one 

substring("rtsghty", 2:7, 4:3)


#splitting 

strsplit("Saheel has great finishing", " ") #returns a list 

#Call elements from that list 
strsplit("Saheel has great finishing", " ")[[1]][4]






# names, note that structures are different
names <- c("Adam Smith", "George W. Bush")
split.names <- strsplit(names, " ")
split.names     # elements with different length
## [[1]]
## [1] "Adam"  "Smith"
##
## [[2]]
## [1] "George" "W."     "Bush"
# first name’s easy to get; how about last name?

last.name <- character(2)
last.name[1] <- tail(split.names[[1]], n = 1)
last.name[2] <- tail(split.names[[2]], n = 1)


#Another split 

names.correct <- c("Paul B. Ellickson", "Oleksandr(’Alex’) Shcherbakov",
                   "Jean-Francois Houde", "Xavi Vidal-Berastein")

#the splitting 
split.names <- strsplit(names.correct, " ")
split.names 

#creating a placeholder  

middle_names <- character (4)

middle_names[1] <- split.names[[1]][2]
middle_names[2] <- split.names[[2]][2]
middle_names[3] <- split.names[[3]][2]
middle_names[4] <- split.names[[4]][2]


#Recognizing patterns using grep and strsplt 

speech.char <- "Yes, we can, to justice and equality. Yes, we can, to opportunity and prosperity. 
Yes, we can heal this nation. Yes, we can repair this world. Yes, we can."

#Confirm if ther IS a can or not 
grep("can", speech.char)

#stplitting and unlisting in one go 
speech.vector <- unlist(strsplit(speech.char, " "))

#figure out the positions 

can_positions <- grep("can", speech.vector)
#figure out how many times occured 

length(can_positions)

#how to replace can with cannot 
gsub("can", "cannot", speech.char)

#to find a pattern and return logicals 
grepl("can", speech.vector)

#to remove any quotes while printing 

cat("yo", "ney", "sunny", "innit")

#get only digits from a character string which are not squenctial 

messed_up_seq <- "5454$233=342+0"

#only get digits using "\\D+" which removes all non-digits 
gsub("\\D+", "", messed_up_seq)



#We want only numeric numbers without even the (-) from this section 

# example
sentense <- "My phone number is (585)-234-5678. His phone number is (426)-811-1234.
And the office hotline is (888)-888-8888"

#first we split and unlist this into a vector 

#ineficient of doing this 
split.sentense <- unlist(strsplit(sentense, " "))
#we use G sub to eliminate all the non. numerics 
phones <- gsub("[^0-9]", "" , split.sentense) 

#then we split this into a list again
strsplit(phones, " ")
#Creating a vector with only phone no. 
numbers <- c(phones[[5]][1], phones[[10]][1], phones[[15]][1])

is.vector(numbers)

#turining into Numerics
as.numeric(numbers)

#efficient way of doing this - using grep pattern recognition separation 

sentense <- "My phone number is (585)-234-5678. His phone number is (426)-811-1234. And the office hotline is (888)-888-8888"

#Unlisting and splitting 
split.sentense <- unlist(strsplit(sentense, " "))

#just the numbers using grep
numbers <- grep("\\([0-9]{3}\\)\\-[0-9]{3}\\-[0-9]{4}", split.sentense, value = T)
numbers 

"\\([0-9]{3}\\)\\-[0-9]{3}\\-[0-9]{4}"

clean_numbers <- as.numeric(gsub("\\D*", "" , numbers))

#others 
#split a word by something
strsplit("together", "e" )
#replace a letter with another letter in a chr 
gsub("e", "f", "together")


#Working with dates 

date1 <- c("1999aug3", "2000jan25", "2001sep16")        # YYYY - %Y
#formatting into YYYY-MM-DD                             # aug/feb - %b
updated_date <- as.Date(date1, "%Y%b%d")                # mm(04) - %m
#Removing the "-" to get a whole number 
date_whole <- gsub("-", "", updated_date)

is.Date(date_whole) #it is no longer date 
as.numeric(date_whole) 

#Time difference 

difftime(updated_date[2], updated_date[1], units = "days")

#what if there is a separator?
date2 <- c("01-01-2001", "03-04-2002", "25-03-2003")
#match the d-m-y to how it is in the original vector 
date2_clean <- as.Date(date2, "%d-%m-%Y")
#if you just want the years 
years <- format(date2_clean, "%Y")
years
#if you just want the months 
months <- format(date2_clean, "%m")
months

# test 
date3 <- c("990101", "000202", "010325")
Date3 <- as.Date(date3, "%y%m%d")
Date3
#figure out month name
monthname <- months(Date3)
monthname
#Figure out weekdays 
weekday <- weekdays(Date3)
weekday


sentence <- ("His palms are sweaty, knees weak, arms are heavy,
## There's vomit on his sweater already,
## mom's spaghetti. He's nervous,
## but on the surface he looks calm and ready,
## To drop bombs, but he keeps on forgettin")

split.sentence <- unlist(strsplit(sentence, " "))
split.sentence
words <- c(split.sentence[4], split.sentence[9], split.sentence[15])












