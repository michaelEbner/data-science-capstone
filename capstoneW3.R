library(stringr)
library("tokenizers")
library(dplyr)
library(ggplot2)
library(gridExtra)
library(rJava)
library(openNLP)
library(tm)
library(textcat)

#setwd('/Users/mickey/Documents/GitHub/data-science-capstone')
setwd('/Users/mebner/Documents/for_me/R_coursera/data-science-capstone')

path_to_data <- '/Users/mebner/Documents/for_me/R_coursera/final/'

#table length function
#et_counts <- function (filename) {
# path <- paste0('final/',substring(filename, 1,5),'/',filename)
#con <- file(path,"r")
#set.seed(23)
#full <- readLines(con, encoding="UTF-8")
#print(paste0(filename,'|number of lines: ',length(full)))
#print(paste0(filename,'|number of words: ',(sum(lengths(strsplit(full, "\\W+"))))))
#close(con)
#}

#run getsample for each file selectd
#get_counts('en_US.blogs.txt')
#get_counts('en_US.news.txt')
#suppressWarnings(get_counts('en_US.twitter.txt'))


#for function testing only
filename = 'en_US.news.txt'
prob = 0.05

get_sample <- function (filename,prob) {
  path <- paste0(path_to_data,substring(filename, 1,5),'/',filename)
  con <- file(path,"r")
  full <- readLines(con, encoding="UTF-8")
  close(con)
  set.seed(23)
  sample <- as.data.frame(list(full[rbinom(length(full),1,prob)==1]))
  colnames(sample)[1] <- "words"
  sample
}

clean_data <- function(sample) {
  sample <- sample %>%mutate(words = gsub("(http[s]?://|www)(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+ ", "", words)) %>%
    mutate(words = gsub("@\\w+", " ", words)) %>%
    mutate(words = removePunctuation(words,preserve_intra_word_contractions = TRUE,preserve_intra_word_dashes = TRUE)) %>%
    mutate(words = gsub("[^\x20-\x7E]", "", words)) %>%
    mutate(words = gsub("  "," ",words)) %>%
    mutate(words = gsub("n\'t", " not", words)) %>%
    mutate(words = gsub("\'re", " are", words)) %>%
    mutate(words = gsub("\'s", " is", words)) %>%
    mutate(words = gsub("\'d", " would", words)) %>%
    mutate(words = gsub("\'ll", " will", words)) %>%
    mutate(words = gsub("\'t", " not", words)) %>%
    mutate(words = gsub("\'ve", " have", words)) %>%
    mutate(words = gsub("\'m", " am", words))
  #tokinize the text, get words, 2-grams and 3-grams. We keep stopwords as they are relevant for this task
  sample <- as.vector(sample[,1]) %>%
    tokenize_ngrams(n = 4, n_min = 1) %>%
    unlist() %>% list() %>%
    as.data.frame() %>% mutate(language = str_sub(filename,1,5),media = str_extract(filename,'blogs|news|twitter'))
  colnames(sample)[1] <- "words"
  #sample file with gram type and bad word column
  en_bad <- read.table("badlanguage.txt")
  sample <- sample %>% mutate(type = paste0(str_count(words, ' ')+1,'-grams'),bad_word = grepl(paste0('((^| )',unlist(en_bad),'( |$))', collapse="|"),words))
  #get rid of numbers only recoreds and records that end with a number
  sample <- sample %>% subset(!grepl('^[0-9, ]+$',words) & !grepl('.*[0-9, ]$',words)) %>%
    mutate(words = gsub("\\d+", "<digit>", words))
  #get frequencies without bad words and write file
}

getfrequencies <- function(cleaned_sample) {
  en_bad <- read.csv("badlanguage.txt", header = T)
  freq <- cleaned_sample %>% subset(bad_word = 'FALSE') %>% group_by(words,language,media,type) %>%
    summarise(counts = n()) %>% ungroup() %>% group_by(language,media,type) %>%
    mutate(rank = rank(-counts)) %>%
    ungroup()
}


#possible filenames
files <- list.files("final",recursive=TRUE, full.names=TRUE, pattern="*/*\\.txt")
gsub('[a-z]{5}\\/[a-z]{2}_[A-Z]{2}\\/','',files)

#possible sample values are > 0 and < 1
prob = 0.05

#run getsample for each file selectd
blog_sample <- get_sample('en_US.blogs.txt',prob)
news_sample <- get_sample('en_US.news.txt',prob)
twitter_sample <- get_sample('en_US.twitter.txt',prob)

#run getsample for each file selectd
blog_sample <- clean_data(blog_sample)
news_sample <- clean_data(news_sample)
twitter_sample <- clean_data(twitter_sample)

sample <- rbind(blog_sample %>% subset(bad_word = 'FALSE'),news_sample %>% subset(bad_word = 'FALSE'),twitter_sample %>% subset(bad_word = 'FALSE'))

#read freq files
blog_freq <- getfrequencies(blog_sample)
news_freq <- getfrequencies(news_sample)
twitter_freq <- getfrequencies(twitter_sample)

freq <- rbind(blog_freq,news_freq,twitter_freq)



sample_freq <- sample %>%
  select(words,language,type) %>%
  group_by(words,language,type) %>%
  summarise(counts = n()) %>%
  group_by(language,type) %>%
  mutate(rank = rank(-counts, ties.method = 'first')) %>%
  as.data.frame()

#list data frames
df <- sample_freq %>% 
  #subset(grepl('^zurich',words)) %>%
  select(words,type,counts,language) %>%
  arrange(-counts) %>%
  group_by(type,language) %>%
  mutate(total = sum(counts), freq = counts / sum(counts), unique_count = 1) %>%
  mutate(cum_unique = cumsum(unique_count),cum_freq = cumsum(freq))

df_sample_95 <- df %>%
  subset(cum_freq <= .95 & type != 'NA-grams') %>%
        mutate(input = str_replace(words,paste0(' ',str_extract(words,"[<,>,a-z]+$")),""),
               next_word = str_extract(words,"[<,>,a-z]+$")) %>%
  subset(next_word != '<digit>') %>%
  select(type,input,next_word,counts) %>%
  group_by(type, input) %>%
  mutate(n = sum(counts)) %>% 
  distinct(type, input, next_word, n, .keep_all=TRUE) %>%
  mutate(prob = round(counts / n,2))

#ldf <- split(df,df$type,drop=FALSE) 

#ldf <- lapply(df, function(x) {
#  mutate(x,
#         input =str_replace(words,paste0(' ',str_extract(words,"[<,>,a-z]+$")),""),
#         last_word = str_extract(words,"[<,>,a-z]+$"))
#})

df <- df_sample

predict_next_word <- function(user_input) {
#remove urls
user_input <- gsub("(http[s]?://|www)(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", "", user_input)
#remove twitter add mentions
user_input <- gsub("@\\w+", "", user_input)
#remove punctuation
user_input <- removePunctuation(user_input,preserve_intra_word_contractions = TRUE,preserve_intra_word_dashes = TRUE)
#remove records with non-asci characters 
user_input <- gsub("[^\x20-\x7E]", "", user_input)
#to lower
user_input <- tolower(user_input)
#remove double spaces
user_input <- gsub("  "," ",user_input)
#replace digits with placeholder
user_input = gsub("\\d+", "<digit>", user_input)


if(!is.na(as.data.frame(head(df %>% subset(type == '4-grams' & input == str_extract(user_input,'\\S+(?:\\s+\\S+){0,2}$')) %>% arrange(-prob)))[1,4])){
  print(as.data.frame(head(df %>% subset(type == '4-grams' & input == str_extract(user_input,'\\S+(?:\\s+\\S+){0,2}$')) %>% arrange(-prob)))[1,4])
  print(as.data.frame(head(df %>% subset(type == '4-grams' & input == str_extract(user_input,'\\S+(?:\\s+\\S+){0,2}$')) %>% arrange(-prob)))[2,4])
  print(as.data.frame(head(df %>% subset(type == '4-grams' & input == str_extract(user_input,'\\S+(?:\\s+\\S+){0,2}$')) %>% arrange(-prob)))[3,4])
  print(as.data.frame(head(df %>% subset(type == '4-grams' & input == str_extract(user_input,'\\S+(?:\\s+\\S+){0,2}$')) %>% arrange(-prob)))[1,3])
  print(as.data.frame(head(df %>% subset(type == '4-grams' & input == str_extract(user_input,'\\S+(?:\\s+\\S+){0,2}$')) %>% arrange(-prob)))[1,2])
} else if (!is.na(as.data.frame(head(df %>% subset(type == '3-grams' & input == str_extract(user_input,'\\S+(?:\\s+\\S+){0,1}$')) %>% arrange(-prob)))[1,4])){
  print(as.data.frame(head(df %>% subset(type == '3-grams' & input == str_extract(user_input,'\\S+(?:\\s+\\S+){0,1}$')) %>% arrange(-prob)))[1,4])
  print(as.data.frame(head(df %>% subset(type == '3-grams' & input == str_extract(user_input,'\\S+(?:\\s+\\S+){0,1}$')) %>% arrange(-prob)))[2,4])
  print(as.data.frame(head(df %>% subset(type == '3-grams' & input == str_extract(user_input,'\\S+(?:\\s+\\S+){0,1}$')) %>% arrange(-prob)))[3,4])
  print(as.data.frame(head(df %>% subset(type == '3-grams' & input == str_extract(user_input,'\\S+(?:\\s+\\S+){0,1}$')) %>% arrange(-prob)))[1,3])
  print(as.data.frame(head(df %>% subset(type == '3-grams' & input == str_extract(user_input,'\\S+(?:\\s+\\S+){0,1}$')) %>% arrange(-prob)))[1,2])
} else if (!is.na(as.data.frame(head(df %>% subset(type == '2-grams' & input == str_extract(user_input,'\\S+(?:\\s+\\S+){0,0}$')) %>% arrange(-prob)))[1,4])){
  print(as.data.frame(head(df %>% subset(type == '2-grams' & input == str_extract(user_input,'\\S+(?:\\s+\\S+){0,0}$')) %>% arrange(-prob)))[1,4])
  print(as.data.frame(head(df %>% subset(type == '2-grams' & input == str_extract(user_input,'\\S+(?:\\s+\\S+){0,0}$')) %>% arrange(-prob)))[2,4])
  print(as.data.frame(head(df %>% subset(type == '2-grams' & input == str_extract(user_input,'\\S+(?:\\s+\\S+){0,0}$')) %>% arrange(-prob)))[3,4])
  print(as.data.frame(head(df %>% subset(type == '2-grams' & input == str_extract(user_input,'\\S+(?:\\s+\\S+){0,0}$')) %>% arrange(-prob)))[1,3])
  print(as.data.frame(head(df %>% subset(type == '2-grams' & input == str_extract(user_input,'\\S+(?:\\s+\\S+){0,0}$')) %>% arrange(-prob)))[1,2])
} else {
  print(as.data.frame(head(df %>% subset(type == '1-grams') %>% arrange(-prob)))[1,4])
  print(as.data.frame(head(df %>% subset(type == '1-grams') %>% arrange(-prob)))[2,4])
  print(as.data.frame(head(df %>% subset(type == '1-grams') %>% arrange(-prob)))[3,4])
  print(as.data.frame(head(df %>% subset(type == '1-grams') %>% arrange(-prob)))[1,3])
  print(as.data.frame(head(df %>% subset(type == '1-grams') %>% arrange(-prob)))[1,2])
}
}
user_input <- 'no I just'
start_time <- Sys.time()
user_input <- predict_next_word(user_input)
end_time <- Sys.time()
start_time - end_time
system.time(predict_next_word(user_input))

