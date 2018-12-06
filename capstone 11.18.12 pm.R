#possible filenames
files <- list.files("final",recursive=TRUE, full.names=TRUE, pattern="*/*\\.txt")
gsub('[a-z]{5}\\/[a-z]{2}_[A-Z]{2}\\/','',files)

#possible sample values are > 0 and < 1
prob = 0.01

#run getsample for each file selectd
get_sample('en_US.blogs.txt',prob)
get_sample('en_US.news.txt',prob)
get_sample('en_US.twitter.txt',prob)

#read freq files
en_blog_freq <- read.table("en_US.blogs_freq.txt",stringsAsFactors=FALSE)
en_news_freq <- read.table("en_US.news_freq.txt",stringsAsFactors=FALSE)
en_twitter_freq <- read.table("en_US.twitter_freq.txt",stringsAsFactors=FALSE)

en_freq <- rbind(en_blog_freq,en_news_freq,en_twitter_freq)

#read samples
en_blog_sample_01 <- read.table("en_US.blogs_sample.txt",stringsAsFactors=FALSE)
en_news_sample_01 <- read.table("en_US.news_sample.txt",stringsAsFactors=FALSE)
en_twitter_sample_01 <- read.table("en_US.twitter_sample.txt",stringsAsFactors=FALSE)

en_sample_01 <- rbind(en_blog_sample_01,en_news_sample_01,en_twitter_sample_01)

#total counts
counts <- en_freq %>% subset(!is.na(type)) %>% group_by(language,media,type) %>% summarise(total_counts = sum(counts)) %>% arrange(media,-total_counts)
total_counts <- en_freq %>% subset(!is.na(type)) %>% group_by(language,type) %>% summarise(total_counts = sum(counts)) %>% mutate(media = 'all') %>% select(language, media, type,total_counts) %>% arrange(media,-total_counts) 
rbind(counts,total_counts)

#create full data frame
en_sample_freq <- en_sample_01 %>%
                  select(words,language,type) %>%
                  group_by(words,language,type) %>%
                  summarise(counts = n()) %>%
                  group_by(language,type) %>%
                  mutate(rank = rank(-counts, ties.method = 'first')) %>%
                  as.data.frame()

nw = 20
grams = '4-grams'

tab <- en_blog_freq %>% subset(type == grams & rank <= nw )
p1 <- 
  ggplot(tab, aes(counts, reorder(words,counts,mean))) +
  geom_segment(aes(x = 0, y = reorder(words,counts,mean), xend = counts, yend = words)) +
  geom_point() +
  theme_minimal()+
  labs(title = 'blog, en_US',
       x = '# of words',
       y = '')
  
tab <- en_news_freq %>% subset(type == grams & rank <= nw )
p2 <- 
  ggplot(tab, aes(counts, reorder(words,counts,mean))) +
  geom_segment(aes(x = 0, y = reorder(words,counts,mean), xend = counts, yend = words)) +
  geom_point() +
  theme_minimal()+
  labs(title = 'news, en_US',
       x = '# of words',
       y = '')

tab <- en_twitter_freq %>% subset(type == grams & rank <= nw )
p3 <- 
  ggplot(tab, aes(counts, reorder(words,counts,mean))) +
  geom_segment(aes(x = 0, y = reorder(words,counts,mean), xend = counts, yend = words)) +
  geom_point() +
  theme_minimal()+
  labs(title = 'twitter, en_US',
       x = '# of words',
       y = '')

tab <- en_sample_freq %>% subset(type == grams & rank <= nw )
p4 <- 
  ggplot(tab, aes(counts, reorder(words,counts,mean))) +
  geom_segment(aes(x = 0, y = reorder(words,counts,mean), xend = counts, yend = words), colour = "orange") +
  geom_point(colour = "orange") +
  theme_minimal()+
  labs(title = 'total sample, en_US',
       x = '# of words',
       y = '')

grid.arrange(p1, p2, p3, p4, nrow = 2,top = paste0('Top 20 ',grams,' based on frequency'))


grams = '3-grams'

freq <- en_sample_freq %>% 
        subset(type ==grams) %>%
        select(words,type,counts,language) %>%
        arrange(-counts) %>%
        group_by(type,language) %>%
        mutate(total = sum(counts), freq = counts / sum(counts), unique_count = 1) %>%
        mutate(cum_unique = cumsum(unique_count),cum_freq = cumsum(freq))

p50 <- nrow(freq %>% subset(cum_freq <= .5))
p75 <- nrow(freq %>% subset(cum_freq <= .75))
p90 <- nrow(freq %>% subset(cum_freq <= .9))
p95 <- nrow(freq %>% subset(cum_freq <= .95))

ggplot(data=freq, aes(y=cum_freq * 100, x=cum_unique, group=1)) +
  geom_line()+
  geom_hline(yintercept = 50, color = 'yellow') +
  geom_hline(yintercept = 75, color = 'orange') +
  geom_hline(yintercept = 90, color = 'red') +
  geom_hline(yintercept = 95, color = 'dark red') +
  geom_vline(xintercept = p50, color = 'yellow') +
  geom_vline(xintercept = p75, color = 'orange') +
  geom_vline(xintercept = p90, color = 'red') +
  geom_vline(xintercept = p95, color = 'dark red') +
  theme_minimal()+
  scale_y_continuous(breaks=seq(0,100,5))+
  scale_x_continuous(breaks=c(p50,p75,p90,p95))+
  labs(title = paste0('Number of Unique ',grams,' needed to cover 50% and 90% of all ',grams,' in the corpus'),
       x = 'k of unique words',
       y = '% of total words')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))