library(tidytext)
library(tidyr)
library(dplyr)
library(tm)
library(qdapDictionaries)
library(VennDiagram)
library(wordcloud)

#get data
cran_code <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-12/loc_cran_packages.csv")

#convert package names to tidytext format and clean
cran_names_df <- distinct(data.frame(cran_code$pkg_name)) #only count names once
cran_names_df <- cran_names_df %>%
  mutate(original_name = cran_code.pkg_name) %>%
  mutate(cran_code.pkg_name = tolower(cran_code.pkg_name)) %>% #convert to lowercase
  mutate(cran_code.pkg_name = removeNumbers(cran_code.pkg_name)) %>% #remove digits
  mutate(cran_code.pkg_name = gsub(x = cran_code.pkg_name, pattern = "\\.", replacement=" ")) %>% #split into two words
  filter(nchar(cran_code.pkg_name) > 1) #keep names with len > 1; leaves us with 14682 observations

#attempt to split names into words - another time
#for now, just keep CRAN names that are recognized words (1,495 of them)
cran_names_words <- cran_names_df %>%
  filter(cran_names_df$cran_code.pkg_name %in% GradyAugmented) %>%
  select(cran_code.pkg_name)

#get sentiments databases
nrc <- get_sentiments("nrc")
afinn <- get_sentiments("afinn")
bing <- get_sentiments("bing")

#perform inner join to get associated sentiments 
#and then collapse multiple hits into a list
names(cran_names_words) <- c("word")
cran_names_sentiments <- cran_names_words %>%
  left_join(nrc, by="word") %>%
  #group_by(word) %>% summarise(sentiment = list(sentiment)) %>%
  left_join(afinn, by="word") %>%
  left_join(bing, by="word") %>%
  filter(rowSums(is.na(.)) != 3) #remove words that didnʻt match in any database
  

#now, plot! First, AFINN scores and names
names(cran_names_df)[1] <- "word"
cran_names_afinn <- cran_names_sentiments[,c(1,3)] %>%
  filter(!is.na(score)) %>%
  inner_join(cran_names_df)

#hacky - add position for text
cran_names_afinn <- unique(cran_names_afinn[order(cran_names_afinn$score),])
cran_names_afinn$pos <- c(0.5:2.5, 0.5:15.5, 0.5:4.5, 0.5:12.5, 0.5:20.5, 0.5:3.5, 0.5)

ggplot(data=cran_names_afinn) + geom_bar(aes(x = score), fill="grey15", col="grey15") + 
  geom_text(aes(x=score, y=pos, label=original_name, col=score), check_overlap = FALSE, family = "Arial") + 
  scale_color_gradient(low="magenta", high="cyan") + guides(color=FALSE) +
  xlab("AFINN database score") + ylab("Count") + 
  theme_minimal(base_size = 16)

#cleaning nrc data
cran_names_nrc <- cran_names_sentiments[,1:2] %>%
  filter(!is.na(sentiment.x)) %>% 
  inner_join(cran_names_df) %>%
  select(-word) %>%
  mutate(tmp = 1) %>%
  spread(key=sentiment.x, value = tmp)

cran_names_nrc[is.na(cran_names_nrc)] <- 0

#create word cloud
#http://rstudio-pubs-static.s3.amazonaws.com/283881_efbb666d653a4eb3b0c5e5672e3446c6.html
#thanks!
all = c(
  paste(cran_names_nrc[cran_names_nrc$anger > 0,]$original_name, collapse=" "),
  paste(cran_names_nrc[cran_names_nrc$anticipation > 0,]$original_name, collapse=" "),
  paste(cran_names_nrc[cran_names_nrc$disgust > 0,]$original_name, collapse=" "),
  paste(cran_names_nrc[cran_names_nrc$fear > 0,]$original_name, collapse=" "),
  paste(cran_names_nrc[cran_names_nrc$joy > 0,]$original_name, collapse=" "),
  paste(cran_names_nrc[cran_names_nrc$sadness > 0,]$original_name, collapse=" "),
  paste(cran_names_nrc[cran_names_nrc$surprise > 0,]$original_name, collapse=" "),
  paste(cran_names_nrc[cran_names_nrc$trust > 0,]$original_name, collapse=" ")
)

corpus = Corpus(VectorSource(all))
tdm = TermDocumentMatrix(corpus, control=list(tolower=FALSE))
tdm = as.matrix(tdm)
cols = rainbow(8); cols[2] <- "darkcyan"; cols[8] <- "cyan"
colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy','sadness', 'surprise', 'trust')
comparison.cloud(tdm, title.size = 1, scale=c(1.2, 0.3), colors = cols)
