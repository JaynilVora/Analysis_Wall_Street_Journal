library("twitteR")
library("syuzhet")
library("SentimentAnalysis")
library("plyr")
library("ggplot2")
library("wordcloud")
library("RColorBrewer")
require('ROAuth')
require('RCurl')
key <- "vZSxLplTfbaXI8gVyThJ4JqvR"
secret <- "c4LKqTMoia49wAup3lEAEGr3z0OfaMxgyZniPioOWjSTwZPTpu"
secrettk <- "3FYoesz1EOeB4CbwUHy5cHv38zH6B0RTsE1MpyWxlBFxQ"
mytoken <- "915707499172556801-TAAl4VV55IELsToq3M2rMD6yt0MZm8g"
setup_twitter_oauth(key, secret, mytoken, secrettk)
library("httr")
library("tm")
library(stringi)


WSJOP = userTimeline("WSJ", n=2500)
head(WSJOP)

wsjtext=sapply(WSJOP, function(x) x$getText())
wsjtext=unlist(wsjtext)
wsjtext <- stri_encode(wsjtext, "", "UTF-8")
wsjtext = tolower(wsjtext)

mySentiment234 <- get_nrc_sentiment(wsjtext)
mySentiment234
write.csv(mySentiment234, "WSJEmotions.csv")


udemylist <- sapply(WSJOP, function(x) x$getText())
udemycorpus <- Corpus(VectorSource(udemylist))
udemycorpus <- tm_map(udemycorpus, removeWords, stopwords("english"))
udemycorpus <- tm_map(udemycorpus,function(x)removeWords(x,stopwords()))
udemycorpus <- tm_map(udemycorpus, removeWords, "the")
udemycorpus <- tm_map(udemycorpus, removeWords, "says")
udemycorpus <- tm_map(udemycorpus, removeWords, "new")
udemycorpus <- tm_map(udemycorpus, removeWords, "will")
udemycorpus <- tm_map(udemycorpus, removeWords, "now")
udemycorpus <- tm_map(udemycorpus, removeWords, stopwords("english"))
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern," ",x))})
udemycorpus<- tm_map(udemycorpus,toSpace,"[^[:graph:]]")
udemycorpus <- tm_map(udemycorpus, content_transformer(tolower))
udemycorpus <- tm_map(udemycorpus,removePunctuation)

udemycorpus <- tm_map(udemycorpus, PlainTextDocument)
udemycorpus
udemycorpus = tm_map(udemycorpus, content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')))
# UTF-8-MAC b/c I'm on macOS

library("wordcloud")

wordcloud(udemycorpus, min.freq = 2, scale = c(5,1), random.color = F, max.word = 50, colors=brewer.pal(8, "Dark2"), random.order = F)

udemytdm <- TermDocumentMatrix(udemycorpus)
findFreqTerms(udemytdm, lowfreq = 70)
findAssocs(udemytdm, 'trump', 0.15)
findAssocs(udemytdm, 'facebook', 0.21)
findAssocs(udemytdm, 'china', 0.19)
findAssocs(udemytdm, 'president', 0.18)
findAssocs(udemytdm, 'trade', 0.19)




