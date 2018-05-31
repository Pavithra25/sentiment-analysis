require(twitteR)
library(ROAuth)
library(plyr)
library(httr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(stringr)
library(syuzhet)
cKey = "kVD68TfuxhZOd25hzNhLEZe2n"
Skey = "nAvUUNt8SMSb3M9NuIkq0GHhlM2I8WPa9xZY1V9T5g0DyA1Vw3"
A_token="1698053497-Q88KMdihT6iA0nHMJQejDvjdCeClRMwgmLcPNHi"
A_tokenS="9i9QKfkI0s1y6xmgryIpIjDwcJUUQyYs0Txnnz8L4mALN"
setup_twitter_oauth(cKey,Skey,A_token,A_tokenS)
ML <- searchTwitter("Indira canteen",n=1000,lang = "en")


txt = sapply(ML,function(x) x$getText())
txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", txt)
# remove at people
txt = gsub("@\\w+", "", txt)
# remove punctuation
txt = gsub("[[:punct:]]", "", txt)
# remove numbers
txt = gsub("[[:digit:]]", "", txt)
# remove html links
txt = gsub("http\\w+", "", txt)
# remove unnecessary spaces
txt = gsub("[ \t]{2,}", "", txt)
txt = gsub("^\\s+|\\s+$", "", txt)
txt = tolower(txt)
word.list = str_split(txt, '\\s+') # splits the tweets by word in a list

words = unlist(word.list) # turns the list into vector
wordcloud(words,random.order = F,colors = rainbow(50))

k = get_sentiment(some_txt)(txt)
h = get_nrc_sentiment(txt)
sentimentscore = data.frame(colSums(h[,]))
sentimentscore = cbind("sentiment"=rownames(sentimentscore),sentimentscore)
#ggplot(data=sentimentscore,aes(sentiment,y))+geom_bar(aes(fill=sentiment))
barplot(
  sort(colSums(h[, 1:10])), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, col=blues9,
  main = "Emotions about India canteen", xlab="Percentage"
)




