install.packages(c("twitteR", "ggplot2", "plyr", "stringr", "ROAuth", "httr"))
library(twitteR)
library(ROAuth)
library(plyr)
library(ggplot2)
library(stringr)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL = "http://api.twitter.com/oauth/access_token"
authURL = "http://api.twitter.com/oauth/authorize"
consumerKey = "YTKH1bq3OOiTmF2wAYOrLlSsv"
consumerSecret = "ATjFT4L2lM22PG08CrGeyaME3DsBn0M1MSxr6nf6yH3pIlRjC1"
Cred <- OAuthFactory$new(consumerKey=consumerKey,consumerSecret=consumerSecret,requestURL=requestURL,accessURL=accessURL,authURL=authURL)
Cred$handshake(cainfo = system.file("CurlSSL","cacert.pem",package = "RCurl"))
7780609
save(Cred, file="twitter authentication.Rdata")
load("twitter authentication.Rdata")
setup_twitter_oauth("YTKH1bq3OOiTmF2wAYOrLlSsv", "ATjFT4L2lM22PG08CrGeyaME3DsBn0M1MSxr6nf6yH3pIlRjC1", "763291279123898368-Z6CvXSpuqM8fVWkeF8fgGABEcspXmwv", "yHbCCExkW4351oxAKxa3HrJ6BKIfvvdypgcUq83QNrP0m")
Brexit.list <- searchTwitteR('#Brexit', n=100, lang = "en")
Brexit.list
Brexit.df = twListToDF(Brexit.list)
write.csv(Brexit.df, file = "~/Brexit1.csv", row.names = F)
library(plyr)
library(stringr)
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores = laply(sentences, function(sentence, pos.words, neg.words){
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    sentence = tolower(sentence)
    word.list = str_split(sentence, '\\s+')
    words= unlist(word.list)
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
} 
hu.liu.pos = scan('C:\\Users\\SONY\\Desktop\\abc\\positive-words.txt', what='character', comment.char=';')
hu.liu.neg = scan('C:\\Users\\SONY\\Desktop\\abc\\negative-words.txt', what='character', comment.char=';')
pos.words = c(hu.liu.pos, 'upgrade')
neg.words = c(hu.liu.neg, 'wtf', 'wait', 'waiting', 'epicfail', 'mechanical')
DatasetBrexit <-read.csv("C:\\Users\\SONY\\Documents\\Brexit1.csv")
DatasetBrexit$text<-as.factor(DatasetBrexit$text)
Brexit.scores = score.sentiment(DatasetBrexit$text, pos.words, neg.words, .progress = 'text')
path<-"C:\\Users\\SONY\\Documents"
write.csv(Brexit.scores,file=paste(path,"BrexitScores.csv",sep=""),row.names = TRUE)
View(Brexit.scores)
qplot(Brexit.scores$score, xlab="Scoreof Tweets")
