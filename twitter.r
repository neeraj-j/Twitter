#install.packages(c("devtools", "rjson", "bit64", "httr","curl"))

library(devtools)

#install_github("geoffjentry/twitteR").

library(twitteR)

# required pakacges
library(sentiment)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(scales)
library(syuzhet)

api_key<- "<api Key>"

api_secret<- "<Secret>"

access_token<- "<Token>"

access_token_secret<- "<Token Secret>"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

setwd("/home/neeraj/Data Analysis/projects/twitter")

catch.error = function(x)
  
{
  
  # let us create a missing value for test purpose
  
  y = NA
  
  # Try to catch that error (NA) we just created
  
  catch_error = tryCatch(tolower(x), error=function(e) e)
  
  # if not an error
  
  if (!inherits(catch_error, "error"))
    
    y = tolower(x)
  
  # check result if error exists, otherwise the function works fine.
  
  return(y)
  
}



cleanTweets<- function(tweet){
  
  # Clean the tweet for sentiment analysis
  
  # remove html links, which are not required for sentiment analysis
  
  tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
  
  # First we will remove retweet entities from  the stored tweets (text)
  
  tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  
  # Then remove all "#Hashtag"
  
  tweet = gsub("#\\w+", " ", tweet)
  
  # Then remove all "@people"
  
  tweet = gsub("@\\w+", " ", tweet)
  
  # Then remove all the punctuation
  
  tweet = gsub("[[:punct:]]", " ", tweet)
  
  tweet <- gsub(","," ",tweet)
  # Then remove numbers, we need only text for analytics
  
  tweet = gsub("[[:digit:]]", " ", tweet)
  
  # finally, we remove unnecessary spaces (white spaces, tabs etc)
  
  tweet = gsub("[ \t]{2,}", " ", tweet)
  
  tweet = gsub("^\\s+|\\s+$", "", tweet)
  
 
  
  #tweet <- iconv(tweet, to = "utf-8", sub="")
  tweet <- iconv(tweet, "latin1", "ASCII", sub="")

    # if anything else, you feel, should be removed, you can.  For example "slang words" etc using the above function and methods.
  
  # Next we'll convert all the word in lower case.  This makes uniform pattern.
  
  tweet = catch.error(tweet)
  
  tweet
  
}



cleanTweetsAndRemoveNAs<- function(Tweets) {
  
  TweetsCleaned = sapply(Tweets, cleanTweets)
  
  # Remove the "NA" tweets from this tweet list
  
  TweetsCleaned = TweetsCleaned[!is.na(TweetsCleaned)]
  
  names(TweetsCleaned) = NULL
  
  # Remove the repetitive tweets from this tweet list
  
  TweetsCleaned = unique(TweetsCleaned)
  
  TweetsCleaned
  
}


ratio <- data.frame(time1= as.POSIXct(Sys.time()), Trumpr = numeric(10), Clintonr = numeric(10), stringsAsFactors = FALSE)
i=1
while (i<11) {
  print("getting Trump...")
  Trump = searchTwitter("Trump", n=20, lang="en")
  print("getting Clinton...")
  Clinton = searchTwitter("Clinton", n=20, lang="en")
  print("got it!!")
  
  TrumpText <- sapply(Trump, function(x) x$getText())
  
  ClintonText = sapply(Clinton, function(x) x$getText())
  
  #save(TrumpText,file="trump.tweet",ascii = TRUE)
  
  
  TrumpCleaned = cleanTweetsAndRemoveNAs(TrumpText)
  
  ClintonCleaned = cleanTweetsAndRemoveNAs(ClintonText)
  
  #save(TrumpCleaned,file="trumpClean.tweet",ascii = TRUE)
  #save(ClintonCleaned,file="clinton.tweet",ascii = TRUE)
  # classify emotion
  TrumpEmo = classify_emotion(TrumpCleaned, algorithm="bayes", prior=1.0)
  ClintonEmo = classify_emotion(ClintonCleaned, algorithm="bayes", prior=1.0)
  
  xxx = get_nrc_sentiment(TrumpCleaned)
  # get emotion best fit
  Temotion = TrumpEmo[,7]
  Cemotion = ClintonEmo[,7]
  # substitute NA's by "unknown"bkup
  
  Temotion[is.na(Temotion)] = "unknown"
  Cemotion[is.na(Cemotion)] = "unknown"
  
  # classify polarity
  T_pol = classify_polarity(TrumpCleaned, algorithm="bayes")
  C_pol = classify_polarity(ClintonCleaned, algorithm="bayes")
  # get polarity best fit
  Tpolarity = T_pol[,4]
  Cpolarity = C_pol[,4]
  
  # create data frame
  Trump_DF = data.frame(text=TrumpCleaned, 
                                      emotion=Temotion, polarity=Tpolarity, stringsAsFactors=FALSE)
  
  Clinton_DF = data.frame(text=ClintonCleaned,
                        emotion=Cemotion, polarity=Cpolarity, stringsAsFactors=FALSE)
  
  tpc = count(Trump_DF,'polarity')
  neg = apply(tpc, 1, function(row) any(row =="negative" ))
  pos = apply(tpc, 1, function(row) any(row =="positive" ))
  tpr = tpc[pos,]$freq/tpc[neg,]$freq
  
  cpc = count(Clinton_DF,'polarity')
  neg = apply(cpc, 1, function(row) any(row =="negative" ))
  pos = apply(cpc, 1, function(row) any(row =="positive" ))
  cpr = cpc[pos,]$freq/cpc[neg,]$freq
  
  ratio$time1[i]=Sys.time()
  ratio$Trumpr[i] <- tpr
  ratio$Clintonr[i] <- cpr
  saveRDS(ratio,file="ratio.tweet")
  bkup = readRDS("ratio.tweet")
  i=i+1
  Sys.sleep(5)
}

# plot line graph

bkup[bkup==0] <-NA
p = ggplot(bkup, aes(x=time1)) + 
  geom_line(aes(y = Clintonr, colour = "Clinton")) +
  geom_line(aes(y = Trumpr, colour = "Trump")) + 
  xlab("Date/TIme") + ylab("Ratio") + # Set axis labels
  ggtitle("Tweet polarity graph") +
  scale_x_datetime(labels = date_format("%d/%m %H:%M"))
  

print (p)
# plot distribution of emotions
p<-ggplot(data=Trump_DF, aes(x=emotion))  +  geom_bar(aes(y=..count.., fill=emotion)) +  scale_fill_brewer(palette="Dark2") +ggtitle("Trump Emotions") +  theme(legend.position='right') + ylab('Number of Tweets') +  xlab('Trump Emotions')

print(p)

p<- ggplot(data=Clinton_DF, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("Clinton Emotions") +
  theme(legend.position='right') + ylab('Number of Tweets') +
  xlab("Clinton Emotions")

print(p)
# plot distribution of polarity
p<-ggplot(data=Trump_DF, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  ggtitle("Trump polarity") +
  theme(legend.position='right') + ylab('Number of Tweets') +
  xlab('Trump Polarity')

print(p)
# plot distribution of polarity
p<-ggplot(data=Clinton_DF, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  ggtitle("Clinton Polarity") +
  theme(legend.position='right') + ylab('Number of Tweets') +
  xlab('Clinton Polarity')

print(p)



removeCustomeWords <- function (TweetsCleaned) {
  for(i in 1:length(TweetsCleaned)){
    TweetsCleaned[i] <- tryCatch({
      TweetsCleaned[i] = removeWords(TweetsCleaned[i],
                                     c(stopwords("english"), "care", "guys", "can", "dis", "didn",
                                       "guy" ,"booked", "plz"))
      TweetsCleaned[i]
    }, error=function(cond) {
      TweetsCleaned[i]
    }, warning=function(cond) {
      TweetsCleaned[i]
    })
  }
  return(TweetsCleaned)
}
getWordCloud <- function
(sentiment_dataframe, TweetsCleaned, Emotion) {
  emos = levels(factor(sentiment_dataframe$emotion))
  n_emos = length(emos)
  emo.docs = rep("", n_emos)
  TweetsCleaned = removeCustomeWords(TweetsCleaned)
  for (i in 1:n_emos){
    emo.docs[i] = paste(TweetsCleaned[Emotion ==
                                        emos[i]], collapse=" ")
  }
  corpus = Corpus(VectorSource(emo.docs))
  tdm = TermDocumentMatrix(corpus)
  tdm = as.matrix(tdm)
  colnames(tdm) = emos
  require(wordcloud)
  suppressWarnings(comparison.cloud(tdm, colors =
                                      brewer.pal(n_emos, "Dark2"), scale = c(3,.5), random.order =
                                      FALSE, title.size = 1.5))
}
getWordCloud(Trump_DF, TrumpCleaned, Temotion)
getWordCloud(Clinton_DF, ClintonCleaned, Cemotion)
