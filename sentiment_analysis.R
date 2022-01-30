#This is an analysis of the KFC brand in Kenya after the recent online furore over their supply decisions

#Lets import all the libraries we need
library(rtweet)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidytext)
library(twitteR)
library(DBI)
library(wordcloud)
library(tm)
#the authorization tokens
app_name='duncan_sentiment'
consumer_key='2tAEYBmfcrz3dz6EUESkrR357'
consumer_secret='AQvuUFkuJlumr7NOAGqCrT6QuulurAP7XACwLREoDjjVa43Gdw'
access_token='2858930885-7xVKspZRoObsPYYs6FC9RWp05N30p1kIyePlG6z'
access_secret='Pdj32tGqlbsLcUg7Zk8XexQGJeSH5csuBTihrwsLFAJ3X'


auth_token=rtweet::create_token(
  app=app_name,
  consumer_key = api_key,
  consumer_secret = api_secret,
  access_token = access_token,
  access_secret = access_secret
)

#setup_twitter_oauth(consumer_key = consumer_key,consumer_secret = consumer_secret,access_token = access_token,access_secret = access_secret)
register_db_backend(con)

#Lets get the information we need
kfcData_timeline2=search_tweets('@KFCinKenya',n=10000,fromDate = '2015-01-01')

tweets2=search_tweets('#BoycottKFC',n=5000)

#We have data from the KFC Kenya page and we want to see how the brand has been affected.
#Lets start some analysis!!
#Lets view the dataset
tweets2%>%sample_n(5)%>%select(created_at,screen_name,text,favorite_count,retweet_count)
head(tweets2$text)
#removing all characters,urls,hashtags and other twitter handles
tweet_data=tweets2
by='day'
tweet_data$stripped_text1=gsub("http\\S+","",tweet_data$text)
#convert to lowercase, remove punctuation and add id for each tweet
tweet_data.text_stem=tweet_data%>%select(stripped_text1)%>%unnest_tokens(word,stripped_text1)
#remove stop words from the list of words
cleaned_tweets.KFCtweets=tweet_data.text_stem%>%anti_join(stop_words)
#what are the top 20 words
cleaned_tweets.KFCtweets%>%count(word,sort = TRUE)%>%top_n(20)%>%mutate(word=reorder(word,n))%>%
  ggplot(aes(x=word,y=n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_classic() +
  labs(x= "Count",y="Unique Words",title = "Unique word counts found in #BoycottKFC tweets")



#Sentiment analysis using the Bing lexicon
bing_KFC_tweets=cleaned_tweets.KFCtweets%>%inner_join(get_sentiments("bing"))%>%
  count(word,sentiment,sort = TRUE)%>%ungroup()

kfcTweets.text.corpus=tm_map(tweet_data.text_stem,function(x)removeWords(x,stop_words()))

#load the data as a corpus
text_corpus=Corpus(VectorSource(tweet_KFC_text))
tweet_KFC_corpus=tm_map(text_corpus,function(x)removeWords(x,stopwords()))

#generate a wordcloud
wordcloud(tweet_KFC_corpus,min.freq = 10,colors = brewer.pal(8,"Dark2"),random.color = TRUE,max.words = 500)


#Building a term document matrix to find most frequent words
textDoc_dtm=TermDocumentMatrix(tweet_KFC_corpus)
dtm_m=as.matrix(textDoc_dtm)
#Sort by descearing value of frequency
dtm_v=sort(rowSums(dtm_m),decreasing = TRUE)
dtm_d=data.frame(word=names(dtm_v),freq=dtm_v)
head(dtm_d,5)

#Plotting the most frequent words
barplot(dtm_d[1:10,]$freq,las=2,names.arg = dtm_d[1:10,]$word,col = "lightgreen",main = "Top 5 most frequent words",
        ylab = "Word Frequencies")



#visualize the words
wordcloud(cleaned_tweets.KFCtweets,min.freq = 10,colors = brewer.pal(8,"Dark2"),random.color = TRUE,max.words = 500)

#To visually depict word counts, lets filter and plot words side by side to compare the negative vs positive emotion
bing_KFC_tweets%>%group_by(sentiment)%>%top_n(10)%>%
  ungroup()%>%mutate(word=reorder(word,n))%>%ggplot(aes(word,n,fill=sentiment)) +
  geom_col(show.legend = FALSE) + facet_wrap(~sentiment,scales = "free_y") + 
  labs(title = "Tweets in the BoycottKFC hashtag",y="Contribution to sentiment",x=NULL) + 
  coord_flip() + theme_bw()



#Lets get the sentiment score for each tweet
sentiment_bing=function(twt){
  #perform basic cleaning on the tweet
  twt_tbl=tibble(text=twt)%>%mutate(
    #remove the http elements manually
    stripped_text=gsub("http\\S+","",text)
  ) %>%unnest_tokens(word,stripped_text)%>%anti_join(stop_words)%>% #remove stop words
    inner_join(get_sentiments("bing")) %>% #merge with bing sentiment
    count(word,sentiment,sort = TRUE)%>%ungroup()%>%
    #create a column score that that assigns -1 to all negative words and 1 to positive words
    mutate(
      score=case_when(
        sentiment=='negative'~n*(-1),
        sentiment=='positive'~n*1)
      )
      #Cal
      sent.score=case_when(
        nrow(twt_tbl)==0~0, #if there are no words,score is 0
        nrow(twt_tbl)>0~sum(twt_tbl$score) #otherwise, sum the positives and negatives
      )
      #this is to keep track of which tweets contained no words from the bing list
      zero.type=case_when(
        nrow(twt_tbl)==0~"Type 1", #Type 1: no words at all,zero=no"
        nrow(twt_tbl)>0~"Type 2" #Type 2: zero means sum of words=0
      )
      list(score=sent.score,type=zero.type,twt_tbl=twt_tbl)
}

#Apply the function
#the lapply function returns a list of all the sentiment scores,types and tables of the tweets
kfcTweets_sentiment=lapply(tweet_data$text, function(x){sentiment_bing(x)})
kfcTweets_sentiment_final=tibble(
  score=unlist(kfcTweets_sentiment,'score'),
  type=unlist(kfcTweets_sentiment,'type')
)
kfcTweets_sentiment_final

#Let us visualize this 
ggplot(kfcTweets_sentiment_final,aes(x=score))+geom_histogram(bins = 5,stat = "count")+theme_bw()
