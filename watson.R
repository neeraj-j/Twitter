library("cognizer")

url <- "https://gateway-a.watsonplatform.net/calls"
#"note": "It may take up to 5 minutes for this key to become active",
apikey <- "your key"

#Sentiment Analysis
text <- c("Columbus, Ohio is Awesome!", "Looking forward to UseR2017 in Brussels!")
result <- text_sentiment(text, apikey)
str(result)

#Keyword Extraction -top-

text <- c("Columbus, Ohio is Awesome!", "Looking forward to UseR2017 in Brussels!")
result <- text_keywords(text, apikey)
str(result)

#Emotion Analysis -top-

text <- c("Columbus, Ohio is Awesome!", "Looking forward to UseR2017 in Brussels!")
result <- text_emotion(text, apikey)
str(result)
#Language Detection -top-

text <- c("Columbus, Ohio is Awesome!", "Mirando hacia adelante a UseR2017 en Bruselas!")
result <- text_language(text, apikey)
str(result)

#Entity Extraction -top-

text <- c("Columbus, Ohio is Awesome!", "Looking forward to UseR2017 in Brussels!")
result <- text_entity(text, apikey)
str(result)

#Concept Tagging -top-

text <- "Columbus, Ohio is Awesome!"
result <- text_concept(text, apikey)
str(result)

#Relation Extraction -top-
  
text <- "Columbus, Ohio is Awesome!"
result <- text_relations(text, apikey)
str(result)

#Taxonomy Classification -top-
  
text <- "Columbus, Ohio is Awesome!"
result <- text_taxonomy(text, apikey)
str(result)

# language translate
url <- "https://gateway.watsonplatform.net/language-translator/api"
password <- "Bg5aAUf8Sgrb"
username <- "b59c5a76-8a4d-4dd4-b358-323c716fa7bd"

lang_translate_key <- paste(username,password,sep=":")
text <- c("Mirando hacia adelante a UseR2017 en Bruselas!")
result <- text_translate(text, lang_translate_key)
str(result)

#personality insight
url <- "https://gateway.watsonplatform.net/personality-insights/api"
password <- "Xf1CRhb5fNbF"
username <- "bcc72bf2-c387-41a4-a70e-b9b4307f8c8b"

personality_key <- paste(username,password,sep=":")
text <- paste(replicate(1000, rmsfact::rmsfact()), collapse = ' ') #Ten Richard Stallman Facts used for Personality Insights.
result <- text_personality(text, personality_key)
str(result)

# Tone analyzer
url <- "https://gateway.watsonplatform.net/tone-analyzer/api"
password <- "LCaqnwOjP5sc"
username <- "c834ea43-4abd-478f-97cc-32a0bdd80e0b"

tone_key <- paste(username,password,sep=":")
text <- c("Columbus, Ohio is Awesome!")
result <- text_tone(text, tone_key)
str(result)

