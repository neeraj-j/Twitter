library(devtools)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(scales)


setwd("/home/neeraj/Data Analysis/projects/twitter")

bkup = readRDS("ratio1.tweet")

# plot line graph

bkup[bkup==0] <-NA
p = ggplot(bkup, aes(x=time)) + 
  geom_line(aes(y = Clintonr, colour = "Clinton")) +
  geom_line(aes(y = Trumpr, colour = "Trump")) + 
  xlab("Date/TIme") + ylab("Ratio") + # Set axis labels
  ggtitle("Tweet polarity graph") +
  scale_x_datetime(labels = date_format("%d/%m %H:%M"))

ggsave("elections.png")
