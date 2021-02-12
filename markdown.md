## Twitteranalyysi

Analysoidaan INVEST-projektin Twitter-dataa kahdella eri tavalla. Ensin
analysoidaan INVESTin virallisen twitter-profiilin seinä (eli
timelineä). Sitten analysoidaan INVESTin virallista häshtägiä eli
\#eriarvo.

Ladataan ensin tarvittavat paketit

``` r
library("twitteR")
library("ROAuth")
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("tidyverse")
library("ggplot2")
```

## Autentikaatio

    ## [1] "Using direct authentication"

## Kerätään INVESTin twitter-profiilin twiitit.

Tämä komento noutaa maksimissaan 3200 twiittiä, viimeisen viikon ajalta.

``` r
tweets <- userTimeline("INVEST_flagship", n = 3200)
```

Saimme 52 twiittiä. Tallennetaan kerätyt twiitit dataframeksi.

``` r
tweets.df <- twListToDF(tweets)
```

## Tarkastellaan twiittejä

``` r
head(tweets.df$text)
```

    ## [1] "@niko_eskelinen @SaariJuho @LiisaBjorklund Onnittelut kaikille kirjoittajille jo tässä kohtaa! Täällä peukalot ovat… https://t.co/N2aNtT3m1K"
    ## [2] "Elämäntapahtumilla ja vanhempien resursseilla on tärkeä rooli lapsen aikuisuuden tulemissa. Yhteys näkyy… https://t.co/1Ch7oNyrQ0"           
    ## [3] "Word. <U+0001F449><U+0001F3FB> ”Tiedon määrä ei ole ongelma, siitä viestiminen on. Asiantuntijoiden on opittava saamaan viestinsä perille… https://t.co/vD2tlULkpD"
    ## [4] "Opintokamu-ohjelma tarjoaa työkaluja opiskelijoiden hyvinvoinnin edistämiseksi toisella asteella. Kaikki materiaali… https://t.co/i1IGDWKWDV"
    ## [5] "Keskustelu #opiskelu'sta ja #korona'n vaikutuksista toisen asteen opiskelijoihin käy vilkkaana. Tutkimme keväällä… https://t.co/e6gh0QPMah"  
    ## [6] "Haluatko mukaan toteuttamaan uudenlaista, korkeatasoista ja yhteiskunnallisesti vaikuttavaa tutkimuskokonaisuutta j… https://t.co/FE6yPxowjk"

d %\>% arrange(desc(freq)) %\>% slice(1:20) %\>%
ggplot(aes(x=reorder(word, freq), y=freq)) + geom_bar(stat=“identity”) +
xlab(“Words”) + ylab(“Count”) + coord_flip() +
theme(axis.text=element_text(size=7))

## Siivootaan tekstit

Luodaan tekstistä ensin corpus

``` r
myCorpus <- Corpus(VectorSource(tweets.df$text))
```

Tehdään funktio, jonka avulla poistetaan erilaisia symboleja tekstistä
(muuttaa ne välilyönniksi)

``` r
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
# poistetaan kaikki tinyurl-osoitteet 
myCorpus <- tm_map(myCorpus, toSpace, " ?(f|ht)tp(s?)://(.*)")
# poistetaan kaikki profiili-tagit
myCorpus <- tm_map(myCorpus, toSpace, "@[A-Za-z]+")
# poistetaan kenoviivat
myCorpus <- tm_map(myCorpus, toSpace, "/")
myCorpus <- tm_map(myCorpus, toSpace, "\\|")
```

Jatketaan tekstimassan siistimistä

``` r
# Kaikki kirjaimet pieniksi
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# Poistetaan numerot
myCorpus <- tm_map(myCorpus, removeNumbers)
# Poistetaan pisteet
myCorpus <- tm_map(myCorpus, removePunctuation)
# Poistetaan ylimääräiset välilyönnit
myCorpus <- tm_map(myCorpus, stripWhitespace)
# poistetaan itse määriteltyjä sanoja
myCorpus <- tm_map(myCorpus, removeWords, c("the", "and", "you", "very", "are", "for"))
```

Tehdään tekstimassasta sanamatriisi, järjestetään yleisimmästä sanasta
harvimpaan. Muutetaan data.frameksi

``` r
dtm <- TermDocumentMatrix(myCorpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
```

## Sanapilvi

``` r
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=150, random.order=FALSE, rot.per=0.15, 
          colors=brewer.pal(8, "Dark2"))
```

![](markdown_files/figure-markdown_github/unnamed-chunk-10-1.png) \#\#
barplot yleisimmistä twiiteistä

``` r
d %>% arrange(desc(freq)) %>% slice(1:20) %>% 
  ggplot(aes(x=reorder(word, freq), y=freq)) + geom_bar(stat="identity") + xlab("Words") + ylab("Count") +
         coord_flip() + theme(axis.text=element_text(size=7))
```

![](markdown_files/figure-markdown_github/unnamed-chunk-11-1.png)

## Kerätään hashtag-twiitit

``` r
tweets <- searchTwitter("#eriarvoisuus",n=1000,lang="fi")
```

    ## Warning in doRppAPICall("search/tweets", n, params = params, retryOnRateLimit =
    ## retryOnRateLimit, : 1000 tweets were requested but the API can only return 67

Pyysimme 1000 twiittiä, saimme vain 88.

Tallennetaan data.frameksi

``` r
tweets.df <- twListToDF(tweets)
```

Siisistään teksti kuten yllä

``` r
myCorpus <- Corpus(VectorSource(tweets.df$text))
```

Tehdään funktio, jonka avulla poistetaan erilaisia symboleja tekstistä
(muuttaa ne välilyönniksi)

``` r
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
# poistetaan kaikki tinyurl-osoitteet 
myCorpus <- tm_map(myCorpus, toSpace, " ?(f|ht)tp(s?)://(.*)")
# poistetaan kaikki profiili-tagit
myCorpus <- tm_map(myCorpus, toSpace, "@[A-Za-z]+")
# poistetaan kenoviivat
myCorpus <- tm_map(myCorpus, toSpace, "/")
myCorpus <- tm_map(myCorpus, toSpace, "\\|")
# Kaikki kirjaimet pieniksi
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# Poistetaan numerot
myCorpus <- tm_map(myCorpus, removeNumbers)
# Poistetaan pisteet
myCorpus <- tm_map(myCorpus, removePunctuation)
# Poistetaan ylimääräiset välilyönnit
myCorpus <- tm_map(myCorpus, stripWhitespace)
# poistetaan itse määriteltyjä sanoja
myCorpus <- tm_map(myCorpus, removeWords, c("the", "and", "you", "very", "are", "for"))
dtm <- TermDocumentMatrix(myCorpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
```

## Sanapilvi

``` r
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=150, random.order=FALSE, rot.per=0.15, 
          colors=brewer.pal(8, "Dark2"))
```

![](markdown_files/figure-markdown_github/unnamed-chunk-16-1.png)

## barplot yleisimmistä twiiteistä

``` r
d %>% arrange(desc(freq)) %>% slice(1:20) %>% 
  ggplot(aes(x=reorder(word, freq), y=freq)) + geom_bar(stat="identity") + xlab("Words") + ylab("Count") +
         coord_flip() + theme(axis.text=element_text(size=7))
```

![](markdown_files/figure-markdown_github/unnamed-chunk-17-1.png)
