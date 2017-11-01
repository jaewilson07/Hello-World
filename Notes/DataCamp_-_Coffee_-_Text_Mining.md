Coffee - Text Analsyis
================
Jae Wilson
2017-11-01

``` r
#install.packages("qdap", dependencies = TRUE)
#install.packages("tm", dependencies = TRUE)
#install.packages("wordcloud", dependencies = TRUE)
library(knitr)
library(magrittr)

library(qdap) #Quantitative Discourse Analysis Packag
```

    ## Loading required package: qdapDictionaries

    ## Loading required package: qdapRegex

    ## Loading required package: qdapTools

    ## Loading required package: RColorBrewer

    ## 
    ## Attaching package: 'qdap'

    ## The following object is masked from 'package:magrittr':
    ## 
    ##     %>%

    ## The following object is masked from 'package:base':
    ## 
    ##     Filter

``` r
library(tm) #text mining
```

    ## Loading required package: NLP

    ## 
    ## Attaching package: 'NLP'

    ## The following object is masked from 'package:qdap':
    ## 
    ##     ngrams

    ## 
    ## Attaching package: 'tm'

    ## The following objects are masked from 'package:qdap':
    ## 
    ##     as.DocumentTermMatrix, as.TermDocumentMatrix

``` r
library("wordcloud")
```

``` r
tweets<- read.csv("https://raw.githubusercontent.com/jaewilson07/Hello-World/master/Datasets/DataCamp/CoffeeTweets.txt", stringsAsFactors = FALSE)
coffee_tweets <- tweets$text

#plot bag of words
coffee_freq <- freq_terms(coffee_tweets, 10)
plot(coffee_freq)
```

![](DataCamp_-_Coffee_-_Text_Mining_files/figure-markdown_github-ascii_identifiers/simpleCorpus-1.png)

``` r
# Make a vector source (list): coffee_source
coffee_source <- VectorSource(coffee_tweets)
#NOTE:  DataframeSource() will convert entire row into ONE DOCUMENT
#NOTE : VectorSource( df[,2:4]) will convert entire df$column into one document (combine all observations)


# Make a volatile corpus: coffee_corpus
coffee_corpus <- VCorpus(coffee_source)

# Print out coffee_corpus
print(coffee_corpus)
```

    ## <<VCorpus>>
    ## Metadata:  corpus specific: 0, document level (indexed): 0
    ## Content:  documents: 1000

``` r
#Metadata:  corpus specific: 0, document level (indexed): 0
#Content:  documents: 1000

# Print data on the 15th tweet in coffee_corpus
print(coffee_corpus[[15]])
```

    ## <<PlainTextDocument>>
    ## Metadata:  7
    ## Content:  chars: 111

``` r
#Metadata:  7
#Content:  chars: 111


# Print the content of the 15th tweet in coffee_corpus
print(coffee_corpus[[15]][1])
```

    ## $content
    ## [1] "@HeatherWhaley I was about 2 joke it takes 2 hands to hold hot coffee...then I read headline! #Don'tDrinkNShoot"

``` r
#"@HeatherWhaley I was about 2 joke it takes 2 hands to hold hot coffee...then I read headline! #Don'tDrinkNShoot"
```

``` r
#create a function to apply transformations
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(replace_abbreviation))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "coffee"))
  corpus <- tm_map(corpus, content_transformer(tolower))
  return(corpus)
}
coffee_corpus_clean <- clean_corpus(coffee_corpus)

(coffee_dtm <- DocumentTermMatrix(coffee_corpus_clean))
```

    ## <<DocumentTermMatrix (documents: 1000, terms: 3098)>>
    ## Non-/sparse entries: 7772/3090228
    ## Sparsity           : 100%
    ## Maximal term length: 27
    ## Weighting          : term frequency (tf)

``` r
#DocumentTermMatrix (documents: 1000, terms: 3098)>>
#Non-/sparse entries: 7772/3090228
#Sparsity           : 100%
#Maximal term length: 27
#Weighting          : term frequency (tf)

coffee_m <- as.matrix(coffee_dtm)
dim(coffee_m) #there are 1000 documents and 3098 unique words in coffee_m
```

    ## [1] 1000 3098

``` r
(coffee_tdm <- TermDocumentMatrix(coffee_corpus_clean))
```

    ## <<TermDocumentMatrix (terms: 3098, documents: 1000)>>
    ## Non-/sparse entries: 7772/3090228
    ## Sparsity           : 100%
    ## Maximal term length: 27
    ## Weighting          : term frequency (tf)

``` r
coffee_m <- as.matrix(coffee_tdm)
dim(coffee_m) #there are 1000 documents and 3098 unique words in coffee_m
```

    ## [1] 3098 1000

``` r
term_frequency <- rowSums(coffee_m) %>%
    sort(decreasing = TRUE)
barplot(term_frequency[1:10], col = "tan", las = 2)
```

![](DataCamp_-_Coffee_-_Text_Mining_files/figure-markdown_github-ascii_identifiers/corpusClean-1.png)

``` r
qdapFreq <- freq_terms(
    coffee_tweets,
    top = 10,
    at.least = 3,
    stopwords = "Top200Words")
plot(qdapFreq)
```

![](DataCamp_-_Coffee_-_Text_Mining_files/figure-markdown_github-ascii_identifiers/corpusClean-2.png)

``` r
wordcloud(coffee_corpus_clean, max.words = 100, random.order = TRUE)
```

![](DataCamp_-_Coffee_-_Text_Mining_files/figure-markdown_github-ascii_identifiers/corpusClean-3.png)

``` r
text_data <- "In a complicated haste, Tom rushed to fix a new complication, too complicatedly"

# Remove punctuation: rm_punc
rm_punc <- removePunctuation(text_data)

# Create character vector: n_char_vec
n_char_vec <- unlist(strsplit(rm_punc, split = ' '))
#stem document will consider each element of a vector as one word.
#use strsplit() to break a sentence into vectors of words

str(n_char_vec)
```

    ##  chr [1:13] "In" "a" "complicated" "haste" "Tom" "rushed" "to" "fix" ...

``` r
#chr [1:13] "In" "a" "complicated" "haste" "Tom" "rushed" "to" "fix" "a" ...

# Perform word stemming: stem_doc
stem_doc <- stemDocument(n_char_vec)
# [1] "In"      "a"       "complic" "hast"    "Tom"     "rush"    "to"     
# [8] "fix"     "a"       "new"     "complic" "too"     "complic"

# Print stem_doc
print(stem_doc)
```

    ##  [1] "In"      "a"       "complic" "hast"    "Tom"     "rush"    "to"     
    ##  [8] "fix"     "a"       "new"     "complic" "too"     "complic"

``` r
comp_dict <- c("In", "a", "complicate", "haste", "Tom", "rush", "to", "fix", "a", "new", "too")

# Re-complete stemmed document: complete_doc
#the stemCompletion is a complete list of all the 'valid' words
complete_doc <-stemCompletion(stem_doc, comp_dict) 

# Print complete_doc
complete_doc
```

    ##           In            a      complic         hast          Tom 
    ##         "In"          "a" "complicate"      "haste"        "Tom" 
    ##         rush           to          fix            a          new 
    ##       "rush"         "to"        "fix"          "a"        "new" 
    ##      complic          too      complic 
    ## "complicate"        "too" "complicate"
