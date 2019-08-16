##################################################################
##	LEMMA DICTIONARY
##################################################################

# url <- "http://www.lexiconista.com/Datasets/lemmatization-en.zip"
# tmp <- tempfile()
# download.file(url, tmp)
# # extract the contents
# con <- unz(tmp, "lemmatization-en.txt", encoding = "UTF-8")
# tab <- read.delim(con, header=FALSE, stringsAsFactors = FALSE)
# names(tab) <- c("stem", "term")
# saveRDS(tab, "temp/lemma_dictionary.RDS")

##################################################################
##	BEGIN: clean_corpus_tm()
##################################################################
# Cleans a TM corpus including stem completion

clean_corpus_tm <- function(corpus, my_stopwords = NULL, stemcomp = FALSE){
  corpus %<>% 
    tm_map(content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")) ) %>%
    tm_map(stripWhitespace) %>%
    tm_map(removePunctuation) %>%
    tm_map(content_transformer(tolower))
  
  if(stemcomp == TRUE){
    dict <- corpus
    corpus %<>%
      tm_map(stemDocument) %>%
      tm_map(stemCompletion, dictionary = dict, lazy = TRUE)
  }
  return(corpus)
}

##################################################################
##	BEGIN: topicmodels_json_ldavis()
##################################################################
# takes the output of a topicmodel lda and transforms it to ldaviz json
# TODO: Include option to select only certain documents, topics

topicmodels_json_ldavis <- function(fitted, doc_fm, method = "PCA", EID_in = NULL, topic_in = NULL){
  # Required packages
  library(topicmodels)
  library(dplyr)
  library(LDAvis)
  
  # Find required quantities
  phi <- posterior(fitted)$terms %>% as.matrix # Topic-term distribution
  theta <- posterior(fitted)$topics %>% as.matrix # Document-topic matrix
  
  # Restrict
  if(!is_null(EID_in)){
    theta <- theta[rownames(theta) %in%  EID_in,]
    doc_fm  %<>% dfm_subset(dimnames(doc_fm)$docs %in% EID_in)
  }
  
  # Restrict
  if(!is_null(topic_in)){
    phi <- phi[topic_in, ]
    theta <- theta[ , topic_in]
  }
  
  vocab <- colnames(phi)
  doc_length <- tibble(EID = rownames(theta)) %>%
    left_join( tibble(EID = names(ntoken(doc_fm)), n = ntoken(doc_fm)) , by = "EID") 
  
  tf <- tibble(feature = vocab) %>%
    left_join((textstat_frequency(doc_fm) %>% select(feature, frequency) ), by = "feature") 
  
  if(method == "PCA"){mds <- jsPCA}
  if(method == "TSNE"){library(tsne); mds <- function(x){tsne(svd(x)$u)} }
  
  # Convert to json
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                 vocab = vocab,
                                 doc.length = doc_length %>% pull(n),
                                 term.frequency = tf %>% pull(frequency),
                                 reorder.topics = FALSE,
                                 mds.method = mds, 
                                 plot.opts = list(xlab = "Dim.1", ylab = "Dim.2")) 
  return(json_lda)
}

