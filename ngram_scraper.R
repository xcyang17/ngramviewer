
# this contains two functions that would help scraping numerical data from google ngram viewer
# the frequency/proportion of occurence of a phrase in copora of various languages

# these packages are necessary
require(XML)
require(RCurl)
require(xlsx)
require(stringr)

## the first function generates a url according to desired start/end year, language, phrases
NgramUrl = function(phrase, startyear, endyear, corpus) {
  # phrase as a character vector containing one phrase of interest
  # startyear as a numeric (as early as 1500)
  # endyear as a numeric (e.g. 2008)
  # corpus as a numeric indicating the language. See README for information about 'corpus'
  
  # modify the phrase for first part of url generation
  empty_space_url = '+'; single_quote_url = '%27'; btw_keywords_url = '%2C'
  phrase_modified = gsub(' ', empty_space_url, phrase)
  phrase_modified = gsub("'", single_quote_url, phrase_modified)
  
  # generate first part of url
  main = 'https://books.google.com/ngrams/graph?content='
  rest = paste0('&year_start=', startyear, '&year_end=', endyear, '&corpus=', corpus, '&smoothing=0')
  a1 = paste(phrase_modified, collapse = btw_keywords_url)
  a2 = paste0(main, a1, rest)
  
  # generate second part of url
  main_rest = '&direct_url='
  rbefore_keyword = 't1%3B%2C'; rbtw_keyword = '%3B%2Cc0%3B.t1%3B%2C'
  rafter_keyword = '%3B%2Cc0'; rwithin_keyword = '%20'
  b1 = gsub(x = phrase, pattern = "'", replacement = single_quote_url)
  b2 = gsub(x = b1, pattern = ' ', replacement = rwithin_keyword)
  b3 = paste(b2, collapse = rbtw_keyword)
  b4 = paste(b3, rafter_keyword, sep = '')
  b5 = paste(rbefore_keyword, b4, sep = '')
  b6 = paste(main_rest, b5, sep = '')
  
  # concatenate first part and second part
  ngram_url = paste0(a2, b6, collapse = '')
  
  return(ngram_url)
}


## the second function generates a url according to desired start/end year, language, phrase
FindFrequency = function(url, phrase, startyear, endyear) { 
  # url as a character vector containing an output of NgramUrl or any valid url leading to a google ngram viewer search
  # phrase as a character vector containing one phrase of interest
  # year as a 2-element numeric vector (startyear and endyear as in the url)
  # returns a matrix containing frequencies of sites (row ~ freq, col ~ time(yr))
  
  # generate a matrix with 1 row and (startyear - endyear + 1) columns
  time = seq(startyear, endyear)
  m = matrix(nrow = 1, ncol = length(startyear:endyear)) # stores time series data
  rownames(m) = phrase; colnames(m) = time
  
  # process the url for data extraction
  t0 = htmlTreeParse(getURL(url), useInternalNodes = T) # an html object
  t1 = as.character(unlist(xpathApply(t0, '//script[@type]', xmlValue)[5]))
  t2 = unlist(strsplit(x = t1, split = 'parent')) # all useful except last one
  t3 = t2[-length(t2)] # remove last string
  t3 = gsub(x = t3, pattern = "&#39;", replacement = "'") # see ngram result "Eland's Bay Cave"
  t3 = gsub(x = t3, pattern = " / ", replacement = "/") # see ngram result "Valhingen / Enz", note that the parentheses around the phrase in source doesn't matter since will use grepl then
  
  Encoding(t3) = 'latin2' # deals with potential latin2 encoding letters in t3
  
  # t3 is the character string containing all time series data points we need
  # if the site could be found in google ngram viewer, then t3 would be non-empty, i.e. length(t3) = 1
  if(length(t3) == 1) {
    p1 = unlist(str_split(string = t3, pattern = ",|\\[|\\]"))
    p2 = as.numeric(p1[5:(length(p1)-2)])
    len = endyear - startyear + 1
    if(length(p2) != len) {
      # create/write in a txt to record any possible error/failure of scraping
      sink('error.txt', append=TRUE)
      cat(phrase, startyear, endyear, "\n")
      cat(p2, '\n')
      sink()      
    } else {
      m[1,] = as.numeric(p2)
    }
  } else {
    m[1,] = m[1,]
  }
  return(m)
}

