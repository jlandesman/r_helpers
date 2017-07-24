##A series of helper functions for R 
## source(helper_functions.R)

##load_libraries
## Function to load my standard libraries
load_libraries<-function(){
  library(tidyverse)
  library(tidyquant)
  library(xts)
  library(RODBC)
  library(RSQLite)
  library(DBI)
  library(stringr)
}

## %like%
## note: setup is X, pattern = so "test!" %like% test == TRUE, "test" %like% "test!" == FALSE
`%like%` <- function(x, pattern) str_detect(x, pattern)


## %notin%
## "test" %notint% c("this", "is", "a") == TRUE, "test" %notint% c("this", "is", "a", "test") == FALSE
`%notin%` <- function(x, y) !(x %in% y)

## sample_for_replication()
## Use with replicate(n=num_samples, sm(seq_to_sample, size_of_sample))
#x <- replicate(n=1000, sample_for_replication(5))
sample_for_replication<-function(seq_to_sample, size_of_sample){
    sample(children, size=n, replace = FALSE)
}

## copyTable()
##   Copy a data frame to clipboard.
copyTable <- function(x, row.names=FALSE){
  write.table(x, "clipboard-10000", sep="\t", row.names=row.names, na="")
}

## readClip()
## Read from clipboard
readClip <- function() read.table("clipboard-128", header=T, sep="\t", stringsAsFactors=F)

## Format strings to search_engine format for regex searching
## Takes care of parentheses etc
## input: 'george washington, "george washington"'
## output: george|washington|george washington

get_search_engine_format<-function(string){
  regex.escape <- function(string) {
    gsub("([][{}()+*^${|\\\\?])", "\\\\\\1", string)
  }
  
  string<-gsub(",", "", string)
  
  sort.by.length.desc <- function (v) v[order( -nchar(v)) ] 
  
  keys <- c(t(read.table(text=string, header=FALSE)))     # Read in the values
  keys <- sort.by.length.desc(keys)                       # Sort the values
  pattern = paste(regex.escape(keys), collapse="|")       # Create the pattern
  return(pattern)
}
