CleanText <- function(text,
                      lemmatize = FALSE,
                      encode.to = "ASCII//TRANSLIT",
                      case = "lower",
                      delete = c("\\$", "[ \t]{2,}", "^\\s+|\\s+$",
                                 "[[:punct:]]", "\\d+",
                                 "NA"),
                      space = c(),
                      split = NA,
                      strip = NA){
  if(is.na(encode.to) == FALSE){
    text <- iconv(text, to = encode.to)
  }

  if(case == "lower"){
    text <- tolower(text)
  } else if(case == "upper"){
    text <- toupper(text)
  }

  for(RegEx in space){
    text <- gsub(RegEx, " ",
                 text)
  }

  for(RegEx in delete){
    text <- gsub(RegEx, "",
                 text)
  }

  #Lemmatization
  if(lemmatize == TRUE){
    if(length(delete) == 0){
      warning("Lemmatization may lead to unexpected results if string is contains characters other than alfanumeric")
    }
    text <- textstem::lemmatize_strings(text)
  }

  if(is.na(strip) == FALSE){
    text <- substr(text,
                   start = strip + 1,
                   stop = (nchar(text) - strip))
  }

  if(is.na(split) == FALSE){
    text <- strsplit(x = text, split = split)
  }

  return(text)
}

CleanKeywords <- function(x,
                          delete = c(),
                          space = c("-"),
                          strip = 1,
                          split = "><",
                          lemmatize = FALSE,
                          ...){
  CleanText(x,
            delete = delete,
            space = space,
            strip = strip,
            split = split,
            lemmatize = lemmatize,
            ...)
}
