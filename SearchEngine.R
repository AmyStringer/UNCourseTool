#### Search Engine function #### 
## Given some keywords and a dataset of a particular format
## this function detect the keywords within the contents
## column of the dataset 

searchEngine <- function(keywords, dat){
  
  # need to confirm that the dataset has the appropriate information 
  cols <- names(dat)
  
  # ensure the following columns exist in the inputted dataframe 
  validate(need("Contents" %in% cols, "Contents"))
  validate(need("Provider" %in% cols, "Provider"))
  validate(need("Company" %in% cols, "Company"))
  validate(need("CourseName" %in% cols, "CourseName"))
  validate(need("Link" %in% cols, "Link"))
  
  ## keywords are entered into the application as a text string. 
  ## each word should be separated by a ; to allow for separation and search
  # we start with the case where no keywords are entered 
  # in this instance we want to show all of the courses 
  if (keywords == ""){
    datSelect <- dat %>%
      # select the cols we would like to show the user 
      select(Provider, Company, CourseName, Link, Level, User)
  } else{

  # take big long string of words and split into character vector 
  # make keywords all lower case 
  # strsplit prints a list not a vector to need to unlist 
  keywords <- strsplit(keywords, ";") %>%
    unlist() %>%
    str_to_lower()
  
  # store the current number of columns
  numcol <- ncol(dat)
  
  #### THERE MAY BE A BETTER WAY TO DO THIS 
  ## this was actually tricky, i hope there is a better way to do this
  ## the idea 
  # loop over the keywords list, create a new column for each keyword 
  # containing the results from str_detect. 
  # use a for loop to create a column for each keyword and whether or not it was detected
  for (a in 1:length(keywords)){
    
    dat[, paste0("detect_", a, sep = "")] = str_detect(dat$Contents, regex(keywords[a]))
    
  }
  
  # this is similar to above, but this time we store the results from str_count
  # want to tally up the number of keyword occurrences, so compute a column for each key word 
  for (b in 1:length(keywords)){
    
    dat[, paste0("contained_", b, sep = "")] = str_count(dat$Contents, regex(keywords[b]))
    
  }
  
  # this is some hacky nonsense 
  # number of columns before new created detection vars = numcol
  # we want all the detected columns, so sum over col numcol+1 to col numcol + length(keywords) 
  dat$detected <- apply(dat[,c((numcol+1):(numcol+length(keywords)))], 1, sum)
  
  # same problem here, contains vars start at 9+length(keywords) and find one before the end 
  # one before the end because we just created detected 
  dat$Contained <- apply(dat[ , c((numcol+1+length(keywords)):(ncol(dat)-1))], 1, sum)
  
  # we want to be able to easily reference the different entries 
  dat <- dat %>%
    mutate(Index = 1:nrow(dat))
  
  # if there are no keywords found in the contents 
  # we want the app to throw a pop up message 
  if (sum(dat$detected == length(keywords)) == 0){
    
    showModal(modalDialog(
      title = "Important Message! ", 
      "Try another search! This one came up blank! ", 
      easyClose = TRUE 
    ))
    
    # then we still want the table to show up, just with nothing in it 
    # find indices for rows that match
    indices <- dat$Index[dat$detected == length(keywords)]
    
    # select rows that match
    datSelect <- dat[indices, ]
    
    # User only need certain info
    datSelect <- datSelect %>%
      select(Provider, Company, CourseName, Link, Level, User, Contained) %>%
      rename(KeywordCount = Contained) %>% 
      arrange(desc(KeywordCount))
    
    # then we need to consider the case where the keywords are found 
  } else if (sum(dat$detected == length(keywords)) > 0) {
    
    # find indices for rows that match 
    indices <- dat$Index[dat$detected == length(keywords)]
    # select rows that match 
    datSelect <- dat[indices, ]
    
    # User only need certain info 
    datSelect <- datSelect %>%
      select(Provider, Company, CourseName, Link, Level, User, Contained) %>% 
      rename(KeywordCount = Contained) %>% 
      arrange(desc(KeywordCount))
  }
  }
  
  return(datSelect)
  
}
