#### Recommender App #### 
## where the search engine app should have narrowed down the search with more input 
## we want the recommender function output to grow with more input

## the input to the shiny app are 
# user - specifying the user profile, can select multiple 
# core - specifying the core skills, can select multiple  
# level - specifying current level as a numeric, select only one 
# LevelAim - specifying desired level as numeric, select only one 


Recommender <- function(dat, input){
  
  # convert everything we need to search on to lower case
  datLower <- dat %>% 
    mutate(
      Contents = str_to_lower(Contents), 
      User = str_to_lower(User), 
      Level = str_to_lower(Level)
    )
  
  # Create a new variable which stores the level as a numeric 
  # value we can then search between 
  datLower <- datLower %>% 
    mutate(LevelNumeric = ifelse(Level == "fundamental" | Level == "fundamental;", 1, 
                                 ifelse(Level == "introductory" | Level == "introductory;", 2, 
                                        ifelse(Level == "intermediate"|Level == "intermediate;", 3, 
                                               ifelse(Level == "advanced"|Level == "advanced;", 4, 5
                                                      )
                                               )
                                         )
                                  )
    )
  
  # take input$user and compute to a regex string, by first converting
  # the inputs to all lower case 
  user <- str_to_lower(input$user)
  # we want all the results that contain any of the selected users 
  userString <- paste0(user, collapse = "|")
  
  # Take input$core and do the same 
  core <- str_to_lower(input$core)
  coreString <- paste0(core, collapse = "|")
  
  # filter the data frame to only include courses which 
  # are between the current level and the level aim 
  # not including the current level 
  # first we need to convert the inputs to numeric 
  level <- as.numeric(input$level)
  levelAim <- as.numeric(input$levelAim)
  
  # do the filter 
  datLower <- datLower %>% 
    filter(
      between(LevelNumeric, 
              level + 1, 
              levelAim)
    )
  
  # along with this we need to detect the appropriate users and the core skills 
  datLower <- datLower %>% 
    mutate(
      detectUser = str_detect(User, regex(userString))
    ) %>% 
    mutate(
      detectCore = str_detect(Contents, regex(coreString))
    )
  
  # make a few final changes to the way in which the data will present in the app 
  datFinal <- datLower %>% 
    mutate(
      # looks nicer in title than all lower case
      User = str_to_title(User), 
      Level = str_to_title(Level)
    ) %>% 
    rowwise() %>% 
    mutate(
      # sum the logical cols, the goal is to keep the rows which sum to 2
      Sum = sum(detectUser, detectCore)
    ) %>% 
    group_by(LevelNumeric) %>%
    # this ensures that in the viewable table, the results will be in order of difficulty 
    arrange(.by_group = TRUE)
  
  # final filter to keep those with all things detected 
  datSelect <- datFinal[datFinal$Sum == 2, ]
  
  # choose what to display to the user 
  datSelect <- datSelect %>% 
    ungroup() %>% 
    select(
      Provider, Company, CourseName, Link, Level, User
    )
  
  return(datSelect)
  
}


