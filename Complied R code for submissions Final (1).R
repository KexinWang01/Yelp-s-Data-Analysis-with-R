library(tidyverse)
library(jsonlite)
library(skimr)
library(stringr)
library(tm)
library(makedummies)
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(readr)

## All cleansed data can be found at https://drive.google.com/drive/folders/1-P_aWdOldSEeLO-ihXB11RC6YVYj_85Q?usp=sharing


#####################################################
##### Data Import and Transformation 
## Yelp Data 
# Import JSON files to dataframes
setwd("/Users/nick/Documents/APAN/Central_Dataset/yelp_dataset/")

json_file <- "yelp_academic_dataset_business.json"
json_file_contents <- readLines(json_file)
json_file_contents <- paste(json_file_contents, collapse = ",")
json_file_contents <- paste(c("[", json_file_contents, "]"), collapse = "")
df_business <- fromJSON(json_file_contents)

json_file <- "yelp_academic_dataset_review.json"
json_file_contents <- readLines(json_file)
json_file_contents <- paste(json_file_contents, collapse = ",")
json_file_contents <- paste(c("[", json_file_contents, "]"), collapse = "")
df_review <- fromJSON(json_file_contents)

# Check for na rating 
sum(is.na(df_business$stars)) 

# Filter for only restaurants 
filter_text <- paste("Restaurants", "Food" , "Bars" , "Nightlife" , "Coffee" , "Pizza" , "Burgers" , "Beer" , "Cafes" , "Breakfast", sep = "|")
df_business %>%
  dplyr::filter(., grepl(filter_text, categories)) %>%
  mutate(Y = ifelse(stars >= 4, 2, ifelse(stars <= 2, 0, 1))) -> df_business # 2 good, 1 neutral, 0 bad 
# Quick check 
dim(df_business)
df_business %>%
  select(stars, Y) %>%
  .[711:721,]

# Merge with review data 
biz_reviews <- cbind(df_business, df_review, on = "business_id")

# Convert datetime to just date
biz_reviews$date <- as.Date(biz_reviews$date,  format = "%Y-%m-%d %H:%M:%S")

# Convert reviews to a matrix using TF_IDF 

# create a corpus
corpus = Corpus(VectorSource(biz_reviews$text))

# convert to lower case
corpus = tm_map(corpus,FUN = content_transformer(tolower))

# remove punctuation
corpus = tm_map(corpus,FUN = removePunctuation)

# remove stopwords
corpus = tm_map(corpus,FUN = removeWords,c(stopwords('english')))

# strip whitespace (ex. \n)
corpus = tm_map(corpus,FUN = stripWhitespace)

# create a dict.
dict = findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(biz_reviews$text))), lowfreq = 0)
dict_corpus = Corpus(VectorSource(dict))

# stem document
corpus = tm_map(corpus,FUN = stemDocument)

# create tfidf (document term matrix)
dtm_tfidf = DocumentTermMatrix(x=corpus,
                               control = list(weighting=function(x) weightTfIdf(x,normalize=F)))
xdtm_tfidf = removeSparseTerms(dtm_tfidf,sparse = 0.95)
xdtm_tfidf = as.data.frame(as.matrix(xdtm_tfidf))
colnames(xdtm_tfidf) = stemCompletion(x = colnames(xdtm_tfidf),
                                      dictionary = dict_corpus,
                                      type='prevalent')
colnames(xdtm_tfidf) = make.names(colnames(xdtm_tfidf))
sort(colSums(xdtm_tfidf),decreasing = T)[1:20]

# Add word_ to all columns in tfidf (b/c of same column names in biz_reviews)
colnames(xdtm_tfidf) <- paste("word", colnames(xdtm_tfidf), sep="_")
colnames(xdtm_tfidf)

# combine tfidf and review df
biz_reviews <- cbind(biz_reviews, xdtm_tfidf)


# Dummy encoding restaurants' categories 

corpus2 = Corpus(VectorSource(biz_reviews$categories))
corpus2 = tm_map(corpus2,
                 FUN = content_transformer(FUN = function(x)gsub(")", "", x, fixed = TRUE)))
corpus2 = tm_map(corpus2,
                 FUN = content_transformer(FUN = function(x)gsub("(", "", x, fixed = TRUE)))
corpus2 = tm_map(corpus2,
                 FUN = content_transformer(FUN = function(x)gsub("&", "", x, fixed = TRUE)))
corpus2 = tm_map(corpus2,
                 FUN = content_transformer(FUN = function(x)gsub("/", "", x, fixed = TRUE)))
corpus2 = tm_map(corpus2,
                 FUN = content_transformer(FUN = function(x)gsub(" ", "", x, fixed = TRUE)))
corpus2 = tm_map(corpus2,
                 FUN = content_transformer(FUN = function(x)gsub(",", " ", x, fixed = TRUE)))
corpus2 <- tm_map(corpus2, content_transformer(tolower))
corpus2 = tm_map(corpus2,FUN = removeWords,c(stopwords('english')))
corpus2 <- tm_map(corpus2, stripWhitespace)
corpus2[[30]][1]

dict = findFreqTerms(DocumentTermMatrix(corpus2),
                     lowfreq = 0)

dict_corpus = Corpus(VectorSource(dict))
corpus2 = tm_map(corpus2,FUN = stemDocument)
dtm = DocumentTermMatrix(corpus2)
dtm
dtm = as.data.frame(as.matrix(xdtm))
colnames(dtm) = stemCompletion(x = colnames(dtm),
                               dictionary = dict_corpus,
                               type='prevalent')
colnames(dtm) = make.names(colnames(dtm))
sort(colSums(dtm),decreasing = T)
biz_reviews$categories = tolower(biz_reviews$categories)
biz_reviews <- biz_reviews%>%
  mutate(Streetvendors = ifelse(grepl('streetvendors',categories,fixed = TRUE),1,0))%>%
  mutate(Noodles = ifelse(grepl('noodles',categories,fixed = TRUE),1,0))%>%
  mutate(Caribbean = ifelse(grepl('caribbean',categories,fixed = TRUE),1,0))%>%
  mutate(Bagels = ifelse(grepl('bagels',categories,fixed = TRUE),1,0))%>%
  mutate(Comfortfood = ifelse(grepl('comfortfood',categories,fixed = TRUE),1,0))%>%
  mutate(Brewries = ifelse(grepl('breweries',categories,fixed = TRUE),1,0))%>%
  mutate(Greek = ifelse(grepl('greek',categories,fixed = TRUE),1,0))%>%
  mutate(Tacos = ifelse(grepl('tacos',categories,fixed = TRUE),1,0))%>%
  mutate(BubbleTea = ifelse(grepl('bubble tea',categories,fixed = TRUE),1,0))%>%
  mutate(Southern = ifelse(grepl('southern',categories,fixed = TRUE),1,0))%>%
  mutate(MiddleEastern = ifelse(grepl('middleeastern',categories,fixed = TRUE),1,0))%>%
  mutate(KoreanBeerbar = ifelse(grepl('korean beerbar',categories,fixed = TRUE),1,0))%>%
  mutate(Hotelstravel = ifelse(grepl('hotelstravel',categories,fixed = TRUE),1,0))%>%
  mutate(LatinAmerican = ifelse(grepl('latinamerican',categories,fixed = TRUE),1,0))%>%
  mutate(FoodDeliveryServices = ifelse(grepl('fooddeliveryservices',categories,fixed = TRUE),1,0))%>%
  mutate(Soup = ifelse(grepl('soup',categories,fixed = TRUE),1,0))%>%
  mutate(Donuts = ifelse(grepl('donuts',categories,fixed = TRUE),1,0))%>%
  mutate(Venueseventspaces = ifelse(grepl('venueseventspaces',categories,fixed = TRUE),1,0))%>%
  mutate(Tex.mex  = ifelse(grepl('tex.mex',categories,fixed = TRUE),1,0))%>%
  mutate(Localflavor = ifelse(grepl('Localflavor',categories,fixed = TRUE),1,0))%>%
  mutate(Musicvenues = ifelse(grepl('musicvenues',categories,fixed = TRUE),1,0))%>%
  mutate(Indian = ifelse(grepl('indian',categories,fixed = TRUE),1,0))%>%
  mutate(Drugstores = ifelse(grepl('drugstores',categories,fixed = TRUE),1,0))%>%
  mutate(SteakHouses = ifelse(grepl('steakhouses',categories,fixed = TRUE),1,0))%>%
  mutate(americannew  = ifelse(grepl('americannew ',categories,fixed = FALSE),1,0))%>%
  mutate(fastfood  = ifelse(grepl('fastfood',categories,fixed = FALSE),1,0))%>%
  mutate(burgers  = ifelse(grepl('burgers',categories,fixed = FALSE),1,0))%>%
  mutate(specialtyfood   = ifelse(grepl('specialtyfood ',categories,fixed = FALSE),1,0))%>%
  mutate(mexican   = ifelse(grepl('mexican ',categories,fixed = FALSE),1,0))%>%
  mutate(eventplanningservices = ifelse(grepl('eventplanningservices',categories,fixed = FALSE),1,0))%>%
  mutate(bakeries  = ifelse(grepl('bakeries',categories,fixed = FALSE),1,0))%>%
  mutate(italian  = ifelse(grepl('italian',categories,fixed = FALSE),1,0))%>%
  mutate(chinese   = ifelse(grepl('chinese ',categories,fixed = FALSE),1,0))%>%
  mutate(desserts    = ifelse(grepl('desserts',categories,fixed = FALSE),1,0))%>%
  mutate(shopping  = ifelse(grepl('shopping',categories,fixed = FALSE),1,0))%>%
  mutate(seafood   = ifelse(grepl('seafood',categories,fixed = FALSE),1,0))%>%
  mutate(cafes    = ifelse(grepl('cafes',categories,fixed = FALSE),1,0))%>%
  mutate( japanese    = ifelse(grepl(' japanese',categories,fixed = FALSE),1,0))%>%
  mutate(beer    = ifelse(grepl('beer',categories,fixed = FALSE),1,0))%>%
  mutate(winespirits    = ifelse(grepl('winespirits',categories,fixed = FALSE),1,0))%>%
  mutate(salad    = ifelse(grepl('salad',categories,fixed = FALSE),1,0))%>%
  mutate(foodtrucks    = ifelse(grepl('foodtrucks',categories,fixed = FALSE),1,0))


# Dummy encoding restaurants' attributes 

biz_reviews <- flatten(biz_reviews)

# For column attributes.Ambience
vec_ambience <- function(df){
  vec <- df['attributes.Ambience'] %>%
    str_sub(start = 2, end = -2) %>% 
    strsplit(",") %>% 
    unlist()
  return(vec)
}

get_touristy <- function(df){
  if(is.null(df['vec_ambience'])){
    touristy = NULL
  }else{
    touristy <- str_subset(unlist(df['vec_ambience']), 'touristy') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'touristy':", replacement = '')
  }
  return(touristy)
}

get_hipster <- function(df){
  if(is.null(df['vec_ambience'])){
    hipster = NULL
  }else{
    hipster <- str_subset(unlist(df['vec_ambience']), 'hipster') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'hipster':", replacement = '')
  }
  return(hipster)
}

get_romantic <- function(df){
  if(is.null(df['vec_ambience'])){
    romantic = NULL
  }else{
    romantic <- str_subset(unlist(df['vec_ambience']), 'romantic') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'romantic':", replacement = '')
  }
  return(romantic)
}

get_divey <- function(df){
  if(is.null(df['vec_ambience'])){
    divey = NULL
  }else{
    divey <- str_subset(unlist(df['vec_ambience']), 'divey') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'divey':", replacement = '')
  }
  return(divey)
}

get_intimate <- function(df){
  if(is.null(df['vec_ambience'])){
    intimate = NULL
  }else{
    intimate <- str_subset(unlist(df['vec_ambience']), 'intimate') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'intimate':", replacement = '')
  }
  return(intimate)
}

get_trendy <- function(df){
  if(is.null(df['vec_ambience'])){
    trendy = NULL
  }else{
    trendy <- str_subset(unlist(df['vec_ambience']), 'trendy') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'trendy':", replacement = '')
  }
  return(trendy)
}

get_upscale <- function(df){
  if(is.null(df['vec_ambience'])){
    upscale = NULL
  }else{
    upscale <- str_subset(unlist(df['vec_ambience']), 'upscale') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'upscale':", replacement = '')
  }
  return(upscale)
}

get_classy <- function(df){
  if(is.null(df['vec_ambience'])){
    classy = NULL
  }else{
    classy <- str_subset(unlist(df['vec_ambience']), 'classy') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'classy':", replacement = '')
  }
  return(classy)
}

get_casual <- function(df){
  if(is.null(df['vec_ambience'])){
    casual = NULL
  }else{
    casual <- str_subset(unlist(df['vec_ambience']), 'casual') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'casual':", replacement = '')
  }
  return(casual)
}

biz_reviews$vec_ambience <- apply(biz_reviews, 1, vec_ambience)
biz_reviews$touristy <- apply(biz_reviews, 1, get_touristy)
biz_reviews$hipster <- apply(biz_reviews, 1, get_hipster)
biz_reviews$divey <- apply(biz_reviews, 1, get_divey)
biz_reviews$intimate <- apply(biz_reviews, 1, get_intimate)
biz_reviews$trendy <- apply(biz_reviews, 1, get_trendy)
biz_reviews$upscale <- apply(biz_reviews, 1, get_upscale)
biz_reviews$classy <- apply(biz_reviews, 1, get_classy)
biz_reviews$casual <- apply(biz_reviews, 1, get_casual)
biz_reviews$romantic <- apply(biz_reviews, 1, get_romantic)

biz_reviews <- unnest(biz_reviews,
                      c(touristy,hipster,divey,intimate,trendy,upscale,classy,casual,romantic),
                      keep_empty = TRUE)

biz_reviews <- biz_reviews %>% 
  select(-vec_ambience, -attributes.Ambience)

# For attributes.BusinessParking

vec_parking <- function(df){
  vec <- df['attributes.BusinessParking'] %>%
    str_sub(start = 2, end = -2) %>% 
    strsplit(",") %>% 
    unlist()
  return(vec)
}

get_garage <- function(df){
  if(is.null(df['vec_parking'])){
    garage = NULL
  }else{
    garage <- str_subset(unlist(df['vec_parking']), 'garage') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'garage':", replacement = '')
  }
  return(garage)
}

get_street <- function(df){
  if(is.null(df['vec_parking'])){
    street = NULL
  }else{
    street <- str_subset(unlist(df['vec_parking']), 'street') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'street':", replacement = '')
  }
  return(street)
}

get_validated <- function(df){
  if(is.null(df['vec_parking'])){
    validated = NULL
  }else{
    validated <- str_subset(unlist(df['vec_parking']), 'validated') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'validated':", replacement = '')
  }
  return(validated)
}

get_lot <- function(df){
  if(is.null(df['vec_parking'])){
    lot = NULL
  }else{
    lot <- str_subset(unlist(df['vec_parking']), 'lot') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'lot':", replacement = '')
  }
  return(lot)
}

get_valet <- function(df){
  if(is.null(df['vec_parking'])){
    valet = NULL
  }else{
    valet <- str_subset(unlist(df['vec_parking']), 'valet') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'valet':", replacement = '')
  }
  return(valet)
}

biz_reviews$vec_parking <- apply(biz_reviews, 1, vec_parking)
biz_reviews$garage <- apply(biz_reviews, 1, get_garage)
biz_reviews$street <- apply(biz_reviews, 1, get_street)
biz_reviews$validated <- apply(biz_reviews, 1, get_validated)
biz_reviews$lot <- apply(biz_reviews, 1, get_lot)
biz_reviews$valet <- apply(biz_reviews, 1, get_valet)

biz_reviews <- unnest(biz_reviews,
                      c(garage,street,validated,lot,valet),
                      keep_empty = TRUE)

biz_reviews <- biz_reviews %>% 
  select(-vec_parking, -attributes.BusinessParking)


# For attributes.GoodForMeal

vec_GoodForMeal <- function(df){
  vec <- df['attributes.GoodForMeal'] %>%
    str_sub(start = 2, end = -2) %>% 
    strsplit(",") %>% 
    unlist()
  return(vec)
}

get_dessert <- function(df){
  if(is.null(df['vec_GoodForMeal'])){
    dessert = NULL
  }else{
    dessert <- str_subset(unlist(df['vec_GoodForMeal']), 'dessert') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'dessert':", replacement = '')
  }
  return(dessert)
}

get_latenight <- function(df){
  if(is.null(df['vec_GoodForMeal'])){
    latenight = NULL
  }else{
    latenight <- str_subset(unlist(df['vec_GoodForMeal']), 'latenight') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'latenight':", replacement = '')
  }
  return(latenight)
}

get_lunch <- function(df){
  if(is.null(df['vec_GoodForMeal'])){
    lunch = NULL
  }else{
    lunch <- str_subset(unlist(df['vec_GoodForMeal']), 'lunch') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'lunch':", replacement = '')
  }
  return(lunch)
}

get_dinner <- function(df){
  if(is.null(df['vec_GoodForMeal'])){
    dinner = NULL
  }else{
    dinner <- str_subset(unlist(df['vec_GoodForMeal']), 'dinner') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'dinner':", replacement = '')
  }
  return(dinner)
}

get_breakfast <- function(df){
  if(is.null(df['vec_GoodForMeal'])){
    breakfast = NULL
  }else{
    breakfast <- str_subset(unlist(df['vec_GoodForMeal']), 'breakfast') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'breakfast':", replacement = '')
  }
  return(breakfast)
}

get_brunch <- function(df){
  if(is.null(df['vec_GoodForMeal'])){
    brunch = NULL
  }else{
    brunch <- str_subset(unlist(df['vec_GoodForMeal']), 'brunch') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'brunch':", replacement = '')
  }
  return(brunch)
}

biz_reviews$vec_GoodForMeal <- apply(biz_reviews, 1, vec_GoodForMeal)
biz_reviews$dessert <- apply(biz_reviews, 1, get_dessert)
biz_reviews$latenight <- apply(biz_reviews, 1, get_latenight)
biz_reviews$lunch <- apply(biz_reviews, 1, get_lunch)
biz_reviews$dinner <- apply(biz_reviews, 1, get_dinner)
biz_reviews$breakfast <- apply(biz_reviews, 1, get_breakfast)
biz_reviews$brunch <- apply(biz_reviews, 1, get_brunch)

biz_reviews <- unnest(biz_reviews,
                      c(dessert,latenight,lunch,dinner,breakfast,brunch),
                      keep_empty = TRUE)

biz_reviews <- biz_reviews %>% 
  select(-vec_GoodForMeal, -attributes.GoodForMeal)


# For attributes.HairSpecializesIn

vec_HairSpecializesIn <- function(df){
  vec <- df['attributes.HairSpecializesIn'] %>%
    str_sub(start = 2, end = -2) %>% 
    strsplit(",") %>% 
    unlist()
  return(vec)
}

get_perms <- function(df){
  if(is.null(df['vec_HairSpecializesIn'])){
    perms = NULL
  }else{
    perms <- str_subset(unlist(df['vec_HairSpecializesIn']), 'perms') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'perms':", replacement = '')
  }
  return(perms)
}

get_coloring <- function(df){
  if(is.null(df['vec_HairSpecializesIn'])){
    coloring = NULL
  }else{
    coloring <- str_subset(unlist(df['vec_HairSpecializesIn']), 'coloring') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'coloring':", replacement = '')
  }
  return(coloring)
}

get_extensions <- function(df){
  if(is.null(df['vec_HairSpecializesIn'])){
    extensions = NULL
  }else{
    extensions <- str_subset(unlist(df['vec_HairSpecializesIn']), 'extensions') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'extensions':", replacement = '')
  }
  return(extensions)
}

get_curly <- function(df){
  if(is.null(df['vec_HairSpecializesIn'])){
    curly = NULL
  }else{
    curly <- str_subset(unlist(df['vec_HairSpecializesIn']), 'curly') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'curly':", replacement = '')
  }
  return(curly)
}

get_kids <- function(df){
  if(is.null(df['vec_HairSpecializesIn'])){
    kids = NULL
  }else{
    kids <- str_subset(unlist(df['vec_HairSpecializesIn']), 'kids') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'kids':", replacement = '')
  }
  return(kids)
}

biz_reviews$vec_HairSpecializesIn <- apply(biz_reviews, 1, vec_HairSpecializesIn)
biz_reviews$perms <- apply(biz_reviews, 1, get_perms)
biz_reviews$coloring <- apply(biz_reviews, 1, get_coloring)
biz_reviews$extensions <- apply(biz_reviews, 1, get_extensions)
biz_reviews$curly <- apply(biz_reviews, 1, get_curly)
biz_reviews$kids <- apply(biz_reviews, 1, get_kids)

biz_reviews <- unnest(biz_reviews,
                      c(perms,coloring,extensions,curly,kids),
                      keep_empty = TRUE)

biz_reviews <- biz_reviews %>% 
  select(-vec_HairSpecializesIn, -attributes.HairSpecializesIn)


# For attributes.BestNights

vec_BestNights <- function(df){
  vec <- df['attributes.BestNights'] %>%
    str_sub(start = 2, end = -2) %>% 
    strsplit(",") %>% 
    unlist()
  return(vec)
}

get_monday <- function(df){
  if(is.null(df['vec_BestNights'])){
    monday = NULL
  }else{
    monday <- str_subset(unlist(df['vec_BestNights']), 'monday') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'monday':", replacement = '')
  }
  return(monday)
}

get_tuesday <- function(df){
  if(is.null(df['vec_BestNights'])){
    tuesday = NULL
  }else{
    tuesday <- str_subset(unlist(df['vec_BestNights']), 'tuesday') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'tuesday':", replacement = '')
  }
  return(tuesday)
}

get_wednesday <- function(df){
  if(is.null(df['vec_BestNights'])){
    wednesday = NULL
  }else{
    wednesday <- str_subset(unlist(df['vec_BestNights']), 'wednesday') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'wednesday':", replacement = '')
  }
  return(wednesday)
}

get_thursday <- function(df){
  if(is.null(df['vec_BestNights'])){
    thursday = NULL
  }else{
    thursday <- str_subset(unlist(df['vec_BestNights']), 'thursday') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'thursday':", replacement = '')
  }
  return(thursday)
}

get_friday <- function(df){
  if(is.null(df['vec_BestNights'])){
    friday = NULL
  }else{
    friday <- str_subset(unlist(df['vec_BestNights']), 'friday') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'friday':", replacement = '')
  }
  return(friday)
}

get_saturday <- function(df){
  if(is.null(df['vec_BestNights'])){
    saturday = NULL
  }else{
    saturday <- str_subset(unlist(df['vec_BestNights']), 'saturday') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'saturday':", replacement = '')
  }
  return(saturday)
}

get_sunday <- function(df){
  if(is.null(df['vec_BestNights'])){
    sunday = NULL
  }else{
    sunday <- str_subset(unlist(df['vec_BestNights']), 'sunday') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'sunday':", replacement = '')
  }
  return(sunday)
}

biz_reviews$vec_BestNights <- apply(biz_reviews, 1, vec_BestNights)
biz_reviews$monday <- apply(biz_reviews, 1, get_monday)
biz_reviews$tuesday <- apply(biz_reviews, 1, get_tuesday)
biz_reviews$wednesday <- apply(biz_reviews, 1, get_wednesday)
biz_reviews$thursday <- apply(biz_reviews, 1, get_thursday)
biz_reviews$friday <- apply(biz_reviews, 1, get_friday)
biz_reviews$saturday <- apply(biz_reviews, 1, get_saturday)
biz_reviews$sunday <- apply(biz_reviews, 1, get_sunday)

biz_reviews <- unnest(biz_reviews,
                      c(monday,tuesday,wednesday,thursday,friday,saturday,sunday),
                      keep_empty = TRUE)

biz_reviews <- biz_reviews %>% 
  select(-vec_BestNights, -attributes.BestNights)


# For attributes.Music

vec_Music <- function(df){
  vec <- df['attributes.Music'] %>%
    str_sub(start = 2, end = -2) %>% 
    strsplit(",") %>% 
    unlist()
  return(vec)
}

get_dj <- function(df){
  if(is.null(df['vec_Music'])){
    dj = NULL
  }else{
    dj <- str_subset(unlist(df['vec_Music']), 'dj') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'dj':", replacement = '')
  }
  return(dj)
}

get_background_music <- function(df){
  if(is.null(df['vec_Music'])){
    background_music = NULL
  }else{
    background_music <- str_subset(unlist(df['vec_Music']), 'background_music') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'background_music':", replacement = '')
  }
  return(background_music)
}

get_jukebox <- function(df){
  if(is.null(df['vec_Music'])){
    jukebox = NULL
  }else{
    jukebox <- str_subset(unlist(df['vec_Music']), 'jukebox') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'jukebox':", replacement = '')
  }
  return(jukebox)
}

get_live <- function(df){
  if(is.null(df['vec_Music'])){
    live = NULL
  }else{
    live <- str_subset(unlist(df['vec_Music']), 'live') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'live':", replacement = '')
  }
  return(live)
}

get_video <- function(df){
  if(is.null(df['vec_Music'])){
    video = NULL
  }else{
    video <- str_subset(unlist(df['vec_Music']), 'video') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'video':", replacement = '')
  }
  return(video)
}

get_karaoke <- function(df){
  if(is.null(df['vec_Music'])){
    karaoke = NULL
  }else{
    karaoke <- str_subset(unlist(df['vec_Music']), 'karaoke') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'karaoke':", replacement = '')
  }
  return(karaoke)
}

biz_reviews$vec_Music <- apply(biz_reviews, 1, vec_Music)
biz_reviews$dj <- apply(biz_reviews, 1, get_dj)
biz_reviews$background_music <- apply(biz_reviews, 1, get_background_music)
biz_reviews$jukebox <- apply(biz_reviews, 1, get_jukebox)
biz_reviews$live <- apply(biz_reviews, 1, get_live)
biz_reviews$video <- apply(biz_reviews, 1, get_video)
biz_reviews$karaoke <- apply(biz_reviews, 1, get_karaoke)

biz_reviews <- unnest(biz_reviews,
                      c(dj,background_music,jukebox,live,video,karaoke),
                      keep_empty = TRUE)

biz_reviews <- biz_reviews %>% 
  select(-vec_Music, -attributes.Music)


# For attributes.DietaryRestrictions

vec_DietaryRestrictions <- function(df){
  vec <- df['attributes.DietaryRestrictions'] %>%
    str_sub(start = 2, end = -2) %>% 
    strsplit(",") %>% 
    unlist()
  return(vec)
}

get_dairy_free <- function(df){
  if(is.null(df['vec_DietaryRestrictions'])){
    dairy_free = NULL
  }else{
    dairy_free <- str_subset(unlist(df['vec_DietaryRestrictions']), 'dairy-free') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'dairy-free':", replacement = '')
  }
  return(dairy_free)
}

get_gluten_free <- function(df){
  if(is.null(df['vec_DietaryRestrictions'])){
    gluten_free = NULL
  }else{
    gluten_free <- str_subset(unlist(df['vec_DietaryRestrictions']), 'gluten-free') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'gluten-free':", replacement = '')
  }
  return(gluten_free)
}

get_vegan <- function(df){
  if(is.null(df['vec_DietaryRestrictions'])){
    vegan = NULL
  }else{
    vegan <- str_subset(unlist(df['vec_DietaryRestrictions']), 'vegan') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'vegan':", replacement = '')
  }
  return(vegan)
}

get_kosher <- function(df){
  if(is.null(df['vec_DietaryRestrictions'])){
    kosher = NULL
  }else{
    kosher <- str_subset(unlist(df['vec_DietaryRestrictions']), 'kosher') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'kosher':", replacement = '')
  }
  return(kosher)
}

get_halal <- function(df){
  if(is.null(df['vec_DietaryRestrictions'])){
    halal = NULL
  }else{
    halal <- str_subset(unlist(df['vec_DietaryRestrictions']), 'halal') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'halal':", replacement = '')
  }
  return(halal)
}

get_soy_free <- function(df){
  if(is.null(df['vec_DietaryRestrictions'])){
    soy_free = NULL
  }else{
    soy_free <- str_subset(unlist(df['vec_DietaryRestrictions']), 'soy-free') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'soy-free':", replacement = '')
  }
  return(soy_free)
}

get_vegetarian <- function(df){
  if(is.null(df['vec_DietaryRestrictions'])){
    vegetarian = NULL
  }else{
    vegetarian <- str_subset(unlist(df['vec_DietaryRestrictions']), 'vegetarian') %>% 
      gsub(pattern = ' ', replacement = '') %>% 
      gsub(pattern = "'vegetarian':", replacement = '')
  }
  return(vegetarian)
}

biz_reviews$vec_DietaryRestrictions <- apply(biz_reviews, 1, vec_DietaryRestrictions)
biz_reviews$dairy_free <- apply(biz_reviews, 1, get_dairy_free)
biz_reviews$gluten_free <- apply(biz_reviews, 1, get_gluten_free)
biz_reviews$vegan <- apply(biz_reviews, 1, get_vegan)
biz_reviews$kosher <- apply(biz_reviews, 1, get_kosher)
biz_reviews$halal <- apply(biz_reviews, 1, get_halal)
biz_reviews$soy_free <- apply(biz_reviews, 1, get_soy_free)
biz_reviews$vegetarian <- apply(biz_reviews, 1, get_vegetarian)

biz_reviews <- unnest(biz_reviews,
                      c(dairy_free,gluten_free,vegan,kosher,halal,soy_free,vegetarian),
                      keep_empty = TRUE)

biz_reviews <- biz_reviews %>% 
  select(-vec_DietaryRestrictions, -attributes.DietaryRestrictions)


# Rename some columns

biz_reviews <- biz_reviews %>% 
  rename(
    RestaurantsTableService = attributes.RestaurantsTableService,
    WiFi = attributes.WiFi,
    BikeParking = attributes.BikeParking,
    BusinessParking = attributes.BusinessParking,
    BusinessAcceptsCreditCards = attributes.BusinessAcceptsCreditCards,
    RestaurantsReservations = attributes.RestaurantsReservations,
    WheelchairAccessible = attributes.WheelchairAccessible,
    Caters = attributes.Caters,
    OutdoorSeating = attributes.OutdoorSeating,
    RestaurantsGoodForGroups = attributes.RestaurantsGoodForGroups,
    HappyHour = attributes.HappyHour,
    BusinessAcceptsBitcoin = attributes.BusinessAcceptsBitcoin,
    RestaurantsPriceRange2 = attributes.RestaurantsPriceRange2,
    HasTV = attributes.HasTV,
    Alcohol = attributes.Alcohol,
    DogsAllowed = attributes.DogsAllowed,
    RestaurantsTakeOut = attributes.RestaurantsTakeOut,
    NoiseLevel = attributes.NoiseLevel,
    RestaurantsAttire = attributes.RestaurantsAttire,
    RestaurantsDelivery = attributes.RestaurantsDelivery,
    GoodForKids = attributes.GoodForKids,
    ByAppointmentOnly = attributes.ByAppointmentOnly,
    AcceptsInsurance = attributes.AcceptsInsurance,
    GoodForDancing = attributes.GoodForDancing,
    BYOB = attributes.BYOB,
    CoatCheck = attributes.CoatCheck,
    Smoking = attributes.Smoking,
    DriveThru = attributes.DriveThru,
    BYOBCorkage = attributes.BYOBCorkage,
    Corkage = attributes.Corkage,
    RestaurantsCounterService = attributes.RestaurantsCounterService,
    AgesAllowed = attributes.AgesAllowed,
    Open24Hours = attributes.Open24Hours
  )



# WiFi
# Remove "u" and "'"
biz_reviews$WiFi <- gsub("u'", "", biz_reviews$WiFi)
biz_reviews$WiFi <- gsub("'", "", biz_reviews$WiFi)
#convert None and "" into free (the most common value) 
biz_reviews$WiFi <- str_replace(biz_reviews$WiFi, "None", "free")
biz_reviews$WiFi <- gsub("^$", "free", biz_reviews$WiFi)

# Alcohol
biz_reviews$Alcohol <- gsub("u'", "", biz_reviews$Alcohol)
biz_reviews$Alcohol <- gsub("'", "", biz_reviews$Alcohol)
#convert none, None, "" into full_bar (same reason above)
biz_reviews$Alcohol <- gsub("none", "full_bar", biz_reviews$Alcohol)
biz_reviews$Alcohol <- gsub("None", "full_bar", biz_reviews$Alcohol)
biz_reviews$Alcohol <- gsub("^$", "full_bar", biz_reviews$Alcohol)

# NoiseLevel
biz_reviews$NoiseLevel <- gsub("u'", "", biz_reviews$NoiseLevel)
biz_reviews$NoiseLevel <- gsub("'", "", biz_reviews$NoiseLevel)
#convert None and "" into average
biz_reviews$NoiseLevel <- gsub("None", "average", biz_reviews$NoiseLevel)
biz_reviews$NoiseLevel <- gsub("^$", "average", biz_reviews$NoiseLevel)

# RestaurantsAttire
biz_reviews$RestaurantsAttire <- gsub("u'", "", biz_reviews$RestaurantsAttire)
biz_reviews$RestaurantsAttire <- gsub("'", "", biz_reviews$RestaurantsAttire)
#convert None and "" into casual
biz_reviews$RestaurantsAttire <- gsub("None", "casual", biz_reviews$RestaurantsAttire)
biz_reviews$RestaurantsAttire <- gsub("^$", "casual", biz_reviews$RestaurantsAttire)

# Smoking
biz_reviews$Smoking <- gsub("u'", "", biz_reviews$Smoking)
biz_reviews$Smoking <- gsub("'", "", biz_reviews$Smoking)
#convert None and "" into no
biz_reviews$Smoking <- gsub("None", "no", biz_reviews$Smoking)
biz_reviews$Smoking <- gsub("^$", "no", biz_reviews$Smoking)

# BYOBCorkage
biz_reviews$BYOBCorkage <- gsub("u'", "", biz_reviews$BYOBCorkage)
biz_reviews$BYOBCorkage <- gsub("'", "", biz_reviews$BYOBCorkage)
#convert None and "" into no
biz_reviews$BYOBCorkage <- gsub("None", "no", biz_reviews$BYOBCorkage)
biz_reviews$BYOBCorkage <- gsub("^$", "no", biz_reviews$BYOBCorkage)

# AgesAllowed
biz_reviews$AgesAllowed <- gsub("u'", "", biz_reviews$AgesAllowed)
biz_reviews$AgesAllowed <- gsub("'", "", biz_reviews$AgesAllowed)
#convert None and "" into 21plus
biz_reviews$AgesAllowed <- gsub("None", "21plus", biz_reviews$AgesAllowed)
biz_reviews$AgesAllowed <- gsub("^$", "21plus", biz_reviews$AgesAllowed)

# RestaurantsPriceRange2
#convert None and "" into 2
biz_reviews$RestaurantsPriceRange2 <- gsub("None", "2", biz_reviews$RestaurantsPriceRange2)
biz_reviews$RestaurantsPriceRange2 <- gsub("^$", "2", biz_reviews$RestaurantsPriceRange2)


# Create dummy variables from binary (True/False) variables
biz_reviews$WiFi <- factor(biz_reviews$WiFi)
biz_reviews$Alcohol <- factor(biz_reviews$Alcohol)
biz_reviews$NoiseLevel <- factor(biz_reviews$NoiseLevel)
biz_reviews$RestaurantsAttire <- factor(biz_reviews$RestaurantsAttire)
biz_reviews$Smoking <- factor(biz_reviews$Smoking)
biz_reviews$BYOBCorkage <- factor(biz_reviews$BYOBCorkage)
biz_reviews$AgesAllowed <- factor(biz_reviews$AgesAllowed)
biz_reviews$RestaurantsPriceRange2 <- factor(biz_reviews$RestaurantsPriceRange2)


biz_reviews <- makedummies(biz_reviews, basal_level = TRUE)


# Convert None or "" into FALSE for binary (True/False) variables
cols <- c("RestaurantsTableService", "BikeParking", "BusinessAcceptsCreditCards", "RestaurantsReservations", "WheelchairAccessible", "Caters", "OutdoorSeating", "RestaurantsGoodForGroups", "HappyHour", "BusinessAcceptsBitcoin", "HasTV", "DogsAllowed", "RestaurantsTakeOut", "RestaurantsDelivery", "GoodForKids", "ByAppointmentOnly", "AcceptsInsurance", "GoodForDancing", "BYOB", "CoatCheck", "DriveThru", "Corkage", "touristy", "hipster", "divey", "intimate", "trendy", "upscale", "classy", "casual", "romantic", "garage", "street", "validated", "lot", "valet", "dessert", "latenight", "lunch", "dinner", "breakfast", "brunch", "perms")
for(i in cols){
  biz_reviews[[i]] <- gsub("None", "False", biz_reviews[[i]])
  biz_reviews[[i]] <- gsub("^$", "False", biz_reviews[[i]])
}

# Change values for perms varriable
biz_reviews$perms <- gsub("'straightperms':False", "False", biz_reviews$perms)
biz_reviews$perms <- gsub("'straightperms':True", "True", biz_reviews$perms)

# Change all NAs for other variables into FALSE
biz_reviews[is.na(biz_reviews)] <- FALSE 

# Create new variables about business days
biz_reviews <- biz_reviews %>% 
  mutate(monday_open = ifelse(hours.Monday == "", 0, 1)) %>% 
  mutate(tuesday_open = ifelse(hours.Tuesday == "", 0, 1)) %>% 
  mutate(wednesday_open = ifelse(hours.Wednesday == "", 0, 1)) %>% 
  mutate(thursday_open = ifelse(hours.Thursday == "", 0, 1)) %>% 
  mutate(friday_open = ifelse(hours.Friday == "", 0, 1)) %>% 
  mutate(saturday_open = ifelse(hours.Saturday == "", 0, 1)) %>% 
  mutate(sunday_open = ifelse(hours.Sunday == "", 0, 1)) %>% 
  mutate(num_days_open = monday_open+tuesday_open+wednesday_open+
           thursday_open+friday_open+saturday_open+
           sunday_open) %>% 
  mutate(weekends_open = ifelse(saturday_open == 1 & sunday_open == 1, 1, 0)) %>% 
  mutate(weekdays_open = ifelse(monday_open == 1 & tuesday_open == 1 &
                                  wednesday_open == 1 & thursday_open == 1 &
                                  friday_open == 1, 1, 0))


## Weather Data 

# Extract only date out of date & time
biz_reviews <- biz_reviews %>%  
  mutate(date_wo_hour = substr(date, 1, 10))

# Functions for weather API

weather_api <- function(df){
  loc <- df["city"]
  date <- df["date_wo_hour"]
  key <- "5RZLNZQUMMCTTWX8NCPMWCVWG"
  url <- paste0("https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline/",
                loc, "/",
                date, "/", date,
                "?unitGroup=metric&include=days&key=", key, "&contentType=json")
  tryCatch({
    weather <- fromJSON(url)
    weather_info <- c(weather$days$tempmax, weather$days$tempmin, weather$days$temp,
                      weather$days$precip, weather$days$humidity, weather$days$windspeed)
    return(weather_info)
  },
  error = function(e){
    weather_info <- c(NA, NA, NA, NA, NA, NA)
    return(weather_info)
  }
  )
}


biz_reviews$weather_info <- t(apply(biz_reviews, 1, weather_api))

biz_reviews <- biz_reviews %>% 
  mutate(tempmax = weather_info[,1]) %>%
  mutate(tempmin = weather_info[,2]) %>%
  mutate(tempavg = weather_info[,3]) %>%
  mutate(precip = weather_info[,4]) %>%
  mutate(humidity = weather_info[,5]) %>%
  mutate(windspeed = weather_info[,6])

biz_reviews <- biz_reviews %>%
  select(-weather_info, -date_wo_hour)


## Traffic Data 
# Import Station ID Location
setwd("/Users/nick/Documents/APAN/2. Spring 2022/5205 Frameworks & Methods II/Group project/Complementary data/")
station_map <- read.csv("Station ID.csv", stringsAsFactors = F)
# Remove NAs and duplicates
station_map %>%
  drop_na(Station_ID) %>%
  select(Station_ID, Latitude, Longitude) -> station_map
station_map <- station_map[!duplicated(station_map$Station_ID),]
# convert lat&lng to zipcode
library(zipcodeR)
search_zip <- function(lat, lng){
  search_radius(lat = lat, lng = lng, radius = 50)$zipcode[1]
}
station_map$postal_code <- mapply(search_zip,station_map$Latitude, station_map$Longitude)
sum(is.na(test))
station_map$postal_code <- as.integer(station_map$postal_code)
# convert zipcode to city
station_map %>%
  left_join(ZipCodes[,c("postal_code", "PlaceName")], by = "postal_code") -> station_map

# Import Traffic data
setwd("/Users/nick/Documents/APAN/2. Spring 2022/5205 Frameworks & Methods II/Group project/Complementary data/US Traffic 2015 - 2020/")
# Import 2015
traffic_2015 <- read.table("TMAS_2015.csv", 
                           header = T, 
                           sep = ",", 
                           colClasses = c("NULL", "NULL","NULL", "NULL", "NULL", "character", "NULL", "NULL","numeric","numeric","numeric","NULL","NULL" ,"numeric","NULL")
)

names(traffic_2015)[names(traffic_2015) == 'Vehicle.Count'] <- 'Vehicle_Count'
names(traffic_2015)[names(traffic_2015) == 'Station.ID'] <- 'Station_ID'
head(traffic_2015)
# Clean Station ID
traffic_2015$Station_ID <- as.integer(str_remove(traffic_2015$Station_ID, "A"))
traffic_2015 %>%
  drop_na(Station_ID) -> traffic_2015
# Map traffic and location
traffic_2015 %>%
  left_join(station_map[,c("Station_ID","PlaceName")], by = "Station_ID") -> traffic_2015
# NA check after mapping
length(traffic_2015$PlaceName)
sum(is.na(traffic_2015$PlaceName))/length(traffic_2015$PlaceName)
# Aggregate
traffic_2015 %>%
  group_by(PlaceName,Year,Month,Day) %>%
  summarize(Avg_Vehicle = mean(Vehicle_Count)) %>%
  ungroup() -> traffic_2015
# Combine Date
traffic_2015$Month <- ifelse(traffic_2015$Month < 10, paste("0",as.character(traffic_2015$Month),sep=""),traffic_2015$Month)
traffic_2015$Day <- ifelse(traffic_2015$Day < 10, paste("0",as.character(traffic_2015$Day),sep=""),traffic_2015$Day)
traffic_2015$Date <- as.Date(paste("20",traffic_2015$Year,"-" ,traffic_2015$Month, "-", traffic_2015$Day, sep = ""), format = "%Y-%m-%d")
# Strip
traffic_2015 %>%
  select(Date, PlaceName, Avg_Vehicle) -> df_traffic
rm(traffic_2015)
# Column check
for (i in 2015:2020){
  df <- read.csv(paste("TMAS_",as.character(i),".csv", sep = ""), 
                 stringsAsFactors = F, 
                 nrows = 100)
  print(i)
  print(colnames(df))
}
# Loop for 2016 - 2020
for (i in 2016:2020){
  if (i == 2016){
    selected_columns <- c("NULL", "NULL","NULL", "NULL", "NULL", "character", "NULL", "NULL","numeric","numeric","numeric","NULL","NULL" ,"numeric","NULL")
  } else {
    selected_columns <- c("NULL", "NULL","NULL", "NULL", "NULL", "character", "NULL", "NULL","numeric","numeric","numeric","NULL","NULL" ,"NULL","numeric","NULL")
  }
  
  df <- read.table(paste("TMAS_",as.character(i),".csv", sep = ""), 
                   header = T, 
                   sep = ",", 
                   colClasses = selected_columns
  )
  names(df)[names(df) == 'Vehicle.Count'] <- 'Vehicle_Count'
  names(df)[names(df) == 'Station.ID'] <- 'Station_ID'
  head(df)
  # Clean Station ID
  df$Station_ID <- as.integer(str_remove(df$Station_ID, "A"))
  df %>%
    drop_na(Station_ID) -> df
  # Map traffic and location
  df %>%
    left_join(station_map[,c("Station_ID","PlaceName")], by = "Station_ID") -> df
  # NA check after mapping
  length(df$PlaceName)
  sum(is.na(df$PlaceName))/length(df$PlaceName)
  # Aggregate
  df %>%
    group_by(PlaceName,Year,Month,Day) %>%
    summarize(Avg_Vehicle = mean(Vehicle_Count)) %>%
    ungroup() -> df
  # Combine Date
  df$Month <- ifelse(df$Month < 10, paste("0",as.character(df$Month),sep=""),df$Month)
  df$Day <- ifelse(df$Day < 10, paste("0",as.character(df$Day),sep=""),df$Day)
  df$Date <- as.Date(paste("20",df$Year,"-" ,df$Month, "-", df$Day, sep = ""), format = "%Y-%m-%d")
  # Strip
  df %>%
    select(Date, PlaceName, Avg_Vehicle) -> df
  # Combine to the main df
  df_traffic <- rbind(df_traffic, df)
  rm(df)
}


## Demographic Data 
setwd("/Users/nick/Documents/APAN/2. Spring 2022/5205 Frameworks & Methods II/Group project/Processed Data")

data3 <- read.csv('data2.csv', stringsAsFactors = F)
data3$County <- gsub(" ","", data3$County)
data3$State <- gsub(" ","", data3$State)

rest_zip <- read.csv("rest_zip.csv", stringsAsFactors = F)
rest_zip$county <- gsub(" ", "", rest_zip$county)
rest_zip$state_name <- gsub(" ", "", rest_zip$state_name)

#Map state_name to restaurant table
state_id <- zipcode$state_id
state_name <- zipcode$state_name
df5 <- data.frame(state_id, state_name)
df5 <- df5[!duplicated(df5),]
rest_state <- left_join(restaurant, df5, by=c("state"="state_id"))


#Rest_demo table
Postal_code <- zipcode$zip
county <- zipcode$county_name
df4 <- data.frame(Postal_code, county)
df4
df4$Postal_code <- as.character(df4$Postal_code)
rest_zip <- left_join(rest_state, df4, by=c("postal_code" = "Postal_code"))
rest_zip <- na.omit(rest_zip)
write.csv(rest_zip, "~/Desktop/Columbia_University/APAN 5205/Demographic New Data/rest_zip.csv")
str_trim(rest_zip$county, "right")
rest_zip$county <- gsub(" ", "", rest_zip$county)
str_trim(rest_zip$state_name, "right")
rest_zip$state_name <- gsub(" ", "", rest_zip$state_name)

rest_demo <-left_join(rest_zip, data3, by=c("county" = "County","state_name" = "State"))
rest_demo <- merge(rest_zip, data3, by = c("county" = "County", "state_name" = "State"), all.x = True)

#Use rest_demo table to run the dummy variable code. Then we can get restaurants1
restaurants1 <- paste("Categories", "Noodles" , "Indian" , "BubbleTea" , "chinese" , "japanese", sep = "|")
restaurants1 <- restaurants1 %>%
  dplyr::filter(., grepl(Asian_rest, categories))

restaurants1 <- restaurants1[-c(1:10, 12:14, 16:17, 21:23)]
restaurants1 <- restaurants1[-5]


#####################################################
##### Modeling

### Q1
## Logistic regression

# Drop unnecessary columns for modeling

biz_reviews <- biz_reviews %>%
  select(-name, -address, -postal_code, -latitude, -longitude, -categories,
         -hours.Monday, -hours.Tuesday, -hours.Wednesday, -hours.Thursday,
         -hours.Friday, -hours.Saturday, -hours.Sunday, -monday_open, -tuesday_open,
         -wednesday_open, -thursday_open, -friday_open, -saturday_open, -sunday_open,
         -city, -state, -review_count, -is_open, -attributes, -hours, -review_id,
         -user_id, -text, -date, -business_id, -stars_x, -Y, -useful, -funny,
         -cool, -stars)


# Create 3 groups based on ratings (low, middle, high)
biz_reviews <- biz_reviews %>%
  mutate(rate_groups = ifelse(stars_y <= 2, 0, 
                              ifelse(stars_y < 4, 1, 2)))

# Create subset where rate_groups is either 0 (low) or 2 (high)
row_nums <- which(biz_reviews$rate_groups %in% c(0, 2))
biz_reviews_sub <- biz_reviews[row_nums,]

# Create a category variable based on rate_group
biz_reviews_sub$rating_hilo = factor(biz_reviews_sub$rate_groups, labels = c('low','high'), ordered = T)

biz_reviews_sub <- biz_reviews_sub %>% 
  select(-rate_groups)

# Logistic regression
df_log <- biz_reviews_sub %>% 
  select(where(~n_distinct(.) > 1))
df_log <- df_log %>% 
  mutate(high_rate = ifelse(rating_hilo == "high", 1, 0)) %>% 
  select(-rating_hilo, -stars_y)

model1 = glm(high_rate~.,data=df_log,family='binomial')
model1
summary <- summary(model1)
summary

# Focus on significant variables (p < 0.05)
signif <- summary(model1)$coeff[-1,4] < 0.05
sig_vars <- names(signif)[signif == TRUE] 
sig_vars

# Get top 5 variables with highest coeff. out of significant variables
coeff <- as.data.frame(summary$coefficients)

top5 <- coeff[sig_vars,] %>% 
  arrange(-Estimate) %>% 
  slice(1:5) %>% 
  select(Estimate)

# Get top 5 variables with lowest coeff. out of significant variables
last5 <- coeff[sig_vars,] %>% 
  arrange(Estimate) %>% 
  slice(1:5) %>% 
  select(Estimate)


# # Visualize logistics 
top_last <- rbind(top5,last5)
nterpret_coef <- function(x){
  100*(exp(x)-1)
}
logis_10 <- sapply(top_last, FUN = interpret_coef)

logis_10 %>%
  data.frame("features" = row.names(top_last), "values" = .) %>%
  mutate(color = ifelse(Estimate >= 0, "positive", "negative")) %>%
  arrange(desc(Estimate)) %>%
  ggplot(., aes(x = reorder(features, Estimate), y = Estimate, fill = color)) +
  geom_bar(width = 0.7, stat="identity") +
  coord_flip() +
  geom_text(aes(label = sprintf("%.1f",Estimate)), size = 5) +
  xlab("Features") +
  ylab("%Change in Possiblility to Get Good Ratings") +
  ggtitle("Key Drivers of Getting Good/Bad Ratings") +
  theme(legend.position="none")

# XGBoost
df_xgb <- biz_reviews %>% 
  select(-rate_groups)

matrix <- model.matrix(stars_y ~.,df_xgb)[,-1]
xgboost <- xgboost(matrix, 
                   label = df_xgb$stars_y,
                   nrounds=100,
                   verbose = 0)
importance_matrix = xgb.importance(colnames(matrix), model = xgboost)
importance_matrix
xgb.plot.importance(importance_matrix[1:11,])

## Topic Modeling 
# Start with Good 
biz_reviews_good <- biz_reviews[biz_reviews$stars_y == 5,]
library(tm); library(SnowballC); library(magrittr)
corpus = Corpus(VectorSource(biz_reviews_good$text))
corpus = 
  corpus%>%
  tm_map(content_transformer(tolower))%>%
  tm_map(content_transformer(FUN = function(x)gsub(pattern = 'http[[:alnum:][:punct:]]*', replacement = ' ',x = x)))%>%
  tm_map(removePunctuation)%>%
  tm_map(removeWords, c(stopwords('english'),'game','play','gaming','games','player','players','playing', 'played'))

dict = findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(biz_reviews_good$review))),lowfreq = 0)
dict_corpus = Corpus(VectorSource(dict))

corpus = 
  corpus %>%
  tm_map(stemDocument)%>%
  tm_map(stripWhitespace)

dtm = DocumentTermMatrix(corpus)
xdtm = removeSparseTerms(dtm,sparse = 0.95)
xdtm_good = as.data.frame(as.matrix(xdtm))
colnames(xdtm_good) = stemCompletion(x = colnames(xdtm_good),dictionary = dict_corpus,type = 'prevalent')
colnames(xdtm_good) = make.names(colnames(xdtm_good))
# Remove all-zeros reviews
xdtm_good = xdtm_tfidf_good[which(rowSums(xdtm_good)!=0),]
# Topic model
library(topicmodels)
set.seed(123)
k <- 20
topic_good = LDA(x = xdtm_good,k = k, method="Gibbs",control=list(iter = 25, verbose = 25))
# See top 20 words in each topic
terms(topic_good,20)
# Now for each doc, find just the top-ranked topic
gammaDF <- as.data.frame(topic_good@gamma) 
names(gammaDF) <- c(1:k)
toptopics <- as.data.frame(cbind(document = row.names(gammaDF), 
                                 topic = apply(gammaDF,1,function(x) names(gammaDF)[which(x==max(x))])))
toptopics[21:30,]
# Modify topics here
mod_topic <- function(x){
  if (x == 10){ # tastes
    result  = "Taste"
  } else if (x == 13){ # Food quality 
    result  = "Food quality"
  } else if (x == 15){ # Vegan Options
    result  = "Vegan Options"
  } else if (x == 17){ # Price
    result  = "Price"
  } else if (x == 5){ # Staff
    result  = "Staff"
  } else if (x == 14){ # Location
    result  = "Location"
  } else { # Others
    result = 7
  }
  return(result)
}
# Prep Data before visualize 
toptopics$topic <- sapply(toptopics$topic , FUN = mod_topic)
toptopics %>%
  .[.$topic != 7,] %>%
  group_by(topic) %>%
  summarize(count = n()*100/nrow(.)) %>%
  arrange(desc(count)) -> mydata
mydata$count <- sapply(mydata$count, round)
# Visualize 
library(waffle)
waffle(mydata, 
       title = "Distribution of Things\nCustomers Are Happy About",
       legend_pos = "bottom",
       glyph_size = 15)

# Now for bad reviews
biz_reviews_bad <- biz_reviews[biz_reviews$stars_y == 1,]
library(tm); library(SnowballC); library(magrittr)
corpus = Corpus(VectorSource(biz_reviews_bad$text))
corpus = 
  corpus%>%
  tm_map(content_transformer(tolower))%>%
  tm_map(content_transformer(FUN = function(x)gsub(pattern = 'http[[:alnum:][:punct:]]*', replacement = ' ',x = x)))%>%
  tm_map(removePunctuation)%>%
  tm_map(removeWords, c(stopwords('english'),'game','play','gaming','games','player','players','playing', 'played'))

dict = findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(biz_reviews_bad$review))),lowfreq = 0)
dict_corpus = Corpus(VectorSource(dict))

corpus = 
  corpus %>%
  tm_map(stemDocument)%>%
  tm_map(stripWhitespace)

dtm = DocumentTermMatrix(corpus)
xdtm = removeSparseTerms(dtm,sparse = 0.95)
xdtm_bad = as.data.frame(as.matrix(xdtm))
colnames(xdtm_bad) = stemCompletion(x = colnames(xdtm_bad),dictionary = dict_corpus,type = 'prevalent')
colnames(xdtm_bad) = make.names(colnames(xdtm_bad))
# Remove all-zeros reviews
xdtm_bad = xdtm_tfidf_good[which(rowSums(xdtm_bad)!=0),]
# Topic model
library(topicmodels)
set.seed(123)
k <- 30
topic_bad = LDA(x = xdtm_bad,k = k, method="Gibbs",control=list(iter = 25, verbose = 25))
# See top 20 words in each topic
terms(topic_bad,20)
# Now for each doc, find just the top-ranked topic
gammaDF <- as.data.frame(topic_bad@gamma) 
names(gammaDF) <- c(1:k)
toptopics <- as.data.frame(cbind(document = row.names(gammaDF), 
                                 topic = apply(gammaDF,1,function(x) names(gammaDF)[which(x==max(x))])))
toptopics[21:30,]
# Modify topics here
mod_topic <- function(x){
  if (x %in% c(1,6,27)){ # tastes
    result  = "Taste"
  } else if (x == 24){ # Food quality and hygienic
    result  = "Food quality"
  } else if (x == 21){ # Price
    result  = "Price"
  } else if (x %in% c(3,10,15,28)){ # Staff
    result  = "Staff"
  } else if (x %in% c(4,5,12,13,30)){ # Seating & Order Management
    result  = "Seating & Order Management"
  } else if (x %in% c(2,9)){ # Drive-Thru & Delivery
    result  = "Drive-Thru & Delivery"
  } else { # Others
    result = 7
  }
  return(result)
}
# Prep Data before visualize 
toptopics$topic <- sapply(toptopics$topic , FUN = mod_topic)
toptopics %>%
  .[.$topic != 7,] %>%
  group_by(topic) %>%
  summarize(count = n()*100/nrow(.)) %>%
  arrange(desc(count)) -> mydata
mydata$count <- sapply(mydata$count, round)
# Visualize 
library(waffle)
waffle(mydata, 
       title = "Distribution of Things\nCustomers Are Not Happy About",
       legend_pos = "bottom",
       glyph_size = 15)


### Q2
## Logistic regression and XGBoost
# (2.1) Weather
review_weather <- # connect to the Yu's cleaning part 
dim(review_weather)
colnames(review_weather)
colnames(review_weather)[2] <- "Date"
# Fix Y
review_weather$Y = ifelse(review_weather$stars_y >= 4, 2, ifelse(review_weather$stars_y <= 2, 0, 1))
# Impute na with means
review_weather$tempavg[is.na(review_weather$tempavg)]<-mean(review_weather$tempavg,na.rm=TRUE)
review_weather$precip[is.na(review_weather$precip)]<-mean(review_weather$precip,na.rm=TRUE)
review_weather$humidity[is.na(review_weather$humidity)]<-mean(review_weather$humidity,na.rm=TRUE)
review_weather$windspeed[is.na(review_weather$windspeed)]<-mean(review_weather$windspeed,na.rm=TRUE)
# select relevant columns
review_weather_cut <- review_weather[,c(2,3,16,19,27:30)]
# convert function 
temp_func <- function(x) {
  if (x > 28){
    result = "Hot"
  } else if (x > 20){
    result = "Warm"
  } else if (x > 15){
    result = "Perfect"
  } else if (x > 5){
    result = "Chill"
  } else if (x <= 5){
    result = "Freezing"
  } else{
    result = "Perfect"
  }
}

prepcip_func <- function(x) {
  if (x == 0){
    result = "No rain"
  } else if (x > 7){
    result = "Heavy_rain"
  } else if (x > 2){
    result = "Moderate_rain"
  } else{
    result = "Light_rain"
  }
} 

humid_func <- function(x) {
  if (x > 70){
    result = "Humid"
  } else if (x < 30){
    result = "Dry"
  } else{
    result = "Normal"
  }
} 

windspeed_func <- function(x) {
  if (x > 40){
    result = "High_wind"
  } else if (x > 25){
    result = "Windy"
  }  else{
    result = "Breezy"
  }
} 
review_weather_cut$tempavg <- sapply(review_weather_cut$tempavg, FUN = temp_func)
review_weather_cut$precip <- sapply(review_weather_cut$precip, FUN = prepcip_func)
review_weather_cut$humidity <- sapply(review_weather_cut$humidity, FUN = humid_func)
review_weather_cut$windspeed <- sapply(review_weather_cut$windspeed, FUN = windspeed_func)
# Slice for only relevant variables (again)
review_weather_cut <- review_weather_cut[, c(3,4, 5:8)]
head(review_weather_cut)
# XGBoost
library(caret)
library(rpart)
library(rpart.plot)
library(ModelMetrics)
library(xgboost)
set.seed(123)
split <- createDataPartition(review_weather_cut$stars_y, p = 0.9, list = F)
train1 <- review_weather_cut[split,]
test1 <- review_weather_cut[-split,]
train_matrix1 <- model.matrix(stars_y ~. -Y,train1)[,-1]
test_matrix1 <- model.matrix(stars_y ~. -Y,test1)[,-1]
xgboost1 <- xgboost(train_matrix1, 
                    label = train1$Y,
                    nrounds=100,
                    verbose = 0)
importance_matrix1 = xgb.importance(colnames(train_matrix1), model = xgboost1)
importance_matrix1
xgb.plot.importance(importance_matrix1[1:11,])

# Logistic regression
library(tidytable)
review_weather_cut %>%
  select(-stars_y) %>% 
  get_dummies.(-Y) %>%
  select(-tempavg, -precip, -humidity, -windspeed) -> review_weather_cut_dummies

review_weather_cut_dummies <- review_weather_cut_dummies[review_weather_cut_dummies$Y != 1,]
review_weather_cut_dummies$Y <- ifelse(review_weather_cut_dummies$Y == 2, 1, review_weather_cut_dummies$Y )

set.seed(123)
split <- createDataPartition(review_weather_cut_dummies$Y, p = 0.9, list = F)
train_logis1 <- review_weather_cut_dummies[split,]
test_logis1 <- review_weather_cut_dummies[-split,]

library(glmnet)
logis1 <- glm(Y ~., data = train_logis1, family = "binomial")
logis1$coef

interpret_coef <- function(x){
  100*(exp(x)-1)
}
logis_weather <- sapply(logis1$coef, FUN = interpret_coef)[-1]
# Visualize logistics 
logis_weather %>%
  data.frame("features" = names(.), "values" = .) %>%
  mutate(color = ifelse(values >= 0, "positive", "negative")) %>%
  arrange(desc(values)) %>%
  ggplot(., aes(x = reorder(features, values), y = values, fill = color)) +
  geom_bar(width = 0.7, stat="identity") +
  coord_flip() +
  geom_text(aes(label = sprintf("%.1f",values)), size = 5) +
  xlab("Features") + 
  ylab("%Change in Possiblility to Get Good Ratings") +
  ggtitle("Key Drivers of Getting Good/Bad Ratings") +
  theme(legend.position="none")

# (2.2) Traffic
head(df_traffic) # calls from the cleaning part above
colnames(df_traffic)[2] <- "city"
# Calculate quantile by city
df_traffic %>%
  group_by(city) %>%
  mutate(Q1st = quantile(Avg_Vehicle, 0.15, na.rm =T),
         Q2nd = quantile(Avg_Vehicle, 0.35, na.rm =T),
         Q3rd = quantile(Avg_Vehicle, 0.60, na.rm =T),
         Q4th = quantile(Avg_Vehicle, 0.85, na.rm =T)
  ) %>%
  ungroup() %>%
  mutate(traffic = ifelse(is.na(Avg_Vehicle) == T, "Normal traffic", 
                          ifelse(Avg_Vehicle > Q4th, "Extremely Heavy traffic", 
                                 ifelse(Avg_Vehicle > Q3rd, "Heavy traffic", 
                                        ifelse(Avg_Vehicle > Q2nd, "Normal traffic", 
                                               ifelse(Avg_Vehicle > Q1st, "Light traffic", "Very light traffic")))))
  ) -> df_traffic
df_traffic[6010:6020,]
# Merge with reviews
review_weather_cut <- review_weather[,c(2,3,16,19)]
review_weather_cut %>% 
  inner_join(df_traffic[,c("Date", "city", "traffic")], by = c("Date", "city")) -> df_merge
dim(df_merge)
head(df_merge)
# Select only relevant columns
df_merge <- df_merge[,c("Y","stars_y", "traffic")]

# XGBoosting
set.seed(123)
split <- createDataPartition(df_merge$stars_y, p = 0.9, list = F)
train2 <- df_merge[split,]
test2 <- df_merge[-split,]
train_matrix2 <- model.matrix(stars_y~. -Y,train2)[,-1]
test_matrix2 <- model.matrix(stars_y~. -Y,test2)[,-1]
library(xgboost)
xgboost2 <- xgboost(train_matrix2, 
                    label = train2$stars_y,
                    nrounds=100,
                    verbose = 0)

importance_matrix2 = xgb.importance(colnames(train_matrix2), model = xgboost2)
xgb.plot.importance(importance_matrix2[1:4,])

# Logistic regression 
library(tidytable)
df_merge %>%
  get_dummies.(-Y)  %>%
  select(1, 9:13) %>%
  select(-5) -> df_merge_dummies
df_merge_dummies <- df_merge_dummies[df_merge_dummies$Y != 1,]
df_merge_dummies$Y <- ifelse(df_merge_dummies$Y == 2, 1, df_merge_dummies$Y )
set.seed(19)
split <- createDataPartition(df_merge_dummies$Y, p = 0.9, list = F)
train_logis2 <- df_merge_dummies[split,]
test_logis2 <- df_merge_dummies[-split,]

logis2 <- glm(Y ~., data = train_logis2, family = "binomial")
logis2$coef
interpret_coef <- function(x){
  100*(exp(x)-1)
}
logis_traffic <- sapply(logis2$coef, FUN = interpret_coef)[-1]
# Visualize logistics 
logis_traffic %>%
  data.frame("features" = names(.), "values" = .) %>%
  mutate(color = ifelse(values >= 0, "positive", "negative")) %>%
  arrange(desc(values)) %>%
  ggplot(., aes(x = reorder(features, values), y = values, fill = color)) +
  geom_bar(width = 0.7, stat="identity") +
  coord_flip() +
  geom_text(aes(label = round(values,1)), size = 5) +
  xlab("Features") + 
  ylab("%Change in Possiblility to Get Good Ratings") +
  ggtitle("Key Drivers of Getting Good/Bad Ratings") +
  theme(legend.position="none")


### Q3
## Logistic regression and XGBoost
# Kexin
restaurants1 <- read.csv("restaurants1.csv", stringsAsFactors = F)
Asian_rest <- paste("Categories", "Noodles" , "Indian" , "BubbleTea" , "chinese" , "japanese", sep = "|")
restaurants1 <- restaurants1 %>%
  dplyr::filter(., grepl(Asian_rest, categories))

restaurants1 <- restaurants1[-c(1:10, 12:14, 16:17, 21:23)]
restaurants1 <- restaurants1[-5]
restaurants1 <- restaurants1[restaurants1$Y != 1,]
restaurants1$Y = ifelse(restaurants1$Y == 2, 1,restaurants1$Y)

restaurants1 %>%
  select(3, 5:19) -> restaurants1


View(restaurants1)

# XGBoosting
train_model1 <- model.matrix(Y~.,restaurants1)[,-1]
library(xgboost)
xgboost <- xgboost(train_model1, 
                   label = restaurants1$Y,
                   nrounds=1000,
                   verbose = 0)

importance_matrix1 = xgb.importance(colnames(train_model1), model = xgboost)
importance_matrix1
summary(importance_matrix1)
xgb.plot.importance(importance_matrix1)

# Logistic regression 
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
restaurants1 %>%
  select(-Y) -> restaurants1_logis

restaurants1_logis <- as.data.frame(lapply(restaurants1_logis, min_max_norm))
restaurants1_logis <- cbind(restaurants1_logis, restaurants1$Y)
colnames(restaurants1_logis)[15] <- "Y"


logis3 <- glm(Y ~., data = restaurants1, family = "binomial")
logis3$coef
interpret_coef <- function(x){
  100*(exp(x)-1)
}
logis_demo<- sapply(logis3$coef, FUN = interpret_coef)[-1]

# Visualize logistics 
logis_demo %>%
  data.frame("features" = names(.), "values" = .) %>%
  .[.$features != "Female",] %>%
  mutate(color = ifelse(values >= 0, "positive", "negative")) %>%
  arrange(desc(values)) %>%
  ggplot(., aes(x = reorder(features, values), y = values, fill = color)) +
  geom_bar(width = 0.7, stat="identity") +
  coord_flip() +
  geom_text(aes(label = round(values,1)), size = 5) +
  xlab("Features") + 
  ylab("%Change in Possiblility to Get Good Ratings") +
  ggtitle("Key Drivers of Getting Good/Bad Ratings") +
  theme(legend.position="none")















