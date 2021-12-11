library(recommenderlab)
library(Matrix)
library(plyr)
library(tidyverse)

############## MUCH OF THIS CODE WAS BORROWED FROM PROF LIANG #############
## WHICH WAS BORROWED FROM [https://github.com/pspachtholz/BookRecommender] ##

get_user_ratings = function(ratings_list) {
  dat = data.table(MovieID = sapply(strsplit(names(ratings_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(ratings_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
  userrating = matrix(ncol = 4, nrow = nrow(dat))
  A = rep("9999", nrow(dat))
  B = rep(1000000, nrow(dat))
  userrating= cbind(A, dat$MovieID, dat$Rating, B)
  userrating = as.data.frame(userrating)
  names(userrating) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
  
  cols.num <- c('UserID', 'MovieID', 'Rating', 'Timestamp')
  userrating[cols.num] <- sapply(userrating[cols.num],as.numeric)
  
  return(userrating)
  
}

# read in data
myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")


# extract year
movies$Year = as.numeric(unlist(
  lapply(movies$Title, function(x) substr(x, nchar(x)-4, nchar(x)-1))))

ratings = read.delim("ratings.dat", header = FALSE, sep = ':')
ratings = subset(ratings, select = c("V1", "V3", "V5", "V7"))
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')

Ratings_Aggregate = aggregate(ratings,
                              list(ratings$MovieID),
                              mean)

Number_Of_Ratings = plyr::count(ratings, vars = "MovieID")
colnames(Number_Of_Ratings) = c("MovieID", "Number.of.Ratings")

movies = transform(merge(movies,Number_Of_Ratings,by=0,all=TRUE), row.names=Row.names, Row.names=NULL)
movies = transform(merge(movies,Ratings_Aggregate,by=0,all=TRUE), row.names=Row.names, Row.names=NULL)

movies = subset(movies, select = c("MovieID.x", "Title", "Genres", "Year", "Number.of.Ratings", "Rating"))
names(movies)[names(movies) == "MovieID.x"] <- "MovieID"

movies = separate(data = movies, col = Genres, into = c('Genre.1', 'Genre.2', 'Genre.3', 'Genre.4', 'Genre.5', 'Genre.6'), sep = "\\|")

print("movies has been successfully processed")
small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))


highest_rated = function(genre){
  relevant_movies = subset(movies, movies$Genre.1==genre | movies$`Genre.2`==genre|movies$`Genre.3`==genre|movies$`Genre.4`==genre|movies$`Genre.5`==genre|movies$`Genre.6`==genre)
  #filter relevant movies
  relevant_movies = subset(relevant_movies, relevant_movies$`Number.of.Ratings`>=100)
  
  #Sort in descending order of ratings 
  newdata = relevant_movies[order(-relevant_movies$Rating),][1:20,]
  #Pick the 1st 20
  #final_list[,i] = newdata$Title[1:20]
  newdata = newdata[1:20,]
  return(newdata)
  
}



shinyServer(function(input, output, session) {
  
  # show the movies to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # books per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], style = "max-height:150")),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  # Calculate recommendations when the sbumbutton is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      ratings_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(ratings_list)
      print(ncol(user_ratings))
      ratings = subset(ratings, select = c('UserID', 'MovieID', 'Rating', 'Timestamp'))
      print(names(ratings))
      
      ratings = rbind(user_ratings, ratings)
      print("rbind has been successfully generated")
      
      i = paste0('u', ratings$UserID)
      j = paste0('m', ratings$MovieID)
      x = ratings$Rating
      tmp = data.frame(i, j, x, stringsAsFactors = T)
      Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
      rownames(Rmat) = levels(tmp$i)
      colnames(Rmat) = levels(tmp$j)
      Rmat = new('realRatingMatrix', data = Rmat)
      print("matrix has been successfully generated")
      
      f = evaluationScheme(Rmat, method="split", train=1.0, given=1, goodRating = 5)
      rec_UBCF = Recommender(getData(f, "train"), method = 'UBCF', parameter = list(normalize = 'z-score', method = 'cosine', nn = 25))
      recom = predict(rec_UBCF, Rmat['u9999'], type = 'ratings', n = 20)
      print("algorithm has run")
      
      recom_df = as(recom, "data.frame")
      print(recom_df)
      recom_df$item <- gsub('m', '', recom_df$item)
      
      recom_df <-recom_df[order(recom_df$rating),][1:20,]
      colnames(recom_df) = c("UserID", "MovieID", "Rating")
      recom_df = merge(recom_df, movies, by = 'MovieID')
      
      recom_results <- data.table(Rank = 1:20, 
                                  MovieID = recom_df$MovieID, 
                                  Title = recom_df$Title) 
      
      return(recom_results)
      
    }) # still busy
    
  }) # clicked on button
  
  
  # display the recommendations
  output$results <- renderUI({
    num_rows <- 4
    num_movies <- 5
    recom_result <- df()
    print(recom_result)
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            #print(recom_result$MovieID[(i - 1) * num_movies + j]),
            
            div(style = "text-align:center", img(src = paste0(small_image_url, recom_result$MovieID[(i - 1) * num_movies + j], '.jpg?raw=true')), style = "max-height:150"),
            div(style="text-align:center; font-size: 100%", 
                strong(recom_result$Title[(i - 1) * num_movies + j])
            )
            
        )        
      }))) # columns
    }) # rows
  }) # renderUI function
  
  
  df_genre <- eventReactive(input$button, {
    withBusyIndicatorServer("button", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      genre <- input$genre
      
      # sort, organize, and return the results
      user_genre_results <- highest_rated(genre)
      user_predicted_ids <- user_genre_results$MovieID
      
      
      recom_genre_results <- data.table(Rank = 1:20,
                                        MovieID = user_predicted_ids,
                                        Title =  user_genre_results$Title)
      
    }) # still busy
  }) # clicked on button
  
  
  
  
  
  output$genreresults <- renderUI({
    num_rows <- 4
    num_movies <- 5
    recom_genre_results <- df_genre()
    print(recom_genre_results)
    print(recom_genre_results$MovieID)
    
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            div(style = "text-align:center", img(src = paste0(small_image_url, recom_genre_results$MovieID[(i - 1) * num_movies + j], '.jpg?raw=true')), style = "max-height:150"),
            div(style="text-align:center; font-size: 100%",
                strong(recom_genre_results$Title[(i - 1) * num_movies + j])
            )
            
        )
      }))) # columns
    }) # rows
  }) # renderUI 
}) # server 
