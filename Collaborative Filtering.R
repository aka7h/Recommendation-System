#Implementing Recommender System Using Collaborative Filtering

library(recommenderlab)

data("MovieLense")

movie.m <- as.data.frame(as.matrix(MovieLense@data))

#To avoid sparse and also for better prediction, selecting users who rated more that 50 movies
#and movies that are rated by more that 100 users.

rowCounts.gen <- function(df,min_ratings=50){
  c <- data.frame(userID=rownames(df),totalRatings=as.vector(rowSums(df != 0)))
  f <- c[order(-c$totalRatings),]
  return(as.numeric(f[f$totalRatings > min_ratings,1]))
}

colCounts.gen <- function(df,min_ratings=100){
  c <- data.frame(movieID=colnames(df),totalRatings=as.vector(colSums(df != 0)))
  f <- c[order(-c$totalRatings),]
  return(as.numeric(f[f$totalRatings > min_ratings,1]))
}

user.rating.count <- sort(rowCounts.gen(movie.m),decreasing = FALSE)
movie.rating.count <- sort(colCounts.gen(movie.m), decreasing = FALSE)

final_rating_data <- movie.m[user.rating.count,movie.rating.count]

#lets scale the data
scal.data <- t(scale(t(final_rating_data))[,])

# #Now lets build an item base collaborative filtering
# pearson.corr <- cor(final_rating_data)

#lets create a train and test dataset from the given final_rating _data

m.m <- as(as.matrix(scal.data),"realRatingMatrix")
eval_set <- evaluationScheme(data= m.m, method="split",given=50, train=.8,goodRating=3, k=3)
train_d <- as.matrix(getRatingMatrix(getData(eval_set, 'train')))
test_d <- as.matrix(getRatingMatrix(getData(eval_set, 'known')))
dd <- as.data.frame(test_d)
tt <- as.data.frame(train_d)
t <- test_d[1,]


#here we are going to recommend the movies for the first user.

#finding the similarity between a train data and a single test user
mov_sim <- cor(t(train_d),test_d[5,], use="pairwise")

#finding the top movies for thst user
top_sim_movies <- order(mov_sim, decreasing = TRUE)[1:50]

#ratings
rating_mov <- order((mov_sim[top_sim_movies,]%*% train_d[top_sim_movies,]), 
                    decreasing=TRUE)[1:10]

cat("We are finding other similar movies for", colnames(test_d)[5])

#lets get the movie list
# getMovieList  <- function(movieID){
#   movieNames <- colnames(final_rating_data)
#   return(movieNames[movieID])
# }

cat("Here are the recommended movie names")
recommended <- colnames(train_d)[rating_mov]
cat(recommended, sep = "\n")

#Now lets test the recommendation using recommender labs
model_ibcf <- Recommender(data=getData(eval_set,"train"), method="IBCF",
                          parameter=list(method="pearson",k=50))
predic_ibcf <- predict(model_ibcf,getData(eval_set,"known"),n=10)

cat("We are finding other similar movies for", colnames(getData(eval_set,"known"))[5])
recc_recommned <- predic_ibcf@itemLabels[as.vector(predic_ibcf@items[[5]])]
cat(recc_recommned, sep = "\n")

similarity_user=function(all_data,user_data){
  #Transpose so that users are on columns
  all_data=t(all_data)
  #Use pearson correlation
  score = cor(all_data,user_data,use = "pairwise")
  return(score)
}

cfs <- function(data,user_data,num_rec=20,num_sim=50){
  user_sim = similarity_user(data,user_data)
  #Replace NA with zero
  data_na0=data
  data_na0[is.na(data_na0)]=0
  top_sim_users=order(user_sim,decreasing = T)[1:num_sim]
  ratings=(user_sim[top_sim_users,1]%*%data_na0[top_sim_users,])
  prediction=NULL
  prediction$ratings=ratings
  #Set rating of already rated item to NA
  ratings[!is.na(user_data)]=NA
  prediction$recommendation=order(ratings,decreasing = T)[1:20]
  #To do remove already visited artists
  return(prediction)
}


#lets evaluate the results
n = nrow(test_d)
recommendation = NULL
same_as_rl=logical(n)
for (i in 1:n){
  recommendation[[i]]=cfs(train_d,test_d[i,])
  same_as_rl[i]=all(predic_ibcf@items[[i]] %in% recommendation[[i]]$recommendation)
}
table(same_as_rl)


#credit : https://rstudio-pubs-static.s3.amazonaws.com/249909_e9f45fd3d3f44cfea9b37a935b286b81.html
