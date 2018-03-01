
#Movie data set
#--------------------------------------------------------------------------#
#Input File:
movie <- read.csv(file =
                         "movie_data.csv",
                       stringsAsFactors=FALSE
                      )
#Display the data
movie
#Summary of the dataset
summary(movie)
movie$Adjusted.Gross <- NULL #Removing unwanted column
#-----------------------------------------------------------------------#
#General Statistics

dim(movie) #608 rows and 17 columns
names(movie)  #row names like Day of week, Director,Movie title...

#displays first 5 rows
head(movie) 
#displays last 5 rows
tail(movie)
#--------------------------------------------------------------------------#
# Case 1: profit with respect to famous genre (action, comedy, adventure,animation)
#filter 1 for filtering the genere
filt1 <- movie$Genre %in% c("action","comedy","adventure","animation","horror")

#filter 2 for filtering based on studio
filt2 <- movie$Studio %in% c("Lionsgate","Sony","Universal","Paramount Pictures","WB","Fox","DreamWorks","Colombia Pictures")


#applying filters to the data set to get desired records
mov2 <- movie[filt1&filt2,]
mov2
#Graph
library(ggplot2)
ggplot(data=movie,aes(x=Genre, y=movie$Gross.p.US)) + geom_jitter(aes(size=Budget,colour=Studio)) + geom_boxplot(alpha=0.6)
#we can see that action movies are the highest grossed movies followed by comedy
#Next we boxplot with number of movies per genre.
genreplot <- ggplot(movie,aes(Genre))
genreplot + geom_bar()
#

#case 2: Trying to find how profit varies with respect to days of the week
install.packages("ggplot2")  
library("ggplot2")
ggplot(data=movie,aes(x=Day.of.Week)) + geom_bar(colour="red")  
#we can see that most of the movies were released on friday, the fun part is, not a single movie was released on mondays.

# case 3: To see which studio made the highest profit under the action movie genere
# filter based on genre
filt3 <- movie$Genre %in% c("action")
mov3 <- movie[filt3&filt2,]
mov3
#Plotting mov3
ggplot(data=mov3,aes(x=Studio,y=Gross...US)) + geom_col()

#This concludes that Warner Brothers have the highest gross in action genre, Paramount Pictures have second
#largest gross in action genre and Fox is not far from the Paramont pictures range.

#case 4 : The highest gross percent under action genre is beverly hills cop,
#the irony is it was released on a wednesday.
max(mov3$Gross...US, na.rm = TRUE)

filt4 <- (mov3$Gross...US==max(mov3$Gross...US, na.rm = TRUE))
filt4

mov4 <- mov3[filt4,]
mov4
#Wednesday, 5/12/1984- Martin Brest;s (action) Beverly Hills Cop  Paramount Pictures-IMDb.Rating 7.3
#-----------------------------------------------------------------------#

#case 5: 
#regression analysis1
#2nd version
# regression to estimate rating based on profit percentage is not best fit for the model as it had very low R squared value.
imdb_rating <- movie$IMDb.Rating
profit_percent <- movie$Profit.
movie_lens <- movie$MovieLens.Rating
#lm.out <- lm(movie_lens~profit_percent)
lm.out <- lm(profit_percent~imdb_rating)
summary(lm.out)
#plots to show profit percent and rating
plot(imdb_rating,
     profit_percent,col = rep(c("red", "blue"), each = 50),
     main = "Movie Gross Percent based on Rating",
     xlab = "imdb_rating",
     ylab = "profit_percent")
abline(lm.out)
# we can see that the movies with higher ratings have a higher gross percent but to show the models fitness, we have to show
#regression for profit based on rating
profit <-movie$Profit
rating <-movie$IMDb.Rating
lm.out <-lm(rating~profit)
summary(lm.out)
#Multiple R-squared:  0.9423,	Adjusted R-squared:  0.1654 
#F-statistic: 1.213 on 565 and 42 DF,  p-value: 0.2225
#the R squared value is 94% which shows that model is good fit based on rating.

##regression analysis2
gross <- movie$Gross
rating <- movie$IMDb.Rating
# Create regression line
lm.out <- lm(rating~gross)
# Create scatterplot and overlay regression line
x<-gross #to convert strings to numeric
x <- gsub(",|;", '', x)
# read in the data
xgross <- scan(text = x, what = 0)
plot(xgross,
     rating,col = rep(c("red", "blue"), each = 50),
     main = "Movie Rating by Gross Content",
     xlab = "Gross Value",
     ylab = "Rating")
abline(lm.out)
summary(lm.out)
#Multiple R-squared:  0.9184,	Adjusted R-squared:  -0.0534 
#F-statistic: 0.9451 on 560 and 47 DF,  p-value: 0.6275. Similarly here r square value is 91%
# for gross based on rating.
#-----------------------------------------------------------------------#
#Case 6: Corelation between 2 variables
cor.test(movie$IMDb.Rating,movie$Gross...US)

#weak relation
cor.test(movie$IMDb.Rating,movie$Profit.)

#weak relation
cor.test(movie$MovieLens.Rating,movie$Profit.)

#------------------------------------------------------------------------#
#Case 7:
#frequency distribution
#The frequency distribution of IMDB rating across action movies in the current data set
mov3

duration = mov3$IMDb.Rating 
range(duration) 

breaks = seq(1.5,9,by=0.5)
breaks
duration.cut = cut(duration, breaks, right=FALSE)
duration.freq = table(duration.cut)
duration.freq
#The highest number of movies in this data set are between the rating 6 and 8
#-----------------------------------------------------------------------#
#Binning the dates
library(zoo) # package for irregular time series of numeric vectors/matrices and factors.
yq <- as.yearqtr(as.yearmon(movie$Release.Date, "%d/%m/%Y") + 1/12)
movie$Season <- factor(format(yq, "%q"), levels = 1:4, 
                    labels = c("winter", "spring", "summer", "fall"))
library(plyr) #Tools for Splitting, Applying and Combining Data
# Save the file.
dev.off()
require(grDevices)#graphics and colors
x<- count(movie$Season) 
xx <-c(125,145,217,121)
piepercent<- round(100*xx/sum(xx))
piepercent
#Then we apply the pie function to produce its pie chart.
labels = c("winter", "spring", "summer", "fall")
pie(piepercent, labels,  col=rainbow(length(piepercent)),main="Pie Chart of Seasons")              # apply the pie function
#------------------------------------------------------------------------------#
#Discretization of budget using k means clustering
budget.cluster<-kmeans(movie$Budget, 3, nstart=20)
#storing the result of clustering
budget.cluster<- budget.cluster$cluster
#changing the column values to the cluster
movie$budgetnew <- budget.cluster

head(movie)

movie$budgetnew[(movie$budgetnew) == "1" ] <- "Low budget"
movie$budgetnew[(movie$budgetnew) == "2" ] <- "Moderate budget"
movie$budgetnew[(movie$budgetnew) == "3" ] <- "High budget"

budgetnew <- movie$budgetnew
budgetnew
#-------------------------------------------------------------------------------#
#Discretization of overseas using k means clustering
overseas.cluster<-kmeans(overseas, 3, nstart=20)
#storing the result of clustering
overseas.cluster<- overseas.cluster$cluster
#changing the column values to the cluster
movie$overseasnew <- overseas.cluster

head(movie)

movie$overseasnew[(movie$overseasnew) == "1" ] <- "Low"
movie$overseasnew[(movie$overseasnew) == "2" ] <- "Moderate"
movie$overseasnew[(movie$overseasnew) == "3" ] <- "High"

overseasnew <- movie$overseasnew
overseasnew
#----------------------------------------------------------------------------------#
movie$statesnew <- NULL
movie$budgetneww <- NULL
#Discretization of US using k means clustering
US.cluster<-kmeans(movie$US, 3, nstart=20)
#storing the result of clustering
US.cluster<- US.cluster$cluster
#changing the column values to the cluster
movie$USnew <- US.cluster

head(movie)

movie$USnew[(movie$USnew) == "1" ] <- "Low"
movie$USnew[(movie$USnew) == "2" ] <- "Moderate"
movie$USnew[(movie$USnew) == "3" ] <- "High"

USnew <- movie$USnew
USnew
#----------------------------------------------------------------------------------#
#Normalizing Movielens Rating and IMDB Rating using min-max Normalization in order to compare which has better rating.
ratings <- data.frame( "IMDB rating" = movie$IMDb.Rating, "Movielens" = movie$MovieLens.Rating, stringsAsFactors = FALSE)
summary(ratings)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

movie$ratingsNorm <- as.data.frame(lapply(ratings, normalize))
# One could also use sequence such as df[1:2]
movie$ratingsNorm <- as.data.frame(lapply(ratings[1:2], normalize))
ratingsNorm
summary(ratingsNorm)
library(plyr)
movie$newcolumn <- 1:nrow(movie)
ratingsNorm$newcolumn <- 1:nrow(ratingsNorm)
tail(ratingsNorm)
Movie<-merge(movie,ratingsNorm)
head(Movie)
# PLOTS###
# IMDB Rating and Genre relationship

#creating counts table
counts <- table(Movie$Genre, Movie$IMDB.ratingnew,
                dnn=c("Genre", "IMDB Rating"))
counts

#creating a sumtable
sumtable <- addmargins(counts, FUN = sum)
sumtable

#proportion over rows
row.margin <- round(prop.table(counts, margin = 1),4)*100
row.margin

#proportion over columns
col.margin <- round(prop.table(counts,margin = 2), 4)*100
col.margin

#clustered bar chart of Genre vs IMDB Rating
barplot(counts,
        col = c("blue", "gray", "brown", 
                "red", "green", "black", 
                "maroon", "magenta","yellow",
                "white", "gold","cyan",
                "lightblue", "chocolate","gold"),
        ylim = c(0, 150),
        ylab = "Count",
        xlab = "IMDB Rating",
        main = "Genre vs IMDB Rating",
        beside = TRUE)
legend("topright",
       c(rownames(counts)),
       col = c("blue", "gray", "brown", 
               "red", "green", "black", 
               "maroon", "magenta","yellow",
               "white", "gold","cyan",
               "lightblue", "chocolate","gold"),
       pch = 15,
       title = "Genre")
box(which = "plot",
    lty = "solid",
    col="black")

#clustered bar chart of IMDB Rating vs Genre
barplot(t(counts),
        col = c("blue", "gray", "brown", 
                "red", "green", "black", 
                "maroon", "magenta","yellow",
                "white", "gold","cyan",
                "lightblue", "chocolate","gold"),
        ylim = c(0, 150),
        ylab = "Counts",
        xlab = "Genre",
        main = "IMDB Rating vs Genre",
        beside = TRUE)
legend("topright",
       c(colnames(counts)),
       col = c("blue", "gray", "brown", 
               "red", "green", "black", 
               "maroon", "magenta","yellow",
               "white", "gold","cyan",
               "lightblue", "chocolate","gold"),
       pch = 15,
       title = "IMDB Rating")
box(which = "plot",
    lty = "solid",
    col="black")

#overlay
ggplot() +
  geom_bar(data=Movie,
           aes(x = factor(Genre),
               fill = factor(Movie$IMDB.ratingnew)),
           position = "fill") +
  scale_x_discrete("Genre") +
  scale_y_continuous("Percent") +
  guides(fill=guide_legend(title="IMDB Rating")) +
  scale_fill_manual(values=c("blue", "red", "green", "white", "yellow"))




#######################################################################################################################
#########################################################################################
#k-means clustering

#Getting out the numerical values of behavioral factors in Movie
df <- data.frame(Movie$Budget, Movie$IMDB.rating, 
                 Movie$Movielens)

View(df)
library(plyr)

str(df)
#All the data is num
#Normalization
z <- df[,-5]
m <- apply(z,2,mean)
s <- apply(z,2,sd)
z <- scale(z,m,s)
8773352746
#Calculating Euclidean distance
distance <- dist(z)
print(distance, digits=3)

#Cluster Dendogram with complete linkage
hc.c <- hclust(distance)
plot(hc.c, labels = df$Movie.IMDB.rating)
plot(hc.c, labels = df$Movie.Movielens, hang =-1)

#Cluster Dendogram with average linkage
hc.a <- hclust(distance, method = "average")
plot(hc.a, hang=-1)

#Cluster Membership
member.c <- cutree(hc.c, 3)
member.a <- cutree(hc.a, 3)
table(member.c, member.a)

#Cluster means
aggregate(z, list(member.c), mean)
aggregate(df[,-5], list(member.c), mean)

#Silhouette Plot
#library(cluster)
#plot(silhouette(cutree(hc.c, 2), distance))

#Scree Plot
wss <- (nrow(z)-1)*sum(apply(z,2,var))
for(i in 2:20) wss[i] <- sum(kmeans(z, centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within group SS")

########################################################################################
#Kmeans Clustering

kc <- kmeans(z,2)

plot(Movie$IMDB.rating~Movie$Movielens, df, col=kc$cluster)
#_________________________________________________________#
#Discretization of US using k means clustering
IMDB.cluster<-kmeans(Movie$IMDB.rating, 5, nstart=20)
#storing the result of clustering
IMDB.cluster<- IMDB.cluster$cluster
#changing the column values to the cluster
Movie$IMDB.ratingnew <- IMDB.cluster

head(Movie)

Movie$IMDB.ratingnew[(Movie$IMDB.ratingnew) <= "1" ] <- "very low"
Movie$IMDB.ratingnew[(Movie$IMDB.ratingnew) <= "2" ] <- "low"
Movie$IMDB.ratingnew[(Movie$IMDB.ratingnew) == "3" ] <- "moderate"
Movie$IMDB.ratingnew[(Movie$IMDB.ratingnew) == "4" ] <- "high"
Movie$IMDB.ratingnew[(Movie$IMDB.ratingnew) == "5" ] <- "very high"

IMDBnew <- Movie$IMDB.ratingnew
IMDBnew

#Discretization of US using k means clustering
Movielens.cluster<-kmeans(Movie$Movielens, 5, nstart=20)
#storing the result of clustering
Movielens.cluster<- Movielens.cluster$cluster
#changing the column values to the cluster
Movie$Movielensnew <- Movielens.cluster

head(Movie)

Movie$Movielensnew[(Movie$Movielensnew) <= "1" ] <- "very low"
Movie$Movielensnew[(Movie$Movielensnew) <= "2" ] <- "low"
Movie$Movielensnew[(Movie$Movielensnew) == "3" ] <- "moderate"
Movie$Movielensnew[(Movie$Movielensnew) == "4" ] <- "high"
Movie$Movielensnew[(Movie$Movielensnew) == "5" ] <- "very high"

Movielensnew <- Movie$Movielensnew
Movielensnew
head(Movie)
#-------------------------------------------------------------------------------------#
movie_train <- movie[1:65,]
movie_test <- movie[66:100,]
movie_train_labels <- movie[1:65, 1]
movie_test_labels <- movie[66:100, 1] 
install.packages('class') #Various functions for classification, including k-nearest neighbour
library(class)
movie_test_pred <- knn(train = movie_train, test = movie_test,cl = movie_train_labels, k=24)
