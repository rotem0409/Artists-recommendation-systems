Recommendation Systems
================
Rotem Klein

 

In this project I will present two recommendation systems for artists,
using two algorithms: Item-based Collaborative Filtering and Association
Rules for the csv file, “lastfm-matrix-germany”, where each row
represents a user, and each column represents an artist.

You can see my R Shiny app here:
<https://rotemklein.shinyapps.io/BD_project/>

The report presents general statistical information about the data,
running the two algorithms using R source code, and comparing accuracy
between the two algorithms.

``` r
require(dplyr)  
require(ggplot2)
require(arules) 
require(stringr)
require(pander)
```

``` r
data.germany <- read.csv(file="lastfm-matrix-germany.csv")
pander(head(data.germany[,c(1,3:8)]))
```

| user | abba | ac.dc | adam.green | aerosmith | afi | air |
|:----:|:----:|:-----:|:----------:|:---------:|:---:|:---:|
|  1   |  0   |   0   |     0      |     0     |  0  |  0  |
|  33  |  0   |   0   |     1      |     0     |  0  |  0  |
|  42  |  0   |   0   |     0      |     0     |  0  |  0  |
|  51  |  0   |   0   |     0      |     0     |  0  |  0  |
|  62  |  0   |   0   |     0      |     0     |  0  |  0  |
|  75  |  0   |   0   |     0      |     0     |  0  |  0  |

The table shows the first part of the data, each user rated 1 if he
listened to the artist and 0 if otherwise.

 

``` r
#delete user column
data.germany.ibs <- (data.germany[,!(names(data.germany) %in% c("user"))])

# deleting the empty 31 rows
data.germany.ibs <- data.germany.ibs[!apply(data.germany.ibs == 0, 1, all), ] 
```

The user’s column was deleted since it was not needed. There are 31
empty rows in the original data which were deleted too.

 

# Descriptive statistics

![](Artists-recommendation-systems_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
This graph shows the number of plays of all users per artist You can see
in the graph the 10 most played artists, “Coldplay” and “Linkin Park”
leading the list.

 

## Train & Test

I will divide the information into train data and test data: The train
data will contain the first 859 users and the test data will contain the
remaining 357 users. Using test data I will check the accuracy of the
two algorithms at the end.

``` r
train <- data.germany.ibs[1:859,]
test <- data.germany.ibs[860:nrow(data.germany.ibs),]
```

# Item-based Collaborative Filtering:

Collaborative item-based filtering takes the similarity between the
consumption and the history of the items (played artists) User-based
collaborative filtering weights similarities between user consumption
history. I will calculate the similarities of plays for each artist with
the other artists, using the Jaccard Index which is a statistical value
often used to compare the similarity between sets of binary variables.
It measures the size ratio of the intersection between the sets divided
by the length of its union.

``` r
CF <- function(dataset){
  # Create a helper function
    
  Jaccard_index = function (x, y) {
    a = sum(x == 1 & y == 1)
    b = sum(x == 1 & y == 0)
    c = sum(x == 0 & y == 1)
    return (a / (a + b + c))
}
  
  # Create a placeholder dataframe listing item vs. item
  dataset.similarity  <- matrix(NA, nrow = ncol(dataset), ncol = ncol(dataset),
                                dimnames = list(colnames(dataset), colnames(dataset)))
  
  # fill in those empty spaces with Jaccard index similarities
  # Loop through the columns
  for(i in 1:ncol(dataset)) {
    # Loop through the columns for each column
    for(j in 1:ncol(dataset)) {
      # Fill in placeholder with Jaccard index similarities
      dataset.similarity[i,j] <- Jaccard_index(as.matrix(dataset[i]),as.matrix(dataset[j]))
    }
  }
  
  # Back to dataframe
  dataset.similarity <- as.data.frame(dataset.similarity)
  
  
  # Get the top 5 neighbours for each
  dataset.neighbours <- matrix(NA, nrow = ncol(dataset.similarity), ncol = 6,
                               dimnames = list(colnames(dataset.similarity)))
  
  for(i in 1:ncol(dataset)) 
  {
    dataset.neighbours[i,] <- (t(head(n = 6,
                                      rownames(dataset.similarity[order(dataset.similarity[,i],
                                                                        decreasing=TRUE),][i]))))
    
  }
  
  dataset.neighbours <- as.data.frame(dataset.neighbours[, 1:ncol(dataset.neighbours)])
  colnames(dataset.neighbours) <- c("Artist","Option 1", "Option 2", "Option 3", 
                                    "Option 4", "Option 5")
  
  
  return(dataset.neighbours)  
}

write.csv(CF(train),"CF.csv", row.names = FALSE)
pander(head(read.csv("CF.csv")))
```

|      Artist      |   Option.1    |    Option.2    |       Option.3        |
|:----------------:|:-------------:|:--------------:|:---------------------:|
| a.perfect.circle |     tool      |    deftones    |         dredg         |
|       abba       |    madonna    | elvis.presley  |         queen         |
|      ac.dc       | the.offspring |  iron.maiden   | red.hot.chili.peppers |
|    adam.green    |  the.strokes  | the.libertines |     babyshambles      |
|    aerosmith     |      u2       |  led.zeppelin  |     lenny.kravitz     |
|       afi        |   anti.flag   |  rise.against  |      millencolin      |

Table continues below

|    Option.4     |       Option.5        |
|:---------------:|:---------------------:|
| nine.inch.nails | the.smashing.pumpkins |
|      mika       |    michael.jackson    |
|  black.sabbath  |       metallica       |
|    the.kooks    |       interpol        |
|      ac.dc      |     jack.johnson      |
|      nofx       |        sum.41         |

The above table shows 5 recommendations for each artist displayed in the
left column.

(Code reference:
<http://www.salemmarafi.com/code/collaborative-filtering-r/>)  

# Association Rules:

The Apriori algorithm finds connections between artists. Apriori find
these relationships based on the frequency of artists who are in the
same item set. The algorithm uses prior knowledge of frequent item
features. The rules are determined according to pre-determined
indications: Support - indicates the frequency of occurrences in the
data set. Confidence - measures the times which the same user who
listened to artist ‘x’ also listened to artist ‘y’.

``` r
AR <- function(data_set){
  #changing format of the data set
  dataset_format <- apply(dataset, 2, as.logical)
  dataset_format <- as(dataset_format, "transactions")
  n <- ncol(dataset)
  dec_df <- data.frame(matrix(ncol = 6, nrow = n))
  colnames(dec_df) <- c("Artist", "Option 1", "Option 2", "Option 3", "Option 4", "Option 5")
  
  # apriori algorithm
  for(i in 1:n){
    apriori_rules <- apriori(dataset_format, 
                             parameter = list(supp = 0.003, conf = 0.08, target = "rules", 
                                              minlen = 2),
                             appearance = list(default = "rhs",lhs = colnames(dataset[i])),
                             control = list(load = FALSE))
    
    
    decision_sort <- sort(apriori_rules, by = "confidence", decreasing = TRUE)
    decision <- inspect(decision_sort[1:5])
    rules_dataframe <- as(decision, 'data.frame')
    dec_df[i,] <- rbind(c(rules_dataframe$lhs[1], rules_dataframe$rhs))
    dec_df[i,] <- str_remove_all(dec_df[i,], "[{}]")
    
  }
  
  return(dec_df)
  
}

write.csv(AR(train),"AR.csv", row.names = FALSE)
```

``` r
pander(head(read.csv("AR.csv")))
```

|      Artist      |       Option.1        |       Option.2        |
|:----------------:|:---------------------:|:---------------------:|
| a.perfect.circle |         tool          |   system.of.a.down    |
|       abba       |      the.beatles      |        madonna        |
|      ac.dc       | red.hot.chili.peppers |       metallica       |
|    adam.green    |       coldplay        |       radiohead       |
|    aerosmith     |       metallica       | red.hot.chili.peppers |
|       afi        |     rise.against      |     billy.talent      |

Table continues below

|     Option.3     |    Option.4     |    Option.5     |
|:----------------:|:---------------:|:---------------:|
|      dredg       |     incubus     |    metallica    |
|      queen       |    coldplay     | robbie.williams |
|    rammstein     | die.toten.hosen |   the.beatles   |
|   the.beatles    |    the.kooks    |  foo.fighters   |
|      ac.dc       |  jack.johnson   |   linkin.park   |
| system.of.a.down |  fall.out.boy   |   linkin.park   |

  The above table shows 5 recommendations for each artist displayed in
the left column

 

# Simulation:

In this simulation, I will compare two sets of data. One dataset shows
the user and artists which the user actually had heard from the test
data. Another dataset shows the artists that the system recommended to
the user. The accuracy calculation checks some of the artists that the
system recommended for the user, to the artists which the user has
actually heard. The function of accuracy calculation checks how many
artists the user had actually heard - were recommended by the algorithm.

``` r
set.seed(123)
prec <- function(recommended_1, df_1){
  cnames <- colnames(df_1[which(df_1==1)])
  s <- sum(cnames %in% recommended_1)
  
  return(s/5)
}

sim_iteration <- function(original_df, recommended_df){
  rez_vec <- numeric()
  for (mc in 1:20) {
    prec_vec <- numeric()
    
    for(i in 1:nrow(original_df)){
      user_chosen <- original_df[i, ]
      all_artists <- user_chosen[which(user_chosen == 1)]
      all_artists <- as.data.frame(all_artists)
      artist <- sample(all_artists, 1)
      df.artist <- colnames(artist)
      df.rec.index <- which(recommended_df$Artist == df.artist)
      rec <- recommended_df[df.rec.index,2:6]
      
      prec_vec[i] <- prec(rec, user_chosen)
      
    }
    m <- mean(prec_vec)  
    rez_vec[mc] <- m  
  }
  
return(mean(rez_vec))
}

recommended_df1 <- read.csv("CF.csv")
recommended_df2 <- read.csv("AR.csv")

CF_sim <- sim_iteration(original_df = test, recommended_df = recommended_df1)
AR_sim <- sim_iteration(original_df = test, recommended_df = recommended_df2)
CF_AR_df <- rbind(c(CF_sim,AR_sim))
colnames(CF_AR_df) <- c("Collaborative Filtering", "Association Rules")
pander(CF_AR_df)
```

| Collaborative Filtering | Association Rules |
|:-----------------------:|:-----------------:|
|         0.1719          |      0.2428       |

The “Association Rules” algorithm seems to be better. For better results
it is recommended to divide the train and test groups using a random
division.
