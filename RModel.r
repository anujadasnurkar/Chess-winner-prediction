data <- read.csv("data.csv")
# calling the function summary() on our data
summary(data)

# duplicated() returns a logical vector where TRUE specifies which 
# elements of a vector or data frame are duplicates. !duplicated() means 
# that we don’t want duplicate rows
data <- data[!duplicated(data$id), ]

# Random sample indexes
train_index <- sample(1:nrow(data), 0.8 * nrow(data))
test_index <- setdiff(1:nrow(data), train_index)

# Build data_train and data_test dataframes
data_train <- data[train_index, ]
data_test <- data[test_index, ]

data_obvious <- data_train[data_train$victory_status != "resign", ]
data_toAnalyze <- data_train[data_train$victory_status == "resign", ]

predict_notResign <- function(data_obvious){
  prediction <- c()
  # we make a prediction for each observation, i.e. for each row in the data frame
  for(i in 1:nrow(data_obvious)){
    if((data_obvious[i, ]$victory_status) == "mate" || 
       (data_obvious[i, ]$victory_status) == "outoftime"){
      # if the number of turns is even - the winner is "black"
      if(data_obvious[i, ]$turns %% 2 == 0)
        prediction <- c(prediction, "black")
      else
        prediction <- c(prediction, "white")
    }
    else{ 
      # victory_status = "draw"
      # winner is "draw"
      prediction <- c(prediction, "draw")
    }
  }
  return(prediction)
}

prediction_notResign <- predict_notResign(data_obvious)
accuracy_notResign <- sum(prediction_notResign == 
                            data_obvious$winner)/nrow(data_obvious)
accuracy_notResign

info <- function(data){
  n <- nrow(data)
  
  pawn <- 1
  knight <- 3
  bishoop <- 3.5
  rook <- 5
  queen <- 9
  
  # allocating arrays
  white_score <- integer(n)
  black_score <- integer(n)
  check_black <- integer(n)
  check_white <- integer(n)
  king_moves_black <- integer(n)
  king_moves_white <- integer(n)
  
  # forming arrays for each observation, i.e. for each row in the data frame
  for(m in 1:nrow(data)){
    row <- data[m,]
    
    # we are going to analyze the moves of each game. For that we are going to 
    # make an array of strings, where each string is one move.
    # Bear in mind that odd moves are played by the white player,
    # and even moves by the black player
    tokens <- strsplit(row$moves, " ")
    tokens <- unlist(tokens)
    
    # for the games with more than 10 moves, we are going to calculate
    # numbers of chess by white/black player in the last 10 moves, as well as
    # the number of king moves by the white/black player
    if(length(tokens) > 10){
      # we want to keep the same parity format, i.e. odd moves are played
      # by the white player, and even by the black;
      # variable l will enable us just that. If the number of moves is even - we
      # will observe the last 11; if the number of moves is odd - we will observe
      # the last 10 (so the first move is always played by the white player)
      l <- ifelse(length(tokens) %% 2 == 0, 11, 10)
      tmp <- tokens[(length(tokens) - l):length(tokens)]
      
      for(i in 1:length(tmp)){
        # grepl(needle, haystack)
        # in the standard chess notation check is labeled as +
        # + is a special character, so we need to search it as "\\+", rather than "+"
        if(grepl("\\+", tmp[i])){
          # the move is odd -> the white player played it -> the check is his
          if(i %% 2 == 1)
            check_white[m] = check_white[m] + 1
          else
            check_black[m] = check_black[m] + 1
        }
        # in the standard key notation king is labeled as "K"
        if(grepl("K", tmp[i])){
          if(i %% 2 == 1)
            king_moves_white[m] = king_moves_white[m] + 1
          else
            king_moves_black[m] = king_moves_black[m] + 1
        }
      }
    }
    
    for (i in 1:length(tokens)) {
      # pawn promotion: 
      # e.g. e8=Q means that the pawn is promoted to a queen
      # when a move contains "=" we can easily parse to which figure had it been promoted,
      # since the format of these moves is always "##=#"
      # when a player promoted one of his pawns, his score should be increased for the
      # corresponding value
      if (grepl("=", tokens[i])) {
        # the last character is the character to which the pawn is promoted
        piece <- substr(tokens[i], 4, 4)
        # even move -> the black player made it -> the promotion is hiss -> 
        # -> increase his score -> continue to the next move
        if (i %% 2 == 0) {
          if (piece == "R") { # it's rook
            black_score[m] = black_score[m] + rook
            break
          }
          else if(piece == "N"){ # it's knight
            black_score[m] = black_score[m] + knight
            break
          }
          else if(piece == "B"){ # it's bishop
            black_score[m] = black_score[m] + bishoop
            break
          }
          else if(piece == "Q"){ # it's queen
            black_score[m] = black_score[m] + queen
            break
          }
        }
        else{
          # odd move -> the white player made it -> the promotion is hiss -> 
          # -> increase his score -> continue to the next move
          if (piece == "R") { # it's rook
            white_score[m] = white_score[m] + rook
            break
          }
          else if (piece == "N") { # it's knight
            white_score[m] = white_score[m] + knight
            break
          }
          else if (piece == "B") { # it's bishop
            white_score[m] = white_score[m] + bishoop
            break
          }
          else if (piece == "Q") { # it's queen
            white_score[m] = white_score[m] + queen
            break
          }
        }
      }
      # captures:
      # e.g. Bxe5 = the bishop captures the piece standing on the field e5
      # when we encounter a move with the capture label (Bxe5), we should go through all
      # of the previous moves (backwards!) until we find the first occurrence of the
      # field where the capture occurred (e5). That occurrence would look like Re5 or Rxe5 or e5. 
      # Either way, the first character signals the piece being captured (if there is no capital
      # letter as a first character, the pawn had been captured)
      if (grepl("x", tokens[i])) {
        # field where the capture occurred is always the last two characters of the move
        field <- substr(tokens[i], 3, 4)
        # searching backwards through the previous moves in order to determine the piece
        # being captured
        for (j in (i - 1):1) {
          if (grepl(field, tokens[j])) {
            # when the first occurrence (backwards) of the field had been found, the piece
            # captured will be extracted from the first character
            piece <- substr(tokens[j], 1, 1)
            # even move -> it's made by the black player -> black captured white ->
            # -> subtract the piece value from the white
            if (i %% 2 == 0) {
              if (piece == "R") { # it's rook
                white_score[m] = white_score[m] - rook
                break
              }
              else if (piece == "N") { # it's knight
                white_score[m] = white_score[m] - knight
                break
              }
              else if (piece == "B") { # it's bishop
                white_score[m] = white_score[m] - bishoop
                break
              }
              else if (piece == "Q") { # it's queen
                white_score[m] = white_score[m] - queen
                break
              }
              else{ # it's pawn
                white_score[m] = white_score[m] - pawn
                break
              }
            }
            # odd move -> it's made by the white player -> white captured black ->
            # -> subtract the piece value from the black
            else{
              if (piece == "R") { # it's rook
                black_score[m] = black_score[m] - rook
                break
              }
              else if(piece == "N"){ # it's knight
                black_score[m] = black_score[m] - knight
                break
              }
              else if(piece == "B"){ # it's bishop
                black_score[m] = black_score[m] - bishoop
                break
              }
              else if(piece == "Q"){ # it's queen
                black_score[m] = black_score[m] - queen
                break
              }
              else{ # it's pawn
                black_score[m] = black_score[m] - pawn
                break
              }
            }
          }
        }
      }
    }
  }
  return(data.frame(white_score, 
                    black_score, 
                    check_white, 
                    check_black, 
                    king_moves_white, 
                    king_moves_black))
}

# calculating info for data_toAnalyze
info_res <- info(data_toAnalyze)
white_score <- info_res$white_score
black_score <- info_res$black_score
check_white <- info_res$check_white
check_black <- info_res$check_black
king_moves_white <- info_res$king_moves_white
king_moves_black <- info_res$king_moves_black

# calculating info for data_test
info_res_test <- info(data_test)
white_score_test <- info_res_test$white_score
black_score_test <- info_res_test$black_score
check_white_test <- info_res_test$check_white
check_black_test <- info_res_test$check_black
king_moves_white_test <- info_res_test$king_moves_white
king_moves_black_test <- info_res_test$king_moves_black

# column winner has to be transformed for logistic regression
winner <- as.factor(data_toAnalyze$winner)

# predictors are being calculated on the data_toAnalyze
a <- data_toAnalyze$white_rating - data_toAnalyze$black_rating
b <- white_score - black_score
c <- data_toAnalyze$turns %% 2
d <- check_white - check_black
e <- king_moves_black - king_moves_white
model <- glm(winner ~ a + b + c + d + e, family = binomial)
# we can see that all of our predictors are significant
summary(model)

# predictions are being tested on data_test
newdata <- data.frame(a = data_test$white_rating - data_test$black_rating,
                      b = white_score_test - black_score_test, 
                      c = data_test$turns%%2, 
                      d = check_white_test - check_black_test,
                      e = king_moves_black_test - king_moves_white_test)

glm.probs <- predict(model, newdata = newdata, type = "response")
glm.predict <- ifelse(glm.probs > 0.5, "white", "black")

# comparing our predicted values with the actual ones
accuracy.glm <- mean(glm.predict == data_test$winner)

# total accuracy can be calculated by the formula
# accuracy <- (accuracy_on_set1 * size(set1) + 
#              accuracy_on_set2 * size(set2))/size(disjointUnion(set1, set2))
# have in mind that data_train was disjointly partitioned on data_obvious and 
# data_toAnalyze
accuracy <- (accuracy_notResign * nrow(data_obvious) + 
               accuracy.glm * nrow(data_toAnalyze))/nrow(data_train)

glue("total accuracy in 1 iteration: {accuracy}")

# accuracy_total is the vector containing accuracy values for all 10 iterations
accuracy_total <- c()

for(i in 1:10){
 # Random sample indexes
 train_index <- sample(1:nrow(data), 0.8 * nrow(data))
 test_index <- setdiff(1:nrow(data), train_index)
 
 # Build data_train and data_test data frames
 data_train <- data[train_index, ]
 data_test <- data[test_index, ]
 
 # partition to data that can be obviously predicted, and data that needs to be predicted
 # via logistic regression
 data_obvious <- data_train[data_train$victory_status != "resign", ]
 data_toAnalyze <- data_train[data_train$victory_status == "resign", ]
 
 # make obvious predictions and calculate its accuracy
 prediction_notResign <- predict_notResign(data_obvious)
 accuracy_notResign <- sum(prediction_notResign ==
                           data_obvious$winner)/nrow(data_obvious)
 
 # calculating info for data_toAnalyze
 info_res <- info(data_toAnalyze)
 white_score <- info_res$white_score
 black_score <- info_res$black_score
 check_white <- info_res$check_white
 check_black <- info_res$check_black
 king_moves_white <- info_res$king_moves_white
 king_moves_black <- info_res$king_moves_black
 # calculating info for data_test
 info_res_test <- info(data_test)
 white_score_test <- info_res_test$white_score
 black_score_test <- info_res_test$black_score
 check_white_test <- info_res_test$check_white
 check_black_test <- info_res_test$check_black
 king_moves_white_test <- info_res_test$king_moves_white
 king_moves_black_test <- info_res_test$king_moves_black
 
 # column winner has to be transformed for logistic regression
 winner <- as.factor(data_toAnalyze$winner)
 
 # predictors are beeing calculated on the data_toAnalyze
 a <- data_toAnalyze$white_rating - data_toAnalyze$black_rating
 b <- white_score - black_score
 c <- data_toAnalyze$turns %% 2
 d <- check_white - check_black
 e <- king_moves_black - king_moves_white

 # make model
 model <- glm(winner ~ a + b + c + d + e, family = binomial)
 
 # we can see that all of our predictors are significant
 # predictions are being tested on data_test
 newdata <- data.frame(a = data_test$white_rating - data_test$black_rating,
                       b = white_score_test - black_score_test,
                       c = data_test$turns%%2,
                       d = check_white_test - check_black_test,
                       e = king_moves_black_test - king_moves_white_test)
 
 # predicting the values in newdata
 glm.probs <- predict(model, newdata = newdata, type = "response")
 glm.predict <- ifelse(glm.probs > 0.5, "white", "black")
 
 # comparing our predicted values with the actual ones
 accuracy.glm <- mean(glm.predict == data_test$winner)
 
 # accuracy of each iteration can be calculated by the formula
 # accuracy <- (accuracy_on_set1 * size(set1) +
 #              accuracy_on_set2 * size(set2))/size(disjointUnion(set1, set2))
 # have in mind that data_train was disjointedly partitioned on data_obvious and
 # data_toAnalyze
 # accuracy_total is vector containing accuracy values from each iteration
 accuracy_total <- c(accuracy_total, (accuracy_notResign * nrow(data_obvious) +
                                     accuracy.glm * nrow(data_toAnalyze))/
                                     nrow(data_train))
}

# print the final accuracy
glue("total accuracy in 10 iterations: {mean(accuracy_total)}")