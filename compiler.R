# only either one can be true
# CV_thin <- FALSE; CV_chess <- TRUE
# CV_thin <- TRUE; CV_chess <- FALSE
# source("read_data.R");source("model.R");source("pred.R");source("pred_zm.R")
# Sys.sleep(21600);
Sys.sleep(43200)

train <- "white"
# train <- "black"
cv_chess_resol <- c(3, 3)
CV_thin <- FALSE; CV_chess <- TRUE
trainset <- ""
source("read_data.R");source("model.R");source("score.R");source("compute_time.R");q()


# source("read_data.R");source("model.R");source("pred.R");source("score.R"); source("coefvar.R"); source("compute_time.R")
# source("read_data.R");source("model.R");source("score_b.R")
# source("read_data.R");source("model.R");source("score.R");q()

Sys.sleep(43200)
CV_thin <- TRUE; CV_chess <- FALSE
# trainset <- "thinA" # "thinA" or "thinB"
trainset <- "thinB"
cv_thin_resol <- c(3, 3)
# cv_thin_resol <- c(5, 5)
source("read_data.R");source("model.R");source("score.R");  source("compute_time.R"); source("coefvar.R")
source("pred_zm.R")

# source("read_data.R");source("model.R");source("score_b.R")
# source("read_data.R");source("model.R");source("score.R");q()
# source("read_data.R");source("model.R");source("score.R");q()

# source("read_data.R");source("model.R");source("pred.R")
# source("read_data.R");source("model.R");source("score.R")
# source("read_data.R");source("model.R")