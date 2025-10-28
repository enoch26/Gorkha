# only either one can be true
# CV_thin <- FALSE; CV_chess <- TRUE
# CV_thin <- TRUE; CV_chess <- FALSE
# source("read_data.R");source("model.R");source("pred.R");source("pred_zm.R")
# Sys.sleep(21600)
# Sys.sleep(43200)

# GRID CV-------------------------------------------------------------------
# Code to run Grid CV, run each section chunk separately
## white gridCV ------------------------------------------------------------
train <- "white"
# cv_chess_resol <- c(5, 5)
cv_chess_resol <- c(3, 3)
CV_thin <- FALSE; CV_chess <- TRUE
trainset <- ""
source("read_data.R");source("model.R");source("score.R");source("compute_time.R");q()

# Sys.sleep(20000)
## black gridCV ------------------------------------------------------------
Sys.sleep(3600)
train <- "black"
cv_chess_resol <- c(3, 3)
CV_thin <- FALSE; CV_chess <- TRUE
trainset <- ""
source("read_data.R");source("model.R");source("score.R"); source("compute_time.R");q()


# Sys.sleep(10000)
# train <- "black"
# cv_chess_resol <- c(3, 3)
# CV_thin <- FALSE; CV_chess <- TRUE
# trainset <- ""
# source("read_data.R");source("model_su.R");source("score_su.R");q()

# Sys.sleep(15000)
# train <- "white"
# cv_chess_resol <- c(3, 3)
# CV_thin <- FALSE; CV_chess <- TRUE
# trainset <- ""
# source("read_data.R");source("model_su.R");source("score_su.R"); q()
# source("compute_time.R");q()



# source("read_data.R");source("model.R");source("pred.R");source("score.R"); source("coefvar.R"); source("compute_time.R")
# source("read_data.R");source("model.R");source("score_b.R")
# source("read_data.R");source("model.R");source("score.R");q()

# THINNING CV ----------------------------------------------------------------
# Code to run Thnning CV, run either "thinA" or "thinB" separately
# either choose thinA or thinB for trainset
# Sys.sleep(3600)
CV_thin <- TRUE; CV_chess <- FALSE
# trainset <- "thinA" # "thinA" or "thinB"
trainset <- "thinB"
cv_thin_resol <- c(3, 3)
# cv_thin_resol <- c(5, 5)
source("read_data.R");source("model.R");source("score.R")

# source("model_su.R");source("score_su.R"); q()

source("compute_time.R"); q()

# FULL Model for Model Case Study --------------------------------------------------------------------
# Code to run Full model for Model Case Study
CV_thin <- FALSE; CV_chess <- FALSE
trainset <- ""
cv_thin_resol <- c(3, 3)
# cv_thin_resol <- c(5, 5)
source("read_data.R");source("model.R");source("score.R");  
# source("compute_time.R"); 
source("coefvar.R")
x_pxl <- 1000
source("pxl.R")
source("pred.R");source("pred_zm.R");q()


# fit all landslides data -------------------------------------------------
CV_thin <- FALSE; CV_chess <- FALSE
trainset <- ""
cv_thin_resol <- c(3, 3)
source("read_data.R");source("model.R");source("pred.R")

# source("read_data.R");source("model.R");source("score_b.R")
# source("read_data.R");source("model.R");source("score.R");q()
# source("read_data.R");source("model.R");source("score.R");q()

# source("read_data.R");source("model.R");source("pred.R")
# source("read_data.R");source("model.R");source("score.R")
# source("read_data.R");source("model.R")