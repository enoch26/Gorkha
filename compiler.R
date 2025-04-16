# only either one can be true
# CV_thin <- FALSE; CV_chess <- TRUE
# CV_thin <- TRUE; CV_chess <- FALSE
# source("read_data.R");source("model.R");source("pred.R");source("pred_zm.R")
# Sys.sleep(21600);
Sys.sleep(43200)
CV_thin <- FALSE; CV_chess <- TRUE
source("read_data.R");source("model.R");source("pred.R");source("score.R")
CV_thin <- TRUE; CV_chess <- FALSE
source("read_data.R");source("model.R");source("pred.R");source("score.R")

# source("read_data.R");source("model.R");source("pred.R")
# source("read_data.R");source("model.R");source("score.R")
# source("read_data.R");source("model.R")