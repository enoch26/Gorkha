# =============================================================================
# Model run script
# =============================================================================
# Choose ONE section to run at a time.
#
# Cross-validation options:
#   - Grid CV:     CV_thin = FALSE, CV_chess = TRUE
#   - Thinning CV: CV_thin = TRUE,  CV_chess = FALSE
#   - Full model:  CV_thin = FALSE, CV_chess = FALSE
# =============================================================================


# =============================================================================
# GRID CROSS-VALIDATION
# =============================================================================

# -----------------------------------------------------------------------------
# White grid CV
# -----------------------------------------------------------------------------
CV_thin  <- FALSE
CV_chess <- TRUE

train <- "white"
trainset <- "grid"
cv_chess_resol <- c(3, 3)

source("read_data.R")
source("model.R")
source("score.R")
source("compute_time.R")

q()


# -----------------------------------------------------------------------------
# Black grid CV
# -----------------------------------------------------------------------------
CV_thin  <- FALSE
CV_chess <- TRUE

train <- "black"
trainset <- "grid"
cv_chess_resol <- c(3, 3)

source("read_data.R")
source("model.R")
source("score.R")
source("compute_time.R")

q()


# =============================================================================
# THINNING CROSS-VALIDATION
# =============================================================================
# Choose either:
#   trainset <- "thinA"
#   trainset <- "thinB"
# =============================================================================

CV_thin  <- TRUE
CV_chess <- FALSE

trainset <- "thinB"
cv_thin_resol <- c(3, 3)

source("read_data.R")
source("model.R")
source("score.R")
source("compute_time.R")

q()


# =============================================================================
# FULL MODEL FOR CASE STUDY
# =============================================================================

CV_thin  <- FALSE
CV_chess <- FALSE

trainset <- ""
cv_thin_resol <- c(3, 3)

source("read_data.R")
source("model.R")
source("score.R")
source("coefvar.R")

x_pxl <- 1000
source("pxl.R")

source("pred.R")
source("pred_zm.R")

q()


# =============================================================================
# FIT ALL LANDSLIDE DATA AND GENERATE POSTERIOR PREDICTIONS
# =============================================================================

CV_thin  <- FALSE
CV_chess <- FALSE

trainset <- ""
cv_thin_resol <- c(3, 3)

source("read_data.R")
source("model.R")
source("pred.R")
