# tons of constants used across files
source("./helper_functions.R")

PATH_PRE_SURVEY <- get_dir("pre")
PATH_POST_SURVEY <- get_dir("post")
PATH_AUTH <- get_dir("auth")
PATH_LOG <- get_dir("cyberlearning-1d4e0-Users-export")
PATH_AUTH_COL_NAMES <- "./data/meta/auth_col_names.csv"
PATH_EMAIL_MATCH <- "./data/email_matching.csv" # mapping between survey & codeitz emails, if different
PATH_GRADES <- get_dir("graded", dir_path = "./data")
PATH_CONCEPT_EX_MAP <- "./data/maps/concept_exercise_map.json"
PATH_POST_GRADES_CNANE_MAP <- "./data/maps/post_grades_cname_map.csv"
PATH_OMIT <- "./data/users_omit.csv"
PATH_FEATURE_MAP <- "./data/maps/codeitz_feature_map.csv"
N_BINS <- 12
E1 <- "E1"
C1 <- "C1"
C2 <- "C2"

# what's used in paper
UH <- "C1"
IH <- "E1"
IL <- "C2"
CONDITIONS <-  c(E1, C2, C1)