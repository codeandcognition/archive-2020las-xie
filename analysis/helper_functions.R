data_path <- "./data"

# given part of file name (to string match), return relative path to file name
# of first value found
get_dir <- function(file_name_part, dir_path = data_path) {
  contents <- list.files(dir_path)
  fname <- contents[grepl(file_name_part, contents, fixed = TRUE)]
  if(!identical(fname, character(0))) {
    path <- paste0(dir_path, "/", fname)
    return(path)
  }
  warning(paste("Couldn't find file with", file_name_part, 
                "in name. Looking in directory", dir_path))
}

# Given email (from survey), return corresponding uid or NA if none found
get_uid <- function(target_email, data = data_auth, 
                    data_email = data_email_match) {
  email_clean <- tolower(trimws(toString(target_email)))
  uid <- data[data$email == email_clean, "uid"][1]
  
  # if not found, try to use other email from data_email_match to get uid
  if(identical(uid, character(0)) || is.null(uid) || is.na(uid)) {
    print(paste0("No UID found for ", email_clean, ". Checking for alternates..."))
    alt_email <- data_email %>% 
      dplyr::filter(post_survey_email == target_email) %>% 
      dplyr::select(codeitz_email) %>% pull(1)
    if(is_email(alt_email)) {
      uid <- data[data$email == alt_email, "uid"][1]
      print(paste0("Alt email found (", alt_email, "). UID is ", uid, ". All good!"))
    } else {
      print(paste0("No UID found for ", email_clean, ". RISK OF DATA LOSS."))
    }
  }
  
  if(identical(uid, character(0))) {
    uid <- NA
  }
  return(uid)
}

# given uid, get email (from firebase/Codeitz)
get_email <- function(uid, data = data_auth) {
  data[data$uid == uid, "email"]
}

# check if value is email by looking for "@" in it
is_email <- function(raw_input) {
  if(is.null(raw_input)) {
    return(FALSE)
  }
  input <- as.character(raw_input)
  if(identical(input, character(0)) || str_length(input) < 2) {
    return(FALSE)
  }
  output <- grepl("@", input, fixed = TRUE)
  return(output)
}

# Given uid, return string for condition {C1, E1, C2}.
# If no condition can be found, return NULL
get_condition <- function(uid, data = data_log) {
  if(!is.na(uid) && length(uid) > 0) {
    return( data[[uid]][["condition"]][[1]] )
  } 
  return(NA)
}

# given uid, return value `prop` (column name) from data_auth
# if prop is not a valid column name, throw a warning
get_from_auth <- function(inp_uid, prop, data = data_auth) {
  if(!prop %in% colnames(data)) {
    warning(paste(prop, "not a valid column name (in data_auth?)"))
  } else {
    data[data["uid"] == inp_uid, prop][[1]]
  }
}

# convert server time (e.g. 1.57092e+12) to date time
convert_time <- function(time_as_int) {
  as.POSIXct(time_as_int/1000, origin="1970-01-01", tz="GMT")
}

# given question header (e.g. "Q1") and dataframe (e.g. data_pre), 
# get avg self-efficacy for each participant
get_avg_se <- function(data = data_pre, question_header = "Q1") {
  se_cols <- colnames(data)[grepl(question_header, colnames(data))]
  ONE <- "1 (not at all confident)"
  SEVEN <- "7 (absolutely confident)"
  
  # replace ONE with 1, SEVEN w/ 7. cardinal sin of mutating original data here...
  data[se_cols][data[se_cols] == ONE] <- 1
  data[se_cols][data[se_cols] == SEVEN] <- 7
  # convert se cols to numeric and then get mean
  return(rowMeans(sapply(data[se_cols], as.numeric)))
}

# given a data frame and a vector of question numbers (for mindset questions),
# translate data into mindsets
get_mindset <- function(data = data_pre, questions = c("Q2", "Q3", "Q4")) {
  revalue_map <- c("Strongly disagree" = -3, "Disagree" = -2, 
                   "Mostly disagree" = -1, "Mostly agree" = 1, 
                   "Agree" = 2, "Strongly agree" = 3)
  # turn text to numbers
  mindset <- sapply(data[questions], function(x){revalue(x, revalue_map)})
  dims <- dim(mindset)
  mindset <- as.numeric(mindset) # convert to numeric
  dim(mindset) <- dims # convert to numeric loses dimensions, so fixing that
  
  mean_mindset <- rowMeans(mindset)
  return(as.character(lapply(mean_mindset, function(x){
    if(is.na(x)) {
      # print(paste("No mindset for", x)) # uncomment for flag
      return(NA)
    } 
    if(abs(x) <= 1) return("mix")
    if(x > 1) return("fixed")
    if(x < 1) return("growth")
  })))
}

# Given a vector of self-efficacy values, return labels of "low", "med", "high"
# based on which 1/3 of data value is in (e.g. "low" if bottom 1/3)
get_self_efficacy_label <- function(se_vec, thresholds = c(.33, .67)) {
  percentiles <- quantile(se_vec, thresholds)
  thres_low <- as.numeric(percentiles[1])
  thres_high <- as.numeric(percentiles[2])
  
  output <- sapply(se_vec, function(val){
    if(is.na(val)) {
      return(NA)
    }
    if(val <= thres_low) {
      return("low")
    } else if (val <= thres_high) {
      return("med")
    } else {
      return("high")
    }
  })
  return(output)
  
}

# Given a user id, return a dataframe where each row is an exercise attempt
# df columns: {id, eid, timestamp, correct, resulting_recs} 
get_exercise_attempts <- function(uid) {
  submissions <- data_log[[uid]]$Data$AnswerSubmission
  ids <- c()
  exercise_ids <- c()
  timestamps <- c()
  correct <- c()
  resulting_recs <- c()
  
  for(id in names(submissions)) {
    x <- submissions[[id]]
    ids <- append(ids, id)
    exercise_ids <- append(exercise_ids, x$exerciseId)
    timestamps <- append(timestamps, convert_time(x$timestamp))
    correct <- append(correct, x$correctness)
    resulting_recs <- append(resulting_recs, 
                             paste(names(x$resultingRecommendations), collapse=", "))
  }
  
  df_user_exercises <- data.frame(id = ids, eid = exercise_ids,
                                  timestamp = timestamps,
                                  correct = correct,
                                  resulting_recs = resulting_recs)
  
  return(df_user_exercises)
}


# given a uid (as character), return a vector of exercise ids for exercises 
# user attempted (only include correct ones if only_correct is TRUE)
get_exercises_completed <- function(uid, only_correct = TRUE) {
  df_user_exercises <- get_exercise_attempts(uid)
  if(nrow(df_user_exercises) == 0) {
    print(paste0("WARNING: No exercises found for ", uid, 
                 ".", PATH_LOG, "May not be up to date."))
  }
  if(only_correct) {
    results <- df_user_exercises %>% filter(correct == T) %>% 
      dplyr::select(eid) %>% distinct() %>% pull(1)
  } else {
    results <- df_user_exercises %>% dplyr::select(eid) %>% distinct() %>% pull(1)
  }
  
  return(as.vector(results))
}

populate_concept_ex <- function(data = data_concept_exercise) {
  eid <- c()
  type <- c() # {READ, WRITE}
  concept <- c()
  
  for(target_concept in names(data)) {
    for(exercise_id in data[[target_concept]]$READ) {
      eid <- append(eid, exercise_id)
      type <- append(type, "READ")
      concept <- append(concept, target_concept)
    }
    
    for(exercise_id in data[[target_concept]]$WRITE) {
      eid <- append(eid, exercise_id)
      type <- append(type, "WRITE")
      concept <- append(concept, target_concept)
    }
  }
  
  return(data.frame(eid, type, concept))
}

# for Latex sparkmaps, get list with vector that assigns values to bins (bins)
# and vector that assigns values to bin index (bin_nums)
get_bins <- function(data_vec, n_bins = N_BINS) {
  # assign values to bins of uniform size
  bins <- bin(data_vec, nbins = n_bins, method = "length")
  
  bin_map <- setNames(c(1:length(levels(bins))), levels(bins))
  bin_nums <- revalue(bins, bin_map) # index of bin (1 to n_bins)
  
  list(bins = bins, bin_nums = bin_nums)
}

# given a value, a range, and a number of bins,
# return number (1:num_bins) corresponding to what bin value belongs in
# assuming ( ] (exclude lower bound, include upper bound)
get_bin_index <- function(inp, min_val, max_val, num_bins = N_BINS) {
  ceiling((inp - min_val) / (max_val - min_val) * num_bins)
}

# given names of value column and corresponding column's bin indexes,
# output latex for sparklines that can be put into a sparkline
# example call: get_sparklines("test_score", "bin_test_score_num")
get_sparklines <- function(cname_val, cname_bin_num, data_inp = df_scores, bin_width = 0.88,
                           iqr_line_height = -0.25) {
  
  # get global range for get_bin_index()
  data_vec_all <- data_inp %>% dplyr::select(as.name(cname_val)) %>% pull(1)
  min_val <- min(data_vec_all)
  max_val <- max(data_vec_all)
  
  # get max value in any bin across 3 conditions (to normalize bin height)
  max_bin_val <- -1
  for(cond in CONDITIONS) {
    max_bin_val <- max(max_bin_val, 
                       max(xtabs(as.formula(paste0("~",cname_bin_num)), 
                                 data = data_inp %>% dplyr::filter(condition == cond))))
  }
  
  outputs <- list()
  for(cond in c(E1, C2, C1)) {
    # normalized bin height
    bin_heights <-  xtabs(as.formula(paste0("~",cname_bin_num)), data = data_inp %>% dplyr::filter(condition == cond)) / max_bin_val
    
    bumper_width <- (1.0 - bin_width) / 2
    n_bins <- length(bin_heights)
    x_inds <- seq(0 + bumper_width, 1 - bumper_width, length.out = n_bins)
    ind <- c(1:n_bins)
    
    # combine histogram into single string
    output_hist <- paste0("\\sparkspike ", x_inds, " ", round(bin_heights, digits = 2),
                          " % ", ind, "\n")
    
    data_vec_cond <- data_inp %>% 
      dplyr::filter(condition == cond) %>% 
      dplyr::select(as.name(cname_val)) %>% pull(1)
    
    # get desc. stats.
    med <- median(data_vec_cond)
    iqr <- IQR(data_vec_cond)
    iqr_low <- quantile(data_vec_cond, 0.25)
    iqr_high <- quantile(data_vec_cond, 0.75)
    
    x_low <- x_inds[get_bin_index(iqr_low, 
                                  min_val, max_val, num_bins = n_bins)]
    x_high <- x_inds[get_bin_index(iqr_high, 
                                   min_val, max_val, num_bins = n_bins)]
    x_med <- x_inds[get_bin_index(med, min_val, max_val, num_bins = n_bins)]
    
    output_iqr <- paste("\\spark", x_low, iqr_line_height, x_high,
                        iqr_line_height, "/ %IQR:", iqr_low, "-", iqr_high, "\n")
    
    output_median <- paste("\\sparkdot", x_med, iqr_line_height,
                           "magenta % median:", med, "\n")
    
    output_comment <- paste("%", cond, cname_val, "\n")
    
    outputs[cond] <- cat(output_comment, output_iqr, output_hist, output_median)
    
  }
  
  outputs
  
  
}
