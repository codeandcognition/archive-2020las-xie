# Given a uid (string) and JSON data of the log (from Users collection),
# return dataframe where each row is an action
# columns: {id (of log), action (worldview, instruction, exercise), timestamp,
#           item_id (for exercise or instruciton, NA for worldview), concept}
get_action_log <- function(uid, json_log = data_log) {
  user_log <- json_log[[uid]]
  
  visits <- user_log$Data$NewPageVisit # list of unique actions
  
  ids <- c()
  action <- c()
  timestamp <- c()
  item_id <- c()
  concepts <- c()
  
  EXERCISE <- "exercise"
  INSTRUCTION <- "instruction"
  WORLDVIEW <- "worldview"
  
  # Given entry from NewPageVisit, return item id 
  # (e.g. READ1 for instruction, exerciseId for exercise
  # 
  get_item_id <- function(visit) {
    if (visit$pageType == EXERCISE) {
      return(visit$exerciseId)
    } else if (visit$pageType == INSTRUCTION) {
      return(paste0(visit$readOrWrite, visit$page))
    } else {
      return(NA)
    }
  }
  
  for(action_id in names(visits)) {
    x <- visits[[action_id]] # info on log
    ids <- append(ids, action_id) # id for log
    action <- append(action, x$pageType) # {exercise, instruction, worldview}
    timestamp <- append(timestamp, convert_time(x$timestamp))
    
    target_id <- get_item_id(x) # instruction id, exercise id, or null
    item_id <- append(item_id, target_id)
    
    target_concept <- NA
    if(x$pageType == "instruction") {
      target_concept <- x$concept
    } else if(x$pageType == "exercise" && str_length(target_id) > 0) {
      target_concept <- as.character(
        df_concept_ex_map %>% 
          dplyr::filter(eid == target_id) %>% 
          dplyr::select(concept) %>% pull(1)
      )
    }
    
    concepts <- append(concepts, target_concept)
    
  }
  
  # TODO: remove duplicate logs for visit instruction
  df_user_log <- data.frame(id = ids, action = action, 
                            timestamp = timestamp, item_id = item_id,
                            concept = concepts)
  
  # filter out duplicate instructions (duplicate logs made within 1 second)
  THRESHOLD <- 1
  df_user_log <- df_user_log %>% dplyr::filter(!( (action == "instruction") & 
                                                    (lag(action) == action) & 
                                                    (timestamp - lag(timestamp) 
                                                     < THRESHOLD)))
  
  return(df_user_log)
  
}

EXERCISE <- "exercise"
INSTRUCTION <- "instruction"
WORLDVIEW <- "worldview"
CONCEPT <- "concept"

NORTH <- "north"
EAST <- "east"
SOUTH <- "south"
WEST <- "west"

DIR_ASSIGNMENT <- list()
DIR_ASSIGNMENT[[EXERCISE]] = SOUTH
DIR_ASSIGNMENT[[INSTRUCTION]] = NORTH
# DIR_ASSIGNMENT[[WORLDVIEW]] = WEST
DIR_ASSIGNMENT[["(no action)"]] = WEST
DIR_ASSIGNMENT[[CONCEPT]] = EAST

# Given a dataframe where each row is a log & list mapping action to direction, 
# return dataframe with x, y as coordinates for each step 
# (first step starts (0,0) ) & euclidean distance (dist_euc)
# north: lesson. east: exercise. south: world. west: change concept
# input dataframe has following columns:
#   - action: {worldview, instruction, exercise}
#   - timestamp
#   - item_id (NA for "worldview" actions")
#   - concept
generate_points <- function(df_log, dir_assign = DIR_ASSIGNMENT) {
  AXIS_X <- "X"
  AXIS_Y <- "Y"
  
  # given prev_loc (vector of numerics), direction (north, east, south, west),
  # and coordinate (x, y),
  # return numeric value corresponding to update. Value may be same as prev_loc
  get_new_loc <- function(prev_locs, direction, coordinate) {
    prev_loc <- tail(prev_locs, n = 1)
    if(direction == NORTH) {
      if(coordinate == AXIS_X) return(prev_loc)
      if(coordinate == AXIS_Y) return(prev_loc + 1)
    }
    
    else if(direction == EAST) {
      if(coordinate == AXIS_X) return(prev_loc + 1)
      if(coordinate == AXIS_Y) return(prev_loc)
    }
    
    else if(direction == SOUTH) {
      if(coordinate == AXIS_X) return(prev_loc)
      if(coordinate == AXIS_Y) return(prev_loc - 1)
    }
    
    else if(direction == WEST) {
      if(coordinate == AXIS_X) return(prev_loc - 1)
      if(coordinate == AXIS_Y) return(prev_loc)
    }
    
    else {
      warning(paste("INVALID DIRECTION OR COORDIANTE for", direction, 
                    coordinate))
      return(NA)
    }
  }
  
  x <- c(0)
  y <- c(0)
  prev_concept <- NA
  for(i in 1:nrow(df_log)) {
    log <- df_log[i,]
    # concept change (NA to defined concept doesn't count as change)
    if(!is.na(log$concept)){
      if(is.na(prev_concept)) { # cold start
        prev_concept <- log$concept
      }
      else if(log$concept != prev_concept) {
        prev_concept <- log$concept
        x <- append(x, get_new_loc(x, dir_assign[[CONCEPT]], AXIS_X))
        y <- append(y, get_new_loc(y, dir_assign[[CONCEPT]], AXIS_Y))
      }
    }
    
    if(log$action == EXERCISE) {
      x <- append(x, get_new_loc(x, dir_assign[[EXERCISE]], AXIS_X))
      y <- append(y, get_new_loc(y, dir_assign[[EXERCISE]], AXIS_Y))
    }
    else if(log$action == INSTRUCTION) {
      x <- append(x, get_new_loc(x, dir_assign[[INSTRUCTION]], AXIS_X))
      y <- append(y, get_new_loc(y, dir_assign[[INSTRUCTION]], AXIS_Y))
    }
    else if(log$action == WORLDVIEW) { # DO NOTHING
      # x <- append(x, get_new_loc(x, dir_assign[[WORLDVIEW]], AXIS_X))
      # y <- append(y, get_new_loc(y, dir_assign[[WORLDVIEW]], AXIS_Y))
    }
    else {
      warning(paste0("Invalid action for ", log$id, ". Action was ",
                     log$action))
    }
  }
  
  df_points <- data.frame(x = x, y = y)
  df_points <- df_points %>% mutate(dist_euc = sqrt(y^2 + x^2))
  
  return(df_points)
}

# plot random walk from df which has 2 columns: x, y
plot_random_walk <- function(df_points, title = "", dir_assign = DIR_ASSIGNMENT) {
  MAX_X <- max(abs(df_points$x))
  MAX_Y <- max(abs(df_points$y))
  
  # given direction (e.g. north), return action (e.g. "worldview")
  get_action = function(direction, dir_action_map = dir_assign) {
    dir_stack <- stack(dir_action_map)
    return(as.character(dir_stack[match(direction, dir_stack[[1]]), 2]))
  }
  
  plot_title <- ""
  
  if(str_length(title)>0) {
    plot_title <- paste0("Random Walk for ",
                         title,
                         ". (Entropy = ", 
                         round(get_entropy_from_dist(df_points$dist_euc), 3), 
                         ")"
                         )
  } else {
    plot_title <- paste0("Random Walk",
                         " (Entropy = ", 
                         round(get_entropy_from_dist(df_points$dist_euc), 3), 
                         ")"
                         )
  }

  m <- ggplot(df_points, aes(x, y)) + 
    geom_vline(xintercept = 0,  color="darkgray") + 
    geom_hline(yintercept = 0, color="darkgray") + 
    # geom_path(size=1.5, alpha = 0.4) + xlim(-MAX_X, MAX_X) + 
    geom_path(size=1.5, alpha = 0.4, position=position_dodge(width=0.2)) + 
    xlim(0, MAX_X) + 
    # xlab(paste0("<-", get_action(WEST), " | ", get_action(EAST), " ->")) +
    xlab(paste0(get_action(EAST), " ->")) +
    ylab(paste0("<-", get_action(SOUTH), " | ", get_action(NORTH), " ->")) +
    ylim(-MAX_Y, MAX_Y) + theme_minimal() + 
    ggtitle(plot_title)
  
  return(m)
}

# given vectors of numbers (e.g. distances of points traveled)
# if optional prop_of_max is TRUE, then return entropy as proportion of maximum
# possible
# returns entropy in bits (log 2)
get_entropy_from_dist <- function(dist_vals, prop_of_max = FALSE) {
  freq_table <- table(dist_vals)
  entropy_val <- entropy(as.vector(freq_table), unit = "log2")
  
  if(prop_of_max) {
    theoretical_entropy_max <- log(length(freq_table), base = 2)
    return(entropy_val / theoretical_entropy_max)
  } else {
    return(entropy_val)
  }
}

# given uid (char), return entropy value (as bits)
get_entropy <- function(uid, prop_of_max = FALSE, json_log = data_log) {
  user_log_data <- get_action_log(uid, json_log)
  df_points <- generate_points(user_log_data)
  return(get_entropy_from_dist(df_points$dist_euc, prop_of_max))
}