# wrapper around eval, with some extra functionality
eval_expr <- function(expr) {
  if (nchar(expr) == 0) {
    return("")
  }
  value <- ""
  value <- tryCatch(eval(parse(text = expr), envir = xda_env))
  return(value)
}

# map true to character 'y'
true_to_y <- function(expr) {
  value <- eval_expr(expr)
  if (value == TRUE) {
    return("y")
  } else {
    return("")
  }
}

multi_select_contains <- function(what, who_id) {
  if (who_id == -1) {
    return(FALSE)
  }
  split_expression <- as.list(strsplit(who_id, " ")[[1]])
  found <- FALSE
  for (selection in split_expression) {
    if (grepl(what, selection)) {
      found <- TRUE
    }
  }
  return(found)
}

yes_to_code <- function(qlist, clist, default) {
  code <- ""
  for (i in 1:length(qlist)) {
    if (get(qlist[i], envir = xda_env) == "yes") {
      code <- paste(code, clist[i])
    }
  }
  # use default if code is empty
  if (nchar(code) == 0) {
    code <- default
  }
  return(trimws(code))
}

# from_list: upper limits of range to_list: codes to map to
range_to_code <- function(from_list, to_list, default, who_id) {
  if (get(who_id, envir = xda_env) == "") {
    return("")
  }
  code <- default
  value <- as.numeric(get(who_id, envir = xda_env))
  for (i in 1:length(to_list)) {
    if (value > from_list[i] && value <= from_list[i + 1]) {
      code <- to_list[i]
    }
  }
  return(code)
}

#map between sets of codes
map_code <- function(from_list, to_list, who_id) {
  code <- ""
  value <- get(who_id, envir = xda_env)
  for (i in 1:length(from_list)) {
    if (from_list[i] == value) {
      code <- to_list[i]
    }
  }
  # leave empty if no match
  return(code)
}

#Create ODK-style muli-select answer (chosen options listed separated by space)
map_multi_code <- function(from_list, to_list, who_id) {
  code <- ""
  values <- strsplit(as.character(get(who_id)), " ")[[1]]
  for (i in 1:length(from_list)) {
    if (any(from_list[i] == values)) {
      code <- paste(code, to_list[i])
    }
  }
  # leave empty if no match
  return(trimws(code))
}

# evaluate expression, return default on empty
exp_def <- function(expr, default) {
  value <- eval_expr(expr)
  if (nchar(value) > 0) {
    return(value)
  } else {
    return(default)
  }
}
