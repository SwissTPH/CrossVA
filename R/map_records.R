# wrapper around eval, with some extra functionality
eval_expr <- function(expr) {
  if (nchar(expr) == 0) {
    return("")
  }
  value <- ""
  value <- tryCatch(eval(parse(text = expr)))
  return(value)
}

#wrapper around get
get_value <- function(var_name){
  return(get(var_name))
}


#' @export
map_records <- function(records, mapping_file) {
  mapping_f_name <- system.file('mapping', mapping_file, package = 'xva')
  records[is.na(records)]<-""
  headers <- names(records)

  # Load mapping tab-delim file:
  mapping <- read.delim(mapping_f_name)

  # number of variables required by coding algorithm
  target_n <- nrow(mapping)
  output_data <- data.frame(matrix(ncol = target_n))
  colnames(output_data) <- mapping[, 1]
  for (rec_count in 1:nrow(records)) {
    assign("rec_id", rec_count, envir = parent.frame())
    record <- records[rec_count, ]
    for (j in 1:length(headers)) {
      value <- as.character(record[1, j])
      header <- headers[j]
      header_cleaned <-
        regmatches(header, regexpr("[^\\.]*$", header))
      assign(header_cleaned, value, envir = parent.frame())
    }
    current_data <- data.frame(matrix(ncol = target_n))
    for (i in 1:target_n) {
      target_var <- as.character(mapping[i, 1])
      expr <- as.character(mapping[i, 2])
      current_data[i] <- eval_expr(expr)
      # make the value available for reference later in the destination var set
      name <- regmatches(target_var, regexpr("[^\\-]*$", target_var))
      name <- paste("t_", name, sep = "")
      assign(name, current_data[i][[1]], envir = parent.frame())
    }
    output_data[rec_count, ] <- current_data
  }
  return(output_data)
}

