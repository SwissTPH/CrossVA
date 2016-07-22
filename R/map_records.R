#' Map VA records to coding algorithm.
#'
#' \code{map_records} transform data collected with the WHO VA instrument
#'   to serve different alogrithms for coding cause of death.
#'
#' @param records A dataframe, obtained from reading an ODKBriefcase
#'   export of records collected with the WHO questionnaire.
#' @param mapping_file Path to a mapping file
#' @param csv_outfile Path to a file to write transformed data to.
#'   Defaults to empty string, in which case no file is written.
#' @return A dataframe, with the VA records mapped to the variables required
#'   by a coding algorithm, as specified in the mapping file.
#'
#' @examples
#' \dontrun{
#' records <- read.csv('who_va_output.csv')
#' output_data <- map_records(records,"interva4_mapping.txt")
#' }
#'
#' @export
map_records <- function(records, mapping_file, csv_outfile = "") {
  mapping_f_name <-
    system.file('mapping', mapping_file, package = 'xva')
  records[is.na(records)] <- ""
  headers <- names(records)

  # Load mapping tab-delim file:
  mapping <- read.delim(mapping_f_name)

  # number of variables required by coding algorithm
  target_n <- nrow(mapping)
  output_data <- data.frame(matrix(ncol = target_n))
  colnames(output_data) <- mapping[, 1]
  for (rec_count in 1:nrow(records)) {
    assign("rec_id", rec_count, envir = parent.frame())
    record <- records[rec_count,]
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
      name <-
        regmatches(target_var, regexpr("[^\\-]*$", target_var))
      name <- paste("t_", name, sep = "")
      assign(name, current_data[i][[1]], envir = parent.frame())
    }
    output_data[rec_count,] <- current_data
  }
  if (csv_outfile != "") {
    write.table(
      output_data,
      csv_outfile,
      row.names = FALSE,
      na = "",
      qmethod = "escape",
      sep = ","
    )
  }
  return(output_data)
}

#' Map VA records to InterVA4.
#'
#' \code{map_records} transform data collected with the WHO VA instrument
#'   for coding with the InterVA4 algorithm.
#'
#' @param records A dataframe, obtained from reading an ODKBriefcase
#'   export of records collected with the WHO questionnaire.
#' @param csv_outfile Path to a file to write transformed data to.
#'   Defaults to empty string, in which case no file is written.
#' @return A dataframe, with the VA records mapped to the variables required
#'   by InterVA4.
#'
#' @examples
#' \dontrun{
#' records <- read.csv('who_va_output.csv')
#' output_data <- map_records_interva4(records)
#' }
#' @references http://www.interva.net/
#' @export
map_records_interva4 <- function(records, csv_outfile = "" ){
  return (map_records(records,"interva4_mapping.txt", csv_outfile))
}

#' Map VA records to Tariff 2.
#'
#' \code{map_records} transform data collected with the WHO VA instrument
#'   for coding with the Tariff 2 algorithm.
#'
#' @param records A dataframe, obtained from reading an ODKBriefcase
#'   export of records collected with the WHO questionnaire.
#' @param csv_outfile Path to a file to write transformed data to.
#'   Defaults to empty string, in which case no file is written.
#' @return A dataframe, with the VA records mapped to the variables required
#'   by Tariff 2.
#'
#' @examples
#' \dontrun{
#' records <- read.csv('who_va_output.csv')
#' output_data <- map_records_tariff2(records)
#' }
#'
#' @references James, S. L., Flaxman, A. D., Murray, C. J., & Population Health Metrics Research Consortium. (2011). \emph{Performance of the Tariff Method: validation of a simple additive algorithm for analysis of verbal autopsies.} \emph{Population Health Metrics, 9(1), 1-16.}
#' @export
map_records_tariff2 <- function(records, csv_outfile = "" ){
  return (map_records(records,"tariff2_mapping.txt", csv_outfile))
}
