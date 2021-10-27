#' Read Crane File
#'
#' @description This function takes a file path, looks for .csv files and reads in an expected crane file format
#'
#' @param file_path A file path string, which is the file to read
#' @param include_angle A boolean, which indicates if the file should have angle measurements
#'
#' @return a data frame with columns "time"; "torque"; "angle" (optional)
#'
#' @import readr
#' @export
#'
#' @examples
#'
#' read_crane_file("Path/to/filename.csv")
#' read_crane_file("Path/to/filename.csv", include_angle = TRUE)
read_crane_file <- function(file_path, include_angle = FALSE) {

  header_length <-  3
  column_names  <- if (!include_angle) c("time","torque") else c("time","torque","angle")



  ## Find extraneous columns outside of expected
  expected_num_cols <-  length(column_names)
  actual_num_cols <- length(gregexpr(",",readr::read_lines(file_path,skip = header_length,n_max = 1),fixed = TRUE)[[1]]) + 1
  skip_cols <- actual_num_cols - expected_num_cols

  ## Define column types, and which columns to skip with the "_" character.
  column_types <- paste(c(rep("d",expected_num_cols),rep("_",skip_cols)), collapse = "")

  ## return the contents of the file
  readr::read_csv(file_path, skip = header_length, col_names = column_names, col_types = column_types)
}


#' Read Crane Folder
#'
#' @description This function takes a path to a folder containing crane files, and recursively reads all data by calling read_crane_file()
#'
#' @param folder_path A folder path string, which contains files (or subfolders with files)
#' @param include_angle A boolean, which indicates if the file should have angle measurements
#'
#' @return a data frame with columns "path"; time"; "torque"; "angle" (optional)
#'
#' @import readr
#' @import fs
#' @export
#'
#' @examples
#' read_crane_folder("Path/to/folder")
#' read_crane_folder("Path/to/folder", include_angle = TRUE)
#'
read_crane_folder <- function(folder_path, include_angle = FALSE) {
  file_paths <- fs::dir_ls(folder_path, recurse = TRUE, glob = "*.csv")
  ##file_paths
  purrr::map_df(file_paths, ~ dplyr::tibble(path = .x, read_crane_file(.x, include_angle)))
}
