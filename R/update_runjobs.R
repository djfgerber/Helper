
#' Function to streamline runjob script with available scripts in R/
#' @description Function should be used in a ..._jobs project to update jobs shortlist.
#' Takes only R files starting with a digit followed by -.
#' @param dest_dir Directory to check
#'
#' @return
#' @export
update_runjobs <- function(dest_dir){
  stopifnot(dir.exists(dest_dir))
  rscript_names <- list.files(paste0(dest_dir, "R/"), ".*.R$",
                              recursive = TRUE)
  rscript_names <- rscript_names[rscript_names %>%
                                   stringr::str_detect("deprecated", negate = TRUE)]
  run_jobs_text <- readLines("run_jobs.R") %>%
    paste(collapse = "\n")
  missing_scripts <- rscript_names[!run_jobs_text %>%
                                     stringr::str_detect(rscript_names)]
  is_bookdown <- missing_scripts %>%
    stringr::str_detect("render_bookdown")
  missing_bookdown_scripts <- missing_scripts[is_bookdown]
  missing_code_scripts <- missing_scripts[!is_bookdown]

  if(length(missing_code_scripts) > 0){
    n_subdir <- missing_code_scripts %>%
      stringr::str_count("/")
    missing_code_scripts <- purrr::map(
      1:max(n_subdir),
      ~ missing_code_scripts[n_subdir == .x]
      [missing_code_scripts[n_subdir == .x] %>%
          stringr::str_extract("(?<=/)\\d{1,3}(?=[^/]*?.R$)") %>%
          as.numeric() %>%
          order()]) %>%
      unlist()
  }

  for(i in seq_along(missing_bookdown_scripts)) {
    bookdown_yml_filename <- dest_dir %>%
      paste0("R/", missing_bookdown_scripts[i]) %>%
      readLines() %>%
      paste(collapse = "\n") %>%
      stringr::str_extract('(?<=\\").*(?=\\")')

    bookdown_yml_fullpath <- dest_dir %>%
      paste0(bookdown_yml_filename)
    if(!file.exists(bookdown_yml_fullpath)){
      stop("The file", bookdown_yml_fullpath, "does not exist.", call. = FALSE)
    }

    bookdown_yml_content <- bookdown_yml_fullpath %>%
      readLines() %>%
      paste(collapse = "\n")
    file_to_delete <- bookdown_yml_content %>% #to delete if rendering fails
      stringr::str_extract('(?<=book_filename:) *\\".*\\" *(?=\\n|$)') %>%
      stringr::str_replace_all('(\\"| *)', "")
    pdf_file_to_open <- bookdown_yml_content %>%
      stringr::str_extract('(?<=output_dir:) *\\".*\\" *(\\n|$)') %>%
      stringr::str_replace_all('(\\"|\\n|\\s*)', "")
    conn <- file("run_jobs.R", open = "at")
    paste0('
if (file.exists("', dest_dir, file_to_delete, '")) {
  file.remove("', dest_dir, file_to_delete, '")
}') %>%
      writeLines(con = conn)
    paste0('rstudioapi::jobRunScript(
  path = "',dest_dir,'R/', missing_bookdown_scripts[i], '",
  workingDir = "', dest_dir, '"
)') %>%
      writeLines(con = conn)
    paste0('browseURL("',
           dest_dir,
           pdf_file_to_open,
           "/",
           file_to_delete %>%
             stringr::str_replace(".Rmd$", ".pdf"),
           '")') %>%
      writeLines(con = conn)
    close.connection(conn)
  }

  for(i in seq_along(missing_code_scripts)) {
    conn <- file("run_jobs.R", open = "at")
    paste0('rstudioapi::jobRunScript(
  path = "',dest_dir,'R/', missing_code_scripts[i], '",
  workingDir = "', dest_dir, '"
)') %>%
      writeLines(con = conn)
    close.connection(conn)
  }
}
