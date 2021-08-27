#' Read LI-COR 6400 data
#'@param file Path to file to import
#'
#'
#'
#' @importFrom readr read_delim read_lines
#' @importFrom dplyr select filter arrange group_by mutate n %>% bind_rows
#' @importFrom rlang .data
#' @importFrom tidyr fill
#' @importFrom tibble tibble
#' @examples
#' file <- system.file("0000-AAA8891-210718", package = "LICOR6400")
#' lic <- read_licor6400(file = file)
#' @export


read_licor6400 <- function(file){

  is_XLS <-grepl("\\.xls$", file)
  #check if excel file
  if(is_XLS){
    tmpdir <- tempdir()
    newfile <- file.path(
      tmpdir,
      "licor",
      gsub("xls$", "csv", basename(file))
    )
    system(paste(system.file("unoconv", package = "LICOR6400"), "-f csv", "-o", newfile, file))
    file <- newfile
  }

  ## reading in raw licor data file
  f <- read_lines(file = file) #min_N = 15

  #find header_row
  header_row <- grep("^.{0,1}Obs", f)
  #remove extra header rows
  if(length(header_row) > 1){
    f <- f[-header_row[-1]]
    header_row <- header_row[1]
  }
  #check if header row
  if(length(header_row) == 0){
    warning("No header row in ", file)
    return(tibble())
  }

  #remove everything above header row
  f <- f[-(1:(header_row - 1))]

  #removing junk  - where licore restarts
  if(!is_XLS & any(grepl("\\$", f))){
    remove <- grep("\\$", f)
    f <- f[-c(remove, remove - 1)]
    remove <- grep("OPEN", f)
    f <- f[-c(remove, remove + 1)]
    remove <- grep("<", f)
    f <- f[-remove]
  }


  #remove in/out line from excel file
  in_out <- grep("^in", f)
  if(length(in_out) > 0){
    f <- f[-in_out]
  }

  ##finding rows of metadata
  meta_rows <- grep("^\"|Remark", f)
  if(!is_XLS){
    meta_rows <- meta_rows[-1] #keep header row
  }

  if(length(meta_rows) == 0){
    return(tibble())
  }

  meta <- f[meta_rows]
  dat <- f[-meta_rows]

  if(length(dat) <= 1){
    return(tibble())
  }

  meta <- gsub("\"|Remark=,", "", meta)
  ## replace multiple spaces with a single space
  meta <- gsub(" +", " ", meta)
  ## grouping multiple worded parameters
  meta <- gsub("CO2 Mixer", "CO2_Mixer", meta)
  #meta must have arrow
  meta <- grep("->", meta, value = TRUE)#some rows only have a timestamp
  #CO2_Mixer -> OFF upsets things remove
  meta <- grep("CO2_Mixer -> OFF", meta, invert = TRUE, value = TRUE)


  #remove comma from csv
  meta <- gsub(",", "", meta)
  dat <- gsub(",", "\t", dat)


  dat <- read_delim(paste(dat, collapse = "\n"), delim = "\t")

  if (length(meta) == 0) { # no meta data
    dat <- dat %>% 
      arrange(HHMMSS) # sort data by time logged
  } else {
    meta <- read_delim(
      file = paste(meta, collapse = "\n"),
      delim = " ",
      col_names = c("HHMMSS", "variable", "parameter",
                    "arrow", "level", "unit")) %>%
      select(-.data$arrow)

    dat <- bind_rows(# bind logged data and metadata
      meta %>%
        # discard parameter changes except block temp
        filter(.data$parameter == "Tblock"),
      dat) %>%
      arrange(.data$HHMMSS) %>% # sort data by time logged
      # fill block temp parameter in for logged data
      fill(.data$variable, .data$parameter, .data$level, .data$unit) %>%
      group_by(.data$level) # group by block temperature
  }
  
  dat <- dat %>% 
    filter(!is.na(.data$Obs)) %>%
    mutate(n = n())
  
  return(dat)
}
