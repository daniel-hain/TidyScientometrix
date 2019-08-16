##################################################################
##	BEGIN: read_scopus_csv_collection():
##################################################################
#' read_scopus_csv_collection()
#'
#' Reads and parses a collection of csv files from Scopus
#'
#' Takes a weighted edge-list and adjusts the weight by dividing the two nodes
#' union by their intercept.
#'
#' @param i The outgoing node in an edgelist.
#' @param j The outgoing node in an edgelist.
#' @param w The weight of the edge between i and j.
#' @export
# Reads a collection of CSV obtained from scopus and parses it nicely. Wrapper around read_scopus_csv():

read_scopus_collection <- function(file_list, TC_min = 0, TC_year_min = 0, PY_max = Inf, PY_min = 0, n_max = Inf, type = NULL, fields = NULL, exclude = NULL) {

  x <- tibble()
  for(i in 1:length(file_list)) {
    cat("===== Loading scopus dataframe", i, "out of", length(file_list), "=====\n", sep = " ")

    y <- read_scopus(file = file_list[i], TC_min = TC_min, TC_year_min = TC_year_min, PY_max = PY_max, PY_min = PY_min, n_max = n_max, type = type, fields = fields, exclude = exclude)
    count_before <- nrow(x)
    x %<>% rbind(y) %>%  distinct(EID, .keep_all = TRUE)
    count_after <- nrow(x)
    cat("-> Loading dataframe", i, "with", nrow(y), "rows complete.", "Added", (count_after - count_before), "rows\n", sep = " ")
  }

  cat("-> Done! Loading", length(file_list), "dataframes with", nrow(x), "rows complete\n", sep = " ")

  return(x)
}

##################################################################
##	BEGIN: read_scopus_csv():
##################################################################
# Wrapper around different types of reading the data

read_scopus <- function(file, TC_min = 0, TC_year_min = 0, PY_max = Inf, PY_min = 0, n_max = Inf, type = NULL, fields = NULL, exclude = NULL){
require(data.table)

  # Check if all there
  if(is_null(type) & is_null(fields)){break("Either a type or a number of fields have to be specified.")}
  if(!is_null(type) & !is_null(fields)){warning("Both type and fields are specified. Type will be used.")}

  # Select fields
  tag <- fields
  if(!is_null(type)){
    if(type == "complete"){tag <- scopus_field_names %>% pull(TAG)}
    if(type == "reduced"){tag <- select_reduced}
  }

  selection <- scopus_field_names %<>%
    filter(TAG %in% tag)

  # Read the file
  x <- fread(file, data.table = FALSE, encoding = "UTF-8", header = TRUE, check.names = FALSE,
             stringsAsFactors = FALSE, strip.white = TRUE, fill = FALSE, select = selection %>% pull(FIELD)) %>%
    as_tibble(validate = TRUE)

  # Some first generic cosmetics
  colnames(x) <- selection %>% pull(TAG)
  x %<>%
    mutate_if(is_character, str_squish) %>%
    mutate_if(is_character, ~sub("[\\,\\;]+$", "", x = .)) %>%
    mutate_all(~na_if(., "") ) %>% mutate_all(~na_if(., " ") ) %>% mutate_all(~na_if(., "[No abstract available]") ) %>% mutate_all(~na_if(., "[No author name available]") ) %>%
    drop_na(EID)

  # Do specific things for the variables
  if("TC" %in% colnames(x)){x %<>% mutate(TC_year = round(TC / ( (Sys.Date() %>% lubridate::year()) - PY + 1), 2) ) }

  x %<>% group_by(EID)
  if("AU" %in% colnames(x)){x %<>%
      mutate(AU =  map(AU, str_split, pattern = ", ") %>% flatten(),
             AU_NR = AU %>% list_lengths_max()) %>%
      mutate(AU1 = ifelse(AU_NR == 0, NA, ifelse(AU_NR == 1, AU[[1]][1],ifelse(AU_NR == 2, paste(AU[[1]][1], AU[[1]][2], sep = " & "), paste(AU[[1]][1],"et al.", sep = " ")))))
    }
  if("AU_ID" %in% colnames(x)){x %<>% mutate(AU_ID = map(AU_ID, str_split, pattern = ";") %>% flatten() ) }
  if("DE" %in% colnames(x)){x %<>% mutate(DE = DE %>% tolower() %>% str_split(pattern = "; ")  ) }
  if("ID" %in% colnames(x)){x %<>% mutate(ID = ID %>% tolower() %>% str_split(pattern = "; ")  ) }
  if("C1" %in% colnames(x)){x %<>% mutate(C1 =  C1 %>% str_split(pattern = "; ")) }
  if("CR" %in% colnames(x)){x %<>% mutate(CR =  CR %>% str_split(pattern = "; "),
                                          NR = CR %>% list_lengths_max()) }
  x %<>% ungroup()

  # Final selection
  x %<>%
    distinct(EID, .keep_all = TRUE)

  if("PY" %in% colnames(x)){x %<>% filter(PY <= PY_max & PY >= PY_min)  }
  if("TC" %in% colnames(x)){x %<>% filter(TC >= TC_min & TC_year >= TC_year_min)  }

  if(!is_null(exclude)){
    for(i in 1:length(exclude)){
      x <- x[x[,names(exclude)[i]] != exclude[i], ]
    }
  }
  if(n_max < nrow(x)){x %<>% slice(1:n_max)}

  return(x)
}








##################################################################
##	BEGIN: scopus_search_ID():
##################################################################
# Retrieves scopus articles (abstracts) via ID search. Therefore, up to 25-200 entries can be recieved for 1 request.

scopus_search_ID <- function(ID, idtype, datatype = "application/json", scopus_key, content = "complete", start = 0, retCount = 200, t_limit = 6, outfile) {
  require(httr)
  require(XML)
  require(jsonlite)
  require(tictoc)

  ## Some upfront checks
  if (content == "complete" & retCount > 25) {
    retCount = 25
    cat("Note: Reducing request-count to 25 for complete content retrieval. Up to 200 only available for standard content. \n")
  }

  ID <- unique(as.character(ID))
  resultCount <- as.numeric(length(ID)) ## get the total number of IDs
  retrievedCount <- 0 ## set the current number of records retrieved to zero
  idList <- split(ID, ceiling(seq_along(ID)/retCount)) ## split the IDs into batches

  data <-  list() # Create empty list

  ## append the correct scopus search syntax around each number
  if (idtype == "eid") {
    idList <- lapply(mapply(paste, "EID(", idList, collapse = ") OR "), paste, ")")
  }
  else if (idtype == "doi") {
    idList <- lapply(mapply(paste, "DOI(", idList, collapse = ") OR "), paste, ")")
  }
  else {
    stop("Invalid idtype. Valid idtypes are 'doi', or 'eid'")
  }
  cat("============================================\n")
  cat("Retrieving", resultCount, "records.\n", sep = " ")

  ## loop through the list of search strings and return data for each one
  for (i in 1:length(idList)) {
    t = proc.time() # For timing the loop

    string <- idList[i]
    res <- httr::GET(url = "https://api.elsevier.com/content/search/scopus",
                     query = list(apiKey = scopus_key, query = string, httpAccept = "application/json", view = content, count = retCount, start = start))
    res_parsed <- content(res, as = "parsed")

    ## check if there's an HTTP error
    if (httr::http_error(res) == TRUE) { ## check if there's an HTTP error
      cat("Encountered an HTTP error. Details follow.\n") ## alert the user to the error
      print(httr::http_status(res)) ## print out the error category, reason, and message
      return(data)
      break ## if there's an HTTP error, break out of the loop and return the data that has been retrieved
    }

    ## if the results are a list of multiple entries, get rid of one hirarchy of the list
    if (length(IDs) > 1) {
      res_parsed <- res_parsed$`search-results`$entry
    }
    data <- append(data, res_parsed)

    ## Update count
    retrievedCount <- retrievedCount + length(res_parsed)
    cat("Retrieved", retrievedCount, "of", resultCount, "records. \n", sep = " ")

    ## If necessary, put system to sleep to respect scopus quota
    tt = (proc.time() - t)[3]
    if( tt < (1/t_limit) ) { Sys.sleep( (1/t_limit) - tt ) }
  }
  cat("Done.\n")
  cat("============================================\n")
  return(data)
}


##################################################################
##	BEGIN: scopus_document_ID():
##################################################################
# Retrieves scopus documents via ID search one-by-one in different views

scopus_document_ID <- function(ID, idtype = "eid", type = "abstract", view = "FULL", scopus_key, start = 1, t_limit = 6) {

  require(httr); require(XML); require(jsonlite)

  ID <- unique(as.character(ID))
  path <- paste("content", type, idtype, ID, sep = "/")

  n <- as.numeric(length(ID)) ## get the total number of IDs
  count <- 0 ## set the current number of records retrieved to zero
  data <- vector("list", length = length(ID))

  cat("============================================\n")
  cat("Retrieving", n, "records of type", type, "(", view, ") \n", sep = " ")

  ## loop through the IDs and return data for each one
  for (i in start:length(ID)) {
    t = proc.time() # For timing the loop

    res <- GET(url = "https://api.elsevier.com", path = path[i], query = list(apiKey = scopus_key,  httpAccept = "application/json", view = view) )
    res_parsed <- content(res, as = "parsed")

    ## check if there's an HTTP error
    if (httr::http_error(res) == TRUE) { ## check if there's an HTTP error
      cat("Encountered an HTTP error at entry:", i , "Details follow.\n") ## alert the user to the error
      print(httr::http_status(res)) ## print out the error category, reason, and message
      return(data)
      break ## if there's an HTTP error, break out of the loop and return the data that has been retrieved
    }

    data[[i]] <- res_parsed

    ## Report status
    if(i %in% seq(from = 0, to = n, by = 100)) {
      cat("Retrieved", i, "of", n, "records. \n", sep = " ")
    }

    ## If necessary, put system to sleep to respect scopus quota
    tt = (proc.time() - t)[3]
    if( tt < (1/t_limit) ) { Sys.sleep( (1/t_limit) - tt ) }
  }
  cat("Done.\n")
  cat("============================================\n")
  return(data)
}



##################################################################
##	More utilities for scopus
##################################################################

subset_name <- function(x, select){ x <- subset(x, names(x) %in% select) }

replace_NULL <- function(x){ x <- x %>% replace(is.null(.), NA) %>% replace(.== "NULL", NA) %>% replace(.== "NA", NA) %>%
  replace(.=="list()", NA) %>% replace(.=="character()", NA) %>% replace(.=="numeric()", NA)
}

clean_nested <- function(x) {
  x %<>%
    unnest() %>%
    mutate_all(as.character) %>%
    mutate_if(is_character, str_squish) %>%
    replace_NULL()
}

clean_AU <- function(x) {
  x %<>%
    mutate(AU = str_squish(AU) ) %>%
    mutate(AU = str_remove_all(AU, "\\..*") ) %>%
    mutate(AU = ifelse(AU == "Dosi", "Dosi G", AU) )
} # TODO: MAke it more generic

gen_AU1  <- function(x) {
  x %<>%
    mutate(AU1 =  ifelse(N_AU == 0, NA,
                         ifelse(N_AU == 1, (AU %>% unlist())[1],
                                ifelse(N_AU == 2, paste( (AU %>% unlist())[1], (AU %>% unlist())[2], sep = " & "),
                                       paste( (AU %>% unlist())[1],"et al.", sep = " ") ) ) ) )
}

# TODO: MAke them all in their numbvering more generic


##################################################################
##	BEGIN: scopus_extract_AU()
##################################################################

scopus_extract_AU <- function(x, index, level = "document"){
  y <- x %>%
    list.select(AU = authors$author) %>%
    map("AU") %>%
    {tibble(EID = index,
            AU = map_depth(., 2, "ce:indexed-name") %>% map_depth(2, replace_NULL),
            AU_ID = map_depth(., 2, "@auid") %>% map_depth(2, replace_NULL),
            AU_C1_ID = map_depth(., 2, "affiliation") %>% map_depth(2, "@id") %>% map_depth(2, replace_NULL)
    ) } %>% replace_NULL()

  y %<>%
    clean_nested() %>%
    clean_AU() %>%
    replace_NULL() %>%
    group_by(EID) %>%
    mutate(N_AU = n()) %>%
    filter(N_AU >= 1) %>%
    ungroup()

  if(level == "entity"){
    y %<>%
      nest(-AU_ID)
  }

  if(level == "document"){
    y %<>%
      nest(-EID, - N_AU, .key = "AU") %>%
      group_by(EID) %>% gen_AU1() %>% ungroup() %>%
      select(EID, AU1, N_AU, AU)
  }
  return(y)
}


##################################################################
##	BEGIN: scopus_extract_C1()
##################################################################

scopus_extract_C1 <- function(x, index, level = "document"){
  y <- x %>%
    map("affiliation") %>%
    {tibble(EID = index,
            C1 = map(., "affilname"),
            C1_x = map_depth(., 2, "affilname") %>% map_depth(2, replace_NULL),
            C1_ID = map(., "@id"),
            C1_ID_x = map_depth(., 2, "@id") %>% map_depth(2, replace_NULL),
            C1_CT = map(., "affiliation-city"),
            C1_CT_x = map_depth(., 2, "affiliation-city") %>% map_depth(2, replace_NULL),
            C1_CN = map(., "affiliation-country"),
            C1_CN_x = map_depth(., 2, "affiliation-country") %>% map_depth(2, replace_NULL)
    ) } %>%
    group_by(EID) %>%
    mutate(C1    = ifelse(C1 != "NULL",  list(C1), C1_x),
           C1_ID = ifelse(C1_ID != "NULL",  list(C1_ID), C1_ID_x),
           C1_CT = ifelse(C1_CT != "NULL",  list(C1_CT), C1_CT_x),
           C1_CN = ifelse(C1_CN != "NULL",  list(C1_CN), C1_CN_x) ) %>%
    ungroup() %>%
    select(-C1_x, -C1_ID_x, -C1_CT_x, -C1_CN_x) %>%
    replace_NULL()

  z <- tibble(EID = y %>% pull(EID)) %>%
    bind_cols(y %>% select(EID, C1) %>% clean_nested() %>% nest(-EID, .key = "C1") %>% select(-EID)) %>%
    bind_cols(y %>% select(EID, C1_ID) %>% clean_nested() %>% nest(-EID, .key = "C1_ID") %>% select(-EID)) %>%
    bind_cols(y %>% select(EID, C1_CT) %>% clean_nested() %>% nest(-EID, .key = "C1_CT") %>% select(-EID)) %>%
    bind_cols(y %>% select(EID, C1_CN) %>% clean_nested() %>% nest(-EID, .key = "C1_CN") %>% select(-EID) ) %>%
    nest(-EID, .key = "C1")


  if(level == "entity"){
    z %<>%
      select(C1_ID, EID) %>%
      unnest() %>%
      drop_na() %>%
      nest(-C1_ID)
  }
  return(z)
}


##################################################################
##	BEGIN: scopus_extract_SC()
##################################################################

scopus_extract_SC <- function(x, index, level = "document"){
  x %<>%
    list.select(SC = `subject-areas`$`subject-area`) %>%
    map("SC") %>%
    {tibble(EID = index,
            SC = map_depth(., 2, unlist) %>% map_depth(2, "$") %>% map_depth(2, replace_NULL) %>% map(unlist),
            SC_ID = map_depth(., 2, unlist) %>% map_depth(2, "@code") %>% map_depth(2, replace_NULL) %>% map(unlist),
            SC_CODE = map_depth(., 2, unlist) %>% map_depth(2, "@abbrev") %>% map_depth(2, replace_NULL) %>% map(unlist)
    ) } %>% replace_NULL()

  x %<>% clean_nested() %>%
    nest(-EID, .key = "SC")

  return(x)
}

##################################################################
##	BEGIN: scopus_extract_SC()
##################################################################

scopus_extract_FX <- function(x, index, level = "document"){
  x %<>%
      list.select(FX = item$`xocs:meta`$`xocs:funding-list`) %>%
      map("FX") %>%
      {tibble(EID = index,
              FX = map(., "xocs:funding") %>% map("xocs:funding-agency-matched-string"),
              FX_CODE = map(., "xocs:funding") %>% map("xocs:funding-agency-acronym"),
              FX_ID = map(., "xocs:funding") %>% map("xocs:funding-agency-id"),
              FX_CN = map(., "xocs:funding") %>% map("xocs:funding-agency-country"),
              FX_TXT = map(., "xocs:funding-text")
      ) } %>% replace_NULL()

    x %<>%
      mutate_all(as.character) %>%
      mutate_if(is_character, str_squish) %>%
      replace_NULL() %>%
      nest(-EID, .key = "FX")

  return(x)
}


##################################################################
##	BEGIN: scopus_extract_MX()
##################################################################

scopus_extract_MX <- function(x, index, level = "document"){
  y <- x%>%
    list.select(MX = coredata) %>%
    map("MX") %>%
    {tibble(EID = index,
            SO_ID = map(., "source-id")
    ) } %>%
    mutate(SO_ID = as.character(SO_ID))%>%
    replace_NULL()

  ID <- x %>%
    list.select(ID = item$bibrecord$`item-info`$itemidlist$itemid) %>%
    map("ID") %>%
    {tibble(EID = EID_index,
            SID = map_depth(., 2, unlist) %>% map_depth(2, function(x){paste(x,collapse="")})  %>% map(unlist) %>%
              map(list.filter, grepl("SGR",.)) %>% map(str_remove, "SGR") %>% map(unlist)
    ) }  %>%
    mutate(SID = as.character(SID)) %>%
    replace_NULL()

  y %<>%
    left_join(ID, by = "EID") %>% distinct(EID, .keep_all = TRUE)

  return(y)
}


