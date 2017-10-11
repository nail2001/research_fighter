library(R.cache)
library(dplyr)
library(httr)
library(rvest)
library(stringr)
library(xml2)

##' scholar
##'
##' The \code{scholar} package provides functions to extract citation
##' data from Google Scholar.  There are also convenience functions
##' for comparing multiple scholars and predicting h-index scores
##' based on past publication records.
##'
##' @note A complementary set of Google Scholar functions can be found
##' at  \url{http://biostat.jhsph.edu/~jleek/code/googleCite.r}.  The
##' \code{scholar} package was developed independently.
##'
##' @source The package reads data from
##' \url{http://scholar.google.com}.  Dates and citation counts are
##' estimated and are determined automatically by a computer program.
##' Use at your own risk.
##' 
##' @name scholar
##' @docType package
options("scholar_call_home"=TRUE)


##' Ensures that specified IDs are correctly formatted
##'
##' @param id a character string specifying the Google Scholar ID.
##' If multiple ids are specified, only the first value is used and a
##' warning is generated.
##' @export
##' @importFrom httr GET
##' @keywords internal
tidy_id <- function(id) {
  if (length(id)!=1) {
    id <- id[1]
    msg <- sprintf("Only one ID at a time; retrieving %s", id)
    warning(msg)
  }
  
  ## Check with Google to set cookies
  if (getOption("scholar_call_home")) {
    sample_url <- "https://scholar.google.com/citations?user=B7vSqZsAAAAJ"
    sink <- GET(sample_url)      
    options("scholar_call_home"=FALSE, "scholar_handle"=sink)
  }
  
  return(id)
}

## Originally started on 14 May 2013

##' Gets profile information for a scholar
##'
##' Gets profile information for a researcher from Google Scholar.
##' Each scholar profile page gives the researcher's name,
##' affiliation, their homepage (if specified), and a summary of their
##' key citation and impact metrics.  The scholar ID can be found by
##' searching Google Scholar at \url{http://scholar.google.com}.
##' 
##' @param id 	a character string specifying the Google Scholar ID.
##' If multiple ids are specified, only the first value is used and a
##' warning is generated.  See the example below for how to profile
##' multiple scholars.
##' 
##' @return 	a list containing the scholar's name, affiliation,
##' citations, impact metrics, fields of study, and homepage
##' 
##' @examples {
##'    ## Gets profiles of some famous physicists
##'    ids <- c("xJaxiEEAAAAJ", "qj74uXkAAAAJ")
##'    profiles <- lapply(ids, get_profile)
##' }
##' @export
##' @importFrom stringr str_trim str_split
##' @importFrom xml2 read_html
##' @importFrom rvest html_table html_nodes html_text
##' @importFrom dplyr "%>%"
##' @importFrom httr GET
get_profile <- function(id) {
  
  id <- tidy_id(id)
  
  url_template <- "http://scholar.google.com/citations?hl=en&user=%s"
  url <- sprintf(url_template, id)
  
  ## Generate a list of all the tables identified by the scholar ID
  page <- GET(url, handle=getOption("scholar_handle")) %>% read_html()
  tables <- page %>% html_table()
  
  ## The citation stats are in tables[[1]]$tables$stats
  ## but the number of rows seems to vary by OS
  stats <- tables[[1]]
  rows <- nrow(stats)
  
  ## The personal info is in
  name <- page %>% html_nodes(xpath="//*/div[@id='gsc_prf_in']") %>% html_text()
  bio_info <- page %>% html_nodes(xpath="//*/div[@class='gsc_prf_il']") %>% html_text()
  affiliation <- bio_info[1]
  
  ## Specialities (trim out HTML non-breaking space)
  specs <- iconv(bio_info[2], from="UTF8", to="ASCII")
  specs <- str_trim(tolower(str_split(specs, ",")[[1]]))
  
  ## Extract the homepage
  homepage <- page %>% html_nodes(xpath="//*/div[@id='gsc_prf_ivh']//a/@href") %>% html_text() 
  
  return(list(id=id, name=name, affiliation=affiliation,
              total_cites=as.numeric(as.character(stats[rows-2,2])),
              h_index=as.numeric(as.character(stats[rows-1,2])),
              i10_index=as.numeric(as.character(stats[rows,2])),
              fields=specs,
              homepage=homepage))
}

substrRight <- function(x, n) {
  substr(x, nchar(x)-n+1, nchar(x))
}

lastNumber <- function(x) {
  gsub("[^0-9]", "", substrRight(x, 2))
}

##' Get historical citation data for a scholar
##'
##' Gets the number of citations to a scholar's articles over the past
##' nine years.
##'
##' @param id a character string specifying the Google Scholar ID.  If
##' multiple ids are specified, only the first value is used and a
##' warning is generated.
##' @details This information is displayed as a bar plot at the top of
##' a standard Google Scholar page and only covers the past nine
##' years.
##' @return a data frame giving the number of citations per year to
##' work by the given scholar
##' @export
##' @importFrom xml2 read_html
##' @importFrom rvest html_nodes html_text
##' @importFrom dplyr "%>%"
##' @importFrom httr GET
get_citation_history <- function(id) {
  
  ## Ensure only one ID
  id <- tidy_id(id)
  
  ## Read the page and parse the key data
  url_template <- "http://scholar.google.com/citations?hl=en&user=%s&pagesize=100&view_op=list_works"
  url <- sprintf(url_template, id)
  
  ## A better way would actually be to read out the plot of citations
  page <- GET(url, handle=getOption("scholar_handle")) %>% read_html()
  years <- page %>% html_nodes(xpath="//*/span[@class='gsc_g_t']") %>% html_text() %>% as.numeric()
  vals <- page %>% html_nodes(xpath="//*/span[@class='gsc_g_al']") %>% html_text() %>% as.numeric()
  vals_z <- page %>% html_nodes(xpath="//*/a[@class='gsc_g_a']") %>% html_attr("style") %>% lastNumber() %>% as.numeric()
  
  df <- data.frame(year=years, cites=rep(0, length(years)))
  df<- df[seq(dim(df)[1],1),]
  df$cites[vals_z] <- vals
  df<- df[seq(dim(df)[1],1),]
  
  return(df)
}


##' Gets the number of distinct journals in which a scholar has
##' published
##'
##' Gets the number of distinct journals in which a scholar has
##' published.  Note that Google Scholar doesn't provide information
##' on journals \emph{per se}, but instead gives a title for the
##' containing publication where applicable.  So a \emph{journal} here
##' might actually be a journal, a book, a report, or some other
##' publication outlet.
##' 
##' @param id 	a character string giving the Google Scholar id
##' @return the number of distinct journals
##' @export 
get_num_distinct_journals <- function(id) {
  id <- tidy_id(id)
  papers <- get_publications(id)
  return(length(unique(papers$journal)))
}

##' Gets the number of top journals in which a scholar has published
##'
##' Gets the number of top journals in which a scholar has published.
##' The definition of a 'top journal' comes from Acuna et al. and the
##' original list was based on the field of neuroscience.  This
##' function allows users to specify that list for themselves, or use
##' the default Acuna et al. list.  
##'
##' @source DE Acuna, S Allesina, KP Kording (2012) Future impact:
##' Predicting scientific success.  Nature 489,
##' 201-202. \url{http://dx.doi.org/10.1038/489201a}.
##'
##' @param id 	a character string giving a Google Scholar ID
##' @param journals a character vector giving the names of the top
##' journals.  Defaults to Nature, Science, Nature Neuroscience,
##' Proceedings of the National Academy of Sciences, and Neuron.
##' @export
get_num_top_journals <- function(id, journals) {
  id <- tidy_id(id)
  papers <- get_publications(id)
  
  if (missing(journals)) {
    journals <-c("Nature", "Science", "Nature Neuroscience",
                 "Proceedings of the National Academy of Sciences", "Neuron")
  }
  
  return(length(which(is.element(papers$journal, journals))))
}

# Ugly hack for CRAN checks
utils::globalVariables(c("id", "year", "cites"))

##' Compare the citation records of multiple scholars
##'
##' Compares the citation records of multiple scholars.  This function
##' compiles a data frame comparing the citations received by each of
##' the scholar's publications by year of publication.
##'
##' @param ids 	a vector of Google Scholar IDs
##' @param pagesize an integer specifying the number of articles to
##' fetch for each scholar
##' @return a data frame giving the ID of each scholar and the total
##' number of citations received by work published in a year.
##' @examples {
##' \dontrun{
##'     ## How do Richard Feynmann and Isaac Newton compare?
##' 	ids <- c("B7vSqZsAAAAJ", "xJaxiEEAAAAJ")
##'     df <- compare_scholars(ids)
##' }
##' }
##' @export
##' @importFrom dplyr "%>%" summarize mutate group_by
compare_scholars <- function(ids, pagesize=100) {
  
  ## Load in the publication data and summarize
  data <- lapply(ids, function(x) cbind(id=x, get_publications(x, pagesize=pagesize)))
  data <- do.call("rbind", data)
  data <- data %>% group_by(id, year) %>%
    summarize(cites=sum(cites, na.rm=TRUE)) %>%
    mutate(total=cumsum(cites))
  
  ## Fetch the scholar names 
  names <- lapply(ids, function(i) {
    p <- get_profile(i)
    data.frame(id=p$id, name=p$name)
  })
  names <- do.call("rbind", names)
  
  ## Merge together with the citation info
  final <- merge(data, names)
  return(final)
}

##' Compare the careers of multiple scholars
##'
##' Compares the careers of multiple scholars based on their citation
##' histories.  The scholar's \emph{career} is defined by the number
##' of citations to his or her work in a given year (i.e. the bar
##' chart at the top of a scholar's profile). The function has an
##' \code{career} option that allows users to compare scholars
##' directly, i.e. relative to the first year in which their
##' publications are cited.
##'
##' @param ids 	a character vector of Google Scholar IDs
##' @param career  a boolean, should a column be added to the results
##' measuring the year relative to the first citation year.  Default =
##' TRUE
##' 
##' @examples {
##' 	## How do Richard Feynmann and Isaac Newton compare?
##' 	ids <- c("B7vSqZsAAAAJ", "xJaxiEEAAAAJ")
##'     df <- compare_scholar_careers(ids)
##' }
##' @export
##' @importFrom dplyr "%>%" group_by mutate
compare_scholar_careers <- function(ids, career=TRUE) {
  
  data <- lapply(ids, function(x) return(cbind(id=x, get_citation_history(x))))
  data <- do.call("rbind", data)
  
  ## Calculate the minimum year for each scholar and create a career year
  if (career) {
    data <- data %>% group_by(id) %>%
      mutate(career_year=year-min(year))
  } 
  
  ## Fetch the scholar names 
  names <- lapply(ids, function(i) {
    p <- get_profile(i)
    data.frame(id=p$id, name=p$name)
  })
  names <- do.call("rbind", names)
  
  ## Add the name data
  data <- merge(data, names)
  return(data)
}

# Ugly hack for CRAN checks
utils::globalVariables(c("."))

##' Gets the publications for a scholar
##'
##' Gets the publications of a specified scholar.
##'
##' @param id a character string specifying the Google Scholar ID.  If
##' multiple IDs are specified, only the publications of the first
##' scholar will be retrieved.
##' @param cstart an integer specifying the first article to start
##' counting.  To get all publications for an author, omit this
##' parameter.
##' @param pagesize an integer specifying the number of articles to
##' fetch
##' @param flush should the cache be flushed?  Search results are
##' cached by default to speed up repeated queries.  If this argument
##' is TRUE, the cache will be cleared and the data reloaded from
##' Google.
##' @details Google uses two id codes to uniquely reference a
##' publication.  The results of this method includes \code{cid} which
##' can be used to link to a publication's full citation history
##' (i.e. if you click on the number of citations in the main scholar
##' profile page), and \code{pubid} which links to the details of the
##' publication (i.e. if you click on the title of the publication in
##' the main scholar profile page.)
##' @return a data frame listing the publications and their details.
##' These include the publication title, author, journal, number,
##' cites, year, and two id codes (see details).
##' @importFrom stringr str_extract str_sub str_trim str_replace
##' @importFrom dplyr "%>%" row_number filter
##' @importFrom xml2 read_html
##' @importFrom rvest html_nodes html_text html_attr
##' @importFrom httr GET
##' @import R.cache
##' @export
get_publications <- function(id, cstart = 0, pagesize=100, flush=FALSE) {
  
  ## Ensure we're only getting one scholar's publications
  id <- tidy_id(id)
  
  ## Define the cache path 
  cache.dir <- file.path(tempdir(), "r-scholar")
  setCacheRootPath(cache.dir)
  
  ## Clear the cache if requested
  if (flush) saveCache(NULL, key=list(id, cstart))
  
  ## Check if we've cached it already
  data <- loadCache(list(id, cstart))
  
  ## If not, get the data and save it to cache
  if (is.null(data)) {
    
    ## Build the URL
    url_template <- "http://scholar.google.com/citations?hl=en&user=%s&cstart=%d&pagesize=%d"
    url <- sprintf(url_template, id, cstart, pagesize)
    
    ## Load the page
    page <- GET(url, handle=getOption("scholar_handle")) %>% read_html()
    cites <- page %>% html_nodes(xpath="//tr[@class='gsc_a_tr']") 
    
    title <- cites %>% html_nodes(".gsc_a_at") %>% html_text()
    pubid <- cites %>% html_nodes(".gsc_a_at") %>%
      html_attr("href") %>% str_extract(":.*$") %>% str_sub(start=2)
    doc_id <- cites %>% html_nodes(".gsc_a_ac") %>% html_attr("href") %>%
      str_extract("cites=.*$") %>% str_sub(start=7)
    cited_by <- suppressWarnings(cites %>% html_nodes(".gsc_a_ac") %>%
                                   html_text() %>%
                                   as.numeric(.) %>% replace(is.na(.), 0))
    year <- cites %>% html_nodes(".gsc_a_y") %>% html_text() %>%
      as.numeric()
    authors <- cites %>% html_nodes("td .gs_gray") %>% html_text() %>%
      as.data.frame(stringsAsFactors=FALSE) %>%
      filter(row_number() %% 2 == 1)  %>% .[[1]]
    
    ## Get the more complicated parts
    details <- cites %>% html_nodes("td .gs_gray") %>% html_text() %>%
      as.data.frame(stringsAsFactors=FALSE) %>%
      filter(row_number() %% 2 == 0) %>% .[[1]]
    
    
    ## Clean up the journal titles (assume there are no numbers in
    ## the journal title)
    first_digit <- as.numeric(regexpr("[\\[\\(]?\\d", details)) - 1
    journal <- str_trim(str_sub(details, end=first_digit)) %>%
      str_replace(",$", "")
    
    ## Clean up the numbers part
    numbers <- str_sub(details, start=first_digit) %>%
      str_trim() %>% str_sub(end=-5) %>% str_trim() %>% str_replace(",$", "")
    
    ## Put it all together
    data <- data.frame(title=title,
                       author=authors,
                       journal=journal,
                       number=numbers,
                       cites=cited_by,
                       year=year,
                       cid=doc_id,
                       pubid=pubid)
    
    ## Check if we've reached pagesize articles. Might need
    ## to search the next page
    if (nrow(data) > 0 && nrow(data)==pagesize) {
      data <- rbind(data, get_publications(id, cstart=cstart+pagesize, pagesize=pagesize))
    }
    
    ## Save it after everything has been retrieved.
    if (cstart == 0) {
      saveCache(data, key=list(id, cstart))
    }
  }
  
  return(data)
}

##' Gets the citation history of a single article
##'
##' @param id a character string giving the id of the scholar
##' @param article a character string giving the article id.
##' @return a data frame giving the year, citations per year, and
##' publication id
##' @importFrom dplyr "%>%"
##' @importFrom stringr str_replace
##' @importFrom xml2 read_html
##' @importFrom rvest html_nodes html_attr html_text
##' @importFrom httr GET
##' @export
get_article_cite_history <- function (id, article) {
  
  id <- tidy_id(id)
  url_base <- paste0("http://scholar.google.com/citations?", 
                     "view_op=view_citation&hl=en&citation_for_view=")
  url_tail <- paste(id, article, sep=":")
  url <- paste0(url_base, url_tail)
  
  doc <- GET(url, handle=getOption("scholar_handle")) %>% read_html()
  
  ## Inspect the bar chart to retrieve the citation values and years
  years <- doc %>%
    html_nodes(xpath="//*/div[@id='gsc_graph_bars']/a") %>%
    html_attr("href") %>%
    str_replace(".*as_yhi=(.*)$", "\\1") %>%
    as.numeric()
  vals <- doc %>%
    html_nodes(xpath="//*/span[@class='gsc_g_al']") %>%
    html_text() %>%
    as.numeric()
  
  df <- data.frame(year = years, cites = vals)
  
  ## There may be undefined years in the sequence so fill in these gaps
  tmp <- merge(data.frame(year=min(years):max(years)),
               df, all.x=TRUE)
  tmp[is.na(tmp)] <- 0
  
  tmp$pubid <- article
  return(tmp)
}

##' Calculates how many articles a scholar has published
##'
##' Calculate how many articles a scholar has published.
##'
##' @param id a character string giving the Google Scholar ID
##' @return an integer value (max 100)
##' @export
get_num_articles <- function(id) {  
  papers <- get_publications(id)
  return(nrow(papers))
}

##' Gets the year of the oldest article for a scholar
##'
##' Gets the year of the oldest article published by a given scholar.
##' 
##' @param id 	a character string giving the Google Scholar ID
##' @return the year of the oldest article
##' @export
get_oldest_article <- function(id) {
  papers <- get_publications(id)
  return(min(papers$year, na.rm=TRUE))
}
