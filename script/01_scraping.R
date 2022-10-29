options(scipen = 99999)


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(xml2)
library(httr)
library(rvest)
#install.packages("progressr")
library(progressr)
library(purrr)
library(tictoc)


# Step 1 - To get a search page -------------------------------------------

# Parametros para configuracao da busca

search <- "headship"

page <- "1"

# Pagina de busca
u_demography <- glue::glue("https://read.dukeupress.edu/demography/search-results?{search}&fl_SiteID=1000157&page={page}")

# Acesso a pagina de busca or termo

r_demography_search <- httr::GET(
  u_demography,
  user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:104.0) Gecko/20100101 Firefox/104.0"),
  write_disk(paste0("data/raw/demography_search_",search,".html"), overwrite = TRUE)
  )

# Step 2 - To get a list of article links for ONE page --------------------

links <- read_html(r_demography_search) |>
  xml_find_all("//div[@class = 'ww-citation-wrap-doi']//a") |>
  xml_text("href")

# Step 3 - To get ONE article contents ------------------------------------

article_link <- links[[1]] # selecionando um link somente

r_article <- httr::GET(
  article_link,
  user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:104.0) Gecko/20100101 Firefox/104.0"),
  write_disk(glue::glue("./data/raw/articles/{basename(article_link)}.html"),overwrite = TRUE)
  )

xpaths <- c(
  title = "//h1[@class = 'wi-article-title article-title-main']",
  date = "//span[@class = 'article-date']",
  authors = "//a[@class = 'linked-name js-linked-name stats-author-info-trigger']",
  infos = "//div[@class = 'ww-citation-primary']",
  doi = "//div[@class = 'citation-doi']",
  kw = "//div[@class = 'kwd-group']",
  abstract = "//p[@class = 'abstract-border']"
  )

#r_article <- xml2::read_html(glue::glue("data/raw/article_{basename(article_link)}.html"))

resp_title <- r_article |>
  read_html() |>
  xml_find_all("//h1[@class = 'wi-article-title article-title-main']") |>
  xml_text() |>
  str_squish()

resp_date <- r_article |>
  read_html() |>
  xml_find_all("//span[@class = 'article-date']") |>
  xml_text() |>
  str_squish()

resp_authors <- r_article |>
  read_html() |>
  xml_find_all("//a[@class = 'linked-name js-linked-name stats-author-info-trigger']") |>
  xml_text() |>
  str_squish() |>
  paste(collapse = ",")

resp_infos <- r_article |>
  read_html() |>
  xml_find_all("//div[@class = 'ww-citation-primary']") |>
  xml_text() |>
  str_squish()

resp_doi <- r_article |>
  read_html() |>
  xml_find_all("//div[@class = 'citation-doi']") |>
  xml_text() |>
  str_squish()

resp_kw <- r_article |>
  read_html() |>
  xml_find_all("//div[@class = 'kwd-group']") |>
  xml_text() |>
  str_squish()

resp_abstract <- r_article |>
  read_html() |>
  xml_find_all("//section[@class = 'abstract']") |>
  xml_text() |>
  str_squish()

# Joint all informations in df

resp_article <- tibble::tibble(
  id = basename(resp_doi),
  title = resp_title,
  date = lubridate::mdy(resp_date),
  authors = resp_authors,
  infos = resp_infos,
  doi = resp_doi,
  kw = resp_kw,
  abstract = resp_abstract
  ) |>
  mutate(year = as.numeric(lubridate::year(date))) |>
  dplyr::select(id, year, dplyr::everything())


# Step 4 - To get ALL articles contents for ONE page ----------------------

links <- read_html(r_demography_search) |>
  xml_find_all("//div[@class = 'ww-citation-wrap-doi']//a") |>
  xml_text("href")

# Function to get content for one article

f_get_articles_contents <- function(link) {

  r_article <- httr::GET(
    as.character(link),
    user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:104.0) Gecko/20100101 Firefox/104.0"),
    write_disk(glue::glue("./data/raw/articles/{basename(article_link)}.html"),overwrite = TRUE)
  )

  #r_article <- xml2::read_html(glue::glue("./data/raw/article_{basename(article_link)}.html"))

  resp_title <- r_article |>
    read_html() |>
    xml_find_all("//h1[@class = 'wi-article-title article-title-main']") |>
    xml_text() |>
    str_squish()

  resp_date <- r_article |>
    read_html() |>
    xml_find_all("//span[@class = 'article-date']") |>
    xml_text() |>
    str_squish()

  resp_authors <- r_article |>
    read_html() |>
    xml_find_all("//a[@class = 'linked-name js-linked-name stats-author-info-trigger']") |>
    xml_text() |>
    str_squish() |>
    paste(collapse = ",")

  resp_infos <- r_article |>
    read_html() |>
    xml_find_all("//div[@class = 'ww-citation-primary']") |>
    xml_text() |>
    str_squish()

  resp_doi <- r_article |>
    read_html() |>
    xml_find_all("//div[@class = 'citation-doi']") |>
    xml_text() |>
    str_squish()

  resp_kw <- r_article |>
    read_html() |>
    xml_find_all("//div[@class = 'kwd-group']") |>
    xml_text() |>
    str_squish()

  resp_abstract <- r_article |>
    read_html() |>
    xml_find_all("//section[@class = 'abstract']") |>
    xml_text() |>
    str_squish()

  # Joint all informations in df

  resp_article <- tibble::tibble(
    id = basename(resp_doi),
    title = resp_title,
    date = lubridate::mdy(resp_date),
    authors = resp_authors,
    infos = resp_infos,
    doi = resp_doi,
    kw = resp_kw,
    abstract = resp_abstract
  ) |>
    mutate(year = as.numeric(lubridate::year(date))) |>
    dplyr::select(id, year, dplyr::everything())
}

articles_df <- links |>
  purrr::map(f_get_articles_contents) |>
  dplyr::bind_rows()


# Step 5 - Number of pages ------------------------------------------------

n_results_search <- r_demography_search |>
  httr::content() |>
  xml2::xml_find_first("//div[@ class ='sr-statistics at-sr-statistics']") |>
  xml2::xml_text() |>
  stringr::str_squish() |>
  stringr::str_extract("[0-9]+$") |>
  as.numeric()

n_pages <- ceiling(n_results_search / 20)


# Step 6 - Downloading all of pages ---------------------------------------

page_vector <- 1:n_pages

# Acesso a pagina de busca or termo

download_page_search <- function(pag, search = "headship", prog = NULL) {

  if (!is.null(prog)) prog()

  u_pag <- glue::glue("https://read.dukeupress.edu/demography/search-results?{search}&fl_SiteID=1000157&page={pag}")

  httr::GET(
    u_pag,
    user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:104.0) Gecko/20100101 Firefox/104.0"),
    httr::write_disk(
      glue::glue("data/raw/pages/page_{pag}.html"),
      overwrite = TRUE
    )
  )
}

#progressr::with_progress({
#  p <- progressr::progressor(length(page_vector))
#  purrr::walk(page_vector, download_page_search, prog = p)
#})

# Step 7 - To get list of links for MORE THAR ONE page ----------

files <- fs::dir_ls("./data/raw/pages") |> as.vector()

f_get_link_list <- function(files, prog = NULL) {

  if (!is.null(prog)) prog()

  r_page <- xml2::read_html(as.character(files)) |>
    xml_find_all("//div[@class = 'sri-figure']//a") |>
    xml_attr("href") |>
    tibble::tibble() |>
    janitor::clean_names()

  f_apply_html <- function(x){
    paste0("https://read.dukeupress.edu",x)
  }

  r_page_final <- purrr::map(r_page, f_apply_html) |>
    dplyr::bind_rows()

}

#link_df <- progressr::with_progress({
#  p <- progressr::progressor(length(page_vector))
#  purrr::map(files, f_get_link_list, prog = p) |>
#    dplyr::bind_rows()
#})

#link_df <- link_df[-1,]

#saveRDS(link_df, file = "data/dataset/link_df.rds")

# Step 7 - To get ALL of the article contents -----------------------------
## REMEMBER ME, here, to put Sys.sleep(1) for spacing the requires.

link_df <- readRDS(file = "data/dataset/link_df.rds")

link <- as.vector(link_df$xml_attr)

f_get_articles_contents_all <- function(link, prog = NULL) {

  if (!is.null(prog)) prog()

  r_article <- httr::GET(
    link,
    user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:104.0) Gecko/20100101 Firefox/104.0")
    #está dando problema no basename
    #write_disk(glue::glue("data/raw/articles/{basename(as.vector(link))}.html"), overwrite = TRUE)
  )

  resp_title <- r_article |>
    read_html() |>
    xml_find_all("//h1[@class = 'wi-article-title article-title-main']") |>
    xml_text() |>
    str_squish()

  resp_date <- r_article |>
    read_html() |>
    xml_find_all("//span[@class = 'article-date']") |>
    xml_text() |>
    str_squish()

  resp_authors <- r_article |>
    read_html() |>
    xml_find_all("//a[@class = 'linked-name js-linked-name stats-author-info-trigger']") |>
    xml_text() |>
    str_squish() |>
    paste(collapse = ",")

  resp_infos <- r_article |>
    read_html() |>
    xml_find_all("//div[@class = 'ww-citation-primary']") |>
    xml_text() |>
    str_squish()

  resp_doi <- r_article |>
    read_html() |>
    xml_find_all("//div[@class = 'citation-doi']") |>
    xml_text() |>
    str_squish()

  resp_kw <- r_article |>
    read_html() |>
    xml_find_all("//div[@class = 'kwd-group']") |>
    xml_text() |>
    str_squish()

  resp_abstract <- r_article |>
    read_html() |>
    xml_find_all("//section[@class = 'abstract']") |>
    xml_text() |>
    str_squish()

  # Joint all informations in df

  resp_article <- tibble::tibble(
    title = resp_title,
    date = lubridate::mdy(resp_date),
    authors = resp_authors,
    infos = resp_infos,
    doi = resp_doi,
    kw = resp_kw,
    abstract = resp_abstract
  ) |>
    mutate(year = as.numeric(lubridate::year(date))) |>
    dplyr::select(year, dplyr::everything())
}

maybe_get_articles <- purrr::possibly(f_get_articles_contents_all, NULL) # validating collection

# Let's run!!

tic()
articles_df <- progressr::with_progress({

  p <- progressr::progressor(length(link))

  link |>
    purrr::map(maybe_get_articles, prog = p) |>
    dplyr::bind_rows()
})
toc()

articles_df_1 <- articles_df |> distinct()

saveRDS(articles_df_1, file = "data/dataset/articles_df.rds")
