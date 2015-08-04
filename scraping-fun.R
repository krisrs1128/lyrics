library("rvest")
library("stringr")

process_page <- function(url) {
  page_html <- url %>% read_html
  page_data <- list()
  page_data$url <- url
  lyrics <- page_html %>%
    html_nodes("br + div") %>%
    html_text
  lyrics <- gsub("\n", " ", lyrics)
  lyrics <- str_trim(lyrics)
  page_data$lyrics <- gsub("[[:punct:]]||\r", "", lyrics)
  album_year <- page_html %>%
    html_nodes(".album-panel a") %>%
    html_text
  page_data$album <- str_extract(album_year, "[A-z\\s]+")[1]
  page_data$year <- str_extract(album_year, "[0-9]+")[1]
  return (page_data)
}

get_musicians_by_letter <- function(letter) {
  url <- paste0("http://www.azlyrics.com/", letter, ".html")
  page_html <- url %>% read_html
  artists_urls <- page_html %>%
    html_nodes(".artist-col a") %>%
    html_attr("href")
  artists_urls <- paste0("http://www.azlyrics.com/",artists_urls)
  return (artists_urls)
}

get_songs_for_artist <- function(artist_url) {
  page_html <- artist_url %>% read_html
  songs <- page_html %>%
    html_nodes("#listAlbum a") %>%
    html_attr("href")
  songs <- sapply(songs, function(x) {
    prefix <- substr(x, 1, 3) == "../"
    if(!is.na(prefix) && prefix) {
      return (substr(x, 4, nchar(x)))
    } else {
      return (NA)
    }
  })
  paste0("http://www.azlyrics.com/", na.omit(songs))
}

# example text mining
# library("tm")
# songs <- get_songs_for_artist("http://www.azlyrics.com/r/radiohead.html")
# song <- process_page(songs[20])
# song_corp <- VCorpus(VectorSource(gsub("[[:punct:]]||\n", "", song$lyrics)))
# song_corp <- tm_map(song_corp, stripWhitespace)
# song_corp <- tm_map(song_corp, content_transformer(tolower))
# song_corp <- tm_map(song_corp,  removeWords, stopwords("english"))
# as.matrix(DocumentTermMatrix(song_corp))
