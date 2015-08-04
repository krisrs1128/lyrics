metadata_file <- "metadata"
failed_pages <- "fail_page"
failed_artists <- "failed_artists"
failed_letters <- "failed_letters"

cat(c("url\t", "lyrics\t", "album\t", "year", "\n"), file = metadata_file)
cat("failed_pages \n", file = failed_pages)
cat("failed_artists \n", file = failed_artists)
cat("failed_letters \n", file = failed_letters)

for(letter in letters) {
  # Attempt to get artists whose names start with a certain letter
  artists <- try(get_musicians_by_letter(letter))
  if(class(artists) == "try-error") {
    cat(letter, "\n", file = failed_letters, append = TRUE)
  } else {
    for(artist in artists) {
      # Attempt to get paintings for current artist
      songs <- try(get_songs_for_artist(artist))
      if(class(songs) == "try-error") {
        cat(artist, "\n", file = failed_artists, append = TRUE)
      } else {
        for(song in songs) {
          # Attempt to scrape page data
          print(song)
          cur_page <- try(process_page(song))
          if(class(cur_page) == "try-error") {
            cat(song, "\n", file = failed_pages, append = TRUE)
            Sys.sleep(10)
          } else {
            cat(paste0(cur_page, collapse = "\t"), "\n", file = metadata_file, append = TRUE)
            Sys.sleep(10)
          }
        }
      }
    }
  }
}

