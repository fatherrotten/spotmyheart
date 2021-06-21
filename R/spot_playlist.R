spot_playlist <- function() {

  if (Sys.getenv("SPOTIFY_CLIENT_ID") == "") {
    Sys.setenv(SPOTIFY_CLIENT_ID = "23286ab9a1204071b54ad1b8567451fb",
               SPOTIFY_CLIENT_SECRET = "b5fda2113fed4a51a626cf828533e23b")
  }
  smh_auth <- spotifyr::get_spotify_authorization_code()
  smh_token <- spotifyr::get_spotify_access_token()
  smh_play <- spotifyr::get_my_playlists()
  smh_list <- smh_play[which(smh_play$public == TRUE), c("name","tracks.total")]
  colnames(smh_list) <- c("playlist", "tracks")
  smh_select <- 1
  if (dim(smh_list)[1] != 1) {
    print(smh_list)
    smh_select <- as.numeric(readline(prompt = "Enter number of playlist to rank:"))
  }
  smh_playid <- smh_play$id[smh_select]
  smh_playlist <- spotifyr::get_playlist(smh_playid, smh_token)
return(smh_playlist)
}

spot_tracklist <- function(smh_playlist) {

  smh_tracklist <- smh_playlist$tracks$items[,c(9, 31, 18)]
  smh_tracklist$track.artists <- sapply(smh_tracklist$track.artists,
                                        FUN = function(x) x[1,3])
  colnames(smh_tracklist) <- c("artist", "album", "track")
  smh_lang <- cld2::detect_language(smh_tracklist$track)
  smh_lang[is.na(smh_lang)] <- "en"
  other_lang <- which(smh_lang != "en")
  options(warn = -1)
  for (i in other_lang) {
    smh_tracklist$track[i] <-
      translateR::translate(content.vec = smh_tracklist$track[i],
                            google.api.key = "AIzaSyCAlnuhcE38qMQpRI42MKd5NFrk23MCo0s",
                            source.lang = smh_lang[i], target.lang = "en")
  }
  options(warn = 0)
  for (i in 1:3) {
    smh_tracklist[, i] <- trimws(iconv(smh_tracklist[, i], "latin1", "ASCII", sub = ""))
  }
  return(smh_tracklist)
}

spot_popular <- function(smh_playlist) {
  temp_rank <- smh_playlist$tracks$items$track.popularity
  tabl_rank <- as.data.frame(table(temp_rank))
  colnames(tabl_rank)[1] <- "rank"
  tabl_rank$rank <- as.integer(as.character(tabl_rank$rank))

  temp_clst <- kmeans(tabl_rank, 8)
  temp_clst <- temp_clst$cluster
  tabl_rank$cluster <- temp_clst

  tabl_clst <- as.data.frame(table(temp_clst))
  colnames(tabl_clst)[1] <- "cluster"
  tabl_clst$cluster <- as.integer(as.character(tabl_clst$cluster))

  smh_simil <- tabl_rank[tabl_rank$Freq > 3, c("rank","Freq")]
  smh_diffr <- tabl_clst[tabl_clst$Freq > median(temp_clst), ]

  track_pop <- data.frame(id = smh_playlist$tracks$items$track.id,
                          rank = smh_playlist$tracks$items$track.popularity,
                          freq = 0,
                          clas = 0)
  for (i in 1:dim(track_pop)[1]) {
    track_pop$freq[i] <- tabl_rank[tabl_rank$rank == track_pop$rank[i], "Freq"]
    track_pop$clas[i] <- tabl_rank[tabl_rank$rank == track_pop$rank[i], "cluster"]
  }
  smh_list <- list(pop_rank = smh_simil, pop_cluster = smh_diffr, all = track_pop)

  return(smh_list)
}

spot_sample <- function(smh_trackid) {
  smh_analysis <- spotifyr::get_track_audio_analysis(smh_trackid)
  smh_player <- get_my_current_playback()
  if (smh_player$is_playing) {
    pause_my_playback()
  }
  if (smh_player$shuffle_state == TRUE) {
    toggle_my_shuffle()
  }

  spotifyr::seek_to_position(smh_analysis)

  start_my_playback()
}
