library(janitor)
library(factoextra)
library(sp)
library(dplyr)
library(lubridate)
library(rgeos)

top2000 <- read.csv("C:\\Users\\jmhea\\Documents\\Radio_Spotify\\spotify_audio.csv")

# clean column names
top2000 <- clean_names(top2000)

# making clean names of track name column to se as index
top2000$track_name <- janitor::make_clean_names(top2000$track_name)

# sets track name as index
row.names(top2000) <- top2000$track_name

# set to dataframe not itbble
top2000 <- as.data.frame(top2000)

# keeps only numeric values of music
top2000_subset <- base::subset(top2000, select = c(danceability:tempo, track_duration_ms, track_popularity, track_album_release_date))

# prints he first few observations
head(top2000_subset)

top2000_subset$track_album_release_date <- substr(top2000$track_album_release_date, 1,4)

top2000_subset$track_album_release_date <- as.numeric(top2000_subset$track_album_release_date)

# scales the dataset before running pca
df_scale_music <- scale(top2000_subset)

# runs pca on music dataframe
df_music_pca <- prcomp(df_scale_music)

summary(df_music_pca)

# shows elbow plot for pca selection
# factoextra::fviz_eig(df_music_pca)

# View which variable influences PCA the most
# factoextra::fviz_pca_var(
#   X = df_music_pca,
#   col.var = "contrib",
#   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#   repel = TRUE
# )

# From plot only really need the first PCA, but we will take 3 for clustering

# used for selecting the number of pca
list_of_pca <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10")

# how many pca?
number_of_pca <- 4

# gets the dataframe from pca and then keeps only the first 3 pca
df_music_pca <- as.data.frame(df_music_pca$x[,list_of_pca[1:number_of_pca]])

# prints frst few observations
# head(df_music_pca)

# reassigns a column for df_music-pca
df_music_pca$song <- row.names(df_music_pca)

# sets a new df for spaical analysis
spacial_pca <- df_music_pca

# removes row names
row.names(spacial_pca) <- NULL

# gets coordinatoes for spacial_pca
coordinates(spacial_pca) <- eval(parse(text = paste0("~", paste(list_of_pca[1:number_of_pca], collapse = "+"))))

# gets distance between each variable
d <- gDistance(spacial_pca, byid = TRUE)

# how many songs?
playlist_length <- 30

# finds the 30 minimum distance between each point, excluding it point itself
min.d <- apply(d, 1, function(x) order(x, decreasing=F)[2:(playlist_length + 1)])

# creates a matrix of the original song and recommended songs
m_music_rec <- cbind(df_music_pca[,"song"], apply(min.d, 1, function(x) df_music_pca[x, "song"]))

# sets matrix as dataframe
df_music_rec <- as.data.frame(m_music_rec)

# set column names as original song and the recommended songs
names(df_music_rec) <- c("Original_song", paste0("Rec_", seq(from = 1, to = ncol(m_music_rec) - 1)))

# Radio of what song?
## Filter
song_radio <- "losing_my_religion"

df_music_rec %>%
  dplyr::filter(Original_song == song_radio)
