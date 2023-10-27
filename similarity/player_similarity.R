# Libraries
library(tidyverse)
library(kickR) # Use: devtools::install_github('jeffreyohene/kickR') to install

# Scrape data
df <- fbref_big5_player_stats(type = 'passing') # Date: 27-10-2023


# Metrics to analyze
passing <- c(
  'total_passes_completed', 'total_passes_attempted',
  'pass_completion_percentage', 'total_passing_distance',
  'total_progressive_distance', 'key_passes')

creativity_metrics <- c(
  'assists', 'xA', 'xAG', 'key_passes', 'passes_into_penalty_box',
  'xag_performance', 'progressive_passes')


# Convert to numeric
df[passing] <- lapply(df[passing], as.numeric)

# Similar players to TK8 in terms of selected passing metrics
target_player_name <- 'Toni Kroos'

# Select the target player's data based on their name
target_player <- df[df$player == target_player_name, passing]


# Passing similarity
# First approach: cosine
if (nrow(target_player) == 0) {
  cat('Target player not found in the dataset.')
} else {
  # Perform the similarity calculation using the target player's data
  cosine_similarity_with_target <- function(player) {
    dot_product <- sum(target_player * player)
    norm_target <- sqrt(sum(target_player^2))
    norm_player <- sqrt(sum(player^2))
    cosine_sim <- dot_product / (norm_target * norm_player)
    return(cosine_sim)
  }

  # Calculate cosine similarity for all players
  cosine_similarities <- apply(df[passing], 1, cosine_similarity_with_target)

  similar_players_df <- data.frame(player = df$player, similarity = cosine_similarities)

  # Sort the data frame by similarity in descending order
  similar_players_df <- similar_players_df[order(similar_players_df$similarity, decreasing = T), ]
}

# We got this result
#head(similar_players_df, n=15)
#player similarity
#1077        Toni Kroos  1.0000000
#1326    Facundo Medina  0.9999927
#1555     Willian Pacho  0.9999887
#1891      Thiago Silva  0.9999849
#773    Jonathan Gradit  0.9999803
#1474 Rasmus Nicolaisen  0.9999787
#2095   Virgil van Dijk  0.9999778
#1574     Daniel Parejo  0.9999742
#228        Sam Beukema  0.9999699
#1754 Alessio Romagnoli  0.9999673
#893        Timo Hübers  0.9999659
#759     Édgar González  0.9999632
#1617      Lucas Perrin  0.9999606
#821    Norbert Gyömbér  0.9999597
#603            Éderson  0.9999594


# Second approach: Euclidean
if (nrow(target_player) == 0) {
  cat('Target player not found in the dataset.')
} else {
  # Perform the similarity calculation using the target player's data
  euclidean_distance_with_target <- function(player) {
    euclidean_dist <- sqrt(sum((target_player - player)^2))
    return(euclidean_dist)
  }

  # Calculate Euclidean distance for all players
  euclidean_distances <- apply(df[passing], 1, euclidean_distance_with_target)

  # Combine player names and their Euclidean distances in a data frame
  similar_players_df <- data.frame(player = df$player, distance = euclidean_distances)

  # Sort the data frame by distance in ascending order (smaller is more similar)
  similar_players_df <- similar_players_df[order(similar_players_df$distance), ]
}

#head(similar_players_df, n=15)
#                       player distance
#1077         Toni Kroos   0.0000
#244         Daley Blind 202.5893
#1042         Robin Koch 240.5414
#1850 Nico Schlotterbeck 401.0349
#1757    Cristian Romero 462.3906
#228         Sam Beukema 462.9903
#897        Mats Hummels 493.3184
#89       Waldemar Anton 572.2902
#1648    Marin Pongračić 575.6878
#1747   Kirian Rodríguez 580.3398
#1700           Tim Ream 645.3027
#2042      Fikayo Tomori 648.3966
#80     Joachim Andersen 675.1046
#1771    Antonio Rüdiger 692.9293
#543           Issa Diop 704.5833


# Define the name of the target player
target_player_name <- 'Martín Zubimendi'

# Ensure that numeric columns are converted to numeric data type
df[creativity_metrics] <- lapply(df[creativity_metrics], as.numeric)

# Select the target player's data based on their name
target_player <- df[df$player == target_player_name, creativity_metrics]


# Creation
# First approach: cosine
if (nrow(target_player) == 0) {
  cat('Target player not found in the dataset.')
} else {
  # Perform the similarity calculation using the target player's data
  cosine_similarity_with_target <- function(player) {
    dot_product <- sum(target_player * player)
    norm_target <- sqrt(sum(target_player^2))
    norm_player <- sqrt(sum(player^2))
    cosine_sim <- dot_product / (norm_target * norm_player)
    return(cosine_sim)
  }

  # Calculate cosine similarity for all players
  cosine_similarities <- apply(df[creativity_metrics], 1, cosine_similarity_with_target)

  similar_players_df <- data.frame(player = df$player, similarity = cosine_similarities)

  # Sort the data frame by similarity in descending order
  similar_players_df <- similar_players_df[order(similar_players_df$similarity, decreasing = T), ]
}


#head(similar_players_df, n=15)
#                     player similarity
#2237  Martín Zubimendi  1.0000000
#884     Fabian Holland  0.9998253
#1617      Lucas Perrin  0.9996114
#1186     Matteo Lovato  0.9995951
#24    Michel Aebischer  0.9995934
#795         Marc Guéhi  0.9995787
#5    Salis Abdul Samed  0.9995786
#318     Moisés Caicedo  0.9995740
#1443  Joseph N'Duquidi  0.9995520
#31        Joseph Aidoo  0.9995447
#236      Yves Bissouma  0.9995432
#2114       Mikel Vesga  0.9995252
#1579            Patric  0.9995242
#213     Yuri Berchiche  0.9995172
#1757   Cristian Romero  0.9995085


# Second approach: Euclidean
if (nrow(target_player) == 0) {
  cat('Target player not found in the dataset.')
} else {
  # Perform the similarity calculation using the target player's data
  euclidean_distance_with_target <- function(player) {
    euclidean_dist <- sqrt(sum((target_player - player)^2))
    return(euclidean_dist)
  }

  # Calculate Euclidean distance for all players
  euclidean_distances <- apply(df[creativity_metrics], 1, euclidean_distance_with_target)

  # Combine player names and their Euclidean distances in a data frame
  similar_players_df <- data.frame(player = df$player, distance = euclidean_distances)

  # Sort the data frame by distance in ascending order (smaller is more similar)
  similar_players_df <- similar_players_df[order(similar_players_df$distance), ]
}


#head(similar_players_df, n =15)
#                  player distance
#2237   Martín Zubimendi 0.000000
#1264         Marquinhos 2.351595
#1290      Nemanja Matić 2.362202
#683          Juan Foyth 2.570992
#758     Maxime Gonalons 2.592296
#1131        Diogo Leite 2.778489
#884      Fabian Holland 3.165438
#24     Michel Aebischer 3.243455
#5     Salis Abdul Samed 3.249615
#1230  Antoine Makoumbou 3.266497
#1044       Dominik Kohr 3.269557
#213      Yuri Berchiche 3.423449
#861       Mario Hermoso 3.451087
#2077     Destiny Udogie 3.683748
#2204 Warren Zaire-Emery 3.693237

# This makes much more sense as I think Zubimendi has a smilar play style
# to Matić


# Eucledian approach seems to capture feature importance better.
# Script will be continued soon and more tests will be run for more metrics
# with different players
