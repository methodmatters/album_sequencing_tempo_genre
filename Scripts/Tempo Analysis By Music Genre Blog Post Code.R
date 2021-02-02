# load the libraries we'll use
library(plyr); library(dplyr)
library(ggplot2)
library(readr)

# set the directory for the cleaned data file
in_dir <- 'D:\\Directory\\'

# read in the data
raw_data <- read_csv(paste0(in_dir, 'song_level_tempo.csv'))

# how many songs?
# 10054
nrow(raw_data)
# how many albums?
# 694
length(unique(raw_data$album_id))


# head of data set
head(raw_data, 10)  


# color palettes
# really nice guide here: https://github.com/EmilHvitfeldt/r-color-palettes

# load ggthemes in order to use
# economist palette
library(ggthemes)
pal <- economist_pal()(7)
single_color <- economist_pal()(9)[8]


# HISTOGRAMS
# HISTOGRAMS
# HISTOGRAMS
# HISTOGRAMS

# make the overall histogram
raw_data %>% 
  # pass the data to ggplot2
  ggplot(., aes(x=tempo)) +
  # specify that we want a histogram
  geom_histogram(position="identity", 
                 alpha=0.5, 
                 binwidth=1, 
                 color="black", 
                 fill = single_color) + # 'maroon'
  # use the classic theme
  theme_classic() +
  # specify the labels
  labs(x = "Tempo (beats per minute)", 
       y = "Count", 
       title = 'Histogram of Song Tempos (N = 10054 songs)', 
       caption = 'https://methodmatters.github.io')


# panel histogram by genre
raw_data %>% 
  # group the data by genre
  group_by(genre) %>%
  # calculate the number of songs / genre
  mutate(num_per_genre = n()) %>% 
  # remove genres with fewer than 200 songs
  filter(num_per_genre > 200) %>%
  # ungroup the data
  ungroup(genre) %>%
  # make a new genre column that specifies
  # how many songs there are for each genre
  mutate(genre = paste(genre, "  \n(", num_per_genre, " songs)", sep = '')) %>% 
  # pass the data to ggplot2
  ggplot(., aes(x=tempo, color=genre,
          fill=genre, alpha=0.1, position="identity")) +
  # specify that we want a histogram
  geom_histogram() + 
  # specify that we want the colors from the 
  # economist palette (defined above)
  scale_fill_manual(values=pal) +
  scale_color_manual(values=pal) +
  # use the classic theme
  theme_classic() +
  # turn off the legend
  theme(legend.position="none") +
  # specify the labels
  labs(x = "Tempo (beats per minute)", 
       y = "Count", 
       title = 'Histogram of Song Tempos by Genre' , 
       caption = 'https://methodmatters.github.io') +
  # make a separate panel for each genre
  facet_wrap(~ genre) 


# BAR CHARTS
# BAR CHARTS
# BAR CHARTS
# BAR CHARTS

# how many albums in the below selection?
# 613
raw_data %>% filter(num_tracks > 9) %>% select(album_id) %>% distinct() %>% dim 

# how many albums with fewer than 10 tracks?
# 81
raw_data %>% group_by(album_id) %>% 
  summarize(max_track_length = max(num_tracks)) %>% 
  filter(max_track_length < 10) %>%
  dim

# overall bar chart
raw_data %>% 
  # only keep albums with at least 10 tracks
  # (needed to divide album in 10 parts)
  filter(num_tracks > 9) %>% 
  # for each album, we calculate the decile for each track
  # along with the mean and std dev of the tempo per album
  # and finally the standardized value of the tempo per track
  group_by(album_id) %>% mutate(album_decile = ntile(track_number, 10),
                         mean_tempo_album = mean(tempo),
                         sd_tempo_album = sd(tempo),
                         std_tempo_track = (tempo -mean_tempo_album) /sd_tempo_album) %>%   
  # we then group by album decile and calculate the average difference in std dev
  # from the album average
  group_by(album_decile) %>% summarize(mean_tempo_decile = mean(std_tempo_track)) %>%   
  # pass the data to ggplot2
  ggplot(., aes(x = album_decile, y = mean_tempo_decile)) + 
  # specify that we want a bar chart
  geom_bar(stat = 'identity',color="black", fill=single_color,alpha=0.9)  + 
  # the x axis should go from 1 to 10 by 1
  # to indicate the album decile
  scale_x_continuous(breaks = seq(from = 1, to = 10, by = 1)) +
  # add the value labels above the bars
  # https://stackoverflow.com/questions/46300666/put-labels-over-negative-and-positive-geom-bar
  geom_text(aes(y = mean_tempo_decile + .005 * sign(mean_tempo_decile), 
                label = round(mean_tempo_decile,2)),
                position = position_dodge(width = 0.9), 
                size = 4, color = 'black') +  
  # use the classic theme
  theme_classic() +
  # turn off axis elements for a cleaner graph
  theme(axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank()) +
  # specify the labels
  labs(x = "Album Decile", 
       y = "Difference From Album Average (in Standard Deviations)", 
       title = 'Tempo Across Album Decile (613 albums)', 
       caption = 'https://methodmatters.github.io') 


# panel bar chart by genre
raw_data %>% 
  # only keep albums with at least 10 tracks
  # (needed to divide album in 10 parts)
  filter(num_tracks > 9) %>%
  # group by genre
  group_by(genre) %>%
  # calculate the number of albums per genre
  mutate(albums_per_genre = length(unique(album_id))) %>% 
  # only select genres with at least 30 albums
  filter(albums_per_genre > 30) %>%
  # ungroup the data
  ungroup(genre) %>% 
  # make a new genre column that specifies
  # how many albums there are for each genre
  mutate(genre = paste(genre, "  \n(", albums_per_genre, " albums)", sep = '')) %>% 
  # for each album, we calculate the decile for each track
  # along with the mean and std dev of the tempo per album
  # and finally the standardized value of the tempo per track
  group_by(album_id) %>% mutate(album_decile = ntile(track_number, 10),
                            mean_tempo_album = mean(tempo),
                            sd_tempo_album = sd(tempo),
                            centered_tempo_track = (tempo - mean_tempo_album) / sd_tempo_album  ) %>% 
  # we then group by genre and album decile 
  # and calculate the average difference in std dev
  # from the album average, separately per genre
  group_by(genre, album_decile) %>% summarize(mean_tempo_decile = mean(centered_tempo_track)) %>%  
  # pass the data to ggplot2
  ggplot(., aes(x = album_decile, y = mean_tempo_decile, color = genre)) + 
  # specify that we want a bar chart
  geom_bar(aes(fill = genre),stat = 'identity') +
  # specify that we want the colors from the 
  # economist palette (defined above)
  scale_fill_manual(values=pal) +
  scale_color_manual(values=pal) +
  # the x axis should go from 1 to 10 by 1
  # to indicate the album decile
  scale_x_continuous(breaks = seq(from = 1, to = 10, by = 1)) +
  # add the value labels above the bars
  # https://stackoverflow.com/questions/46300666/put-labels-over-negative-and-positive-geom-bar
  geom_text(aes(y = mean_tempo_decile + .03 * sign(mean_tempo_decile), 
                label = round(mean_tempo_decile,2)),
                position = position_dodge(width = 0.9), 
                size = 2.5, color = 'black') +  
  # use the classic theme
  theme_classic() +
  # turn off axis elements for a cleaner graph
  theme(legend.position="none") +
  theme(axis.text.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank())+
  # specify the labels
  labs(x = "Album Decile", 
       y = "Difference From Album Average (in Standard Deviations)", 
       title = 'Tempo Across Album Decile By Music Genre' , 
       caption = 'https://methodmatters.github.io') +
  # make a separate panel for each genre
  facet_wrap(~ genre)
