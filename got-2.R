

# Load the necessary libraries
library("readr")
library("ggplot2")
library("dplyr")
library("ggrepel")


# Import the csv
screentimes <- read_csv("data/GOT_screentimes_1.csv")

# Filter the characters with a
screentimes_high <- top_n(screentimes, 10, screentime)

# Visualize screentime vs. number of episodes
ggplot(screentimes, aes(screentime, episodes)) +
  geom_point() +
  geom_text_repel(data = screentimes_high,aes(label = name),min.segment.length = 0)
