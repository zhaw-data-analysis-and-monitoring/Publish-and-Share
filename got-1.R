
library("readr")
library("ggplot2")
library("dplyr")
library("ggrepel")

screentimes <- read_csv("data/GOT_screentimes_1.csv")

screentimes_high <- top_n(screentimes, 10, screentime)

ggplot(screentimes, aes(screentime, episodes)) +
  geom_point() +
  geom_text_repel(data = screentimes_high,aes(label = name),min.segment.length = 0)
