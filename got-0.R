screentimes <- read_csv("data/GOT_screentimes.csv")


screentimes <- screentimes |> 
  mutate(
    episodes = as.integer(episodes)
  )

write_csv(screentimes, "data/GOT_screentimes_1.csv")
