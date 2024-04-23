
################################################################################
## Get the names of git hosting services
################################################################################


library(rvest)


read_html("https://www.git-tower.com/blog/git-hosting-services-compared/") |>
  html_node(".content") |>
  html_nodes("a") |>
  html_text() |>
  paste() |>
  cat(sep = "\n")






################################################################################
## Get the URLs of CRAN repos
################################################################################


pdb <- tools:::CRAN_package_db() 


library(tidyverse)


pdb$URL

str_detect(pdb$URL)

counts <- list(
  github = "github", 
  bitbucket = "bitbucket",
  gitlab = "gitlab"
) |>
  imap_int(function(pattern, name){sum(str_detect(pdb$URL, pattern) | str_detect(pdb$BugReports, pattern),na.rm = TRUE)}) |>
  as.data.frame() |>
  rownames_to_column() |>
  rename(domain = 1, n = 2)


write_csv(counts, "CRAN_str_detect.csv")
  
