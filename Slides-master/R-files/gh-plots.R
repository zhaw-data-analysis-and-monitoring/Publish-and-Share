library(tidyverse)

library(ggtext)
mytheme <- theme(
  panel.background = element_rect(fill = "#272822",colour = "#272822"),
  plot.background = element_rect(fill = "#272822",colour = "#272822"),
)
geom_arrow <- function(xstart,xend, y, width = NULL, spanend = NULL, label = "",direction = "right", colour = "black", fill = "#E16460", inherit.aes = FALSE) {
  xdelta <- abs(xend-xstart)
  if(is.null(width)){
    width <- xdelta/1.618
    message(paste("width:",width))
  }
  
  if(is.null(spanend)){
    spanend <- xdelta/3*1
    message(paste("spanend:",spanend))
  }
  
  
  
  arrowh <- xend - spanend
  halfwidth <- width/2
  b <- width/2*3
  arroww <- b/2
  
  dd <- tibble(
    x = c(xstart,xstart, arrowh, arrowh, xend, arrowh,arrowh,  xstart, xstart),
    y = c(y,y-halfwidth, y-halfwidth, y-arroww,y, y+arroww, y+halfwidth, y+halfwidth, y)
  )
  if(direction == "left"){
    mid <- median(c(xstart,xend))
    mid - dd$x + mid  -> dd$x
  }
  
  list(
    geom_polygon(data = dd, aes(x,y), colour = "black", fill = fill,inherit.aes = inherit.aes),
    geom_richtext(x = xstart+xdelta/2, y = y, hjust = 0.5, label = label, fill = NA, label.color = NA, text.colour = "white")
    )

}



workflow_plot <- function(arrows, locations, output = NULL){
  
  width = 0.618046971569839
  spanend = 1/3
  # working_directory_x <- 1
  # stagin_area_x <- 2
  # local_repo_x <- 3
  # remote_repo_x <- 5
  
  arrows_l <- list(
    add = geom_arrow(1,2,3.5,label = "git add"),
    commit = geom_arrow(2,3,3.5,label = "git commit"),
    commit_false = geom_arrow(1,3,3.5,label = "git commit",width = width,spanend = spanend),
    push = geom_arrow(3,4,3.5,label = "git push"),
    pull = geom_arrow(1,4,2.5,direction = "left", label = "git pull",width = width,spanend = spanend),
    fetch = geom_arrow(3,4,1.5,direction = "left", label = "git fetch",width = width,spanend = spanend),
    checkout = geom_arrow(1,3,1.5,direction = "left", label = "git checkout",width = width,spanend = spanend)
  
      )
  
  p <- tribble(
    ~label1, ~x, ~label2,
    "working<br>directory", 1,  "your files",
    "staging<br>area", 2, "'selected' files",
    "local repo", 3,  "'.git'-folder",
    "remote<br>repo", 4, "Github"
  ) |>
    ggplot() +
    {if(4 %in% locations)geom_polygon(data= tibble(x = c(3.5, 4.5, 4.5, 3.5), y = c(-.5, -.5, 5.5, 5.5)), aes(x, y), fill = "grey", alpha = 0.2)}+
    geom_segment(data = ~slice(., locations),aes(x = x,y = 0, xend = x),yend = 5)+
    arrows_l[arrows] +
    geom_textbox(data = ~slice(., locations),aes(x,label = label1), y= 5,
                 fill = "lightgrey", 
                 width = unit(2, "cm"),
                 height = unit(1, "cm"),
                 halign = 0.5,
                 valign = 0.5
    ) +
    geom_textbox(data = ~slice(., locations),aes(x,label = label2), y= 0,
                 fill = "lightgrey", 
                 width = unit(2, "cm"),
                 height = unit(1, "cm"),
                 halign = 0.5,
                 valign = 0.5
    )  +
    theme_void() +
    lims(x = c(0, 6), y = c(-.5, 5.5)) +
    mytheme 
  if(!is.null(output)){
    ggsave(output, p,dpi = 300,width = 20,height = 10, units = "cm")
  } else{
    p
  }
}


workflow_plot("commit_false",1)


workflow_plot(output = "images/gh1.jpg", "",1)
workflow_plot(output = "images/gh2.jpg", "",c(1,3))
workflow_plot(output = "images/gh3.jpg", "commit_false",c(1,3))

workflow_plot(output = "images/gh4.jpg", "",c(1:3))
workflow_plot(output = "images/gh5.jpg", c("add"),c(1:3))
workflow_plot(output = "images/gh6.jpg", c("add","commit"),c(1:3))
workflow_plot(output = "images/gh7.jpg", c("add","commit"),c(1:4))
workflow_plot(output = "images/gh8.jpg", c("add","commit","push"),c(1:4))
workflow_plot(output = "images/gh9.jpg", c("add","commit","push","pull"),c(1:4))
workflow_plot(output = "images/gh10.jpg", c("add","commit","push","pull","fetch","checkout"),c(1:4))


  
