library(rvest)
library(tidyverse)
library(imager)
load("Cars_Data.RData")
html <- read_html("https://fastestlaps.com/lists/top-most-expensive-cars")
website <- html %>%  html_elements(".table.table-striped a") %>% html_attr("href")
posters <- list(length = 100)
img_name <- character(length = 100)
link <- character(length = 100)
for(i in 1 : 100){
  website[i] <- paste0("https://fastestlaps.com", website[i])
  html_car <- read_html(website[i])
  link[i] <- html_car %>% html_elements(".fl-img-responsive-fw.fl-cover") %>% html_attr("src")
  #file = paste0("car", i, ".jpeg")
  #download.file(link[i], file)
  #posters[[i]] <- load.image(file)
}
for(i in 1 : 100){
  img_name[i] = paste0("Images/car", i, ".jpeg")
}
library(dplyr)
data <- data %>% mutate(img = img_name)
save(data, file = "Cars_Data.RData")
