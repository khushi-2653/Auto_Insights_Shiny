library(rvest)
library(tidyverse)
html <- read_html("https://fastestlaps.com/lists/top-most-expensive-cars")
website <- html %>%  html_elements(".table.table-striped a") %>% html_attr("href")
names <- html %>% html_elements(".table.table-striped a") %>% html_text()

price <- character(length = 100)
price_asnum <- integer(length = 100)
car_type <- character(length = 100)
weight <- numeric(length = 100)
origin <- character(length = 100)


for(i in 1 : 100){
  website[i] <- paste0("https://fastestlaps.com", website[i])
  html_car <- read_html(website[i])
  demo <- html_car %>% html_elements(".table.fl-datasheet") %>% html_text()
  price[i] <- str_extract(demo[1], "€[0-9,]+")
  #price_asnum[i] <- as.numeric(gsub("[€,]", "", price))
  car_type[i] <- str_match(demo[1], "Car type\\s+([A-Za-z]+)")[,2]
  w <- str_extract(demo[1], "\\d+ kg")
  weight[i] <- as.numeric(str_extract(w, "\\d+"))
  origin[i] <- str_match(demo[1], "Origin country\\s+([A-Za-z]+)")[,2]
}
for(i in c(16,18,29,32,36,37,44,47,51,65,85,91,93)){
  car_type[i] <- "4-door saloon"
}
weight[25] <- 1437

zero_to_100 <- numeric(length = 100)
zero_to_200 <- numeric(length = 100)
hundred_to_200 <- numeric(length = 100)
zero_to_60 <- numeric(length = 100)
top_speed <- numeric(length = 100)
max_acc <- numeric(length = 100)

for(i in 1 : 100){
  html_car <- read_html(website[i])
  demo <- html_car %>% html_elements(".table.fl-datasheet") %>% html_text()
  # Initialize variables to store results
  top_speed[i] <- NA
  zero_to_60[i] <- NA
  max_acc[i] <- NA
  
  # Loop through each string in demo
  for (text in demo) {
    # Extract Top Speed (kph) - looking for "Top speed" with flexible pattern
    if (is.na(top_speed[i])) {  # Only search if not already found
      match_top_speed <- str_match(text, "Top speed\\s*(\\d+)(?=\\s*kph|\\s*\\()")
      if (!is.na(match_top_speed[1, 2])) {
        top_speed[i] <- as.numeric(match_top_speed[1, 2])
      }
    }
    
    # Extract 0 - 60 mph time - flexible pattern to account for different formats
    if (is.na(zero_to_60[i])) {  # Only search if not already found
      match_zero_to_60 <- str_match(text, "0 - 60 mph\\s*(\\d+\\.\\d+)")
      if (!is.na(match_zero_to_60[1, 2])) {
        zero_to_60[i] <- as.numeric(match_zero_to_60[1, 2])
      }
    }
    
    # Extract max acceleration (g) - flexible pattern to capture acceleration in g
    if (is.na(max_acc[i])) {  # Only search if not already found
      match_max_acc <- str_match(text, "max acceleration\\s*(\\d+\\.\\d+)")
      if (!is.na(match_max_acc[1, 2])) {
        max_acc[i] <- as.numeric(match_max_acc[1, 2])
      }
    }
    
    # Exit loop early if all values are found
    if (!is.na(top_speed[i]) && !is.na(zero_to_60[i]) && !is.na(max_acc[i])) {
      break
    }
  }
  zero_to_100[i] <- as.numeric(str_extract(demo[2], "(?<=0 - 100 kph)\\d+\\.\\d+"))
  #zero_to_200[i] <- as.numeric(str_extract(demo[2], "(?<=0 - 200 kph)\\d+\\.\\d+"))
  #hundred_to_200[i] <- as.numeric(str_extract(demo[2], "(?<=100 - 200 kph)\\d+\\.\\d+"))
  #zero_to_60[i] <- as.numeric(str_extract(demo[2], "(?<=0 - 60 mph)\\d+\\.\\d+"))
  # Top speed (in kph) - extracting only the kph value
  #top_speed[i] <- as.numeric(str_extract(demo[2], "(?<=Top speed)\\d+"))
  # Maximum acceleration (in g)
  #max_acc[i] <- as.numeric(str_extract(demo[2], "(?<=max acceleration)\\d+\\.\\d+"))
  
}
engine_type <- character(length = 100)
power <- character(length = 100)
displacement <- character(length = 100)
power_per_liter <- character(length = 100)
for(i in 1 : 100){
  html_car <- read_html(website[i])
  demo <- html_car %>% html_elements(".table.fl-datasheet") %>% html_text()
  engine_type = NA
  power = NA
  displacement = NA
  power_per_liter = NA
  
  # Loop through each element and extract information
  for (item in demo) {
    engine_match <- str_extract(item, "Engine type\\s*\\n*\\s*([^\n]+)")
    if (!is.na(engine_match)) {
      engine_type[i] <- str_trim(sub("Engine type\\s*", "", engine_match))
    }
    
    # Search for "power" pattern (match with ps, bhp, or hp)
    power_match <- str_extract(item, "Power\\s*\\n*\\s*([0-9]+\\s*(ps|bhp|hp))")
    if (!is.na(power_match)) {
      power[i] <- str_trim(sub("Power\\s*", "", power_match))
    }
    
    # Search for "displacement" pattern
    displacement_match <- str_extract(item, "Displacement\\s*\\n*\\s*([^\n]+)")
    if (!is.na(displacement_match)) {
      displacement[i] <- str_trim(sub("Displacement\\s*", "", displacement_match))
    }
    
    # Search for "power per liter" pattern
    power_per_liter_match <- str_extract(item, "Power / liter\\s*\\n*\\s*([^\n]+)")
    if (!is.na(power_per_liter_match)) {
      power_per_liter[i] <- str_trim(sub("Power / liter\\s*", "", power_per_liter_match))
    }
  }
}
data <- data.frame(names, price, car_type, weight, origin, top_speed, zero_to_60, max_acc, zero_to_100)
save(data,  file = "Cars_Initial.Rdata")

load("Cars_Initial.RData")
for(i in 1 : 100){
  #price_asnum[i] <- as.numeric(str_replace_all(str_extract(data$price[i], "\\d+[,]\\d+"), ",", ""))
  price_asnum[i] <- as.numeric(gsub("[€,]", "", data$price[i]))
}
for(i in 1:100){
  if(!is.na(data$origin[i]) && data$origin[i] == "United"){
    data$origin[i] <- "United States"
  }
}
library(dplyr)
data <- data %>% mutate(price_asnum = price_asnum)
data <- data %>% mutate(brand = sub(" .*", "", names))

save(data, file = "Cars_Data.RData")
save(price_asnum, file = "Numeric_Price.RData")
