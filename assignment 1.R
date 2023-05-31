# 1st question:
library(rvest)
library(dplyr)


url <- "https://www.moneyworks4me.com/best-index/nse-stocks/top-nifty50-companies-list/"
page <- read_html(url)
table <- html_table(html_nodes(page, "table")[[1]])
df <- table[, c("Company Name (M.Cap)", "CMP", "Price Change", "Market Cap (Cr)", "52 Week High",
                "52 Week Low", "ROE", "P/E", "P/BV", "EV/EBITDA", "5YSales Gr(%)", "5YProfit Gr(%)")]
colnames(df) <- c("Company Name (M.Cap)", "CMP", "Price Change", "Market Cap (Cr)", "52 Week High",
                  "52 Week Low", "ROE", "P/E", "P/BV", "EV/EBITDA", "5YSales Gr(%)", "5YProfit Gr(%)")
colnames(df) <- gsub("\\s+|\\(|\\)", "", colnames(df))
df
View(df)




# 2nd question:
# giving error


















# 3rd question:
tennis <- function(p) {
  win_count <- 0
  set_count <- 0
  
  while(set_count < 5 &  win_count < 3 & set_count - win_count < 3  ){
    set_count = set_count +1
    win_count = win_count + rbinom(1,1,p)
  }
  x <- set_count
  return(x) 
}
matches <- numeric(1000)
for(i in 1:1000) {
  matches[i] <- tennis(0.7)
}
ans <- mean(matches)




# 4th question:

MontyHall <- function() {
  
  door <- c("goat", "goat", "bugatti")
  player_choice <- sample(1:3, 1)
  monty_choice <- sample(setdiff(1:3, contestant_choice)[doors[setdiff(1:3, contestant_choice)] == "goat"], 1)
  player_switch <- setdiff(1:3, c(contestant_choice, monty_choice))
  if (door[player_switch[1]] == "bugatti") {
    return(1)  
  } else {
    return(0)  
  }
}
num_simulations <- 1000
results <- replicate(num_simulations, MontyHall())
probability_switch <- mean(results)
print(probability_switch)




# 5th question:

library(rvest)
library(dplyr)

url <- "https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-rightnow/"

html <- read_html(url)

MovieData <- page %>%
  html_nodes(".countdown-index") %>%
  html_text() %>%
  as.integer() %>%
  setNames("Ranking") %>%
  tibble() %>%
  mutate(
    Movie = html %>% html_nodes(".article_movie_title") %>% html_text(),
    Tomato_Score = html %>% html_nodes(".tMeterScore") %>% html_text() %>% gsub("%", "", .) %>% as.integer(),
    Year = html %>% html_nodes(".start-year") %>% html_text(trim = TRUE) %>% as.integer()
  ) %>%
  mutate(Year = ifelse(is.na(Year)|Year == 0, NA, Year))


print(MovieData)
