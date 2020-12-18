#install.packages("sjmisc")
library(sjmisc)
library(pracma)
library(rvest)
library(xml2)
library(stringr)
urlbbref <- read_html("https://www.baseball-reference.com/boxes/WAS/WAS202007250.shtml")
#urlbbref <- read_html("https://www.baseball-reference.com/boxes/NYN/NYN202008252.shtml")
urlbbref <- read_html("https://www.baseball-reference.com/boxes/WAS/WAS202007260.shtml")
urlbbref <- read_html("https://www.baseball-reference.com/boxes/BOS/BOS202007240.shtml")

# First table is in the markup
table_one <- xml_find_all(urlbbref, "//table") %>% html_table

# Additional tables are within the comment tags, ie <!-- tables -->
# Which is why your xpath is missing them.
# First get the commented nodes
alt_tables <- xml2::xml_find_all(urlbbref,"//comment()") %>% {
  #Find only commented nodes that contain the regex for html table markup
  raw_parts <- as.character(.[grep("\\</?table", as.character(.))])
  # Remove the comment begin and end tags
  strip_html <- stringi::stri_replace_all_regex(raw_parts, c("<\\!--","-->"),c("",""),
                                                vectorize_all = FALSE)
  # Loop through the pieces that have tables within markup and 
  # apply the same functions
  lapply(grep("<table", strip_html, value = TRUE), function(i){
    rvest::html_table(xml_find_all(read_html(i), "//table")) %>% 
      .[[1]]
  })
}
# Put all the data frames into a list.
all_tables <- c(
  table_one, alt_tables
)
head(all_tables[8])
plays <- as.data.frame(all_tables[8])
plays[26,"Play.Description"]

df <- data.frame(Player=character(),RBI=numeric(),Chances=numeric(),stringsAsFactors = F)

x <- 1

p <- c(1:nrow(plays))
for(val in p){
  if(startsWith(plays[val,"Inn"], "Top") || startsWith(plays[val,"Inn"], "Bottom")){
    
  }
  else if(str_contains(plays[val,"RoB"],"3")){
    if(strcmp(plays[val,"Batter"],plays[val+1,"Batter"]) || str_detect(plays[val,"Play.Description"],"Intentional Walk")){
      print("Equal")
    }
    else if(str_detect(plays[val,"Play.Description"],"Caught Stealing") || str_detect(plays[val,"Play.Description"],"Picked off")){
      print("No PA")
    }
    else{
      print(plays[val,"Batter"])
      rbi <- 0
      if(str_contains(plays[val,"R.O"],"R") && !str_contains(plays[val,"Play.Description"],"No RBI")){
        rbi <- 1
      }
      if(x == 1){
        df[x,1] <- plays[val,"Batter"]
        df[x,2] <- rbi
        df[x,3] <- 1
        x <- x + 1
      }
      else{
        check <- 0
        for(i in 1:x - 1){
          if(strcmp(df[i,1],plays[val,"Batter"])){
            df[i,2] <- df[i,2] + rbi
            df[i,3] <- df[i,3] + 1
            check <- 1
          }
        }
        if(check == 0){
          df[x,1] <- plays[val,"Batter"]
          df[x,2] <- rbi
          df[x,3] <- 1
          x <- x + 1
        }
      }
      
    }
  }
}

