library(sjmisc)
library(pracma)
library(rvest)
library(xml2)
library(stringr)
library(xlsx)


teams <- c("ARI","ATL","BAL","BOS","CHN","CHA","CIN","CLE",
           "COL","DET","HOU","KCA","ANA","LAN","MIA","MIL",
           "MIN","NYN","NYA","OAK","PHI","PIT","SDN","SEA","SFN","SLN","TBA","TEX","TOR","WAS")

rbis <- data.frame(Player=character(),RBI=numeric(),Chances=numeric(),stringsAsFactors = F)

x <- 1

for(i in 1:30){
  for(j in 1:33){
    if(!is.na(home_games_2020[j,i])){
      url <- "https://www.baseball-reference.com/boxes/"
      url <- strcat(url,teams[i])
      url <- strcat(url,"/")
      url <- strcat(url,teams[i])
      url <- strcat(url,"2020")
      url <- strcat(url, home_games_2020[j,i])
      if(j > 1 & strcmp(home_games_2020[j,i],home_games_2020[j-1,i])){
        url <- strcat(url,"2.shtml")
      }
      else if(!is.na(home_games_2020[j+1,i]) & strcmp(home_games_2020[j,i],home_games_2020[j+1,i])){
        url <- strcat(url,"1.shtml")
      }
      else{
        url <- strcat(url,"0.shtml")
      }
      
      print(url)
      urlbbref <- read_html(url)
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
      
      plays <- as.data.frame(all_tables[8])
      
      p <- c(1:nrow(plays))
      for(val in p){
        if(startsWith(plays[val,"Inn"], "Top") || startsWith(plays[val,"Inn"], "Bottom")){
          
        }
        else if(str_contains(plays[val,"RoB"],"3")){
          if(strcmp(plays[val,"Batter"],plays[val+1,"Batter"]) || str_detect(plays[val,"Play.Description"],"Intentional Walk")){
            #print("Equal")
          }
          else if(str_detect(plays[val,"Play.Description"],"Caught Stealing") || str_detect(plays[val,"Play.Description"],"Picked off")){
            #print("No PA")
          }
          else{
            #print(plays[val,"Batter"])
            rbi <- 0
            if(str_contains(plays[val,"R.O"],"R") && !str_contains(plays[val,"Play.Description"],"No RBI")){
              rbi <- 1
            }
            if(x == 1){
              rbis[x,1] <- plays[val,"Batter"]
              rbis[x,2] <- rbi
              rbis[x,3] <- 1
              x <- x + 1
            }
            else{
              check <- 0
              for(k in 1:x - 1){
                if(strcmp(rbis[k,1],plays[val,"Batter"])){
                  rbis[k,2] <- rbis[k,2] + rbi
                  rbis[k,3] <- rbis[k,3] + 1
                  check <- 1
                }
              }
              if(check == 0){
                rbis[x,1] <- plays[val,"Batter"]
                rbis[x,2] <- rbi
                rbis[x,3] <- 1
                x <- x + 1
              }
            }
            
          }
        }
      }
    }
  }
}
write.xlsx(rbis, "c:/users/tdpot/rbis/2020_rbis.xlsx")


for(i in 1:30){
  for(j in 1:81){
    if(!is.na(home_games_2019[j,i])){
      url <- "https://www.baseball-reference.com/boxes/"
      url <- strcat(url,teams[i])
      url <- strcat(url,"/")
      url <- strcat(url,teams[i])
      url <- strcat(url,"2019")
      url <- strcat(url, home_games_2019[j,i])
      if(j > 1 & strcmp(home_games_2019[j,i],home_games_2019[j-1,i])){
        url <- strcat(url,"2.shtml")
      }
      else if(!is.na(home_games_2019[j+1,i]) & strcmp(home_games_2019[j,i],home_games_2019[j+1,i])){
        url <- strcat(url,"1.shtml")
      }
      else{
        url <- strcat(url,"0.shtml")
      }
      
      print(url)
      urlbbref <- read_html(url)
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
      
      plays <- as.data.frame(all_tables[8])
      
      p <- c(1:nrow(plays))
      for(val in p){
        if(startsWith(plays[val,"Inn"], "Top") || startsWith(plays[val,"Inn"], "Bottom")){
          
        }
        else if(str_contains(plays[val,"RoB"],"3")){
          if(strcmp(plays[val,"Batter"],plays[val+1,"Batter"]) || str_detect(plays[val,"Play.Description"],"Intentional Walk")){
            #print("Equal")
          }
          else if(str_detect(plays[val,"Play.Description"],"Caught Stealing") || str_detect(plays[val,"Play.Description"],"Picked off")){
            #print("No PA")
          }
          else{
            #print(plays[val,"Batter"])
            rbi <- 0
            if(str_contains(plays[val,"R.O"],"R") && !str_contains(plays[val,"Play.Description"],"No RBI")){
              rbi <- 1
            }
            if(x == 1){
              rbis[x,1] <- plays[val,"Batter"]
              rbis[x,2] <- rbi
              rbis[x,3] <- 1
              x <- x + 1
            }
            else{
              check <- 0
              for(k in 1:x - 1){
                if(strcmp(rbis[k,1],plays[val,"Batter"])){
                  rbis[k,2] <- rbis[k,2] + rbi
                  rbis[k,3] <- rbis[k,3] + 1
                  check <- 1
                }
              }
              if(check == 0){
                rbis[x,1] <- plays[val,"Batter"]
                rbis[x,2] <- rbi
                rbis[x,3] <- 1
                x <- x + 1
              }
            }
            
          }
        }
      }
    }
  }
}