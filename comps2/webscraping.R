install.packages("rvest")
install.packages("xml2")
library(rvest)
library(xml2)

url <- "https://www.tripadvisor.com/Attraction_Review-g303506-d1637149-Reviews-Favela_Tour-Rio_de_Janeiro_State_of_Rio_de_Janeiro.html#REVIEWS"

morepglist <-seq(10, 470, 10)

pickhotel <- url
# get list of urllinks corresponding to different pages

# url link for first search page
urllinkmain=pickhotel
# counter for additional pages
morepg=as.numeric(morepglist)

urllinkpre=paste(strsplit(urllinkmain,"Reviews-")[[1]][1],"Reviews",sep="")
urllinkpost=strsplit(urllinkmain,"Reviews-")[[1]][2]

urllink=rep(NA,length(morepg)+1)

urllink[1]=urllinkmain
for(i in 1:length(morepg)){
  urllink[i+1]=paste(urllinkpre,"-or",morepg[i],"-",urllinkpost,sep="")
}
head(urllink)
REview_Data <- data.frame()
for (i in 1:30) {
  reviews <- urllink[i] %>%
    read_html() %>%
    html_nodes("#REVIEWS .innerBubble")
  
  usercount <- urllink[i] %>%
    read_html() %>%
    html_nodes("#REVIEWS .memberBadgingNoText")
  
  id <- reviews %>%
    html_node(".quote a") %>%
    html_attr("id")
  
  quote <- reviews %>%
    html_node(".quote span") %>%
    html_text()
  
  value_rating <- reviews %>%
    html_node("li .recommend-answer") %>%
    html_attr("li") %>%
    gsub("ui_bubble_rating bubble_40", "", .) %>%
    gsub("0", "", .) %>%
    as.integer()
  
  
  rating <- reviews %>%
    html_node(".rating span") %>%
    html_attr("class") %>%
    gsub("ui_bubble_rating bubble_", "", .) %>%
    gsub("0", "", .) %>%
    as.integer()
  
  date <- reviews %>%
    html_node(".rating .ratingDate") %>%
    html_attr("title") %>%
    strptime("%d %b %Y") %>%
    as.POSIXct()
  
  review <- reviews %>%
    html_node(".entry .partial_entry") %>%
    html_text()
  webpage <- read_html(urllink[i])
  
  count <- webpage %>%
    html_node(".see_all_count") %>%
    html_text()
  
  things <- webpage %>%
    html_node("span .header_popularity") %>%
    html_text()
  
  Noofcontribution <- usercount %>%
    html_node(".badgetext") %>%
    html_text()
  
  Noofusefulvotes <- usercount %>%
    html_node(".badgetext:last-child") %>%
    html_text()
  Restaurant_Name<-webpage%>%
    html_node(".heading_title")%>%
    html_text()
  REview_Data <- rbind(REview_Data, data.frame(Restaurant_Name,id, quote,value_rating ,rating,date, review,count,things,Noofcontribution,Noofusefulvotes, stringsAsFactors = FALSE))
}

REview_Data
write.csv(REview_Data,'favela.csv')
