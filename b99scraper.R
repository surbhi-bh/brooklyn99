#Scraper to get episode names, seasons, rating

library(rvest)

allseasons <- 1:7

imdbdata <- do.call(rbind, lapply(unique(allseasons), function(f){
    url <- c(paste0("https://www.imdb.com/title/tt2467372/episodes?season=",
                    f))
    webpage <- read_html(url)

# Season/Episode number/Rating
    rating_html <- html_nodes(webpage, '.ipl-rating-star__rating')
    rating <- html_text(rating_html)
    rating <- ifelse(nchar(rating) == 3, rating, "")
    rating <- rating[rating != ""]
    ep <- 1:length(rating)
    season <- f
    season <- paste0("S", season, ", Ep", ep)
    rating <- data.frame(season, rating, ep)

# Episode names    
    ep_html <- html_nodes(webpage, 'a')
    epname <- html_text(ep_html)
    epname <- trimws(epname)
    epname <- epname[- grep("Rate", epname)]
    epname <- epname[epname != ""]
    epname <- epname[54:length(epname)]
    epname <- epname[seq(1, length(epname), 2)]
    epname <- epname[seq(1, nrow(rating)+1,1)]
    epname <- epname[-1]
    fdata <- data.frame(epname, f, rating)
    colnames(fdata) <- c("epname", "season", "id", "rating", "epnumber")
    fdata
}))


write.csv(imdbdata, "imdbb99.csv", row.names = FALSE)

########### End #############
