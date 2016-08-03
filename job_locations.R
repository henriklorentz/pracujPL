library(rvest)
library(foreach)
library(ggplot2)
library(ggmap)


#### Job location analysis ####

dictionary <- c('dolnośląskie', 'kujawsko-pomorskie', 'Kujawsko-Pomorskie', 'lubelskie',
                'łódzkie','Łódzkie', 'małopolskie', 'mazowieckie', 'opolskie',
                'podkarpackie', 'podlaskie', 'pomorskie', 'śląskie', 
                'świętokrzyskie', 'warmińsko-mazurskie', 'wielkopolskie', 
                'zachodniopomorskie', 'lubuskie')

dictionary_countries <- c('Eastern Switzerland ', 'Szwajcaria')


job.locations <- function(URL, number_of_pages){
  cities <- list()
  
  for(i in 1:number_of_pages){
    session <- html_session(URL)
    switch_pages <- jump_to(x = session, 
                            url = paste0(URL , '?pn=', i))
    
    cities[[i]] <- read_html(switch_pages) %>%
      html_nodes(css = '.shortened-text') %>%
      html_text()
  }
  
  names(cities) <- sapply(c(1:number_of_pages), function(x){paste('Page', x, sep = ' ')})
  
  loop  <- foreach(i = c(1:number_of_pages)) %do% {
    temp <- c()
    temp <- cities[[i]] %>%
      strsplit(split = ', ') %>%
      unlist()
    
    cities[[i]] <- temp[seq(from = 1, to = length(temp), by = 2)] 
    
    if(any(temp %in% dictionary)){
      cities[[i]] <- temp[-which(temp %in% dictionary)]
    }
    
    rm(temp)
  }
  
  list_of_id <- lapply(cities,function(x) grep(pattern = '(', x = x, fixed = TRUE))  
  
  loop2 <- foreach(j = c(1:number_of_pages)) %do% {
    
    if(!identical(list_of_id[[j]], integer(0))){
      cities[[j]] <- cities[[j]][-list_of_id[[j]]]
    }
    
    if(any(cities[[j]] %in% dictionary_countries)){
      cities[[j]] <- cities[[j]][-which(cities[[j]] %in% dictionary_countries)]
    }
  }
  
  locations <- cities %>%
    unlist(use.names = FALSE) %>%
    table() %>%
    as.data.frame()
  
  coordinates <- locations$. %>%
    as.vector() %>%
    geocode()
  
  cities <- cbind(locations, coordinates)
  colnames(cities) <- c('Cities', 'Freq', 'Longitude', 'Latitude')
  
  return(cities)
}

map_dictionary <- c('Gdynia', 'Gdańsk', 'Olsztyn', 'Białystok', 'Szczecin', 'Bydgoszcz', 'Poznań',
                    'Łódź', 'Warszawa', 'Lublin', 'Kielce', 'Rzeszów', 'Kraków', 'Katowice', 'Wrocław', 
                    'Warsaw')

cities <- job.locations(URL = 'http://www.pracuj.pl/praca/Data%20Scientist;kw' ,
                        number_of_pages = 4)
diff_Idx <- which(cities$Cities %in% map_dictionary)


newmap <- get_map(location = 'Poland', zoom = 6, maptype = 'hybrid' )

mapPoints <- ggmap(newmap) +
  geom_point(aes(x = Longitude, y = Latitude, size = Freq), data = cities, alpha = .5, colour = 'green1') +
  scale_size() #+
# geom_text(data = cities[-diff_Idx,], aes(label = cities[-diff_Idx, 1],
#                                         x = cities[-diff_Idx, 3], 
#                                        y = cities[-diff_Idx, 4]),
#        colour = 'white',
#       fontface = 'bold')

mapPoints


















