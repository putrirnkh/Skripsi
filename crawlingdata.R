library("rtweet")
library("dplyr")
library("tidyr")

keyword <- "kulon"
jumlahtweet <- 5000
type <- "recent"
bahasa <- "id"

retweet <- FALSE

crawling <- search_tweets(keyword,
                          n = jumlahtweet,
                          include_rts = retweet,
                          type = type,
                          lang = bahasa,
                          retryonratelimit = FALSE)
View(crawling)

#SAVE DATA
write_as_csv(crawling,"kulon5000.csv",
             prepend_ids = TRUE, na ="", fileEncoding = "UTF-8")

#mengambi dengan ketentuan follower
selected <- filter(crawling, followers_count > 100)

#membuat edgelist
edgelist <- select(selected, screen_name, mentions_screen_name)

#memisahkan target jika terjadi duplikasi
edgelist <- edgelist %>% unnest(mentions_screen_name)

#menghilangkan baris kosong
edgelist <- na.exclude(edgelist)

#save sebagai csv berisi edge dan list
write.table(edgelist, file = "data_edge_node_pjj.csv",
            quote = FALSE, sep = ",",
            col.names = FALSE, row.names = FALSE)

