library("rtweet")
library("dplyr")
library("tidyr")

keyword<- "pembelajaranjarakjauh"
jumlahtweet<-500
type<-"recent"
bahasa<-"id"

retweet<- FALSE

token<-create_token(
  app="CrawlingData",
  consumer_key = "Z4XGmp1ESgqtgSWQQH4NkPWee",
  consumer_secret = "6iSUZ4DExeHi0BdBlsyK2pjaNKjYhzbCzHeIGVaSeiLsxLDV6O",
  access_token = "1120678059424661507-ZCFP4gNCYTVyYbyW9zStV7H5myATst",
  access_secret = "289mzV5iMyDbRE7CYNHu7Lo2CsWFGPoMDfvLBeGAHGF8V")

crawling <- search_tweets(keyword,
                          n = jumlahtweet,
                          include_rts = retweet,
                          type = type,
                          lang = bahasa,
                          retryonratelimit = FALSE)
View(crawling)

#SAVE DATA
write_as_csv(crawling,"pembelajaranjarakjauh.csv",
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
write.table(edgelist, file = "edge_node_pjj.csv",
            quote = FALSE, sep = ",",
            col.names = FALSE, row.names = FALSE)

View(edgelist)
