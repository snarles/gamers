####
##  Scrape LG games
####

## start with users

library(scrapeR)
raws <- list()
for (pageno in 1:22) {
  url <- paste0("https://www.littlegolem.net/jsp/info/player_list.jsp?gtvar=fir_SIZE67&filter=&countryid=&page=", pageno)
  raw <- scrape(url, parse = FALSE)[[1]]
  raws[[pageno]] <- raw
}
saveRDS(raws, "lg/four67/players_raw.rds")
splits <- list()
for (raw in raws) {
  splitz <- strsplit(raw, "plid=")[[1]]
  splits <- c(splits, splitz)
}
length(splits)
splits <- splits[sapply(splits, function(v) substr(v, 1, 2) != "<!")]
nos <- sapply(splits, function(v) strsplit(v, "\\>")[[1]][1])
names <- sapply(splits, function(v) strsplit(strsplit(v, ">")[[1]][2], "<")[[1]][1])
four_players <- cbind(names, nos)
saveRDS(four_players, file = "lg/four67/four_players.rds")

## obtain games + metadata
games_raws <- list()
nrow(four_players) # 435
#for (i in 1:nrow(four_players)) {
for (i in inds) {
#for (i in 1:241) {
  plid <- four_players[i, 2]
  url1 <- paste0("https://www.littlegolem.net/jsp/info/player_game_list.jsp?gtid=fir&plid=", plid)
  url2 <- paste0("https://www.littlegolem.net/jsp/info/player_game_list_txt.jsp?plid=", plid, "&gtid=fir")
  raw1 <- scrape(url1, parse = FALSE)[[1]]
  raw2 <- scrape(url2, parse = FALSE)[[1]]
  res <- c(raw1 = raw1, raw2 = raw2)
  games_raws[[four_players[i, 1]]] <- res
}
saveRDS(games_raws, file = "lg/four67/all_raws.rds")

valid_pl <- sapply(games_raws, function(v) nchar(v[1]) >0 && nchar(v[2]) > 0)
inds <- setdiff(1:435, which(valid_pl))
games_raws <- games_raws[valid_pl]

dsplit <- function(v, l, r) {
  strsplit(strsplit(v, l)[[1]][2], r)[[1]][1]
}


sort(sapply(lapply(games_raws, function(v) {
  splits<- strsplit(v[1], "<tr>")[[1]]
  grep("gid=", splits)
}), max))

gametables <- list()
for (i in 1:length(games_raws)) {
  (playername <- names(games_raws)[i])
  raw <- games_raws[[i]]
  splits<- strsplit(raw[1], "<tr>")[[1]][-(1:3)]
  gids <- sapply(splits, function(v) {
    loc <- gregexpr("gid=", v)[[1]][1]
    gidstr <- substr(v, loc, loc + 15)
    gidstr <- strsplit(gidstr, ">")[[1]][1]
    substr(gidstr, 5, nchar(gidstr) - 1)
  }, USE.NAMES = FALSE)
  gametypes <- sapply(splits, function(v) {
    dsplit(v, "<span style='color: FFFFFF;'>", "<")
  }, USE.NAMES = FALSE)
  opps <- sapply(splits, function(v) {
    dsplit(v, "<td bgcolor='#E9D101'>", "<")
  }, USE.NAMES = FALSE)
  opps <- sapply(opps, function(v) {
    if (length(grep("★", v)>0) ){
      v <- substr(v, 1, nchar(v)-2)
    }
    v
  }, USE.NAMES = FALSE)
  nmoves <- sapply(splits, function(v) {
    dsplit(v, "<td align='right'>", "<")
  }, USE.NAMES = FALSE)
  outcomes <- sapply(splits, function(v) {
    dsplit(v, "<td align=center>", "<")
  }, USE.NAMES = FALSE)
  games <- strsplit(raw[2], "\n\n")[[1]]
  games <- games[-length(games)]
  game.table <- cbind(gids, gametypes, player = playername, opps, nmoves, outcomes, games)
  #View(game.table)
  gametables[[playername]] <- game.table
}

gametable <- do.call(rbind, gametables)
nrow(gametable)
View(gametable[1000:1100, ])
colnames(gametable) <- c("gid", "variant", "player1", "player2", "nmoves", "outcome", "game")
unique_inds <- match(unique(gametable[, "gid"]), gametable[, "gid"])
table(gametable[unique_inds, "variant"])

# Mini Shogi          Shogi      Shogi 3x4      Shogi 5x6 Shogi 5x6 PLUS     Tori Shogi 
# 5862           6770            916            655            147            585 

save(gametable, unique_inds, file="minishogi/lg_scraping/gametable.rda")
write.csv(gametable, file = "minishogi/lg_scraping/gametable.csv", row.names = FALSE)

## validation

for (i in 1:nrow(gametable)) {
  matches <- setdiff(which(gametable[, "gid"]==gametable[i, "gid"]), i)
  if (length(matches) > 0) {
    if (gametable[i, "game"] != gametable[matches, "game"]) {
      print(c(i, matches))
    }
  }
}