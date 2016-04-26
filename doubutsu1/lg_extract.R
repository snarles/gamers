####
##  Extract game states from lg games
####

source("minishogi/doubutsu/source.R")

TAB0 <- rbind(c("K", "K"), c("b", "B"), c("r", "R"), c("P", "P"), c("+P", "T"))
move_processor <- function(str, split_form = FALSE) {
  if (str == "resign") return("resign")
  sp <- strsplit(str, "-")[[1]]
  pt <- substr(sp[1], 1, nchar(sp[1])-2)
  pt2 <- TAB0[TAB0[, 1]==pt, 2]
  if (length(sp)==1) {
    l1 <- "**"
    l2 <- substr(sp[1], nchar(sp[1])-1, nchar(sp[1]))
  } else {
    l1 <- substr(sp[1], nchar(sp[1])-1, nchar(sp[1]))
    l2 <- sp[2]
  }
  if (split_form) return(c(pt2, loc_processor(l1), loc_processor(l2)))
  paste(pt2, loc_processor(l1), loc_processor(l2), sep = "-")
}
TAB1 <- rbind(c("2", "1"), c("3", "2"), c("4", "3"), c("*", "*"))
TAB2 <- rbind(c("`", "a"), c("a", "b"), c("b", "c"), c("c", "d"), c("*", "*"))
loc_processor <- function(lc) {
  as <- strsplit(lc, NULL)[[1]]
  paste0(TAB1[TAB1[, 1]==as[1], 2], TAB2[TAB2[, 1]==as[2], 2])
}


load("minishogi/lg_scraping/gametable.rda", verbose = TRUE)
gametable <- gametable[unique_inds, ]
unique(gametable[, "variant"])
gametable <- gametable[gametable[, "variant"]=="Shogi 3x4", ]
nrow(gametable)
colnames(gametable)
glist <- gametable[, "game"]
glist <- sapply(glist, function(v) substr(v, 2, nchar(v) - 1), USE.NAMES = FALSE)
glist <- lapply(glist, strsplit, ";")
glist <- lapply(glist, function(v) v[[1]][-(1:3)])

allmoves <- unique(do.call(c, glist))
trans_moves <- sapply(allmoves, move_processor, USE.NAMES = FALSE)

glist <- lapply(glist, function(v) trans_moves[match(v, allmoves)])
glist[[1]]

save(gametable, glist, file = "minishogi/doubutsu/lg_data.rda")

state_collection <- list()
sink("minishogi/temp/34.txt")
for (i in 1:length(glist)) {
  cat("\n")
  print(c(row = i, gametable[i, 1:6]))
  if (length(glist[[i]]) > 0) {
    states <- statesFromGame(glist[[i]], printt = TRUE)
    state_collection[[i]] <- states
  }
}
sink()

saveRDS(state_collection, file = "minishogi/doubutsu/lg_states.rds")
state_collection <- readRDS("minishogi/doubutsu/lg_states.rds")

allstates <- do.call(c, state_collection)
allstates <- do.call(rbind, allstates)
dim(allstates)
allstates <- unique(allstates)
dim(allstates)

####
##  Sort out sente and gote
####

library(scrapeR)
dsplit <- function(v, l, r) strsplit(strsplit(v, l)[[1]][2], r)[[1]][1]

get_sente_gote <- function(gid) {
  (url <- paste0("https://www.littlegolem.net/jsp/game/game.jsp?gid=", gid))
  raw <- scrape(url, parse = FALSE)[[1]]
  res1 <- dsplit(raw, "Black</div>", "<br>")
  res1 <- dsplit(res1, "plid=", "</a>")
  Psente <- strsplit(res1, ">")[[1]][2]
  res2 <- dsplit(raw, "White</div>", "<br>")
  res2 <- dsplit(res2, "plid=", "</a>")
  Pgote <- strsplit(res2, ">")[[1]][2]
  c(Psente = Psente, Pgote = Pgote)
}

colnames(gametable)
outcome <- gametable[, "outcome"]

stripd <- sapply(glist, function(v) {
  if (length(v) > 0 && rev(v)[1]=="resign") return(v[-length(v)])
  return(v)
})
lens <- sapply(stripd, length)
p1sente <- rep(NA, nrow(gametable))

filt <- (outcome == "win") & (lens %% 2 == 1)
p1sente[filt] <- TRUE
filt <- (outcome == "lose") & (lens %% 2 == 1)
p1sente[filt] <- FALSE
filt <- (outcome == "win") & (lens %% 2 == 0)
p1sente[filt] <- FALSE
filt <- (outcome == "lose") & (lens %% 2 == 0)
p1sente[filt] <- TRUE

sum(is.na(p1sente))

(gno <- sample(which(lens > 0), 1))
gametable[gno, 1:6]
p1sente[gno]

Psente <- get_sente_gote(gametable[gno, "gid"])[1]

c(p1sente[gno], Psente == gametable[gno, "player1"])


sente <- character(nrow(gametable))
gote <- character(nrow(gametable))
pos <- !is.na(p1sente) & p1sente
sente[pos] <- gametable[pos, "player1"]
gote[pos] <- gametable[pos, "player2"]
neg <- !is.na(p1sente) & !p1sente
sente[neg] <- gametable[neg, "player2"]
gote[neg] <- gametable[neg, "player1"]


(gno <- sample(which(lens > 0 & sente != ""), 1))
gametable[gno, 1:6]
res <- get_sente_gote(gametable[gno, "gid"])

c(sente[gno], res[1])
c(gote[gno], res[2])

rem <- which(sente == "")
for (gno in rem) {
  res <- get_sente_gote(gametable[gno, "gid"])
  print(gametable[gno, 1:5])
  sente[gno] <- res[1]
  gote[gno] <- res[2]
  print(res)
}

sente[is.na(sente)] <- ""
gote[is.na(gote)] <- ""

allpl <- c(sente, gote, gametable[, "player1"], gametable[, "player2"])
sort(unique(allpl))
sum(sort(table(allpl), decreasing = TRUE) > 5)
## anonymizing mapping
ANAMES <- c("LukeSkywalker", "ObiWanKenobi", "PrincessLeia", "Chewbacca", "HanSolo",
            "PadmeAmidala", "DarthVader", "AnakinSkywalker", "JarJarBinks", "C3P0",
            "Palpatine", "GenGrievous", "R2D2", "Yoda", "MaceWindu",
            "CountDooku", "BobaFett", "LandoC", "Rey", "Finn",
            "KyloRen", "BB8", "PoeDameron", "AdmiralAckbar", "WedgeAntilles",
            "JangoFett", "QuiGonJinn", "JabbaTheHutt", "OwenLars", "DarthMaul",
            "DarthVader", "RogueSquadron", "DarkSide", "Sebulba", "Snoke",
            "GrandMoffTarkin", "DarthTyranus", "Wookie", "JediKnight", "SithLord",
            "CaptainKirk", "Spock", "LeonardMcCoy", "NyotaUhura", "HikaruSulu",
            "PavelChekov", "JeanLucPicard", "Data", "Worf", "Klingon",
            "TheBorg", "Q", "CaptainJaneway", "Neelix", "SevenOf9", "Scotty",
            "IndianaJones", "JamesBond", "Superman", "Buffy", "Willow",
            "Xander", "ProfGiles", "Angel", "Cortana", "MasterChief",
            "Xena", "Eowyn", "Samus", "Hermione", "HarryPotter",
            "RonWeasley", "Voldemort", "Daenarys", "Dumbledore", "ProfSnape",
            "DracoMalfoy", "Neo", "Trinity", "Morpheus", "HomerSimpson",
            "MargeSimpson", "BartSimpson", "LisaSimpson", "Flanders",
            "KrustyTheClown", "Smithers", "MrBurns", "Millhouse", "Spiderman",
            "Batman", "Gandalf", "Wolverine")
length(ANAMES)
set.seed(0)
conv <- sample(ANAMES, length(ANAMES), FALSE)
names(conv) <- unique(allpl)
conv[sente[1:100]]
saveRDS(conv, "doubutsu1/anonymizer.rds")

winner <- character(nrow(gametable))
loser <- character(nrow(gametable))
winner[outcome == "win"] <- gametable[outcome=="win", "player1"]
winner[outcome == "lost"] <- gametable[outcome=="lost", "player2"]
loser[outcome == "win"] <- gametable[outcome=="win", "player2"]
loser[outcome == "lost"] <- gametable[outcome=="lost", "player1"]

# gametable <- gametable0
gametable0 <- gametable

gametable0[1, ]

gametable <- data.frame(gid = gametable[, "gid"], sente = conv[sente], gote = conv[gote], 
                        winner = conv[winner], loser = conv[loser], len = lens, stringsAsFactors = FALSE)
gametable$winner[is.na(gametable$winner)] <- ""
gametable$loser[is.na(gametable$loser)] <- ""

View(gametable)

save(gametable, glist, file = "doubutsu1/lg_data.rda")


## various testing stuff
# Export the subroutines to run the test!
#  
# state_collection <- readRDS("minishogi/doubutsu/lg_states.rds")
# sourceCpp("minishogi/doubutsu/Rtest.cpp")
# state <- state_collection[[5]][[11]]
# print_state(state)
# print_state(flipState(state))
# print_state(mirrorState(state))
# hashState0(state)
# hashState0(mirrorState(state))





