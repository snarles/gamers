load('doubutsu1/lg_data.rda')
tab <- readRDS("doubutsu1/prediction/player_ranks.rds")

filt <- gametable$len > 1
gt <- gametable[filt, ]

pl1win <- gt$sente==gt$winner
pl1lose <- gt$sente==gt$loser

sum(pl1win) # 339
sum(pl1lose) # 362
sum(pl1win + pl1lose) # 701
sum(pl1win)/sum(pl1win + pl1lose) # 0.48

tier1 <- rownames(tab)[tab$rating > 0]
length(tier1) # 24
filt1 <- (gt$sente %in% tier1) & (gt$gote %in% tier1)
sum(pl1win & filt1) # 53
sum(pl1lose & filt1) # 66
sum(pl1lose & filt1) + sum(pl1win & filt1) # 119
sum(pl1win & filt1)/(sum(pl1win & filt1) + sum(pl1lose & filt1)) # 0.44

(tier1 <- rownames(tab)[tab$rating > 0.1 & tab$total > 5])
length(tier1) # 9 
filt1 <- (gt$sente %in% tier1) & (gt$gote %in% tier1)
sum(pl1win & filt1) # 14
sum(pl1lose & filt1) # 21
sum(pl1lose & filt1) + sum(pl1win & filt1) # 35
sum(pl1win & filt1)/(sum(pl1win & filt1) + sum(pl1lose & filt1)) # 0.4

(tier1 <- rownames(tab)[tab$rating > 0.2 & tab$total > 5])
length(tier1) # 6
filt1 <- (gt$sente %in% tier1) & (gt$gote %in% tier1)
sum(pl1win & filt1) # 6
sum(pl1lose & filt1) # 13
sum(pl1lose & filt1) + sum(pl1win & filt1) # 19
sum(pl1win & filt1)/(sum(pl1win & filt1) + sum(pl1lose & filt1)) # 0.31

(tier1 <- rownames(tab)[tab$rating > 0.3 & tab$total > 5])
length(tier1) # 3
filt1 <- (gt$sente %in% tier1) & (gt$gote %in% tier1)
sum(pl1win & filt1) # 4
sum(pl1lose & filt1) # 6
sum(pl1lose & filt1) + sum(pl1win & filt1) # 10
sum(pl1win & filt1)/(sum(pl1win & filt1) + sum(pl1lose & filt1)) # 0.4
