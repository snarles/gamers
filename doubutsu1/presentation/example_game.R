source("doubutsu1/viz.R")
gstates <- readRDS("doubutsu1/lg_states.rds")
load("doubutsu1/lg_data.rda")

game <- gstates[[140]]
pdf("doubutsu1/presentation/example0.pdf")
draw_state(init_state)
dev.off()
for (i in 1:length(game)) {
  pdf(paste0("doubutsu1/presentation/example", i, ".pdf"))
  draw_state(game[[i]])
  dev.off()
}

tab <- readRDS("doubutsu1/prediction/player_ranks.rds")
View(tab[order(-tab$total), ])
