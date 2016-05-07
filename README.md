# gamers

Hello! This is an ongoing project to model human gameplay in order to create more human-like AI opponents, mostly using data from little golem.

# doubutsu shogi

You can try out the doubutsu shogi program.  It relies on the dobutsu solver at https://dell.tanaka.ecc.u-tokyo.ac.jp/~ktanaka/dobutsushogi/.

Setup instructions:
 * You need R (https://cran.r-project.org/) to run the game.  It is recommended to download Rstudio (http://www.rstudio.com) as well.
 * Compile Teturo Tanaka's solver as indicated by the above link.  The page is in Japanese, but google translate works well.  All you have to do is download the two .tar.gz files, unzip them to the same directory, and run make.
 * Clone this repository from github.  In a text editor, open the file "gamers/doubutsu1/sourceJP.R" and edit the line `SOLVER_DIR <- "~/Downloads/dobutsu"` to indicate the directory where you compiled Tanaka's solver.
 * Open R, install the packages "png", "pracma", and "Rcpp" (I may be forgetting some others.)
 * Set the working directory to "gamers", and run `source("doubtsu1/play_vs_cpu.R")` or `source("doubtsu1/play_vs_human.R")`.
 * You can also play blindfold mode (the difficulty is set higher as well) `source("doubutsu1/play_vs_cpu_blind.R)`.
 * Email me at charles.y.zheng@gmail.com if you have any questions!

Extras:
 * Without any need to install, look in the "doubutsu1/book" folder for mate in X problems!
 * Take a look at the presentation I gave at a graduate student seminar, "doubutsu1/presentation/Zheng_games.tex".
 * If you are interested in this research project, shoot me an email! I am also interested in analyzing Connect Four, Hex, etc.
