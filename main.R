source("./steps/data_analyzer.R")

x <- read.csv("./dataset/raw/X.txt", header = F, sep=" ")
c <- read.csv("./dataset/raw/C.txt", header = F, sep=" ")
y <- read.csv("./dataset/raw/Y.txt", header = F, sep=" ")

plot_dir <- paste(getwd(), "plot/", sep="/" )
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir)
}

data_analyzer(x, plot_dir)
