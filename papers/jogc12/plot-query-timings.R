# Plot query timings from multiple dbs
# Input file columns: wf,method,time
require(ggplot2)
d <- read.csv("all-db-query-timings.csv")
theme_set(theme_bw())
# reorder method by time
d$om <- d$method
d1 <- subset(d, wf=="scec")
v=as.character(d1$method)[order(d1$time)]
d$method <- factor(as.character(d$method), levels=v)
# plot result
p <- ggplot(d, aes(x=method, y=time)) + geom_bar() + coord_flip() + facet_wrap(~wf) + ylab("Time (seconds)") + xlab("Stampede API Query Method")
print(p)
