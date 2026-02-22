library(ggplot2)
library(dplyr)

raw <- read.csv('times.csv',header=TRUE)
data <- data.frame(raw)
data <- mutate(data,
     g = 2*5/time.s^2)
grouped = dplyr::group_by(data,type)
summary_data <- dplyr::summarise(grouped,
	     t = mean(g),
	     sd = sd(g))

fig <- ggplot(summary_data) +
    geom_hline(yintercept=0) + 
    geom_bar( aes(x=type, y=t), stat="identity", fill="gray50",width=0.5) +
    geom_errorbar( aes(x=type, ymin=t-sd, ymax=t+sd), width=0.3) +
    theme_bw(base_size=8) +
    theme(axis.title.x = element_blank()) +
    #xlab("ball type") + 
    ylab("$g$, \\unit{\\meter\\per\\second\\squared}")

# save the figure
ggsave("fig3.svg", plot=fig, width=1.608, height=2, units="in")

# give me the t-test results 
print(t.test(g ~ type, data))

# and give me the groupwise mean and sd
# grouped <- group_by(data, type)
print(dplyr::summarize(grouped,
	mean = mean(g),
	sd = sd(g)))

# give the pooled everything mean and sd
print(summarize(data, mean=mean(g), sd=sd(g)))