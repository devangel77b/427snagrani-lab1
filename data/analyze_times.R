library(ggplot2)
library(dplyr)

raw <- read.csv('times.csv',header=TRUE)
data <- data.frame(raw)
grouped <- dplyr::group_by(data, type)
summary_data <- dplyr::summarise(grouped,
	     t = mean(time.s),
	     sd = sd(time.s))

fig <- ggplot(summary_data) +
    geom_hline(yintercept=0) + 
    geom_bar( aes(x=type, y=t), stat="identity", fill="gray50",width=0.5) +
    geom_errorbar( aes(x=type, ymin=t-sd, ymax=t+sd), width=0.3) +
    theme_classic(base_size=8) +
    theme(axis.title.x = element_blank()) +
    #xlab("ball type") + 
    ylab("$t$, \\unit{\\second}") +
    theme(axis.title.y=element_text(margin=margin(t=0,r=3,b=0,l=0,unit="pt")))

# save the figure
ggsave("fig2.svg", plot=fig, width=1.5, height=2, units="in")

# give me the t-test results 
print(t.test(time.s ~ type, data))

# and give me the groupwise mean and sd
# grouped <- data.group_by(data, type)
print(dplyr::summarise(grouped,
	mean = mean(time.s),
	sd = sd(time.s)))

# give the pooled everything mean and sd
print(summarize(data, mean=mean(time.s), sd=sd(time.s)))