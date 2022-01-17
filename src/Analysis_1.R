## Interplay between increased power draw and total render time

x1 = analysis_dataset %>% filter(eventName == "TotalRender" & eventType == "STOP")
x2 = x1 %>% group_by(x1$time_difference) %>% summarise(average_total_power_drawn = mean(total_power_drawn))
names(x2)[1] = 'time_difference'

plotTR = ggplot(x2,aes(time_difference, average_total_power_drawn))+geom_point()+labs(title = "Average power drawn by taskId vs total rendering time",
x = "Total rendering time", y = "Average power drawn in Watt") %>% ggexport(filename = "graphs/scatter plot of total power drawn by total render time by tasks .png",width = 1000,height = 1000)

plotTR

#Using Correlation method to check more about the relation between total render time vs average (total_power_drawn)
#Using correlation across each pre_loaded different months runned file
cor(x2$time_difference, x2$average_total_power_drawn)

#on basis of plot-pattern obtained in above question,checking the pattern of other gpu variables with total render time


x1 = analysis_dataset %>% filter(eventName == "TotalRender" & eventType == "STOP")
x2 = x1 %>% group_by(x1$time_difference) %>% summarise( average_gpu_temp = mean(average_gpu_temp),
                                                        average_gpu_util = mean(average_gpu_util), average_gpu_memory_util = mean(average_gpu_memory_util))
names(x2)[1] = 'time_difference'


plotTR2 = ggplot(x2,aes(time_difference, average_gpu_temp))+geom_point()+labs(title = "average gpu temperature based on total rendering time")
plotTR3 = ggplot(x2,aes(time_difference, average_gpu_util))+geom_point()+labs(title = "average gpu utilization based on total rendering time")
plotTR4 = ggplot(x2,aes(time_difference, average_gpu_memory_util))+geom_point()+labs(title = "average gpu memory utilization based on total rendering time")


(plotTR2.0 = ggarrange(plotTR2,plotTR3, plotTR4,
                       labels = c("A", "B", "C"))) %>% ggexport(filename = "graphs/scatter plot of gpu performance by total render time by tasks .png",width = 1000,height = 1000)

plotTR2.0

#Using Correlation method to check more about the relation between total render time vs average (total_power_drawn)
#Using correlation across each pre_loaded different months runned file
cor(x2$time_difference, x2$average_gpu_temp)
cor(x2$time_difference, x2$average_gpu_memory_util)
cor(x2$time_difference, x2$average_gpu_util)