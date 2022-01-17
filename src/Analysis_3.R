#Performance analysis based on hostnames/GPU nodes

## grouped by hostname to see if any outlier performance is due to a specific host when gpus are idle
a1 = analysis_dataset %>% drop_na(powerDrawWatt)
a2 = a1 %>% filter(gpuUtilPerc<15 & gpuMemUtilPerc<15)
a4 = a2 %>% 
  group_by(a2$hostname) %>%   summarise(total_power_drawn = sum(powerDrawWatt),
                                        avg_gpu_temp = mean(gpuTempC), avg_gpu_util = mean(gpuUtilPerc), 
                                        avg_gpu_memory_util = mean(gpuMemUtilPerc))
names(a4)[1] = 'hostname'

gpu_idle_outliers = sort(as.data.frame(tail(a4[order(a4$total_power_drawn),],5))$hostname)
gpu_idle_outliers

a4 = a4[order(a4$hostname),]
rownames(a4) = 1:nrow(a4)
a4$index = seq(1:nrow(a4))

plot_i1 = ggplot(a4, aes(index, total_power_drawn/1000))+geom_point()+labs(title = "total power drawn by gpu node in kilo watt")
plot_i2 = ggplot(a4, aes(index, avg_gpu_temp))+geom_point()+labs(title = "average gpu temparature of gpu node in celsius")
plot_i3 = ggplot(a4, aes(index, avg_gpu_util))+geom_point()+labs(title = "average gpu utilization of gpu node")
plot_i4 = ggplot(a4, aes(index, avg_gpu_memory_util))+geom_point()+ labs(title = "average gpu memory utilization of gpu node")

(ploti5 = ggarrange(plot_i1, plot_i2,plot_i3, plot_i4,
                    labels = c("P", "Q", "R","S"),
                    ncol = 2, nrow = 2)) %>% ggexport(filename = "graphs/idle gpu performance by hostname after indexing .png",
                                                      width = 1000,height = 1000)
ploti5
##--------------------------------------------------------------------------------------------------------------------------------------------


## hostname vs gpu performance variables when not idle
rownames(final_host_gpu_performance) = 1:nrow(final_host_gpu_performance)
final_host_gpu_performance$index = seq(1:nrow(final_host_gpu_performance))

gpu_non_idle_outliers = sort(as.data.frame(tail(final_host_gpu_performance[order(final_host_gpu_performance$total_power_drawn),],5))$hostname)
gpu_non_idle_outliers
## plot of gpu power drawn with series

plot_n1 = ggplot(final_host_gpu_performance, aes(index, total_power_drawn/1000))+geom_point()+labs(title = "total power drawn by gpu node in kilo watt")
plot_n2 = ggplot(final_host_gpu_performance, aes(index, average_gpu_temp))+geom_point()+labs(title = "average gpu temparature of gpu node in celsius")
plot_n3 = ggplot(final_host_gpu_performance, aes(index, average_gpu_util))+geom_point()+labs(title = "average gpu utilization of gpu node")
plot_n4 = ggplot(final_host_gpu_performance, aes(index, average_gpu_memory_util))+geom_point()+ labs(title = "average gpu memory utilization of gpu node")

(plotn5 = ggarrange(plot_n1, plot_n2,plot_n3, plot_n4,
                    labels = c("P", "Q", "R","S"),
                    ncol = 2, nrow = 2)) %>% ggexport(filename = "graphs/scatter plot of gpu performance by hostname after indexing .png",
                                                      width = 1000,height = 1000)
plotn5
##--------------------------------------------------------------------------------------------------------------------


## to show idle vs non idle gpu performance by hostnames in same graphs with 2 colors ------------------------------------------------------
a4$state = as.character("idle")
names(a4)[2] = "total_power_drawn_idle"
final_host_gpu_performance$state = as.character("rendering")
idle = union_all(a4,final_host_gpu_performance, by = "index")
idle[is.na(idle)] = 0

idle$power_drawn = idle$total_power_drawn_idle+idle$total_power_drawn
idle$gpu_temp = idle$avg_gpu_temp + idle$average_gpu_temp
idle$gpu_util = idle$avg_gpu_util + idle$average_gpu_util
idle$gpu_memory_util = idle$avg_gpu_memory_util + idle$average_gpu_memory_util
idle = idle[ -c(2:5,8:11) ]

plotP = ggplot(idle, aes(index, power_drawn/1000, colour = state))+geom_point()+ labs(title = "Power drawn by GPU nodes in Idle and Rendering state",
                                                                                      x = "GPU nodes", y = " Power drawn in Kilo Watt")


plotQ = ggplot(idle, aes(index, gpu_temp, colour = state))+geom_point()
plotR = ggplot(idle, aes(index, gpu_util, colour = state))+geom_point()+ylim(0,100)
plotS = ggplot(idle, aes(index, gpu_memory_util, colour = state))+geom_point()

(plot_pqrs = ggarrange(plotP, plotQ,plotR, plotS,
                       labels = c("P", "Q", "R","S"),
                       ncol = 2, nrow = 2)) %>% ggexport(filename = "graphs/gpu performance (idle vs rendering) by hostname after indexing .png",
                                                         width = 1000,height = 1000)
plot_pqrs