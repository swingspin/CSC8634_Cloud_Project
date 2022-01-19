#Analysis on task process scheduling learning
#seconds of day vs gpu performance
a1 = analysis_dataset %>% filter(eventType == "START")
a2 = a1
a2 = a2 %>% 
  group_by(a2$seconds_of_day) %>%   summarise(total_power_drawn = sum(total_power_drawn),
                                              avg_gpu_temp = mean(average_gpu_temp), avg_gpu_util = mean(average_gpu_util), 
                                              avg_gpu_memory_util = mean(average_gpu_memory_util))
names(a2)[1] = "seconds_of_day"
plot_1 = ggplot(a2, aes(seconds_of_day, total_power_drawn/1000))+geom_point()
plot_2 = ggplot(a2, aes(seconds_of_day, avg_gpu_temp))+geom_point()
plot_3 = ggplot(a2, aes(seconds_of_day, avg_gpu_util))+geom_point()
plot_4 = ggplot(a2, aes(seconds_of_day, avg_gpu_memory_util))+geom_point()


(plot5 = ggarrange(plot_1, plot_2,plot_3, plot_4,
                   labels = c("P", "Q", "R","S"),
                   ncol = 2, nrow = 2))%>% ggexport(filename = "graphs/seconds_1.png",width = 1000,height = 1000)

plot5

## idle gpu performance
a1 = analysis_dataset %>% drop_na(powerDrawWatt)
a2 = a1 %>% filter(gpuUtilPerc<15 & gpuMemUtilPerc<15)
a3 = a2 %>% 
  group_by(a2$seconds_of_day) %>%   summarise(total_power_drawn = sum(powerDrawWatt),
                                              avg_gpu_temp = mean(gpuTempC), avg_gpu_util = mean(gpuUtilPerc), 
                                              avg_gpu_memory_util = mean(gpuMemUtilPerc))
names(a3)[1] = "seconds_of_day"


plot_1.1 = ggplot(a3, aes(seconds_of_day,total_power_drawn))+geom_point()+ labs(title = "time series on total power by super cloud computer in idle state ")
plot_2.2= ggplot(a3, aes(seconds_of_day,avg_gpu_temp))+geom_point()+ labs(title = "time series on average gpu temperature by super cloud computer in idle state ")
plot_3.3 = ggplot(a3, aes(seconds_of_day,avg_gpu_util))+geom_point()+ labs(title = "time series on average gpu utilization by super cloud computer in idle state ")
plot_4.4 = ggplot(a3, aes(seconds_of_day,avg_gpu_memory_util))+geom_point()+ labs(title = "time series on average gpu memory utilization by super cloud computer in idle state ")

(plot5.5 = ggarrange(plot_1.1, plot_2.2,plot_3.3, plot_4.4,
                     labels = c("P", "Q", "R","S"),
                     ncol = 2, nrow = 2)) %>% ggexport(filename = "graphs/idel.png",width = 1000,height = 1000)

