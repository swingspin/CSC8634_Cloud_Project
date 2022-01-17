### Study of the variables that affect GPU performance

## for level 12
gpu_analysis_12 = gpu_analysis %>% filter(level == 12)

plotPW = ggplot(gpu_analysis_12, aes(y, -x)) + geom_tile(aes(fill = total_power_drawn))+
  scale_fill_gradientn(colours = c("green","red","black"), values = c(0,0.5,1))+
  labs(title = "total power drawn by taskId in Watt")

plotT = ggplot(gpu_analysis_12, aes(y, -x)) + geom_tile(aes(fill = average_gpu_temp))+
  scale_fill_gradientn(colours = c("green","red","black"), values = c(0,0.5,1))+
  labs(title = "average gpu temparature by taskId in celsius")

plotU = ggplot(gpu_analysis_12, aes(y, -x)) + geom_tile(aes(fill = average_gpu_util))+
  scale_fill_gradientn(colours = c("green","red","black"), values = c(0,0.5,1))+
  labs(title = "average gpu utilization by taskId in percent")

plotM = ggplot(gpu_analysis_12, aes(y, -x)) + geom_tile(aes(fill = average_gpu_memory_util))+
  scale_fill_gradientn(colours = c("green","red","black"), values = c(0,0.5,1))+
  labs(title = "average gpu memory utilization by taskId in percent")

gpu_performance_12  = ggarrange(plotPW, plotT, plotU,plotM, 
                                labels = c("A", "B", "C","D"),
                                ncol = 2, nrow = 2)

(gpu_performance_12 = ggarrange(plotPW, plotT, plotU,plotM, 
                                labels = c("A", "B", "C","D"),
                                ncol = 2, nrow = 2)) %>% ggexport(filename = "graphs/gpu performance at level 12.png",width = 1000,height = 1000)
gpu_performance_12

## for level 8
gpu_analysis_8 = gpu_analysis %>% filter(level == 8)

plotPW8 = ggplot(gpu_analysis_8, aes(y, -x)) + scale_fill_viridis(discrete=FALSE) + theme_ipsum()+ geom_tile(aes(fill = total_power_drawn))+
  labs(title = "total power drawn by taskId in Watt")
plotPW8

plotT8 = ggplot(gpu_analysis_8, aes(y, -x)) +scale_fill_viridis(discrete=FALSE) + theme_ipsum()+ geom_tile(aes(fill = average_gpu_temp))+
  labs(title = "average gpu temparature by taskId in celsius")



plotU8 = ggplot(gpu_analysis_8, aes(y, -x)) +scale_fill_viridis(discrete=FALSE) + theme_ipsum()+ geom_tile(aes(fill = average_gpu_util))+
  labs(title = "average gpu utilization by taskId in percent")


plotM8 = ggplot(gpu_analysis_8, aes(y, -x)) +scale_fill_viridis(discrete=FALSE) + theme_ipsum()+ geom_tile(aes(fill = average_gpu_memory_util))+
  labs(title = "average gpu memory utilization by taskId in percent")



(plot_gpu_performance_8  = ggarrange(plotPW8, plotT8, plotU8,plotM8, 
                                     labels = c("A", "B", "C","D"),
                                     ncol = 2, nrow = 2)) %>% ggexport(filename = "graphs/gpu performance at level 8.png",
                                                                       width = 1000,height = 1000)
plot_gpu_performance_8
##----------------------------------------------------------------------------------------------------
