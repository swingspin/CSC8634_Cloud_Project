# Q1- Event which dominate task-runtime

##Runtime analysis on each event name at level 8

runtime_final_data_8 = runtime_final_data %>% filter(level == 8)

#by total rendering time
total_rendering_8 = runtime_final_data_8 %>% filter(eventName == "TotalRender")
total_rendering_8 = total_rendering_8[,c(2,3,7,8,10,20)]
total_rendering_8 = total_rendering_8 %>% distinct()
plot_A = ggplot(total_rendering_8, aes(y, -x, fill= time_difference)) + geom_tile() +scale_fill_viridis(discrete=FALSE) + theme_ipsum()+labs(title = "Total rendering runtime heat map based on taskId ")
plot_A
install.packages("viridis")
library(viridis)

#by render time
render_8 = runtime_final_data_8 %>% filter(eventName == "Render")
render_8 = render_8[,c(2,3,7,8,10,20)]
render_8 = render_8 %>% distinct()
plot_B = ggplot(render_8, aes(y, -x, fill= time_difference)) + geom_tile()+scale_fill_viridis(discrete=FALSE) + theme_ipsum()+labs(title = " rendering runtime heat map based on taskId ")
plot_B


#by Saving Config time
Saving_Config_8 = runtime_final_data_8 %>% filter(eventName == "Saving Config")
Saving_Config_8 = Saving_Config_8[,c(2,3,7,8,10,20)]
Saving_Config_8 = Saving_Config_8 %>% distinct()
plot_C = ggplot(Saving_Config_8, aes(y, -x, fill= time_difference)) + geom_tile()+scale_fill_viridis(discrete=FALSE) + theme_ipsum()+labs(title = "saving configuration runtime heat map based on taskId ")
plot_C


#by Uploading time
Uploading_8 = runtime_final_data_8 %>% filter(eventName == "Uploading")
Uploading_8 = Uploading_8[,c(2,3,7,8,10,20)]
Uploading_8 = Uploading_8 %>% distinct()
plot_D = ggplot(Uploading_8, aes(y, -x, fill= time_difference)) + geom_tile()+scale_fill_viridis(discrete=FALSE) + theme_ipsum()+labs(title = "uploading runtime heat map based on taskId ")
plot_D


#by Tiling time
Tiling_8 = runtime_final_data_8 %>% filter(eventName == "Tiling")
Tiling_8 = Tiling_8[,c(2,3,7,8,10,20)]
Tiling_8 = Tiling_8 %>% distinct()
plot_E = ggplot(Tiling_8, aes(y, -x, fill= time_difference)) + geom_tile()+scale_fill_viridis(discrete=FALSE) + theme_ipsum()+labs(title = "tiling runtime heat map based on taskId ")
plot_E
library(ggpubr)

(plot_REDA8 = ggarrange(plot_A, plot_B, plot_C,plot_D, plot_E, 
                        labels = c("A", "B", "C","D","E"),
                        ncol = 2, nrow = 3)) %>% ggexport(filename = "graphs/heat map rendering time of events at level 8.png",width = 1000,height = 1000)
plot_REDA8
#Error in ggexport(., filename = "graphs/heat map rendering time of events at level 8.png",  : 
#could not find function "ggexport"
##-------------------------------------------------------------------------------------------------------------------------------

##boxplot at level 8-----------------------------------------------------------------------------------
box_plot_8 = which(is.na(runtime_final_data_8$eventName))
box_plot_8 = runtime_final_data_8[!is.na(runtime_final_data_8$eventName),]
box_plot_8 = box_plot_8 %>% filter(eventType == "STOP")

boxplot_8 = ggplot(box_plot_8, aes(y= time_difference, fill = eventName)) + geom_boxplot()+ labs(title = "boxplot of runtimes of level 8 ")
boxplot_8


##---------

##Runtime analysis on each event name at level 12

runtime_final_data_12 = runtime_final_data %>% filter(level == 12)

#by total rendering time
total_rendering_12 = runtime_final_data_12 %>% filter(eventName == "TotalRender")
total_rendering_12 = total_rendering_12[,c(2,3,7,8,10,20)]
total_rendering_12 = total_rendering_12 %>% distinct()


lst <- sort(total_rendering_12$time_difference, index.return=TRUE, decreasing=TRUE)
lapply = lapply(lst, `[`, lst$x %in% head(unique(lst$x),10))
total_rendering_12_max_i = lapply$ix
total_rendering_12_max = total_rendering_12[total_rendering_12_max_i,]


plot_F = ggplot(total_rendering_12, aes(y, -x)) + geom_tile(aes(fill = time_difference))+ 
  geom_point(data = total_rendering_12_max, aes(y,-x))+labs(title = "Total rendering runtime heat map based on taskId ")
plot_F


#by render time
render_12 = runtime_final_data_12 %>% filter(eventName == "Render")
render_12 = render_12[,c(2,3,7,8,10,20)]
render_12 = render_12 %>% distinct()

lst <- sort(render_12$time_difference, index.return=TRUE, decreasing=TRUE)
lapply = lapply(lst, `[`, lst$x %in% head(unique(lst$x),10))
render_12_max_i = lapply$ix
render_12_max = render_12[render_12_max_i,]



plot_G = ggplot(render_12, aes(y, -x)) + geom_tile(aes(fill = time_difference)) + 
  geom_point(data = render_12_max, aes(y,-x))+labs(title = "Rendering runtime heat map based on taskId ")



#by Saving Config time
Saving_Config_12 = runtime_final_data_12 %>% filter(eventName == "Saving Config")
Saving_Config_12 = Saving_Config_12[,c(2,3,7,8,10,20)]
Saving_Config_12 = Saving_Config_12 %>% distinct()

lst <- sort(Saving_Config_12$time_difference, index.return=TRUE, decreasing=TRUE)
lapply = lapply(lst, `[`, lst$x %in% head(unique(lst$x),10))
Saving_Config_12_max_i = lapply$ix
Saving_Config_12_max = Saving_Config_12[Saving_Config_12_max_i,]



plot_H = ggplot(Saving_Config_12, aes(y, -x)) + geom_tile(aes(fill= time_difference))+
  geom_point(data = Saving_Config_12_max, aes(y,-x))+labs(title = "saving configuration runtime heat map based on taskId ")



#by Uploading time
Uploading_12 = runtime_final_data_12 %>% filter(eventName == "Uploading")
Uploading_12 = Uploading_12[,c(2,3,7,8,10,20)]
Uploading_12 = Uploading_12 %>% distinct()

lst <- sort(Uploading_12$time_difference, index.return=TRUE, decreasing=TRUE)
lapply = lapply(lst, `[`, lst$x %in% head(unique(lst$x),10))
Uploading_12_max_i = lapply$ix
Uploading_12_max = Uploading_12[Uploading_12_max_i,]


plot_I = ggplot(Uploading_12, aes(y, -x)) + geom_tile(aes(fill= time_difference))+
  geom_point(data = Uploading_12_max, aes(y,-x))+labs(title = "Uploading runtime heat map based on taskId ")



#by Tiling time
Tiling_12 = runtime_final_data_12 %>% filter(eventName == "Tiling")
Tiling_12 = Tiling_12[,c(2,3,7,8,10,20)]
Tiling_12 = Tiling_12 %>% distinct()

lst <- sort(Tiling_12$time_difference, index.return=TRUE, decreasing=TRUE)
lapply = lapply(lst, `[`, lst$x %in% head(unique(lst$x),10))
Tiling_12_max_i = lapply$ix
Tiling_12_max = Tiling_12[Tiling_12_max_i,]


plot_J = ggplot(Tiling_12, aes(y, -x)) + geom_tile(aes(fill= time_difference))+
  geom_point(data = Tiling_12_max, aes(y,-x))+labs(title = "Tiling runtime heat map based on taskId ")



(plot_level_12 = ggarrange(plot_F, plot_G, plot_H,plot_I, plot_J, 
                           labels = c("A", "B", "C","D","E"),
                           ncol = 3, nrow = 2)) %>% ggexport(filename = "graphs/heat map rendering time of events at level 12.png",width = 1000,height = 800)

plot_level_12

##-------------------------------------------------------------------------------------------------------------------------------------


## boxplot at level 12----------------------------------------------------------------------
box_plot_12 = which(is.na(runtime_final_data_12$eventName))
box_plot_12 = runtime_final_data_12[!is.na(runtime_final_data_12$eventName),]
box_plot_12 = box_plot_12 %>% filter(eventType == "STOP")

boxplot_12 = ggplot(box_plot_12, aes(y= time_difference, fill = eventName)) + geom_boxplot() + labs(title = "boxplot of runtimes of level 12 ")
boxplot_12
##---------------------------------------------------------------------------------------------


(big_boxplot = ggarrange(boxplot_12, boxplot_8, 
                    labels = c("A", "B"),
                    ncol = 2, nrow = 1)) %>% ggexport(filename = "graphs/boxplot of event run times at level 12 and 8.png",
                                                      width = 1000,height = 500)
