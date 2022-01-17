#data pre-process part 2

## gpu performance analysis with respect to hostname

#filtering the runtime data
host_gpu_performance = runtime_final_data[,c(3,13,14,15,16)]
#omiting the NA values
host_gpu_performance = na.omit(host_gpu_performance)
#re-ordering the serial number as needed
rownames(host_gpu_performance) <- 1:nrow(host_gpu_performance)
#filtering the data considering "gpuUtilPerc>15 & gpuMemUtilPerc>15" performance variables as Non-ideal state of gpu performance
host_gpu_performance = host_gpu_performance %>% filter(gpuUtilPerc>15 & gpuMemUtilPerc>15)



## final_gpu_performace = generating by summarizing the values

final_host_gpu_performance = host_gpu_performance
final_host_gpu_performance = final_host_gpu_performance %>% 
  group_by(final_host_gpu_performance$hostname) %>%   summarise(total_power_drawn = sum(powerDrawWatt), average_gpu_temp = mean(gpuTempC),
                                                                average_gpu_util = mean(gpuUtilPerc),average_gpu_memory_util = mean(gpuMemUtilPerc))


names(final_host_gpu_performance)[1] = "hostname"


# gpu performance preprocessing step

#filtering the runtime data
gpu_performance = runtime_final_data[,c(2,13,14,15,16)]
#omiting the NA values
gpu_performance = na.omit(gpu_performance)
#re-ordering the serial number as needed
rownames(gpu_performance) <- 1:nrow(gpu_performance)
#filtering the data considering "gpuUtilPerc>15 & gpuMemUtilPerc>15" performance variables as Non-ideal state of gpu performance
gpu_performance = gpu_performance %>% filter(gpuUtilPerc>15 & gpuMemUtilPerc>15)



## final_gpu_performace = generating by summarizing the values

final_gpu_performance = gpu_performance
final_gpu_performance = final_gpu_performance %>% 
  group_by(final_gpu_performance$taskId) %>%   summarise(total_power_drawn = sum(powerDrawWatt), average_gpu_temp = mean(gpuTempC),
                                                         average_gpu_util = mean(gpuUtilPerc),average_gpu_memory_util = mean(gpuMemUtilPerc))
names(final_gpu_performance)[1] = "taskId"


analysis_dataset = full_join(runtime_final_data, final_gpu_performance, by = c("taskId"))

## EDA with gpu performance 

gpu_analysis = analysis_dataset %>% filter(eventName == "TotalRender" & eventType == "STOP")
gpu_analysis = gpu_analysis[,c(2,3,7,8,10,6,20,21,22,23,24)]
