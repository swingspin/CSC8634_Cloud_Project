 #library("ProjectTemplate")
#load.project()
#install.packages("ggpubr")


library(ggpubr)

#install.packages("hrbrthemes")
library(hrbrthemes)
library(ggplot2)
#install.packages("viridis")
library(viridis)
##Data-pre_processing part 01
## joining data sets by mutating observations given to analyze

#renaming dataset as per convience
tasks_dataset = task.x.y
gpu_dataset= gpu
rendering_dataset = application.checkpoints
#avoiding similar rows by eliminating similar rows
rendering_dataset = rendering_dataset %>% distinct()
gpu_dataset = gpu_dataset %>% distinct()
tasks_dataset = tasks_dataset %>% distinct()

#checking the datasets
head(tasks_dataset)
head(gpu_dataset)
head(rendering_dataset)

#combining rendering_dataset and tasks_dataset
rendering_tasks_dataset = merge(x = rendering_dataset, y = tasks_dataset, by = "taskId", all = TRUE)

## creating a unique dataset by combination of given 3 files

full_dataset <- full_join(rendering_tasks_dataset,gpu_dataset,by = c("timestamp", "hostname"))
full_dataset = full_dataset[order(full_dataset$hostname, full_dataset$timestamp),]
#mutating the dataset to create new taskid,x and y coordinate,jobid  and levels according to timestam of a particular task process ongoing time
#filled taskId
mutated_full_dataset = full_dataset %>%
  mutate(to_fill = !is.na(zoo::na.locf(taskId, fromLast = TRUE, na.rm = FALSE)),
         refined_taskId = if_else(to_fill,
                                  zoo::na.locf(taskId, na.rm = FALSE),
                                  pmax(first(taskId), zoo::na.locf(taskId, na.rm = FALSE))),
  )
#filled x coordinated-
mutated_full_dataset = mutated_full_dataset %>%
  mutate(to_fill = !is.na(zoo::na.locf(x, fromLast = TRUE, na.rm = FALSE)),
         refined_x = if_else(to_fill,
                             zoo::na.locf(x, na.rm = FALSE),
                             pmax(first(x), zoo::na.locf(x, na.rm = FALSE))),
  )

#filled y coordinates
mutated_full_dataset = mutated_full_dataset %>%
  mutate(to_fill = !is.na(zoo::na.locf(y, fromLast = TRUE, na.rm = FALSE)),
         refined_y = if_else(to_fill,
                             zoo::na.locf(y, na.rm = FALSE),
                             pmax(first(y), zoo::na.locf(y, na.rm = FALSE))),
  )

#filled jobId's
mutated_full_dataset = mutated_full_dataset %>%
  mutate(to_fill = !is.na(zoo::na.locf(jobId.x, fromLast = TRUE, na.rm = FALSE)),
         refined_jobId = if_else(to_fill,
                                 zoo::na.locf(jobId.x, na.rm = FALSE),
                                 pmax(first(jobId.x), zoo::na.locf(jobId.x, na.rm = FALSE))),
  )
#filled levels
mutated_full_dataset = mutated_full_dataset %>%
  mutate(to_fill = !is.na(zoo::na.locf(level, fromLast = TRUE, na.rm = FALSE)),
         refined_level = if_else(to_fill,
                                 zoo::na.locf(level, na.rm = FALSE),
                                 pmax(first(level), zoo::na.locf(level, na.rm = FALSE))),
  )


#adding seconds to calculate event type time and gain the date of the events

mutated_full_dataset = cbind(mutated_full_dataset, read.table(text = as.character(mutated_full_dataset$timestamp), sep = "T"))
mutated_full_dataset = cbind(mutated_full_dataset, read.table(text = as.character(mutated_full_dataset$V2), sep = "."))
mutated_full_dataset = mutated_full_dataset[,-c(24,26)]
mutated_full_dataset = cbind(mutated_full_dataset, read.table(text = as.character(mutated_full_dataset$V1.1), sep = ":"))
mutated_full_dataset = mutated_full_dataset[,-24]
mutated_full_dataset$seconds_of_day = mutated_full_dataset$V1.1*60*60+mutated_full_dataset$V2*60+mutated_full_dataset$V3
mutated_full_dataset = mutated_full_dataset[,-c(24,25,26)]
mutated_full_dataset$V1 = as.Date(mutated_full_dataset$V1)
mutated_full_dataset$seconds_of_day = as.numeric(mutated_full_dataset$seconds_of_day)
names(mutated_full_dataset)[23] = "date"



# final cleaned dataset
#creating a array
drop <- c("taskId","jobId.x","jobId.y","x","y","level","to_fill")
#omitting the columns that are not required for further process
final_dataset = mutated_full_dataset[,!(names(mutated_full_dataset) %in% drop)]

final_dataset <- final_dataset[, c(1, 11, 2, 3, 4, 17, 12, 13, 14, 15, 5, 6, 7, 8, 9, 10, 16)]
#renaming the refined datasets
names(final_dataset)[2] = "taskId"
names(final_dataset)[7] = "x"
names(final_dataset)[8] = "y"
names(final_dataset)[9] = "jobId"
names(final_dataset)[10] = "level"

final_dataset$gpuSerial = as.character(final_dataset$gpuSerial)


## runtime_data_ analysis - calculating  time difference or runtime of each event

runtime_data = final_dataset[,c(2, 4, 5, 6)]
#excluding N/A values
runtime_data =  na.omit(runtime_data)
runtime_data = as.data.frame(runtime_data)
#re-ordering the serial number starting from 1
rownames(runtime_data) <- 1:nrow(runtime_data)
#pivoting the data with start stop columns and processing the seconds of day in their columns
runtime_data = runtime_data %>% pivot_wider(names_from = eventType, values_from = seconds_of_day)
runtime_data = as.data.frame(runtime_data)
runtime_data$time_difference = runtime_data$STOP-runtime_data$START


runtime_final_data = full_join(final_dataset,runtime_data, by = c("taskId","eventName"))



