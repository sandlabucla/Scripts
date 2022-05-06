### Transfer the onset files from the SAND Lab server onto hoffman
### contact yaelwaizman00@ucla.edu with any issues 
### Updated: 3/3/2020

transfer_ev_files2hoffman_func <- function(uname, subs, behav_data_path, curr_wave, time_point, hoffman_path, tasks){
  #install packages if you don't have them already by erasing the "#" below next to each of their corresponding lines. 
  #DO NOT SAVE THIS CHANGE WHEN CLOSING THE DOCUMENT
  #install.packages("ssh")
  #install.packages("readr")
  #install.packages("stringr")
  library(ssh)
  require(ssh)
  library(readr)
  require(readr)
  library(stringr)
  require(stringr)
  
  #log on to hoffman2
  session <- ssh_connect(sprintf("%s@hoffman2.idre.ucla.edu", uname)) #create SSH session to hoffman2
  
  #loop over subjects
  for (curr_sub in 1:length(subs)) { 
    #loop through all the tasks
    for (curr_task in 1:length(tasks)){
      #get onsets for each subject one task at a time  
      ev_files <- Sys.glob(file.path(behav_data_path, subs[curr_sub], curr_wave,"Scan_session", "Onsets", tasks[curr_task], "*.txt"))
      #skip this task if there are no onsets
      if (identical(ev_files, character(0))==FALSE){
        
        # if the participant's subject folder or timepoint or "behav" folder for that timepoint have NOT been generated
        
        # figure out which layer(s) you're missing
        
        # make the apporiate folder
        
        
        #set the path to the subject's behavioral folder on hoffman2
        hoffpath <- file.path(hoffman_path, subs[curr_sub], time_point, "behav") 
        #get the task folders that exist on hoffman2 for the subject's behav folder
        hoffman_output <- rawToChar(ssh_exec_internal(session, command = paste("ls ", hoffpath, sep="", collapse = NULL))$stdout)
        #find the task that does not exist on hoffman2 in the subject's behav folder but we have onsets for
        check <- grep(tasks[curr_task],hoffman_output)
        #if folder does not exist, create it for that task on hoffman2 in the subject's behav folder
        if (identical(check, integer(0)) == TRUE){
          try(ssh_exec_wait(session, command = paste("mkdir ", file.path(hoffpath, tasks[curr_task]), sep = "", collapse = NULL)))
          for (each_ev_file in 1:length(ev_files)){
            #upload the onset files for the task on hoffman2 in the subject's behav folder
            try(scp_upload(session, files = ev_files[each_ev_file], to = file.path(hoffpath, tasks[curr_task])))
            try(ssh_exec_wait(session, command = paste("chmod -R g+xrw ", file.path(hoffpath, "*"), sep = "", collapse = NULL)))
          }
        }else{
          for (each_ev_file in 1:length(ev_files)){
            #upload the onset files for the task on hoffman2 in the subject's behav folder
            try(scp_upload(session, files = ev_files[each_ev_file], to = file.path(hoffpath, tasks[curr_task])))
            try(ssh_exec_wait(session, command = paste("chmod -R g+xrw ", file.path(hoffpath, "*"), sep = "", collapse = NULL)))
          }
        }
      }
    }
  }
  #disconnect the session when we're done!
  ssh_disconnect(session) 
}
