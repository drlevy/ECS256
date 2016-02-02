# test; M/M/1 queue -- exponential ("Markov") job interarrivals,
# exponential service times, 1 server

mms <- function(meaninterarrv, meansrv, managerprob, meanmansrv, meandropout, nservers, buffersize, timelim, dbg=FALSE) {
  
  # set up structures
  simlist <- newsim(dbg)
  simlist$reactevent <- mmsreact  
  simlist$arrvrate <- 1 / meaninterarrv
  simlist$srvrate <- 1 / meansrv
  simlist$mansrvrate <- 1/meanmansrv
  simlist$manprob <- managerprob
  simlist$dropoutrate <- 1/meandropout
  simlist$totarrvs <- 0  
  simlist$totwait <- 0.0
  simlist$totjobs <-0
  simlist$totdrops <- 0
  simlist$totdeclines <- 0
  simlist$sysload <- 0  
  simlist$maxload <- buffersize
  simlist$queue <- newqueue(5)  
  simlist$manqueue <- newqueue(5)
  simlist$manbusy <- FALSE
  simlist$nsrvrfree <- nservers
  simlist$jobnum <- 0
  simlist$qempty <- TRUE
  # event type codes: 1 for arrival, 2 for service completion etc
  simlist$arrvevnt <- 1
  simlist$srvevnt <- 2
  simlist$manarrvevnt <- 3
  simlist$mansrvevnt <- 4
  simlist$dropout <- 5
  
  # set up and schedule first event, including info on this job's 
  # arrival time for later use in finding mean wait until job done
  timeto1starrival <- rexp(1,simlist$arrvrate)
  timeto1stdropout <- timeto1starrival + rexp(1,simlist$dropoutrate)
  jobnum <- incremjobnum(simlist)
 
  schedevnt(timeto1starrival,simlist$arrvevnt,simlist,
            c(timeto1starrival,jobnum, timeto1stdropout))
  
  # start sim
  mainloop(simlist,timelim)
  
  # sim done
  # should print out something near 1 / (srvrate - arrvrate)
  # cat("mean wait:  ")
  # print(simlist$totwait / simlist$totarrvs)
  cat("Proportion of declined:   ")
  print(simlist$totdeclines / simlist$totarrvs)
  cat("Proportion of dropped:    ")
  print(simlist$totdrops / simlist$totarrvs)
}

incremjobnum <- function(simlist) {
  jobnum <- simlist$jobnum + 1
  simlist$jobnum <- jobnum
  jobnum
}

# what new events are triggered by the occurrence of an old one?
mmsreact <- function(evnt,simlist) {
  etype <- evnt[2]
  if (etype == simlist$arrvevnt | etype ==simlist$manarrvevnt ) {  # EVENT1,3: any job arrival
    # bookkeeping
    simlist$totarrvs <- simlist$totarrvs + 1
    
    # schedule next arrival
    timeofnextarrival <- simlist$currtime + rexp(1,simlist$arrvrate)
    timedropout <- timeofnextarrival + rexp(1,simlist$dropoutrate)
    jobnum <- incremjobnum(simlist)
      # schedule normal call or manager call? using probability manprob
    if (rbinom(1,1, simlist$manprob) > 0){ 
      schedevnt(timeofnextarrival,simlist$manarrvevnt,simlist,
              c(timeofnextarrival,jobnum, timedropout))
    } else {
      schedevnt(timeofnextarrival,simlist$arrvevnt,simlist,
                c(timeofnextarrival,jobnum, timedropout))
    }
    schedevnt(timedropout,simlist$dropout,simlist,
              c(timeofnextarrival,jobnum, timedropout))
    
    # is maximum load reached?
    if (simlist$sysload < simlist$maxload){
          simlist$sysload <- simlist$sysload +1
          # start newly-arrived job or queue it
          if (simlist$nsrvrfree > 0) {  # server free, start job service
            if (etype == simlist$arrvevnt){ #if normal job, serve it
              simlist$nsrvrfree <- simlist$nsrvrfree -1
              srvduration <- rexp(1,simlist$srvrate)
              schedevnt(simlist$currtime+srvduration,simlist$srvevnt,
                        simlist,evnt[3:5])  # copy over previous data for this job
            } else if (etype == simlist$manarrvevnt) { # if man job, serve man job or manqueue it
              if (!simlist$manbusy){
                simlist$manbusy <-TRUE
                mansrvduration <- rexp(1,simlist$mansrvrate)
                schedevnt(simlist$currtime+mansrvduration,simlist$mansrvevnt,
                          simlist,evnt[3:5])  # copy over previous data for this job
              } else {
                appendfcfs(simlist$manqueue,evnt)
              }
            }
          } else {  # all servers busy, add job to queue
              appendfcfs(simlist$queue,evnt)
          }
    } else {  # can't take any more calls 
        simlist$totdeclines = simlist$totdeclines + 1
        } 
    
    
  } else if (etype == simlist$srvevnt) {  # EVENT 2: normal job completed
        # wait time = job completion time - job arrival time
        simlist$totwait <- simlist$totwait + simlist$currtime - evnt[3]
        simlist$totjobs <-  simlist$totjobs +1
        simlist$sysload <- simlist$sysload - 1 
        
        simlist$nsrvrfree <-simlist$nsrvrfree +1
        
        # check queue for waiting jobs
        # has this person dropped out already?
#         while (nrow(simlist$queue$m) > 0 ) {
#           qhead <- delfcfs(simlist$queue)
#           if (simlist$currtime > qhead[5]){
#             simlist$totdrops <- simlist$totdrops +1
#           }
#         }
        
        if (nrow(simlist$queue$m) > 0) {  # nonempty queue
          qhead <- delfcfs(simlist$queue)
          # start job service  
          # if man job, need to pass it on
          while (qhead[2] == simlist$manarrvevnt ){
            if (!simlist$manbusy){
              simlist$manbusy <-TRUE
              mansrvduration <- rexp(1,simlist$mansrvrate)
              schedevnt(simlist$currtime+mansrvduration,simlist$mansrvevnt,
                        simlist,qhead[3:5])  # copy over previous data for this job
            } else {
              appendfcfs(simlist$manqueue,evnt)
            }
            # next in line
            if (nrow(simlist$queue$m) > 0) {
              qhead <- delfcfs(simlist$queue)
              # next person in line
            } else {
              # no more in line
              simlist$qempty <- TRUE
              break
            }
          }
          
          if (!simlist$qempty) {   #spagetti code, flag
            simlist$nsrvrfree <-simlist$nsrvrfree -1
            srvduration <- rexp(1,simlist$srvrate)
            schedevnt(simlist$currtime+srvduration,simlist$srvevnt,simlist,
                      qhead[3:5])
          }
        }
  } else if (etype == simlist$mansrvevnt) { # EVENT 4: man job completed
    simlist$totjobs <-  simlist$totjobs +1
    simlist$sysload <- simlist$sysload - 1
    
    # wait time = job completion time - job arrival time
    simlist$totwait <- simlist$totwait + simlist$currtime - evnt[3]
    simlist$manbusy <- FALSE
    # check queue for waiting jobs
    if (nrow(simlist$manqueue$m) > 0) {  # nonempty queue
      qhead <- delfcfs(simlist$manqueue)
      # start job service
      simlist$manbusy <- TRUE
      mansrvduration <- rexp(1,simlist$mansrvrate)
      schedevnt(simlist$currtime+mansrvduration,simlist$mansrvevnt,simlist,
                qhead[3:5])
    }
  } else if (etype == simlist$dropout) { # EVENT 5
      if (nrow(simlist$queue$m) > 0) { 
        qhead <- delfcfs(simlist$queue)
        # is this person about to leave the line?
        if (simlist$currtime >= qhead[5]){
          simlist$totdrops <- simlist$totdrops +1
        } else { # no, then put it back
          simlist$queue$m <- rbind(qhead, simlist$queue$m)
        }
      }
    }
}