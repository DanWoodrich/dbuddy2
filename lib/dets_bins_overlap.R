#this accepts a det and bin dataframe as args, and compares them and specifies relationship. 
#associations can be used to find detections within a specified effort.
#these relationships can later be used to compare labels across different bin extents


suppressWarnings(suppressMessages(library(doParallel)))
suppressWarnings(suppressMessages(library(parallel)))

startLocalPar<-function(num_cores,...){
  
  cluz <<- parallel::makeCluster(num_cores)
  registerDoParallel(cluz)
  
  clusterExport(cluz, c(...))
  
}

compare_dets_bins <-function(bindata,detdata){
  
  detdata$StartTime = round(detdata$StartTime,2)
  detdata$EndTime = round(detdata$EndTime,2)
  
  #first, reduce data by soundfiles
  files = unique(bindata$FileName)
  
  crs = detectCores()
  if(length(files)>crs){
    crs =1 #not worth overhead
  }
  bindata<<-bindata
  detdata<<-detdata
  files<<-files
  startLocalPar(crs,"bindata","detdata","files")
  
  filematchout = foreach(i=1:length(files),.packages=c("doParallel","parallel")) %dopar% {
    
    file = files[i]
    
    file_bins = bindata[which(bindata$FileName == file),]
    file_dets = detdata[which(detdata$StartFile == file | detdata$EndFile == file),]
    
    binmatchout = NULL
    
    if(nrow(file_bins>0)){
      
        binmatchout = foreach(n=1:nrow(file_bins)) %do% {
        
        matchout=NULL
        
        bin = file_bins[n,]
    
        if(nrow(file_dets)>0){
          
          matchout=foreach(p=1:nrow(file_dets)) %do% {
            
            detfile = file_dets[p,]
            
            bin$EndTime = bin$SegStart + bin$SegDur
            
            matches = vector(length=6)
            
            if(detfile$StartFile==detfile$EndFile){
               if(detfile$StartTime>bin$EndTime){
                 return(NULL) #can't be match by definition
               }else if(detfile$EndTime<bin$SegStart){
                 return(NULL) #can't be match by definition
               }
              matches[1] = detfile$StartTime<bin$EndTime & detfile$StartTime>bin$SegStart  #test if det start is between bin dims
              matches[2] = detfile$EndTime<bin$EndTime & detfile$EndTime > bin$SegStart      #test if det end is between bin dims
              matches[3] = detfile$EndTime>bin$EndTime & detfile$StartTime < bin$SegStart   #test if det is larger than bin
              matches[4] = detfile$EndTime==bin$EndTime & detfile$StartTime==bin$SegStart

            }else{
              
              if(detfile$StartFile==bin$FileName){
                matches[5] = detfile$StartTime<bin$EndTime #I think this will always work
              }else{ #endfile is bin name
                matches[6] = detfile$EndTime>bin$SegStart
              }
            }
            
            if(matches[4]){
              relationship="exact"
            }else if(matches[3]){ #I should probably also have a test for exact match, rounded to 2 decimal
              relationship="external"
            }else if(matches[1]&matches[2]){
              relationship="internal"
            }else if(any(matches)){
              relationship = "overlap"
            }else{
              return(NULL)
            }
            
            row = c(detfile$id,relationship,bin$id)
            #print(row)
            
            return(row)
          
          }
          
          matchout = do.call('rbind',matchout)
          
          
         
        }
        #print(matchout)
        return(matchout)
      }
        
      binmatchout= do.call('rbind',binmatchout)
      
    }
    
    #print(binmatchout)
    return(binmatchout)
  
  }
  stopCluster(cluz)
  
  filematchout= do.call('rbind',filematchout)
  bins_detections_tab<-data.frame(filematchout)
  #print(bins_detections_tab)
  colnames(bins_detections_tab)<-c("detections_id","relationship","bins_id")
  
  return(bins_detections_tab)
  
}


