#install.packages("RSQLite")
#setwd("C:/database/dbuddy/lib")
#setwd("C:/Apps/dbuddy/lib")
#class definitions: 
#make a table class. Will have standard methods

user.input <- function(prompt) {
  if (interactive()) {
    return(readline(prompt))
  } else {
    cat(prompt)
    return(readLines("stdin", n=1))
  }
}

args<-commandArgs(trailingOnly = TRUE)

#print(commandArgs())

#print(args)

if(args[1]=="config"){
  
  source("../etc/Installe.R")
  
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  
  stop()
}

source("classes.R")
source("dbcon.R")


suppressWarnings(suppressMessages(library(RSQLite)))


lookup_datatype<-read.csv("../etc/DataTypeLookupR_SQLite3.csv")
suppressWarnings(lookup_datatype$R_name[which(is.na(lookup_datatype$R_name))]<-"NA")
#make a db connection, and set some standard pragma settings.  
con <-standard_con("C:/database/lab_data_exp.db")


#based on the syntax, translate to a class method. 

#get variables: 
#print(args)

badsymbols = c("'*'","gThan","lThan")
if(any(args %in% badsymbols)){   
  badsymbols = badsymbols[which(badsymbols %in% args)]
  for(n in 1:length(badsymbols)){
    if(badsymbols[n]=="lThan"){
	  args[which(args==badsymbols[n])]<- "<"
    }else if(badsymbols[n]=="gThan"){
	  args[which(args==badsymbols[n])]<- ">"
    }else{
    args[which(args==badsymbols[n])]<- gsub("'", '', badsymbols[n])
	}
  }
}

if(length(args)==1){
  #means it came in a a combined string from system2
  args = strsplit(args," ")[[1]]
}

#hardcode everything you can do since it needs to be a defined procedure to enforce trust
if(args[1]=='insert'){
  print("The following changes have been scheduled in this transaction:")
  csvpath = args[3]
  data = read.csv(csvpath)
  if(args[2]=='soundfiles'){
    soundfiles()$insert(data)
  #}else if(args[3]=='bins'){
  #  bins()$insert(data)
  #}else if(args[3]=='detections'){
  #  known_keys = 'y' == args[which(args=="--known_keys")+1] #required argument for detection insertion, y or no
  #  detections().insert(data,known_keys)
  #}
  }else if(args[2]=='filegroups'){
    name = substr(csvpath,1,(nchar(csvpath)-4))
    name = basename(name) #removes path
    
    #--SelectionMethod optional argument
    if("--SelectionMethod" %in% args){
      selmethod = which(args=="--SelectionMethod")
    }
    
    if("--Description" %in% args){
      desc = which(args=="--Description")
    }
    
    #recombine any spaces between these arguments: 
    
    if(selmethod<desc){
      selmethodarg = paste(args[(selmethod+1):(desc-1)],collapse=" ")
      descarg = paste(args[(desc+1):length(args)],collapse=" ")
    }else{
      descarg = paste(args[(desc+1):(selmethodarg-1)],collapse=" ")
      selmethodarg = paste(args[(selmethodarg+1):length(args)],collapse=" ")
    }
   
    filegroups()$insert(data,name,selmethodarg,descarg)
  }else if (args[2]=='detections'){
    data$Comments[is.na(data$Comments)]<-""
    data$VisibleHz = as.character(data$VisibleHz)
    
    #print(data)
    #print(str(data))
    
    if("--out" %in% args){
      
      out = detections()$insert(data,return_id=TRUE)

      write.csv(out,gzfile(args[which(args=="--out")+1]),row.names = FALSE) #file path comes after out
    }else{
      detections()$insert(data)
    }
  }else if(args[2]=="analyses"){
    
    analyses()$insert(data)
  }
}  

if(args[1]=='modify'){
  print("The following changes have been scheduled in this transaction:")
  csvpath = args[3]
  data = read.csv(csvpath)
  
  if(args[2]=='detections'){
    data$Comments[is.na(data$Comments)]<-""
    data$VisibleHz = as.character(data$VisibleHz)
    detections()$modify(data)
  }else if(args[2]=="analyses"){
    
    analyses()$modify(data)
  }
}

if(args[1]=='delete'){
  print("The following changes have been scheduled in this transaction:")
  csvpath = args[3]
  keys = read.csv(csvpath) #expects single column corresponding to keys
  if(args[2]=='detections'){
    detections()$delete(keys[,1])
  }
}

#print(args)
if(args[1] == 'pull'){
  
  outfile = args[3]
  
  if(args[2]=='detections'){
    
    #FileGroups
	
	 command = "SELECT DISTINCT detections.* FROM filegroups JOIN bins_filegroups ON filegroups.Name = bins_filegroups.FG_name 
    JOIN bins_detections ON bins_filegroups.bins_id = bins_detections.bins_id JOIN detections ON bins_detections.detections_id = detections.id "
    
    FG = args[which(args=="--FileGroup")+1]
    #if .csv is present, remove it from string
    if(grepl(".csv",FG)){
      FG = substr(FG,1,nchar(FG)-4)
    }
	
	command = paste(command,"WHERE filegroups.Name='",FG,"'",sep="")
	
	if("--SignalCode" %in% args){
		
		SignalCode = args[which(args=="--SignalCode")+1]
		command = command = paste(command," and detections.SignalCode='",SignalCode,"'",sep="")
	}
	
	if("--Analysis_ID" %in% args){
		
		Analysis_ID = args[which(args=="--Analysis_ID")+1]
		command = command = paste(command," and detections.Analysis_ID='",Analysis_ID,"'",sep="")
	}
	
	if("--label" %in% args){
		
		label = args[which(args=="--label")+1]
		command = command = paste(command," and detections.label='",label,"'",sep="")
	}
	
	if("--Type" %in% args){
		
		Type = args[which(args=="--Type")+1]
		command = command = paste(command," and detections.Type='",Type,"'",sep="")
	}else{
		command = command = paste(command,";",sep="")
	}
                      
    #I think that should only do these hardcoded functions for SELECT needs used through INSTINCT. Otherwise, better to just directly use SQL on DB.  
    
    query = dbSendQuery(con,command)
    out = dbFetch(query)
    
    write.csv(out,gzfile(outfile),row.names = FALSE)
    
  }else if(args[2]=='filegroups'){
    
    FG = args[which(args=="--FileGroup")+1]
    #if .csv is present, remove it from string
    if(grepl(".csv",FG)){
      FG = substr(FG,1,nchar(FG)-4)
    }
    
   command = paste("SELECT FileName,SegStart,Segdur FROM bins JOIN bins_filegroups ON bins.id = bins_filegroups.bins_id JOIN filegroups ON
                   bins_filegroups.FG_name = filegroups.Name WHERE filegroups.Name='",FG,"';",sep="")
   
   query = dbSendQuery(con,command)
   
   out = dbFetch(query)
   dbClearResult(query)  
   
   #now that we have these fields, we also need to get some metadata from soundfiles (StartTime,Duration) and from deployments (Name)
   
   #with the fields contained in out, I want to get other data 
   
   command = "SELECT soundfiles.Name,soundfiles.Duration,soundfiles.DateTime,deployments.Name FROM soundfiles JOIN deployments ON soundfiles.deployments_name = deployments.Name
                   WHERE soundfiles.Name=$fnames;"
   
   moremeta <-  dbSendStatement(con,command)
   dbBind(moremeta, params=list(fnames=out$FileName))
   out2 = dbFetch(moremeta)
   dbClearResult(moremeta)  
   
   out2$DateTime<-as.POSIXct(out2$DateTime,tz="UTC")
   out2=out2[,2:length(out2)]
   #finally, the path can get derived from DateTime and Mooring name. 
   #format datetime to character 6digit date and 6digit time 
   out = data.frame(out$FileName,paste("/",out2$Name,"/",format(out2$DateTime,"%m"),"_",format(out2$DateTime,"%Y"),"/",sep=""),format(out2$DateTime,"%y%m%d-%H%M%S"),out2$Duration,out2$Name,out$SegStart,out$SegDur)
   colnames(out)<-c("FileName","FullPath","StartTime","Duration","Deployment","SegStart","SegDur")
   write.csv(out,gzfile(outfile),row.names = FALSE)
    
  }
  
  if(args[2]=='direct'){
    
    args = paste(args[4:grep(";",args)],collapse=" ") #find the end semicolon and stop there 
    query = dbSendQuery(con,args)
    out = dbFetch(query)
    
    write.csv(out,gzfile(outfile),row.names = FALSE)
    
  }
}

if(args[1]=='direct'){
  
  #this allows you to directly talk to the database using a standard command. Susceptible to attacks/vulnerabilities
  #from malicious use from NOAA lab users (low risk..). There is also a standard backup from db server. 
  
  #note that this doesn't work for SQLite shell commands (ie: .tables, .schema, etc) but works for select options
  
  
  args = paste(args[2:grep(";",args)],collapse=" ") #find the end semicolon and stop there 
  query = dbSendQuery(con,args)
  out = dbFetch(query)
  dbClearResult(query)
  
  print(out)
  
}

if(args[1]=='info'){
  
  if(args[2]=='tables' & length(args)==3){
    #get info on specific tables
    
    exp = parse(text=paste(args[3],"()$getschema()",sep=""))
    
    eval(exp)
    
  }else if(args[2]=='tables'){
    #list tables
    out= dbListTables(con)
    print(out)
  }
}


#if(!("--no_warn" %in% args)){ #if no warn is not in arguments
#  choice = user.input(prompt="Commit db transaction? (y/n):")
#  if(choice=='y'){
#    dbCommit(con)
#  }else{
#    print('exiting without changes...')   
#  }
#}else{
#  dbCommit(con)
#}

dbCommit(con)

dbDisconnect(con)
q()

#stop()

#example functions: 

dontrun = TRUE

if(dontrun){
#see schema of bin table
bins()$getschema()

#create some dummy data
testbindata <-data.frame(c("1","2"),c("1","2"),c(1,2),c(3,4))
colnames(testbindata)<-bins()$getschema()$name

#load in dummy data
bins()$insert(testbindata)
bins()$view_()

#try inserting data with wrong data type
wrongdatatype <-data.frame(c(1,2),c("1","2"),c(1,2),c(3,4))
bins()$insert(wrongdatatype)

#now try inserting data that worked before, again: 
bins()$insert(testbindata)
bins()$view_()

#now delete dummy data from bins 
bins()$table_delete(testbindata$id)
bins()$view_()


#insert and delete will both call other class tables, and their respective functions: soundfiles()$insert, bins_detections$modify() , etc. 
#doing this with classes allows for complex sequences that are performed procedurally. 

#lets try loading in some soundfiles. Currently, their aren't any in the table. 
#Soundfiles currently includes in it's basic import function, a call to 'deployments' import. 

deployments()$table_delete("testmanual")
deployments()$view_()

#can add an entry to deployments: 

deployments()$getschema()

deploymentsdummy<-data.frame("testmanual",30,50,as.integer(42),38,4,40,"then","later",as.integer(16384),"PST","M2",as.integer(1))
colnames(deploymentsdummy)<-deployments()$getschema()$name
deployments()$insert(deploymentsdummy)

deployments()$view_()
deployments()$insert("test2",'key')

#show how deployments is modified when soundfiles are entered
soundfiles()$view_()
soundfiles()$getschema()

soundfiledummy<-data.frame("testSF",600,"Testdeployment")
colnames(soundfiledummy)<-soundfiles()$getschema()$name
soundfiles()$insert(soundfiledummy)

#now we see that deployments and soundfiles were both updated:
soundfiles()$view_()
deployments()$view_()

soundfiles()$table_delete("testSF")
deployments()$table_delete("Testdeployment")

sfs=read.csv("C:/Users/daniel.woodrich/Desktop/database/soundfiles.csv")

#sfs=read.csv(paste(outpath,outnames[i],sep=""))

soundfiles()$insert(sfs)
soundfiles()$table_clear()
bintypes()$table_clear()
bins()$table_clear()

soundfiles()$view_()
bins()$view_()
bintypes()$view_()

}