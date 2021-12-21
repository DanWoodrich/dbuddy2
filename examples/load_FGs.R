#scratch script with dbuddy


FGcsv = read.csv("C:/Users/daniel.woodrich/Desktop/database/filegroups.csv")


for(i in 1:nrow(FGcsv)){
  
  command = paste("dbuddy insert filegroups",paste("C:/Users/daniel.woodrich/Desktop/database/FileGroups/",FGcsv[i,"Name"],".csv",sep=""),
                  "--SelectionMethod",FGcsv[i,"SelectionMethod"],"--Description",FGcsv[i,"Description"])
  system(command)

}


#ok: so the following got into the database. Need to try the rest again and debug. 
deps_l=read.csv("C:/Users/daniel.woodrich/Desktop/database/deps loaded/deps.csv",header=FALSE)
deps_l=unlist(deps_l)
deps_l = paste(deps_l,"_soundfiles.csv",sep="")

outnames<-outnames[-which(outnames %in% deps_l)]

#see what isn't in outnames, but is in deps_l. These must be deployments which are named in duty cycle but are not on NAS. 
print(deps_l[-which(deps_l %in% outnames)])
