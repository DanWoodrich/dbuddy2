#scratch script with dbuddy

outpath = "//161.55.120.117/NMML_AcousticsData/Working_Folders/MetaData/WaveMetaData/"
outnames = dir(outpath)
             
depnames = substr(outnames,1,nchar(outnames)-15)

for(i in 1:length(outnames)){
  
  command = paste("dbuddy insert soundfiles",paste(outpath,outnames[i],sep="")," --no_warn")
  system(command)

}


#ok: so the following got into the database. Need to try the rest again and debug. 
deps_l=read.csv("C:/Users/daniel.woodrich/Desktop/database/deps loaded/deps.csv",header=FALSE)
deps_l=unlist(deps_l)
deps_l = paste(deps_l,"_soundfiles.csv",sep="")

outnames<-outnames[-which(outnames %in% deps_l)]

#see what isn't in outnames, but is in deps_l. These must be deployments which are named in duty cycle but are not on NAS. 
print(deps_l[-which(deps_l %in% outnames)])
