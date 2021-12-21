options(timeout=1800)

Packages<-c("RSQLite","doParallel")

for(n in Packages){
  if(require(n,character.only=TRUE)){
    print(paste(n,"is installed"))
  }else{
    print(paste("trying to install",n))
    install.packages(n, repos = "http://cran.us.r-project.org")
    if(require(n,character.only=TRUE)){
      print(paste(n,"installed"))
    }else{
      stop(paste("could not install",n))
    }
    
  }
  
}

print("R config complete!")