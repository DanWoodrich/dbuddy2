# ABOUT #

DBuddy is companion code for the AFSC MML acoustics lab SQLite database. It contains an execution pattern that stages data (lib/exec) and a system of interconnected R Reference Classes 
to emulate database 'triggers' in R. The overall goal is to automate data staging of common data products. 

The purpose of DBuddy is to automate the underlying processes between our lab data assets, starting with our common entrypoints: namely, retrieval of new data, 
SoundChecker label generation/review and detection label generation/review. s

DBuddy is an independent application called from the command line, and so can be called by processes from INSTINCT and SoundChecker. 

DBuddy can be used with both a local and server SQLite db (see dbuddy_server and dbuddy_client repo for client/server implementation). 

# CONFIG #

If using DBuddy with client/server implementation, instead go to dbuddy_client repo for configuration

For local use of DBuddy, you will need 

* R vers >= 4.0.0
* System path with
	* ~/dbuddy/bin
	* ~/R-x.x.x/bin
* Installation of R packages in ~/dbuddy/etc/Installe.R, OR
* Run in cmd window >dbuddy config 

This should allow you to run dbuddy commands. To see the effect of various commands, look in the text of ~/dbuddy/lib/exec