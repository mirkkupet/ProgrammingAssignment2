corr <- function(directory, threshold=0) {
	source("complete.R")
	comp <- complete(directory)
	ids <- comp$id[comp$nobs>threshold]

	correlations <- function(dir, id) {
		filename <- function(index) {
			index <- toString(index)
			if (nchar(index)==1){
				index <- paste("00",index,sep="")
			}
			if (nchar(index)==2){
				index <- paste("0",index,sep="")
			}
			return(paste(index, ".csv", sep=""))
		}
		filenames <- lapply(id, filename)
		filepaths <- paste(dir, filenames, sep="/")
		v <- vector(mode="numeric", length=0)
	
		for (path in filepaths) {
			if(file.exists(path)){
				tempdf <- data.frame(read.csv(path, header=TRUE))
				v <- c(v, cor(tempdf$nitrate, tempdf$sulfate, use="complete.obs"))
			}
			else {
				v <- c(v, null)
			}
		}
		return(v)
	} 
	
	vec <- correlations(directory, ids)
	return(vec)
}