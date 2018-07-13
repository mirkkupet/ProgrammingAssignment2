complete <- function(directory , id=1:332) {
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
	filepaths <- paste(directory, filenames, sep="/")
	df <- data.frame("ID" = numeric(0), "nobs" = numeric(0))
	i = id[1]
	for (path in filepaths) {
		tempdf <- data.frame(read.csv(path, header=TRUE))
		newrow <- c(i, sum(complete.cases(tempdf)))
		df <- rbind.data.frame(df, newrow)
		i <- i+1
	}
	colnames(df) <- c("id", "nobs")
	return(df)
} 