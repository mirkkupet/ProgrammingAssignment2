pollutantmean <- function(directory , pollutant, id=1:332) {
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
	df <- data.frame(Date = character(0), sulfate = character(0), nitrate = character(0), ID = character(0))
	
	for (path in filepaths) {
		tempdf <- data.frame(read.csv(path, header=TRUE))
		df <- rbind.data.frame(df, tempdf)
	}
	return(mean(df[[pollutant]], na.rm=TRUE))
} 