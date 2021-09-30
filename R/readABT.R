# source: 20210922_AntibodyTiters_1.txt
readABT <- function(fileName = "xxx.xlsx"){
	# require(openxlsx)
	cat("Loading the xlsx file ", fileName, "...\n", sep = "")
	inData <- read.xlsx(xlsxFile = fileName)
	inData <- inData[-grep(pattern = "#", substr(inData$ID, start = 1, stop = 1)),]

	cat("Checking colnames...\n", sep = "")
	minimum.colnames <- c("ID", "pre_vaccination_yyyymmdd", "pre_vaccination_score", 
			"1st_shot_yyyymmdd", "post_1st_shot_yyyymmdd", "post_1st_shot_score", 
			"2nd_shot_yyyymmdd", "post_2nd_shot_yyyymmdd", "post_2nd_shot_score")

	for(i in 1:9){
	if(colnames(inData)[i] != minimum.colnames[i]){
		stop("Invalid colnames\n")
	}
	}

	additional.colnames <- colnames(inData)[10:ncol(inData)]
	if(length(grep(pattern = "point", substr(additional.colnames, start = 1, stop = 5))) != 
		length(additional.colnames)){
			stop("Invalid colnames\n")
	}

	p <- 4
	for(a in seq(1, length(additional.colnames), 2)){
		curr.colname <- additional.colnames[a]
		next.colname <- additional.colnames[a+1]
		POINT <- unlist(strsplit(curr.colname, split = "_"))[1]
		if(p != as.integer(unlist(strsplit(POINT, split = "point"))[2])){
			stop("Invalid colnames\n")
		}
		POINT <- unlist(strsplit(next.colname, split = "_"))[1]
		if(p != as.integer(unlist(strsplit(POINT, split = "point"))[2])){
			stop("Invalid colnames\n")
		}
		if(next.colname != paste(POINT, "_", "score", sep = "")){
			stop("Invalid colnames\n")
		}
		if(curr.colname != paste(POINT, "_", "yyyymmdd", sep = "")){
			stop("Invalid colnames\n")
		}
		p <- p + 1
	}

	cat("Checking ID uniqueness...\n", sep = "")
	if(length(unique(inData$ID)) != length(inData$ID)){
		stop("Identical IDs were found.\n", sep = "")
	}

	yyyymmddCols <- colnames(inData)[grep(pattern = "yyyymmdd", colnames(inData))]
	for(i in 1:length(yyyymmddCols)){
		inData[[yyyymmddCols[i]]] <- as.Date(as.character(inData[[yyyymmddCols[i]]]), "%Y%m%d")
	}

	pmax <- p - 1

	temp <- inData[, grep(pattern = "yyyymmdd", colnames(inData))]
	longestFromSecond <- 0
	shortestFromSecond <- 300
	for(i in 1:nrow(temp)){
		firstShot <- as.integer(temp[["1st_shot_yyyymmdd"]][i])
		secondShot <- as.integer(temp[["2nd_shot_yyyymmdd"]][i])
		for(p in 4:pmax){
			colName <- paste("point", p, "_yyyymmdd", sep = "")
			if(is.na(as.integer(temp[[colName]][i])) == FALSE){
				longestFromSecond <- max(longestFromSecond, 
					as.integer(temp[[colName]][i]) - secondShot)
				shortestFromSecond <- min(shortestFromSecond, 
					as.integer(temp[[colName]][i]) - secondShot)
			}

		}
	}

	inData <- list(DATA = inData, pmax = pmax, 
			longestFromSecond = longestFromSecond, 
			shortestFromSecond = shortestFromSecond)
	class(inData) <- "ABT"
	# assign(objName, inData, envir = .GlobalEnv)
	# cat("An ABT object \"", objName, "\" has been successfully assigned in .GlobalEnv.\n", sep = "")
	cat("This object contains a data.frame named \"DATA\" and a numeric vector \"pmax\".\n", sep = "")
	cat("pmax is ", pmax, "\n", sep = "")
	cat("longestFromSecond is ", longestFromSecond, "\n", sep = "")
	cat("shortestFromSecond is ", shortestFromSecond, "\n", sep = "")
	return(inData)
}
