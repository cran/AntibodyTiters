# source: 20210922_AntibodyTiters_1.txt
readABT <- function(fileName = "xxx.xlsx", attribNumeric = "Age"){
	# require(openxlsx)
	cat("Loading the xlsx file ", fileName, "...\n", sep = "")
	inData <- read.xlsx(xlsxFile = fileName)
	if(length(grep(pattern = "#", substr(inData$ID, start = 1, stop = 1))) > 0){
		inData <- inData[-grep(pattern = "#", substr(inData$ID, start = 1, stop = 1)),]
	}
	cat("Checking colnames...\n", sep = "")
	minimum.colnames <- c("ID", "pre_vaccination_yyyymmdd", "pre_vaccination_score", 
			"1st_shot_yyyymmdd", "post_1st_shot_yyyymmdd", "post_1st_shot_score", 
			"2nd_shot_yyyymmdd", "point3_yyyymmdd", "point3_score")

	for(i in 1:9){
		if(colnames(inData)[i] != minimum.colnames[i]){
			stop("Invalid colnames\n")
		}
	}

	additional.colnames <- colnames(inData)[10:ncol(inData)]
	point.index <- grep(pattern = "point", additional.colnames)
	yyyymmdd.index <- grep(pattern = "_yyyymmdd", additional.colnames)
	score.index <- grep(pattern = "_score", additional.colnames)
	attrib.index <- grep(pattern = "point", additional.colnames, invert = TRUE)
	
	if(length(yyyymmdd.index) != length(score.index)){
			stop("Invalid colnames\n")
	}

	if(length(attrib.index) > 0){
		attribColnames <- additional.colnames[attrib.index]
		for(a in 1:length(attribColnames)){
			for(n in 1:length(attribNumeric)){
				if(attribColnames[a] == attribNumeric[n]){
					class(inData[[attribColnames[a]]]) <- "numeric"
				}
			}	
		}
	}

	Attrib <- list()
	if(length(attrib.index) > 0){
		attribColnames <- additional.colnames[attrib.index]
		for(a in 1:length(attribColnames)){
			if(is.numeric(inData[[attribColnames[a]]]) == FALSE & is.integer(inData[[attribColnames[a]]]) == FALSE){
				colNames <- as.character(unique(inData[[attribColnames[a]]]))
				for(c in 1:length(colNames)){
					if(is.na(colNames[c]) == FALSE){
						Attrib[[attribColnames[a]]][colNames[c]] <- 
							length(which(inData[[attribColnames[a]]] == unique(inData[[attribColnames[a]]])[c]))
					}
					if(is.na(colNames[c]) == TRUE){
						Attrib[[attribColnames[a]]][colNames[c]] <- 
							length(which(is.na(inData[[attribColnames[a]]]) == TRUE))
					}
				}
			}
			if(is.numeric(inData[[attribColnames[a]]]) == TRUE | is.integer(inData[[attribColnames[a]]]) == TRUE){
				# Attrib[[attribColnames[a]]] <- c(min(inData[[attribColnames[a]]]), max(inData[[attribColnames[a]]]))
				Attrib[[attribColnames[a]]] <- inData[[attribColnames[a]]]
			}		
		}
	}

	p <- 4
	for(a in seq(1, length(point.index), 2)){
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

	yyyymmddCols <- colnames(inData)[grep(pattern = "_yyyymmdd", colnames(inData))]
	for(i in 1:length(yyyymmddCols)){
		inData[[yyyymmddCols[i]]] <- as.Date(as.character(inData[[yyyymmddCols[i]]]), "%Y%m%d")
	}

	pmax <- p - 1

	temp <- inData[, grep(pattern = "yyyymmdd", colnames(inData))]
	longestFromSecond <- 0
	shortestFromSecond <- 600
	for(i in 1:nrow(temp)){
		secondShot <- as.integer(temp[["2nd_shot_yyyymmdd"]][i])
		for(p in 3:pmax){
			colName <- paste("point", p, "_yyyymmdd", sep = "")
			if(is.na(as.integer(temp[[colName]][i])) == FALSE){
				longestFromSecond <- max(longestFromSecond, 
					as.integer(temp[[colName]][i]) - secondShot)
				shortestFromSecond <- min(shortestFromSecond, 
					as.integer(temp[[colName]][i]) - secondShot)
			}
		}
	}
	
	# Addition of point*_from2ndShot columns
	for(p in 3:pmax){
		colName <- paste("point", p, "_from2ndShot", sep = "")
		inData[[colName]] <- as.integer(NA)
	}
	
	for(i in 1:nrow(inData)){
			secondShot <- as.integer(inData[["2nd_shot_yyyymmdd"]][i])
		for(p in 3:pmax){
			colName <- paste("point", p, "_yyyymmdd", sep = "")
			Day <- as.integer(inData[[colName]][i]) - secondShot
			colName <- paste("point", p, "_from2ndShot", sep = "")
			inData[[colName]][i] <- Day
		}
	}
	
	post2ndDays <- as.integer(inData[["point3_yyyymmdd"]] - inData[["2nd_shot_yyyymmdd"]])
	
	# Addition of M1_*_from2ndShot columns
	firstMonth <- min(ceiling(shortestFromSecond/30), ceiling(post2ndDays/30), na.rm = TRUE)
	lastMonth <- ceiling(longestFromSecond/30)
	
	for(m in firstMonth:lastMonth){
		colName <- paste("M1_", m, "_from2ndShot", sep = "")
		inData[[colName]] <- as.integer(NA)
	}
	
	for(i in 1:nrow(inData)){
			secondShot <- as.integer(inData[["2nd_shot_yyyymmdd"]][i])
		for(p in 3:pmax){
			colName <- paste("point", p, "_yyyymmdd", sep = "")
			Day <- as.integer(inData[[colName]][i]) - secondShot
			colName <- paste("point", p, "_score", sep = "")
			Score <- inData[[colName]][i]
			Month <- ceiling(Day/30)
			if(is.na(Day) == FALSE & is.na(Score) == FALSE & is.na(Month) == FALSE){
				colName <- paste("M1_", Month, "_from2ndShot", sep = "")
				inData[[colName]][i] <- Score
			}
		}
	}

	# Addition of M2_*-*_from2ndShot columns
	firstM2group <- min(ceiling(shortestFromSecond/60), ceiling(post2ndDays/60), na.rm = TRUE)
	lastM2group <- ceiling(longestFromSecond/60)
	for(m in firstM2group: lastM2group){
		M2start <- (m-1)*2 + 1
		M2end <- (m-1)*2 + 2
		colName <- paste("M2_", M2start, "-", M2end, "_from2ndShot", sep = "")
		inData[[colName]] <- as.integer(NA)
	}
	for(i in 1:nrow(inData)){
			secondShot <- as.integer(inData[["2nd_shot_yyyymmdd"]][i])
		for(p in 3:pmax){
			colName <- paste("point", p, "_yyyymmdd", sep = "")
			Day <- as.integer(inData[[colName]][i]) - secondShot
			colName <- paste("point", p, "_score", sep = "")
			Score <- inData[[colName]][i]
			m <- ceiling(Day/60)
			if(is.na(Day) == FALSE & is.na(Score) == FALSE & is.na(m) == FALSE){
				M2start <- (m-1)*2 + 1
				M2end <- (m-1)*2 + 2
				colName <- paste("M2_", M2start, "-", M2end, "_from2ndShot", sep = "")
				inData[[colName]][i] <- Score
			}
		}
	}

	# Addition of M3_*-*_from2ndShot columns
	firstM3group <- min(ceiling(shortestFromSecond/90), ceiling(post2ndDays/90), na.rm = TRUE)
	lastM3group <- ceiling(longestFromSecond/90)
	for(m in firstM3group: lastM3group){
		M3start <- (m-1)*3 + 1
		M3end <- (m-1)*3 + 3
		colName <- paste("M3_", M3start, "-", M3end, "_from2ndShot", sep = "")
		inData[[colName]] <- as.integer(NA)
	}
	for(i in 1:nrow(inData)){
			secondShot <- as.integer(inData[["2nd_shot_yyyymmdd"]][i])
		for(p in 3:pmax){
			colName <- paste("point", p, "_yyyymmdd", sep = "")
			Day <- as.integer(inData[[colName]][i]) - secondShot
			colName <- paste("point", p, "_score", sep = "")
			Score <- inData[[colName]][i]
			m <- ceiling(Day/90)
			if(is.na(Day) == FALSE & is.na(Score) == FALSE & is.na(m) == FALSE){
				M3start <- (m-1)*3 + 1
				M3end <- (m-1)*3 + 3
				colName <- paste("M3_", M3start, "-", M3end, "_from2ndShot", sep = "")
				inData[[colName]][i] <- Score
			}
		}
	}

	# Addition of M4_*-*_from2ndShot columns
	firstM4group <- min(ceiling(shortestFromSecond/120), ceiling(post2ndDays/120), na.rm = TRUE)
	lastM4group <- ceiling(longestFromSecond/120)
	for(m in firstM4group: lastM4group){
		M4start <- (m-1)*4 + 1
		M4end <- (m-1)*4 + 4
		colName <- paste("M4_", M4start, "-", M4end, "_from2ndShot", sep = "")
		inData[[colName]] <- as.integer(NA)
	}
	for(i in 1:nrow(inData)){
			secondShot <- as.integer(inData[["2nd_shot_yyyymmdd"]][i])
		for(p in 3:pmax){
			colName <- paste("point", p, "_yyyymmdd", sep = "")
			Day <- as.integer(inData[[colName]][i]) - secondShot
			colName <- paste("point", p, "_score", sep = "")
			Score <- inData[[colName]][i]
			m <- ceiling(Day/120)
			if(is.na(Day) == FALSE & is.na(Score) == FALSE & is.na(m) == FALSE){
				M4start <- (m-1)*4 + 1
				M4end <- (m-1)*4 + 4
				colName <- paste("M4_", M4start, "-", M4end, "_from2ndShot", sep = "")
				inData[[colName]][i] <- Score
			}
		}
	}

	# Addition of M6_*-*_from2ndShot columns
	firstM6group <- min(ceiling(shortestFromSecond/180), ceiling(post2ndDays/180), na.rm = TRUE)
	lastM6group <- ceiling(longestFromSecond/180)
	for(m in firstM6group: lastM6group){
		M6start <- (m-1)*6 + 1
		M6end <- (m-1)*6 + 6
		colName <- paste("M6_", M6start, "-", M6end, "_from2ndShot", sep = "")
		inData[[colName]] <- as.integer(NA)
	}
	for(i in 1:nrow(inData)){
			secondShot <- as.integer(inData[["2nd_shot_yyyymmdd"]][i])
		for(p in 3:pmax){
			colName <- paste("point", p, "_yyyymmdd", sep = "")
			Day <- as.integer(inData[[colName]][i]) - secondShot
			colName <- paste("point", p, "_score", sep = "")
			Score <- inData[[colName]][i]
			m <- ceiling(Day/180)
			if(is.na(Day) == FALSE & is.na(Score) == FALSE & is.na(m) == FALSE){
				M6start <- (m-1)*6 + 1
				M6end <- (m-1)*6 + 6
				colName <- paste("M6_", M6start, "-", M6end, "_from2ndShot", sep = "")
				inData[[colName]][i] <- Score
			}
		}
	}

	inData <- list(DATA = inData, pmax = pmax, 
			longestFromSecond = longestFromSecond, 
			shortestFromSecond = shortestFromSecond, 
			Attrib = Attrib)
	class(inData) <- "ABT"
	# assign(objName, inData, envir = .GlobalEnv)
	# cat("An ABT object \"", objName, "\" has been successfully assigned in .GlobalEnv.\n", sep = "")
	
	cat("This object contains a data.frame named \"DATA\", numeric vectors \"pmax\", 
			\"longestFromSecond\" and \"shortestFromSecond\", and a list named \"Attrib\".\n", sep = "")
	cat("patients: ", nrow(inData$DATA), "\n", sep = "")
	cat("pmax is ", pmax, "\n", sep = "")
	cat("longestFromSecond is ", longestFromSecond, " (", longestFromSecond/30, " months)", "\n", sep = "")
	cat("shortestFromSecond is ", shortestFromSecond, " (", shortestFromSecond/30, " months)", "\n", sep = "")
	
	if(length(attrib.index) == 0){
		cat("There is no contents in Attrib.\n")
	}
	if(length(attrib.index) > 0){
	for(a in 1:length(inData$Attrib)){
		if(class(inData$DATA[[names(inData$Attrib[a])]]) != "numeric"){
			cat(paste(names(inData$Attrib)[a], ":\n", sep = ""))
			for(i in 1:length(inData$Attrib[[a]])){
				cat("  ", paste(names(inData$Attrib[[a]][i]), ": ", 
							as.character(inData$Attrib[[a]][i]), "\n", sep = ""))
			}
		}
		if(class(inData$DATA[[names(inData$Attrib[a])]]) == "numeric"){
			cat(paste(names(inData$Attrib)[a], ":\n", sep = ""))
			temp <- summary(inData$Attrib[[a]])
			cat(paste("  ", "Min:    ", temp[1], "\n"))
			cat(paste("  ", "1st Q:  ", temp[2], "\n"))
			cat(paste("  ", "Median: ", temp[3], "\n"))
			cat(paste("  ", "Mean:   ", temp[4], "\n"))
			cat(paste("  ", "3rd Q:  ", temp[5], "\n"))
			cat(paste("  ", "Max:    ", temp[6], "\n"))
		}		
	}
	}

	return(inData)
}
