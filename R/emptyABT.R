# source: 20210924_AntibodyTiters_1.txt
emptyABT <- function(fileName = "empty.xlsx", pmax = 7, returnDF = FALSE, 
				Attrib = c("Sex", "Age", "VeryLow"), 
				attribFactors = list(c("F", "M"), c(18, 80), c(TRUE, FALSE))){
	# pmax should be less than 18
	# require(openxlsx)

	if(length(attribFactors) != length(Attrib)){
		stop("length(attribFactors) and length(Attrib) must be the same.")
	}
	
	minimum.colnames <- c("ID", "pre_vaccination_yyyymmdd", "pre_vaccination_score", 
			"1st_shot_yyyymmdd", "post_1st_shot_yyyymmdd", "post_1st_shot_score", 
			"2nd_shot_yyyymmdd", "point3_yyyymmdd", "point3_score")

	additional.colnames <- ""
	for(p in 4:pmax){
		additional.colnames[(p-4)*2+1] <- paste("point", p, "_yyyymmdd", sep = "")
		additional.colnames[(p-4)*2+2] <- paste("point", p, "_score", sep = "")
	}
	
	C0 <- 1000
	Thalf <- 90
	
	total.colnames <- c(minimum.colnames, additional.colnames)
	emptyDF <- data.frame(matrix(nrow = 1, ncol = length(total.colnames)), 
				stringsAsFactors = FALSE)
	emptyDF[, c(1, 2, 4, 5, 7, 8)] <- as.character(emptyDF[, c(1, 2, 4, 5, 7, 8)])
	emptyDF[, c(3, 6, 9)] <- as.numeric(emptyDF[, c(3, 6, 9)])
	for(p in 4:pmax){
		emptyDF[, 9+(p-4)*2+1] <- as.character(emptyDF[, 9+(p-4)*2+1])
		emptyDF[, 9+(p-4)*2+2] <- as.numeric(emptyDF[, 9+(p-4)*2+2])
	}
	colnames(emptyDF) <- total.colnames
	
	emptyDF[["ID"]][1] <- "#patient X"
	emptyDF[["pre_vaccination_yyyymmdd"]][1] <- "20200901"
	emptyDF[["pre_vaccination_score"]][1] <- 2
	emptyDF[["1st_shot_yyyymmdd"]][1] <- "20201001"
	emptyDF[["post_1st_shot_yyyymmdd"]][1] <- "20201015"
	emptyDF[["post_1st_shot_score"]][1] <- 50
	emptyDF[["2nd_shot_yyyymmdd"]][1] <- "20201101"
	emptyDF[["point3_yyyymmdd"]][1] <- "20201115"
	emptyDF[["point3_score"]][1] <- C0
	
	getCt <- function(t, C0 = 1000, Thalf = 90){
		Ct <- C0 / 2^(t/Thalf)
		return(Ct)
	}

	# days <- integer(length = pmax - 3)
	# scores <- integer(length = pmax - 3)
	T0 <- as.Date(as.character(emptyDF[["point3_yyyymmdd"]][1]), "%Y%m%d")
	C0 <- emptyDF[["point3_score"]][1]
	for(p in 4:pmax){
		DAY <- T0 + 60 * ((p-4)+1)
		Period <- as.integer(DAY - T0)
		# days[p-3] <- gsub(pattern = "-", replacement = "", as.character(DAY))
		# scores[p-3] <- getCt(t = Period, C0 = C0, Thalf = Thalf)
		emptyDF[, 9+(p-4)*2+1][1] <- gsub(pattern = "-", replacement = "", as.character(DAY))
		emptyDF[, 9+(p-4)*2+2][1] <- getCt(t = Period, C0 = C0, Thalf = Thalf)
	}
	
	for(c in 1:length(Attrib)){
		colName <- Attrib[c]
		if(colName != "Age"){
			emptyDF[[colName]] <- sample(attribFactors[[c]], size = 1)
		}
		if(colName == "Age"){
			emptyDF[[colName]] <- sample(attribFactors[[c]][1]:attribFactors[[c]][2], size = 1)
		}
	}
	

	if(returnDF == FALSE)	write.xlsx(emptyDF, file = fileName, overwrite = TRUE)
	if(returnDF == TRUE)	return(emptyDF)
}
