# source: 20210924_AntibodyTiters_1.txt
emptyABT <- function(fileName = "empty.xlsx", pmax = 7, returnDF = FALSE){
	# pmax should be less than 18
	# require(openxlsx)

	minimum.colnames <- c("ID", "pre_vaccination_yyyymmdd", "pre_vaccination_score", 
			"1st_shot_yyyymmdd", "post_1st_shot_yyyymmdd", "post_1st_shot_score", 
			"2nd_shot_yyyymmdd", "post_2nd_shot_yyyymmdd", "post_2nd_shot_score")

	additional.colnames <- ""
	for(p in 4:pmax){
		additional.colnames[(p-4)*2+1] <- paste("point", p, "_yyyymmdd", sep = "")
		additional.colnames[(p-4)*2+2] <- paste("point", p, "_score", sep = "")
	}

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
	emptyDF[["pre_vaccination_yyyymmdd"]][1] <- "20200914"
	emptyDF[["pre_vaccination_score"]][1] <- 0
	emptyDF[["1st_shot_yyyymmdd"]][1] <- "20200921"
	emptyDF[["post_1st_shot_yyyymmdd"]][1] <- "20201004"
	emptyDF[["post_1st_shot_score"]][1] <- 50
	emptyDF[["2nd_shot_yyyymmdd"]][1] <- "20201015"
	emptyDF[["post_2nd_shot_yyyymmdd"]][1] <- "20201107"
	emptyDF[["post_2nd_shot_score"]][1] <- 500

	days <- c("20210112", "20210401", "20210623", "20210910", 
			"20211202", "20220226", "20220507", "20220812", "20221022", 
			"20230122", "20230312", "20230624", "202301003", "20240115")
	scores <- c(400, 200, 100, 25, 12, 6, 3, 2, 1, 0, 0, 0, 0, 0)

	for(p in 4:pmax){
		emptyDF[, 9+(p-4)*2+1][1] <- days[p-3]
		emptyDF[, 9+(p-4)*2+2][1] <- scores[p-3]
	}

	if(returnDF == FALSE)	write.xlsx(emptyDF, file = fileName, overwrite = TRUE)
	if(returnDF == TRUE)	return(emptyDF)
}
