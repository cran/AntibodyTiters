# source: 20210924_AntibodyTiters_1.txt
toyABT <- function(fileName = "toy.xlsx", pmax = 7, patients = 20, outsiderPercent = 2, NaPercent = 10, 
					Attrib = c("Sex", "Age", "VeryLow"), 
					attribFactors = list(c("F", "M"), c(18, 80), c(TRUE, FALSE))){
	# pmax should be less than 18
	# require(openxlsx)
	
	if(length(attribFactors) != length(Attrib)){
		stop("length(attribFactors) and length(Attrib) must be the same.")
	}
	
	toyDF <- emptyABT(pmax = pmax, returnDF = TRUE, 
					Attrib = Attrib, attribFactors = attribFactors)
	toyDF[2:(patients+1),] <- NA

	patientIDs <- paste("patient ", 1:patients, sep = "")
	toyDF[["ID"]][2:(patients+1)] <- patientIDs

	# days
	colIndexs <- c(2, 4, 5, 7, 8, seq(10, (10+(pmax-4)*2), 2))

	for(c in 1:5){
		colIndex <- colIndexs[c]
		DAY <- as.integer(as.Date(toyDF[[colIndex]][1], format = "%Y%m%d"))
		for(p in 1:patients){
			temp <- as.character(as.Date(DAY + sample(0:20, size = 1), 
					origin = "1970-01-01"))
			temp <- paste(strsplit(temp, split = "-")[[1]], collapse = "")
			toyDF[[colIndex]][(p+1)] <- temp
		}
	}
	for(c in 6:length(colIndexs)){
		colIndex <- colIndexs[c]
		DAY <- as.integer(as.Date(toyDF[[colIndex]][1], format = "%Y%m%d"))
		for(p in 1:patients){
			temp <- as.character(as.Date(DAY + sample(0:60, size = 1), 
					origin = "1970-01-01"))
			temp <- paste(strsplit(temp, split = "-")[[1]], collapse = "")
			toyDF[[colIndex]][(p+1)] <- temp
		}
	}

	for(p in 1:patients){
		temp <- as.integer(toyDF[p+1, colIndexs])
		temp <- as.character(temp[order(temp, decreasing = FALSE)])
		toyDF[p+1, colIndexs] <- temp
	}

	# scores
	colIndexs <- c(3, 6, 9, seq(11, (11+(pmax-4)*2), 2))
	smax <- max(toyDF[1, colIndexs], na.rm = TRUE)

	for(c in 1:3){
		colIndex <- colIndexs[c]
		score <- toyDF[[colIndex]][1]
		# scores <- sample((score - score*(1/2)):(score + score*(1/2)), size = patients, replace = TRUE)
		# scores <- exp(rnorm(n = patients, mean = log(score), sd = 0.2))
		scores <- exp(rnorm(n = patients, mean = log(score), sd = 0.4))
		toyDF[[colIndex]][2:(patients+1)] <- scores
	}
	
	if(outsiderPercent > 0){
		# Already high before vaccination
		outsiderIndex <- sample(1:patients, size = ceiling(patients * 0.02 * outsiderPercent))
		for(o in 1:length(outsiderIndex)){
			patientScores <- as.numeric(toyDF[1+outsiderIndex[o], colIndexs])
			# patientScores[1] <- sample((smax*0.3):(smax*0.8), size = 1)
			patientScores[1] <- exp(rnorm(n = 1, mean = log(smax), sd = 0.05))
			patientScores[3] <- patientScores[2] + patientScores[2] + patientScores[1]
			patientScores[2] <- patientScores[2] + patientScores[1]
			# patientScores[3] <- patientScores[2] + patientScores[1] * sample(1:3, size = 1) * 0.1
			toyDF[1+outsiderIndex[o], colIndexs] <- patientScores
		}
	}
	
	getCt <- function(t, C0 = 1000, Thalf = 90){
		Ct <- C0 / 2^(t/Thalf)
		return(Ct)
	}
	
	Thalf <- 90
	for(p in 1:patients){
		T0 <-  as.Date(as.character(toyDF[[colIndexs[3]-1]][p+1]), "%Y%m%d")
		C0 <- toyDF[[colIndexs[3]]][p+1]
		for(c in 4:length(colIndexs)){
			DAY <- as.Date(as.character(toyDF[[colIndexs[c]-1]][p+1]), "%Y%m%d")
			Period <- as.integer(DAY - T0)
			tempCt <- getCt(t = Period, C0 = C0, Thalf = Thalf)
			toyDF[[colIndexs[c]]][p+1] <- exp(rnorm(n = 1, mean = log(tempCt), sd = 0.05))
		}
	}

	
	## Attrib
	for(c in 1:length(Attrib)){
		colName <- Attrib[c]
		if(colName != "Age" | colName != "VeryLow"){
			toyDF[[colName]][2:(patients+1)] <- sample(attribFactors[[c]], 
											size = patients, replace = TRUE)
		}
		if(colName == "Age"){
			toyDF[[colName]][2:(patients+1)] <- sample(attribFactors[[c]][1]:attribFactors[[c]][2], 
											size = patients, replace = TRUE)
		}
		if(colName == "VeryLow"){
			toyDF[[colName]][2:(patients+1)] <- FALSE
			VLindex <- sample(2:(patients+1), size = ceiling(patients/20), replace = TRUE)
			VLindex <- VLindex[VLindex != outsiderIndex]
			toyDF[[colName]][VLindex] <- TRUE
			for(v in 1:length(VLindex)){
				patientScores <- as.numeric(toyDF[VLindex[v], colIndexs])
				# patientScores[1:2] <- sample(1:3, size = 1)
				# patientScores[3:length(patientScores)] <- sample(1:3, 
				# 	size = length(patientScores) -2, replace = TRUE)
				toyDF[VLindex[v], colIndexs] <- exp(log(patientScores)*0.6)
			}
		}
	}
	
	# NA
	if(NaPercent > 0){
	colIndexsC <- c(2, 5, 8, seq(10, (10+(pmax-4)*2), 2))
	colIndexsS <- c(3, 6, 9, seq(11, (11+(pmax-4)*2), 2))
	NaIndex <- sample(1:patients, size = ceiling(patients * 0.02 * NaPercent))
		for(n in 1:length(NaIndex)){
			x <- sample(1:length(colIndexsC), size = 1)
			toyDF[1+NaIndex[n], colIndexsC[x]] <- NA
			toyDF[1+NaIndex[n], colIndexsS[x]] <- NA
		}
	}
	
	# Age
	colIndexsC <- c(2, 5, 8, seq(10, (10+(pmax-4)*2), 2))
	colIndexsS <- c(3, 6, 9, seq(11, (11+(pmax-4)*2), 2))
	OldIndex <- which(toyDF$Age >= 35)
	for(o in 1:length(OldIndex)){
		# toyDF[OldIndex[o], colIndexsS] <- toyDF[OldIndex[o], colIndexsS] * 0.8
		tempScore <- toyDF[OldIndex[o], colIndexsS]
		toyDF[OldIndex[o], colIndexsS]<- exp(log(tempScore)*0.98)
		
	}
	OldIndex <- which(toyDF$Age >= 55)
	for(o in 1:length(OldIndex)){
		tempScore <- toyDF[OldIndex[o], colIndexsS]
		toyDF[OldIndex[o], colIndexsS]<- exp(log(tempScore)*0.98)
}
	OldIndex <- which(toyDF$Age >= 65)
	for(o in 1:length(OldIndex)){
		tempScore <- toyDF[OldIndex[o], colIndexsS]
		toyDF[OldIndex[o], colIndexsS]<- exp(log(tempScore)*0.98)
	}

	write.xlsx(toyDF, file = fileName, overwrite = TRUE)
}
