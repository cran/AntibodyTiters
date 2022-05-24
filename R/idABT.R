# source: 20210922_AntibodyTiters_1.txt
idABT <- function(objName = "inData"){
	if(inherits(get(objName), "ABT") != TRUE) stop("The class must be ABT\n")
	return(get(objName)$DATA$ID)
}
