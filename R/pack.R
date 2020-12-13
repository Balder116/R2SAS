
##' Convert the simple linear model R code to SAS code.
##'
##' Extract the factor and numeric variables from a simple linear model R code 
##' and re-built and export the SAS code for this simple linear model in txt form. 
##' The function can deal with factor and numeric variables as well as interaction terms 
##' (up to 3 interaction terms).
##' @title Convert R SLM code to SAS
##' @param lm a simple linear model
##' @usage SLMRtoSAS(lm)
##' @return 'sascode.txt' the simple linear model in SAS code
##' @author Haoting Han and Zhengya Gao
##' @export
##' @examples 
##' lm1 <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,data = iris)
##' SLMRtoSAS(lm1)
##' lm2 <- lm(Sepal.Length ~ . ,data=iris)
##' SLMRtoSAS(lm2)
##' lm3 <- lm(Sepal.Length ~ Sepal.Width * Petal.Length * Petal.Width + Species,data = iris)
##' SLMRtoSAS(lm3)
##' lm4 <- lm(Sepal.Length ~ Sepal.Width * Petal.Length + Petal.Width * Species,data = iris)
##' SLMRtoSAS(lm4)
##' 



SLMRtoSAS <- function(lm) {
    ## get the factor and numeric names.
    lmmodel <- lm$model
    is.fact <- sapply(lmmodel, is.factor)
    name.fact <- names(which(is.fact, T))
    is.nume <- sapply(lmmodel, is.numeric)
    name.nume <- names(which(is.nume, T))
    classpart <- paste((name.fact), collapse = " ")
    
    
    ## get the model equation.
    lmcall <- lm$call
    dataname <- as.character(lmcall[3])
    variablestring <- gsub(pattern = " ", replacement = "", x = as.character(lmcall[2]))
    outcome <- sub("\\~.*", "", variablestring)
    corvariables <- sub(".*~", "", variablestring)
    
    ## save data to current environment.
    write.csv(x = get(dataname), file = "RdatatoSAS.csv")
    dline1 <- paste0("proc import datafile = ", getwd(), "/RdatatoSAS.csv ", "out=RdatatoSAS dbms=CVS; run;")
    
    
    ## if there is a . representing all the covariates.
    if (corvariables == ".") {
        allvariables <- names(lmmodel)[-which(names(lmmodel) == outcome)]
        line1 <- paste0("proc glm data=", dataname, ";")
        line2 <- paste0("class ", classpart, ";")
        if (classpart == "") {
            line2 <- ""
        }
        line3 <- paste0("model ", outcome, "=", paste((allvariables), collapse = " "), ";")
        line4 <- paste0("run;")
        sascode <- paste(line1, line2, line3, line4, sep = "\n")
        cat(dline1, line1, line2, line3, line4, sep = "\n", file = "sascode.txt")
        file.show("sascode.txt")
    } else {
        # corvariables!='.'
        sepstring <- strsplit(corvariables, split = "[+]")[[1]]
        
        ## if there are interaction.
        if (any(grepl("[*]", sepstring))) {
            multstring <- sepstring[grep("[*]", sepstring)]
            plusstring <- sepstring[-(grep("[*]", sepstring))]
            pluspart <- paste((plusstring), collapse = " ")
            
            multresult <- c()
            multpart <- c()
            for (i in 1:length(multstring)) {
                multvars <- strsplit(multstring, split = "[*]")[[i]]
                if (length(multvars) == 2) {
                  
                  multresult[1] <- multvars[1]
                  multresult[2] <- multvars[2]
                  multresult[3] <- paste0(multvars[1], "*", multvars[2])
                  multpart[i] <- paste(multresult[1], multresult[2], multresult[3])
                }
                if (length(multvars) == 3) {
                  multresult <- c()
                  multresult[1] <- multvars[1]
                  multresult[2] <- multvars[2]
                  multresult[3] <- multvars[3]
                  multresult[4] <- paste0(multvars[1], "*", multvars[2])
                  multresult[5] <- paste0(multvars[1], "*", multvars[3])
                  multresult[6] <- paste0(multvars[2], "*", multvars[2])
                  multresult[7] <- paste0(multvars[1], "*", multvars[2], "*", multvars[2])
                  multpart[i] <- paste(multresult[1], multresult[2], multresult[3], multresult[4], 
                    multresult[5], multresult[6], multresult[7])
                }
                if (length(multvars) > 3) {
                  # more than 3 items interaction is a little complicated and unnecessary.
                  multpart[i] <- "Too much"
                }
            }  # end of  for (i in 1:length(multstring))
            
        } else {
            multstring <- F
            multpart <- ""
            pluspart <- paste(sepstring, collapse = " ")
        }  #end if (any(grepl('[*]', sepstring))) 
        
        if (multpart[1] == "Too much") {
            cat("The interaction is too complicated to be analysis.")
        } else {
            
            multpart2 <- paste((multpart), collapse = " ")
            line1 <- paste0("proc glm data=", dataname, ";")
            line2 <- paste0("class ", classpart, ";")
            if (classpart == "") {
                line2 <- ""
            }
            line3 <- paste0("model ", outcome, "=", pluspart, " ", multpart2, ";")
            line4 <- paste0("run;")
            sascode <- paste(line1, line2, line3, line4, sep = "\n")
            cat(dline1, line1, line2, line3, line4, sep = "\n", file = "sascode.txt")
            file.show("sascode.txt")
        }
        
    }  #end of if (corvariables!='.')
}  #end of function
