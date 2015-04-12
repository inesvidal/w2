corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    # creates the matrix with the data from all the sensors ["Date", "nitrate", "sulfate", "ID"]
    dataread <- function(directory){
        files_full<-list.files(directory, full.names = TRUE)
        data <- data.frame()
        for (i in 1:length(files_full)) {
            data <- rbind(data, read.csv(files_full[i]))
        }
        data
    }
    
    # Creates the ["id" "nobs"] matrix, nobs is the number of complete observations
    complete<-function(data, id=1:322){
        result <- data.frame(row.names = TRUE)
        for (i in id) {
            vector <- c(i, sum(complete.cases(data[which(data$ID == i), ])))
            result <- rbind(result, vector)
        }
        colnames(result) <- c("id", "nobs")
        result  
    }
    
    # identify which of the Rows in the ["id" "nobs"] matrix have more than 
    # threshold occurrences, 
    # so the ids of the sensors that we need to consider in the correlations.
        
    obs <- dataread(directory)
    
    nobs <- complete(obs, id=1:322)
    
#     nobs_above <- nobs[which(nobs$nobs > threshold),]
    nobs_above <- nobs[nobs$nobs > threshold,]
    
    #complete_obs <- obs[complete.cases(obs), ]
    complete_obs <- obs
    
    complete_obs_above <- complete_obs[complete_obs$ID %in% nobs_above$id, ]
    
    # initialise correlation vector, to make sure we provide the right answer 
    #even if there is nothing above the threshold.
    correlation <- numeric(0)
    
    for (i in nobs_above$id) {
        a <- cor(complete_obs_above[which(complete_obs_above$ID == i), ]$nitrate,
                 complete_obs_above[which(complete_obs_above$ID == i), ]$sulfate, 
                 use="pairwise.complete.obs")
        a <- round(a, digits=4)
        correlation <- c(correlation, a) 
                        
    }
    correlation
}

# source("corr.R")
# source("complete.R")
# cr <- corr("specdata", 150)
# head(cr)
## [1] -0.01896 -0.14051 -0.04390 -0.06816 -0.12351 -0.07589
# summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -0.2110 -0.0500  0.0946  0.1250  0.2680  0.7630
# cr <- corr("specdata", 400)
# head(cr)
## [1] -0.01896 -0.04390 -0.06816 -0.07589  0.76313 -0.15783
# summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -0.1760 -0.0311  0.1000  0.1400  0.2680  0.7630
# cr <- corr("specdata", 5000)
# summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 
# length(cr)
## [1] 0
# cr <- corr("specdata")
# summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -1.0000 -0.0528  0.1070  0.1370  0.2780  1.0000
# length(cr)
## [1] 323