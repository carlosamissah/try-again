nGroup <- 3 # number of treatment groups
nName <- c("Legume","Maize", "Mix") # names of groups
nSize <- c(425,389,672) # number of observations in each group
nMean <- c(1.8,2.0,1.9) # mean of each group
nSD <- c(1.9,3.4,1.9)

if (AA=1:425, BB=1:389, CC=1:672)
  
birddata <- data.frame(nName=c("Legume","Maize", "Mix"),
                       nSize=c(425,389,672),
                       nMean=c(1.8,2.0,1.9),
                       nSD=c(1.9,3.4,1.9))
print(birddata)

aov_birds <- function(birdata){
  countc <- c(rnorm(n=nSize[1],mean=nMean[1],sd=nSD[1]),
              rnorm(n=nSize[2],mean=nMean[2],sd=nSD[2]),
              rnorm(n=nSize[3],mean=nMean[3],sd=nSD[3]))
  cGroup <- rep(nName,nSize)
  Croptype <- data.frame(ID,cGroup,countc)
  ANOmodel <- aov(countc~cGroup,data=Croptype)
  print(ANOmodel)
}
print(aov_birds)


compute_anova <- function(x=birddata, nName=nName, nSize= nSize) {
  # Perform ANOVA
  anova_result <- aov(as.formula(paste(birddata$nSize, "~", birddata$nName)), data=x)
  
  # Summarize ANOVA results
  anova_summary <- summary(anova_result)
  
  return(anova_summary)
}

compute_anova(x = birddata, nName, nSize)

