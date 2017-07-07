###########################
#  Project to Analyze Bulk Data from FFIEC
#  https://cdr.ffiec.gov/public/PWS/DownloadBulkData.aspx
#  Call Reports -- Balance Sheet, Income Statement, Past Due -- Four Periods
#  Author: Mark Locatelli
#  Date Created: 5/31/2017
###########################

# set the directory for the project
setwd("\\\\fhb/dfs/Center-Users/mlocatelli/R-Projects/SimilarityScoreBank")

# read in the files from the bulk download
FFIEC1 = read.delim("FFIEC CDR Call Subset of Schedules 2017(1 of 2).txt")
FFIEC2 = read.delim("FFIEC CDR Call Subset of Schedules 2017(2 of 2).txt")

# to get more details on the loan portfolios of the banks, let's grab RC-C Part I
# from the "Call Reports -- Single Period" selector on the FFIEC website.
FFIEC3 = read.delim("FFIEC CDR Call Schedule RCCI 03312017.txt")

# strip out the first row that contains descriptions of the line items
FFIEC1a = FFIEC1[-c(1), ]
FFIEC2a = FFIEC2[-c(1), ]
FFIEC3a = FFIEC3[-c(1), ]

# merge the files into a single data set
FFIECBulk1 = merge(FFIEC1a, FFIEC2a)
FFIECBulk2 = merge(FFIECBulk1, FFIEC3a)

# Reduce the list to a more interesting data set
BankData = FFIECBulk2[ , c("IDRSSD","Financial.Institution.Name",
                            "Financial.Institution.State","RCFD2170","RCON2170",
                            "RCON2122","RCON1766","RCON5367","RCON2200","RCON2948",
                            "RIAD4340","RCON1460","RCONF158","RCONF159","RCONF160",
                            "RCONF161")]

# Better names for the columns
names(BankData) = c("IDRSSD","Financial.Institution.Name",
                     "Financial.Institution.State","RCFD.Total.Assets",
                     "Total.Assets","Total.Loans","CI.Loans","Mortgages",
                     "Total.Deposits","Total.Liabilities","Net.Income",
                     "Multifamily","Res.Construction", "Other.Construction",
                     "Owner.Occ.CRE", "NonOwner.CRE")

# List of variables that need to be coverted to numeric type:
VariableList = c("RCFD.Total.Assets","Total.Assets","Total.Loans","CI.Loans",
                 "Mortgages","Total.Deposits","Total.Liabilities","Net.Income",
                 "Multifamily","Res.Construction", "Other.Construction",
                 "Owner.Occ.CRE", "NonOwner.CRE")
# Convert data to numeric
BankData[,VariableList] = lapply(BankData[,VariableList], 
                                  function(x) as.numeric(as.character(x)))

# Define a "CRE" Portfolio Total:
BankData[,"CRE"] = BankData[,"Multifamily"] + BankData[,"Res.Construction"] + 
  BankData[,"Other.Construction"] + BankData[,"Owner.Occ.CRE"] + 
  BankData[,"NonOwner.CRE"]

# Add some interesting Calculations
# Log of Total Assets
BankData[,"LNAssets"] = log(BankData[,"Total.Assets"])
# Normalized version of Total Assets
BankData[,"NormLNAssets"] = (BankData[,"LNAssets"] - mean(BankData[,"LNAssets"]))/
  sd(BankData[,"LNAssets"])
# Total Loans / Total Assets
LoansToAssets = BankData[,"Total.Loans"]/BankData[,"Total.Assets"]
BankData[,"NormLoanAssets"] = (LoansToAssets - mean(LoansToAssets))/
  sd(LoansToAssets)
# Setting Total.Loans = 0 to Total.Loans = 1 to use it in the denominator
LoanMin = 1
BankData[BankData[,"Total.Loans"] < LoanMin, "Total.Loans"] = LoanMin
# CI Loans / Total Loans
CIRatio = BankData[,"CI.Loans"]/BankData[,"Total.Loans"]
BankData[,"NormCIRatio"] = (CIRatio - mean(CIRatio))/sd(CIRatio)
# CRE Ratio
CRERatio = BankData[,"CRE"]/BankData[,"Total.Loans"]
BankData[,"NormCRERatio"] = (CRERatio - mean(CRERatio))/sd(CRERatio)
# Mortgages / Total Loans
MortRatio = BankData[,"Mortgages"]/BankData[,"Total.Loans"]
BankData[,"NormMortRatio"] = (MortRatio - mean(MortRatio))/sd(MortRatio)
# Total Deposits / Total Liabilities
DepositsToLiab = BankData[,"Total.Deposits"]/BankData[,"Total.Liabilities"]
BankData[,"NormDepositsToLiab"] = (DepositsToLiab - mean(DepositsToLiab))/
  sd(DepositsToLiab)
# Net Income / Total Assets
RoA = BankData[,"Net.Income"]/BankData[,"Total.Assets"]
BankData[,"NormRoA"] = (RoA - mean(RoA))/sd(RoA)

# A list of variables we will use to calcualte Similarity Scores
SimVarList = c("NormLNAssets","NormLoanAssets","NormCIRatio","NormCRERatio",
               "NormMortRatio","NormDepositsToLiab","NormRoA")
MostAverage = rep(0,length(SimVarList))
SimWeights = c(0.3,0.1,0.15,0.1,0.15,0.1,0.1)

# Let's make a Similarity Score Function!
SimScore = function(df,col_lst,simval_lst,wt_lst) {
  value = rep(0, nrow(df))
  for (row_i in seq(1,length(value))){
    # print(row_i)
    for (i in seq(1,length(col_lst))){
      # print(paste(wt_lst[i]," ",col_lst[i]," ",simval_lst[i]))
      value[row_i] = value[row_i]+wt_lst[i]*(df[row_i,col_lst[i]]-simval_lst[i])^2
      # print(value[row_i])
    }
  }
      
 return(sqrt(value)) 
  
}

BankData[,"SimScoreTrial"] = SimScore(BankData,SimVarList,MostAverage,SimWeights)


# Finding the most average bank:
N1 = 0
N2 = 0
N3 = 0
N4 = 0
N5 = 0
N6 = 0
N7 = 0
W1 = 0.3
W2 = 0.1
W3 = 0.15
W4 = 0.1
W5 = 0.15
W6 = 0.1
W7 = 0.1

BankData[,"SimScore0"] = sqrt(W1*(BankData[,"NormLNAssets"]-N1)^2
                 + W2*(BankData[,"NormLoanAssets"]-N2)^2
                 + W3*(BankData[,"NormCIRatio"]-N3)^2
                 + W4*(BankData[,"NormCRERatio"]-N4)^2
                 + W5*(BankData[,"NormMortRatio"]-N5)^2
                 + W6*(BankData[,"NormDepositsToLiab"]-N6)^2
                 + W7*(BankData[,"NormRoA"]-N7)^2)

BankData[BankData[,"SimScore0"]==min(BankData[,"SimScore0"]),
         "Financial.Institution.Name"]

# Finding the bank most similar to FHB (index 5816):
BankIndex = 5816
N1 = BankData[BankIndex,"NormLNAssets"]
N2 = BankData[BankIndex,"NormLoanAssets"]
N3 = BankData[BankIndex,"NormCIRatio"]
N4 = BankData[BankIndex,"NormCRERatio"]
N5 = BankData[BankIndex,"NormMortRatio"]
N6 = BankData[BankIndex,"NormDepositsToLiab"]
N7 = BankData[BankIndex,"NormRoA"]
W1 = 0.3
W2 = 0.1
W3 = 0.15
W4 = 0.1
W5 = 0.15
W6 = 0.1
W7 = 0.1

BankData[,"SimScoreBank"] = sqrt(W1*(BankData[,"NormLNAssets"]-N1)^2
                              + W2*(BankData[,"NormLoanAssets"]-N2)^2
                              + W3*(BankData[,"NormCIRatio"]-N3)^2
                              + W4*(BankData[,"NormCRERatio"]-N4)^2
                              + W5*(BankData[,"NormMortRatio"]-N5)^2
                              + W6*(BankData[,"NormDepositsToLiab"]-N6)^2
                              + W7*(BankData[,"NormRoA"]-N7)^2)

BankData[BankData[,"SimScoreBank"]==min(BankData[,"SimScoreBank"]),
         "Financial.Institution.Name"]

# Now let's repeat that search using the SimScore Function:
BankIndex = 5816
SimFHB = c(BankData[BankIndex,"NormLNAssets"],BankData[BankIndex,"NormLoanAssets"],
           BankData[BankIndex,"NormCIRatio"],BankData[BankIndex,"NormCRERatio"],
           BankData[BankIndex,"NormMortRatio"],
           BankData[BankIndex,"NormDepositsToLiab"],BankData[BankIndex,"NormRoA"])

BankData[,"SimScoreFHBTrial"] = SimScore(BankData,SimVarList,SimFHB,SimWeights)

# create some plots
library(ggplot2)
# plotting the banks on two of the similarity axes:
p = ggplot(BankData, aes(NormLNAssets, NormCIRatio))
p + geom_point()
# plotting the histogram of results for bank size
q = ggplot(BankData, aes(NormLNAssets))
q + geom_histogram(bins = 100)
# plotting the histogram for relative commercial portfolio size
r = ggplot(BankData, aes(NormCIRatio))
r + geom_histogram(bins=100)

# Some Cluster Analysis

# First let's try hierarchical clustering
BankDataCluster = BankData[,SimVarList]
BankDataDist = dist(BankDataCluster)
BankDataAverage = hclust(BankDataDist, method = 'average')
plot(BankDataAverage,main='Average Linkage',sub='',xlab='')
BankDataAvgClust = data.frame(cutree(BankDataAverage,k=8))
summary(BankDataAvgClust)
BankDataAvgClust

# Now let's try kmeans Clustering
n = nrow(BankDataCluster)
wss = rep(0,15)
wss[1] = (n-1) * sum(apply(BankDataCluster,2,var))
for(i in 2:15) wss[i] = sum(kmeans(BankDataCluster,centers=i)$withinss,nstart=20)
plot(1:15,wss,type = 'b',xlab = 'Number of Groups',
     ylab = 'Within Groups Sum of Squares')

# No obvious elbows in the data, how many banks in three clusters?
BankDataCluster_kmeans3 = kmeans(BankDataCluster, centers = 3,nstart=20)
table(BankDataCluster_kmeans3$cluster)
BankDataCluster_kmeans3$centers

# Now analyzing five clusters
BankDataCluster_kmeans5 = kmeans(BankDataCluster,centers = 5,nstart=20)
table(BankDataCluster_kmeans5$cluster)
BankDataCluster_kmeans5$centers

# Now analyzing seven clusters
BankDataCluster_kmeans7 = kmeans(BankDataCluster,centers = 7,nstart=20)
table(BankDataCluster_kmeans7$cluster)
BankDataCluster_kmeans7$centers

# Model based clustering
library('mclust')
BankDataMclust = Mclust(BankDataCluster)
print(BankDataMclust)
plot(BankDataMclust,BankDataCluster,what='BIC',col='black',ylab='-BIC')
clPairs(BankDataCluster,classification=BankDataMclust$classification,symbols=1:7,
        col=c('black','blue','red','green','yellow','orange','pink'))
# scatterplot3d(log(planets$mass),log(planets$period),
#               log(planets$eccen+ifelse(planets$eccen==0,0.001,0)),type='h',angle=55,
#               scale.y=0.7,pch=planet_mclust$classification,
#               y.ticklabs=seq(0,10,by=2),y.margin.add=0.1,xlab='log(mass)',
#               ylab='log(period)',zlab='log(eccen)')
table(BankDataMclust$classification)
summary(BankDataMclust, parameters = TRUE)

# Some Cluster analysis of the Bank data series
# BankCluster1 = kmeans(BankData[, c("NormLNAssets","NormLoanAssets","NormCIRatio",
#                                    "NormMortRatio","NormDepositsToLiab",
#                                    "NormRoA")],1,nstart = 20)
# BankCluster2 = kmeans(BankData[, c("NormLNAssets","NormLoanAssets","NormCIRatio",
#                                    "NormMortRatio","NormDepositsToLiab",
#                                    "NormRoA")],2,nstart = 20)
# BankCluster3 = kmeans(BankData[, c("NormLNAssets","NormLoanAssets","NormCIRatio",
#                                    "NormMortRatio","NormDepositsToLiab",
#                                    "NormRoA")],3,nstart = 20)
# BankCluster4 = kmeans(BankData[, c("NormLNAssets","NormLoanAssets","NormCIRatio",
#                                    "NormMortRatio","NormDepositsToLiab",
#                                    "NormRoA")],4,nstart = 20)
# BankCluster5 = kmeans(BankData[, c("NormLNAssets","NormLoanAssets","NormCIRatio",
#                                    "NormMortRatio","NormDepositsToLiab",
#                                    "NormRoA")],5,nstart = 20)
# BankCluster6 = kmeans(BankData[, c("NormLNAssets","NormLoanAssets","NormCIRatio",
#                                    "NormMortRatio","NormDepositsToLiab",
#                                    "NormRoA")],6,nstart = 20)
# BankCluster7 = kmeans(BankData[, c("NormLNAssets","NormLoanAssets","NormCIRatio",
#                                    "NormMortRatio","NormDepositsToLiab",
#                                    "NormRoA")],7,nstart = 20)
# BankCluster8 = kmeans(BankData[, c("NormLNAssets","NormLoanAssets","NormCIRatio",
#                                    "NormMortRatio","NormDepositsToLiab",
#                                    "NormRoA")],8,nstart = 20)
# BankCluster9 = kmeans(BankData[, c("NormLNAssets","NormLoanAssets","NormCIRatio",
#                                    "NormMortRatio","NormDepositsToLiab",
#                                    "NormRoA")],9,nstart = 20)
# BankCluster10 = kmeans(BankData[, c("NormLNAssets","NormLoanAssets","NormCIRatio",
#                                    "NormMortRatio","NormDepositsToLiab",
#                                    "NormRoA")],10,nstart = 20)

# Plotting the results of the ten clusters
# BankCluster=c(BankCluster1$tot.withinss,BankCluster2$tot.withinss,
#               BankCluster3$tot.withinss,BankCluster4$tot.withinss,
#               BankCluster5$tot.withinss,BankCluster6$tot.withinss,
#               BankCluster7$tot.withinss,BankCluster8$tot.withinss,
#               BankCluster9$tot.withinss,BankCluster10$tot.withinss)
# plot(BankCluster)

# Plotting one of the distributions of clusters
# s = ggplot(BankData,aes(NormLNAssets,NormLoanAssets,color=BankCluster10$cluster))
# s + geom_point()

# Adding the Cluster data back to the data set:
# BankData[,"BankCluster2"] = BankCluster2$cluster
# BankData[,"BankCluster3"] = BankCluster3$cluster
# BankData[,"BankCluster4"] = BankCluster4$cluster
# BankData[,"BankCluster5"] = BankCluster5$cluster
# BankData[,"BankCluster6"] = BankCluster6$cluster
# BankData[,"BankCluster7"] = BankCluster7$cluster
# BankData[,"BankCluster8"] = BankCluster8$cluster
# BankData[,"BankCluster9"] = BankCluster9$cluster
# BankData[,"BankCluster10"] = BankCluster10$cluster

# Let's output our results:
write.csv(BankData, file = "BankData.csv")
