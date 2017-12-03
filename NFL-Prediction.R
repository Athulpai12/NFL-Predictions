install.packages("ggpubr")
install.packages("neuralnet")
library(neuralnet)
library(ggpubr)
library(rvest)
library(stringr)
library(MASS)
library(caret)
#the threshold level for finding the team that wins
epsilon = 10
#threshold for find the covarience
threshold = 0.02
#The data set used for model A that predicts win and loss
nfl = read.csv("SportsData_NFL_2014_REG_PST_teams.csv")
#The data set that is used for building model B used for predicting the rating of each team
raw = read.csv("SportsData_NFL_2014_REG_PST_players.csv")
#omiting the unknown from the data set
nfl = na.omit(nfl)
#The results of the test data set
nfl.teams = nfl[133:232,]$Team

#converting the data set into a datamatrix to convert words into numbers
nfl=data.frame(data.matrix(nfl))
#find all the column names of the dataset
x =c(colnames(nfl))
#a list of the attriubutes that needs to be removed
removable.attribute = c()
# checking how many have co-relation below the threshold defined
for(a in x)
{
  if(var(nfl[a])>0)
  {
    k = (cor(nfl[a] , nfl["Win...Loss"] ))
    if(k==0||(k>0&&k<=threshold)||(k<0&&k>=(threshold*-1)))
      removable.attribute = c(removable.attribute,a);
  }
  else
    removable.attribute = c(removable.attribute,a)
}
#removing the attributes that do not hold much information
xc = nfl[ , !(names(nfl) %in% removable.attribute)]
#creating a training set
trainset <- xc[1:132, ]
#creating a test set
removable.attribute
testset <- xc[133:232, ]
# the win and loss of test data set
WinLoss = testset$Win...Loss
testset$Win...Loss=NULL;
n <- names(trainset)
#geting the features in the format required for neural net
f <- as.formula(paste("Win...Loss ~", paste(n[!n %in% "Win...Loss"],
                                            collapse = " + ")))
#creating model A with a neural net
nn = neuralnet(f,data=trainset,hidden=c(6))
WinLoss[WinLoss==2] = 1
#plot of the model
plot(nn)

#predicting the results of model A
pr.nn <- compute(nn,testset)
#the neural net results
pr.nn  = pr.nn$net.result
#binding the results with the team
team.predictann = cbind.data.frame(pr.nn,nfl.teams)
#the final result with out  refining model A
pr.nn[pr.nn>=2 ] =3
pr.nn[pr.nn<=2] = 1
plot(pr.nn)
print("Prediction of Model A Win and loss before refinement")
table(pr.nn,WinLoss)
#finding the accuracy from the confusion matrix
confusionMatrix(pr.nn, WinLoss)

#building model B
raw = raw[which(raw$Game.ID<257),]
raw = raw[ ,colSums(is.na(raw))==0]

data = raw[order(raw$Player.Name,raw$Week,raw$Team,raw$Position),]
data = data[apply(data[,11:24], 1, function(x) !all(x==0)),]

relevant_players = c(as.character(data[data[,"Week"]==17,"Player.Name"]))

index.rem = c()

for(i in 1:nrow(data)){
  if(!(as.character(data[i,"Player.Name"]) %in% relevant_players)){
    index.rem = c(index.rem,i)
  }
}
data = data[-index.rem,]
data = data[order(data$Team,data$Week,data$Position),]
write.csv(data,file="Ordered_Player_Data.csv")
data.m = data.matrix(data)
data.m[,c(11:ncol(data.m))] = scale(data.m[,c(11:ncol(data.m))])

total.matrix = c()
total.rating = c()
weights = c(0.75,0.8,0.85,0.65,0.7,0.75,0.7,0.9,-0.8,0.6,0.6,0.7,0.8,-0.85)
for(i in 1:32){
  for(j in 1:16){
    if(identical(data.m[which(data.m[,"Week"]==j & data.m[,"Team"]==i),"Week"],numeric(0))){
      j=j+1
    }
    total = c(colSums(data.m[which(data.m[,"Week"]==j & data.m[,"Team"]==i),c(11:ncol(data.m))])*100/ncol(data.m[which(data.m[,"Week"]==j & data.m[,"Team"]==i),c(11:ncol(data.m))]))
    total.matrix=c(total.matrix,list(total))
    total.rating=c(total.rating,1000+unlist(total[1])*weights[1]+unlist(total[2])*weights[2]+unlist(total[3])*weights[3]+unlist(total[4])*weights[4]+unlist(total[5])*weights[5]+unlist(total[6])*weights[6]+unlist(total[7])*weights[7]+unlist(total[8])*weights[8]+unlist(total[9])*weights[9]+unlist(total[10])*weights[10]+unlist(total[11])*weights[11]+unlist(total[12])*weights[12]+unlist(total[13])*weights[13]+unlist(total[14])*weights[14]);
  }
}
total.rating = matrix(total.rating,nrow=32,ncol=16)
# Index a matrix by (Team index-1)*(Total_weeks) + Week number
# Team indices 1 - 49ers ,2 - Bears ,3 - Bengals ,4 - Bills ,5 - Broncos ,6 - Browns ,7 - Buccaneers ,8 - Cardinals ,9 - Chargers ,10 - Chiefs ,
# 11 - Colts ,12 - Cowboys ,13 - Dolphins ,14 - Eagles ,15 - Falcons ,16 - Giants ,17 - Jaguars ,18 - Jets ,19 - Lions ,20 - Packers ,
# 21 - Panthers ,22 - Patriots ,23 - Raiders ,24 - Rams ,25 - Ravens ,26 - Redskins ,27 - Saints ,28 - Seahawks ,29 - Steelers ,30 - Texans ,31 - Titans ,32 - Vikings.
teams =c("SF" , "CHI" , "CIN","BUF","DEN","CLE","TB","ARI","SD","KC","IND","DAL","MIA","PHI","ATL","NYG","JAC","NYJ","DET","GB","CAR","NE","OAK","STL","BAL","WAS","NO","SEA","PIT","HOU","TEN","MIN")
predicted.rating=c()
for(i in 1:32){
  plot(total.rating[i,],type = "o", col = "red", xlab = "Team", ylab = "Rating",main = "Ratings")
  predicted.rating=c(predicted.rating,(15*total.rating[i,15]+14*total.rating[i,14]+13*total.rating[i,13]+12*total.rating[i,12]+11*total.rating[i,11]+10*total.rating[i,10]+9*total.rating[i,9]+8*total.rating[i,8]+7*total.rating[i,7]+6*total.rating[i,6]+5*total.rating[i,5]+4*total.rating[i,4]+3*total.rating[i,3]+2*total.rating[i,2]+1*total.rating[i,1])/120)
}

# Refining Model A with the help of model B

#creating a new environment for mapping the rating to the teams
#a new evironment is implemnted as a hashmap
e = new.env()
#mapping teams to rating
for(i in seq_along(teams))
{
  e$teams[i] = predicted.rating[i];
}
#getring the vector of teams in the test data
nfl.teamsc = as.character(nfl.teams)
j =1;
#refining model A with the help of model B
while(j<nrow(team.predictann))
{
  #for each team in the test dataset we are getting the predicted rating 
  #for each team we are measuring how strong the opponent is
  #if the team and the opponent are of equal rating then we go with the initial predicted value of win and loss
  #if they are not of equal rating then we try to give an edge to the stronger team
  r1.val =  nfl.teamsc[j];
  r2.val =  nfl.teamsc[j+1];
  r1=0
  r2=0
  for(k in seq_along(teams))
  {
    if(r1.val==teams[k])
    {
      r1 = e$teams[k];
    }
    if(r2.val==teams[k])
    {
      r2 = e$teams[k];
      
    }
  }
  if((r1>r2)&&(r1-r2<epsilon))
  {
    if(team.predictann$pr.nn[j]>=2)
    {
      team.predictann$pr.nn[j] = 3
      team.predictann$pr.nn[j+1] = 1
    }
    else
    {
      team.predictann$pr.nn[j] = 1
      team.predictann$pr.nn[j+1] = 3
    }
  }
  else if((r2>r1)&&(r2-r1<epsilon))
  {
    
    if(team.predictann$pr.nn[j]>=2)
    {
      team.predictann$pr.nn[j] = 1
      team.predictann$pr.nn[j+1] = 3
    }
    else
    {
      team.predictann$pr.nn[j] = 3
      team.predictann$pr.nn[j+1] = 1
    }
  }
  else if((r1>r2)&&(r1-r2>epsilon))
  {
    team.predictann$pr.nn[j] = 3
    team.predictann$pr.nn[j+1] = 1
  }
  else
  {
    team.predictann$pr.nn[j] = 1
    team.predictann$pr.nn[j+1] = 3
  }
  j=j+2;
}
print("Prediction of Model A Win and loss after refinement")
table(team.predictann$pr.nn,WinLoss)
#the accuracy form the confusion matrix after refining model A
confusionMatrix(team.predictann$pr.nn,WinLoss )
plot(predicted.rating)

