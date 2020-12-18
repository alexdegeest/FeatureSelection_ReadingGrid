
P3_KStability <- function(DatasetName,Criterion)
{
#####################################################################
# Test a criterion on a dataset DatasetName for property3 "k stability"
#
# Inputs:
#   - DatasetName
#	- Criterion
#
# Outputs:
#   - figure of the results
#   
# Author: A. Degeest, 2020 (alexandra.degeest@gmail.com)
#####################################################################

	if (DatasetName == "anthrokidsV2.dat")
	{
		dataset <- read.csv(file="anthrokidsV2.dat",head=FALSE,sep=" ") 
		dataset_X = matrix(c(dataset[,1],dataset[,11],dataset[,19]), ncol=3)
		dataset_Y = matrix(c(dataset[,21]),ncol=1) 
	}
	if (DatasetName == "poland_30.mat")
	{
		dataset <- readMat(con="poland_30.mat",head=FALSE) 
		dataset_X1 <- dataset$x
		#dataset_X = matrix(c(dataset_X1[,1],dataset_X1[,23],exp(dataset_X1[,24])), ncol=3)
		dataset_X = matrix(c(dataset_X1[,1],dataset_X1[,23],((exp(dataset_X1[,24]))-1.58)/1.5), ncol=3)
		dataset_Y = dataset$y
	}
	if (DatasetName == "santafe.mat")
	{
		dataset <- readMat(con="santafe.mat",head=FALSE)
		dataset_X1 <- dataset$x
		dataset_X = matrix(c(dataset_X1[,1],dataset_X1[,4],16*sqrt(dataset_X1[,5])), ncol=3)
		dataset_Y = dataset$y
	}
	

	Size <- length(dataset_Y)
	Nbr_Features <- ncol(dataset_X)

	if (DatasetName == "anthrokidsV2.dat")
	{
		NewY1 <-matrix(0,Size,1)
		NewY1 <- dataset_Y

		dataset1 <- matrix(0,nrow = Size, ncol = 2)
		dataset1[,1] <- dataset_X[,1]
		dataset1[,2] <- NewY1

		dataset2 <- matrix(0,nrow = Size, ncol = 2)
		dataset2[,1] <- dataset_X[,2]
		dataset2[,2] <- NewY1

		dataset3 <- matrix(0,nrow = Size, ncol = 2)
		dataset3[,1] <- dataset_X[,3]
		dataset3[,2] <- NewY1
	}
	if (DatasetName == "poland_30.mat")
	{
		NewY1 <-matrix(0,Size,1)
		NewY1 <- dataset_Y

		dataset1 <- matrix(0,nrow = Size, ncol = 2)
		dataset1[,1] <- dataset_X[,1]
		dataset1[,2] <- NewY1

		dataset2 <- matrix(0,nrow = Size, ncol = 2)
		dataset2[,1] <- dataset_X[,2]
		dataset2[,2] <- NewY1

		dataset3 <- matrix(0,nrow = Size, ncol = 2)
		dataset3[,1] <- dataset_X[,3]
		dataset3[,2] <- NewY1
	}
	if (DatasetName == "santafe.mat")
	{
		Extract_Prop = 0.4
		Extract_Size = round(Size*Extract_Prop)
		dataset1 <- matrix(0,Size,2)
		dataset1_extract <- matrix(0,Extract_Size,2)
		dataset1[,1] <- dataset_X[,1]
		dataset1[,2] <- dataset_Y
		dataset1_extract <- dataset1[sample(1:nrow(dataset1),Extract_Size,replace=FALSE),]
		dataset2 <- matrix(0,Size,2)
		dataset2_extract <- matrix(0,Extract_Size,2)
		dataset2[,1] <- dataset_X[,2]
		dataset2[,2] <- dataset_Y
		dataset2_extract <- dataset2[sample(1:nrow(dataset2),Extract_Size,replace=FALSE),]
		dataset3 <- matrix(0,Size,2)
		dataset3_extract <- matrix(0,Extract_Size,2)
		dataset3[,1] <- dataset_X[,3]
		dataset3[,2] <- dataset_Y
		dataset3_extract <- dataset3[sample(1:nrow(dataset3),Extract_Size,replace=FALSE),]
		dataset_X <- matrix(0,Extract_Size,3)
		dataset_X[,1] <- dataset1_extract[,1]
		dataset_X[,2] <- dataset2_extract[,1]
		dataset_X[,3] <- dataset3_extract[,1]
		dataset1_Y <- dataset1_extract[,2]
		dataset2_Y <- dataset2_extract[,2]
		dataset3_Y <- dataset3_extract[,2]
		Size <- Extract_Size

		dataset1 <- matrix(0,nrow = Size, ncol = 2)
		dataset1[,1] <- dataset_X[,1]
		dataset1[,2] <- dataset1_Y

		dataset2 <- matrix(0,nrow = Size, ncol = 2)
		dataset2[,1] <- dataset_X[,2]
		dataset2[,2] <- dataset2_Y

		dataset3 <- matrix(0,nrow = Size, ncol = 2)
		dataset3[,1] <- dataset_X[,3]
		dataset3[,2] <- dataset3_Y
	}

	K_min = 4
	K_max = Size
	if (Criterion == "MI") 
	{
		j = 0
		Step_K = 40
	}
	
	if (Criterion == "NLR2") 
	{
		f = 0
		Step_K = 100
	}
	MatrixSize = (K_max/Step_K)-(K_min/Step_K)+1


	SmallXNoise <- rnorm(Size,mean = 0,sd = 0.05)
	dataset1_outliers <- dataset1
	dataset2_outliers <- dataset2
	dataset3_outliers <- dataset3
	dataset1_outliers[,1] <- dataset1[,1] + SmallXNoise
	dataset2_outliers[,1] <- dataset2[,1] + SmallXNoise
	dataset3_outliers[,1] <- dataset3[,1] + SmallXNoise

	if (Criterion == "MI") 
	{
		resultatMI1 <- matrix(0,MatrixSize,2)
		resultatMI2 <- matrix(0,MatrixSize,2)
		resultatMI3 <- matrix(0,MatrixSize,2)
		
		for (K in seq(K_min,K_max,by=Step_K))
		{	
			j = j + 1
			
			X1 <- dataset1_outliers[1:Size,1]
			Y1 <- dataset1_outliers[1:Size,2]
			NewX1 <- matrix(0,Size,1)
			NewX1[,1] <- X1
			MI_test1 <- MIxnyn4(matrix(NewX1,ncol=1), matrix(Y1,ncol=1),K)
			resultatMI1[j,1]<-K
			resultatMI1[j,2]<- MI_test1

			X2 <- dataset2_outliers[1:Size,1]
			Y2 <- dataset2_outliers[1:Size,2]
			NewX2 <- matrix(0,Size,1)
			NewX2[,1] <- X2
			MI_test2 <- MIxnyn4(matrix(NewX2,ncol=1), matrix(Y2,ncol=1),K)
			resultatMI2[j,1]<- K
			resultatMI2[j,2]<- MI_test2

			X3 <- dataset3_outliers[1:Size,1]
			Y3 <- dataset3_outliers[1:Size,2]
			NewX3 <- matrix(0,Size,1)
			NewX3[,1] <- X3
			MI_test3 <- MIxnyn4(matrix(NewX3,ncol=1), matrix(Y3,ncol=1),K)
			resultatMI3[j,1]<- K
			resultatMI3[j,2]<- MI_test3

		}
	}

	if (Criterion == "NLR2")
	{
		Best_Radj_N <- matrix(0,MatrixSize,Nbr_Features+1)
		SelectK_adj <-matrix(0,Nbr_Features,MatrixSize)
			
		dataset_X <- matrix(0,Size,Nbr_Features)
		dataset_X[,1]<-dataset1_outliers[,1]
		dataset_X[,2]<-dataset2_outliers[,1]
		dataset_X[,3]<-dataset3_outliers[,1]

		dataset_Y <- matrix(0,Size,Nbr_Features)
		dataset_Y[,1]<-dataset1_outliers[,2]
		dataset_Y[,2]<-dataset2_outliers[,2]
		dataset_Y[,3]<-dataset3_outliers[,2]
		
		##### Calculs des valeurs de NLR² pour features sélectionnés selon un certain K	
		for (K in seq(K_min,K_max,by=Step_K))
		{	
			f=f+1

			##### Initialisation des tableaux de variables
			NewY1 <-matrix(0,Size,Nbr_Features)
			NewY1 <- dataset_Y
			R_Carre_adj  <- matrix(0,Size,Nbr_Features)
			Mean_R_Carre_adj <- matrix(0,1,Nbr_Features)

			#####Boucle sur le nombre de features
			for (k in seq(1,Nbr_Features,by=1))
			{
				NewX1 <-matrix(0,Size,1)
				NewX1[,1] <- dataset_X[,k]

				knn1<-get.knn(NewX1, k= K, algorithm=c("kd_tree")) 	#Trouve les K voisins les plus proches en X
				NN1 <- knn1$nn.index 								#Donne l'index des K points les plus proches 
				NN1_Y <- matrix(0,Size,K)
				NN1_X <- matrix(0,Size,K)
								
				for (i in seq(1,K,by = 1))
				{
					NN1_Y[,i]<-NewY1[NN1[,i],k]
					NN1_X[,i]<-NewX1[NN1[,i],1]
				}	

				######Pour chaque point du dataset
				for (j in seq(1,Size,by = 1))
				{
					results1 <- lm(NN1_Y[j,]~NN1_X[j,])
					R_Carre_adj[j,k] <- summary(results1)$adj.r.squared
				}
				Mean_R_Carre_adj[1,k]=mean(R_Carre_adj[,k])
			}
			
			Best_Radj_N[f,1]<- K	
			Best_Radj_N[f,2]<-Mean_R_Carre_adj[,1]
			Best_Radj_N[f,3]<-Mean_R_Carre_adj[,2]
			Best_Radj_N[f,4]<-Mean_R_Carre_adj[,3]
		}


	}

	if (DatasetName == "anthrokidsV2.dat")
	{
		if (Criterion == "MI")
		{
			par(mfrow=c(1,1))
			plot(resultatMI1[,1],resultatMI1[,2],xlim=c(K_min,K_max), ylim=c(0,1.5),type='l',lty=1,lwd=2,cex.axis=2,log='x')
			lines(resultatMI2[,1],resultatMI2[,2],pch=10, lty=4,lwd=2)
			lines(resultatMI3[,1],resultatMI3[,2],pch=10, lty=3,lwd=2)
		}

		if (Criterion == "NLR2") 
		{
			par(mfrow=c(1,1))
			plot(Best_Radj_N[,1],Best_Radj_N[,2], ylim=c(-0.02,1),type='l',lty=1,lwd=2,cex.axis=2)
			lines(Best_Radj_N[,1],Best_Radj_N[,3],pch=10, lty=4,lwd=2)
			lines(Best_Radj_N[,1],Best_Radj_N[,4],pch=10, lty=3,lwd=2)
		}
		
		if (Criterion == "DT")
		{
			print("No Need for DT : k is set to 1 by definition")
		}
	}

	if (DatasetName == "poland_30.mat")
	{
		if (Criterion == "MI") 
		{
			par(mfrow=c(1,1))
			plot(resultatMI1[,1],resultatMI1[,2],xlim=c(K_min,K_max), ylim=c(0,1.1),type='l',lty=1,lwd=2,cex.axis=2,log='x')
			lines(resultatMI2[,1],resultatMI2[,2],pch=10, lty=4,lwd=2)
			lines(resultatMI3[,1],resultatMI3[,2],pch=10, lty=3,lwd=2)
		}

		if (Criterion == "NLR2")
		{
			par(mfrow=c(1,1))
			plot(Best_Radj_N[,1],Best_Radj_N[,2], ylim=c(-0.02,0.8),type='l',lty=1,lwd=2,cex.axis=2)
			lines(Best_Radj_N[,1],Best_Radj_N[,3],pch=10, lty=4,lwd=2)
			lines(Best_Radj_N[,1],Best_Radj_N[,4],pch=10, lty=3,lwd=2)
		}
		
		if (Criterion == "DT")
		{
			print("No Need for DT : k is set to 1 by definition")
		}
	}

	if (DatasetName == "santafe.mat")
	{
		if (Criterion == "MI") 
		{
			par(mfrow=c(1,1))
			plot(resultatMI1[,1],resultatMI1[,2],xlim=c(K_min,K_max), ylim=c(0,1.2),type='l',lty=1,lwd=2,cex.axis=2,log='x')
			lines(resultatMI2[,1],resultatMI2[,2],pch=10, lty=4,lwd=2)
			lines(resultatMI3[,1],resultatMI3[,2],pch=10, lty=3,lwd=2)
		}

		if (Criterion == "NLR2")
		{
			par(mfrow=c(1,1))
			plot(Best_Radj_N[,1],Best_Radj_N[,2],ylim=c(-0.005,0.65),type='l',lty=1,lwd=2,cex.axis=2)
			lines(Best_Radj_N[,1],Best_Radj_N[,3],pch=10, lty=4,lwd=2)
			lines(Best_Radj_N[,1],Best_Radj_N[,4],pch=10, lty=3,lwd=2)
		}
		
		if (Criterion == "DT")
		{
			print("No Need for DT : k is set to 1 by definition")
		}
	}
}
