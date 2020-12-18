
P6_XNoiseRobustness_1levelSTD <- function(DatasetName,Criterion)
{
#############################################################################
# Test a criterion on a dataset DatasetName for property 6 "X Noise Robustness"
#
# Inputs:
#   - DatasetName
#	- Criterion
#
# Outputs:
#   - figure of the results
#   
# Author: A. Degeest, 2020 (alexandra.degeest@gmail.com)
#############################################################################
	if (DatasetName == "anthrokidsV2.dat")
	{
		dataset <- read.csv(file="anthrokidsV2.dat",head=FALSE,sep=" ") 
		dataset_X = matrix(c(dataset[,1],dataset[,11],dataset[,19]), ncol=3)
		dataset_Y = matrix(c(dataset[,21]),ncol=1) 
		Size <- length(dataset_Y)
		Nbr_Features <- ncol(dataset_X)
	}
	
	if (DatasetName == "poland_30.mat")
	{
		dataset <- readMat(con="poland_30.mat",head=FALSE) 
		dataset_X1 <- dataset$x
		#dataset_X = matrix(c(dataset_X1[,1],dataset_X1[,23],exp(dataset_X1[,24])), ncol=3)
		dataset_X = matrix(c(dataset_X1[,1],dataset_X1[,23],((exp(dataset_X1[,24]))-1.58)/1.5), ncol=3)
		dataset_Y = dataset$y
		Size <- length(dataset_Y)
		Nbr_Features <- ncol(dataset_X)
	}

	if (DatasetName == "santafe.mat")
	{
		dataset <- readMat(con="santafe.mat",head=FALSE)#,sep=",") 
		dataset_X1 <- dataset$x
		dataset_X = matrix(c(dataset_X1[,1],dataset_X1[,4],16*sqrt(dataset_X1[,5])), ncol=3)
		dataset_Y = dataset$y
		Size <- length(dataset_Y)
		Nbr_Features <- ncol(dataset_X)
	}
	
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
	
	if (DatasetName == "santafe.mat")
	{
		if (Criterion == "NLR2") 
		{
			Extract_Prop = 0.2
			Extract_Size = round(Size*Extract_Prop)
			dataset1 <- matrix(0,Size,2)
			dataset1_extract <- matrix(0,Extract_Size,2)
			dataset1[,1] <- dataset_X_init[,1]
			dataset1[,2] <- dataset_Y_init
			dataset1_extract <- dataset1[sample(1:nrow(dataset1),Extract_Size,replace=FALSE),]
			dataset2 <- matrix(0,Size,2)
			dataset2_extract <- matrix(0,Extract_Size,2)
			dataset2[,1] <- dataset_X_init[,2]
			dataset2[,2] <- dataset_Y_init
			dataset2_extract <- dataset2[sample(1:nrow(dataset2),Extract_Size,replace=FALSE),]
			dataset3 <- matrix(0,Size,2)
			dataset3_extract <- matrix(0,Extract_Size,2)
			dataset3[,1] <- dataset_X_init[,3]
			dataset3[,2] <- dataset_Y_init
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
	}	
		
	outX_MAX = 0.4
	outX_Min = 0
	StepSize = 0.05
	MatrixSize = (outX_MAX/StepSize)-(outX_Min/StepSize)+1
	
	Loop = 10

	if (Criterion == "MI") 
	{	
		K = 6		
		resultatMI1unif_Stock<- matrix(0,MatrixSize,Loop)
		resultatMI2unif_Stock<- matrix(0,MatrixSize,Loop)
		resultatMI3unif_Stock<- matrix(0,MatrixSize,Loop)
		for (rank in seq(1,Loop, by = 1)) #rank = loop number
		{ 
			resultatMI1 <- matrix(0,MatrixSize,2)
			resultatMI2 <- matrix(0,MatrixSize,2)
			resultatMI3 <- matrix(0,MatrixSize,2)

			j=0
			SmallXNoise <- rnorm(Size,mean = 0,sd = 0.05)

			for (outX in seq(outX_Min,outX_MAX,by=StepSize))
			{
				j = j + 1
				
				if (outX > 0)
				{
					###
					outlier_prop = outX							    #Proportion d'outliers
					outlier_nbr = round(Size*outlier_prop)			#Nombre total d'outliers
					outlier_pos = sample(seq(1,Size,1),outlier_nbr)	#Position des outliers au hasard dans le dataset
						
					outlying_X = runif(outlier_nbr,min = -1,max = 1)	#Importance de l'erreur d'outlier en Y
						
					dataset1_outliers <- matrix(0,nrow = Size, ncol = 2)	#Dataset 1 corrompu
					dataset2_outliers <- matrix(0,nrow = Size, ncol = 2)	#Dataset 2 corrompu
					dataset3_outliers <- matrix(0,nrow = Size, ncol = 2)	#Dataset 3 corrompu
						
					dataset1_outliers <- dataset1
					dataset2_outliers <- dataset2
					dataset3_outliers <- dataset3

					for (i in seq(1,outlier_nbr,by=1))
					{
						dataset1_outliers[outlier_pos[i],1]= dataset1_outliers[outlier_pos[i],1] + outlying_X[i]		
						dataset2_outliers[outlier_pos[i],1]= dataset2_outliers[outlier_pos[i],1] + outlying_X[i]		
						dataset3_outliers[outlier_pos[i],1]= dataset3_outliers[outlier_pos[i],1] + outlying_X[i]
					}
						
				}
				if (outX ==0) #on conserve quand même au minimum un léger bruit gaussien
				{
					dataset1_outliers <- dataset1
					dataset2_outliers <- dataset2
					dataset3_outliers <- dataset3
					dataset1_outliers[,1] <- dataset1[,1] + SmallXNoise
					dataset2_outliers[,1] <- dataset2[,1] + SmallXNoise
					dataset3_outliers[,1] <- dataset3[,1] + SmallXNoise
				}
				
				X1 <- dataset1_outliers[1:Size,1]
				Y1 <- dataset1_outliers[1:Size,2]
				NewX1 <- matrix(0,Size,1)
				NewX1[,1] <- X1
				MI_test1 <- MIxnyn4(matrix(NewX1,ncol=1), matrix(Y1,ncol=1),K)
				resultatMI1[j,1]<-outX
				resultatMI1[j,2]<- MI_test1

				X2 <- dataset2_outliers[1:Size,1]
				Y2 <- dataset2_outliers[1:Size,2]
				NewX2 <- matrix(0,Size,1)
				NewX2[,1] <- X2
				MI_test2 <- MIxnyn4(matrix(NewX2,ncol=1), matrix(Y2,ncol=1),K)
				resultatMI2[j,1]<- outX
				resultatMI2[j,2]<- MI_test2

				X3 <- dataset3_outliers[1:Size,1]
				Y3 <- dataset3_outliers[1:Size,2]
				NewX3 <- matrix(0,Size,1)
				NewX3[,1] <- X3
				MI_test3 <- MIxnyn4(matrix(NewX3,ncol=1), matrix(Y3,ncol=1),K)
				resultatMI3[j,1]<- outX
				resultatMI3[j,2]<- MI_test3
			}
			resultatMI1unif_Stock[,rank] <-resultatMI1[,2]
			resultatMI2unif_Stock[,rank] <-resultatMI2[,2]
			resultatMI3unif_Stock[,rank] <-resultatMI3[,2]	
		}

		resultatMI1unif_Avg <- matrix(0,MatrixSize,2)
		resultatMI2unif_Avg <- matrix(0,MatrixSize,2)
		resultatMI3unif_Avg <- matrix(0,MatrixSize,2)

		resultatMI1unif_SD <- matrix(0,MatrixSize,2)
		resultatMI2unif_SD <- matrix(0,MatrixSize,2)
		resultatMI3unif_SD <- matrix(0,MatrixSize,2)

		resultatMI1unif_Avg[,1] <- resultatMI1[,1]
		resultatMI2unif_Avg[,1] <- resultatMI2[,1]
		resultatMI3unif_Avg[,1] <- resultatMI3[,1]
		resultatMI1unif_SD[,1] <- resultatMI1[,1]
		resultatMI2unif_SD[,1] <- resultatMI2[,1]
		resultatMI3unif_SD[,1] <- resultatMI3[,1]

		for (i in seq(1,MatrixSize,by=1))
		{
			resultatMI1unif_Avg[i,2]<-mean(resultatMI1unif_Stock[i,])
			resultatMI2unif_Avg[i,2]<-mean(resultatMI2unif_Stock[i,])
			resultatMI3unif_Avg[i,2]<-mean(resultatMI3unif_Stock[i,])

			resultatMI1unif_SD[i,2]<-sd(resultatMI1unif_Stock[i,])
			resultatMI2unif_SD[i,2]<-sd(resultatMI2unif_Stock[i,])
			resultatMI3unif_SD[i,2]<-sd(resultatMI3unif_Stock[i,])
		}
		
		if (DatasetName == "poland_30.mat")
		{
			plot(resultatMI1unif_Avg[,1],resultatMI1unif_Avg[,2], ylim=c(0,1.45),type='l',lty=1,lwd=2,cex.axis=2)
			lines(resultatMI2unif_Avg[,1],resultatMI2unif_Avg[,2],pch=10, lty=4,lwd=2)
			lines(resultatMI3unif_Avg[,1],resultatMI3unif_Avg[,2],pch=10, lty=3,lwd=2)

			lines(resultatMI1unif_SD[,1],resultatMI1unif_Avg[,2]+0.96*resultatMI1unif_SD[,2], type='l',lty=1,lwd=2,col='gray')
			lines(resultatMI2unif_SD[,1],resultatMI2unif_Avg[,2]+0.96*resultatMI2unif_SD[,2],pch=10, lty=4,lwd=2,col='gray')
			lines(resultatMI3unif_SD[,1],resultatMI3unif_Avg[,2]+0.96*resultatMI3unif_SD[,2],pch=10, lty=3,lwd=2,col='gray')

			lines(resultatMI1unif_SD[,1],resultatMI1unif_Avg[,2]-0.96*resultatMI1unif_SD[,2], type='l',lty=1,lwd=2,col='gray')
			lines(resultatMI2unif_SD[,1],resultatMI2unif_Avg[,2]-0.96*resultatMI2unif_SD[,2],pch=10, lty=4,lwd=2,col='gray')
			lines(resultatMI3unif_SD[,1],resultatMI3unif_Avg[,2]-0.96*resultatMI3unif_SD[,2],pch=10, lty=3,lwd=2,col='gray')
		}
		
		if (DatasetName == "anthrokidsV2.dat")
		{
			plot(resultatMI1unif_Avg[,1],resultatMI1unif_Avg[,2], ylim=c(0,1.45),type='l',lty=1,lwd=2,cex.axis=2)
			lines(resultatMI2unif_Avg[,1],resultatMI2unif_Avg[,2],pch=10, lty=4,lwd=2)
			lines(resultatMI3unif_Avg[,1],resultatMI3unif_Avg[,2],pch=10, lty=3,lwd=2)

			lines(resultatMI1unif_SD[,1],resultatMI1unif_Avg[,2]+0.96*resultatMI1unif_SD[,2], type='l',lty=1,lwd=2,col='gray')
			lines(resultatMI2unif_SD[,1],resultatMI2unif_Avg[,2]+0.96*resultatMI2unif_SD[,2],pch=10, lty=4,lwd=2,col='gray')
			lines(resultatMI3unif_SD[,1],resultatMI3unif_Avg[,2]+0.96*resultatMI3unif_SD[,2],pch=10, lty=3,lwd=2,col='gray')

			lines(resultatMI1unif_SD[,1],resultatMI1unif_Avg[,2]-0.96*resultatMI1unif_SD[,2], type='l',lty=1,lwd=2,col='gray')
			lines(resultatMI2unif_SD[,1],resultatMI2unif_Avg[,2]-0.96*resultatMI2unif_SD[,2],pch=10, lty=4,lwd=2,col='gray')
			lines(resultatMI3unif_SD[,1],resultatMI3unif_Avg[,2]-0.96*resultatMI3unif_SD[,2],pch=10, lty=3,lwd=2,col='gray')
		
		}
		
		if (DatasetName == "santafe.mat")
		{
			plot(resultatMI1unif_Avg[,1],resultatMI1unif_Avg[,2], type='l',lty=1,lwd=2,cex.axis=2)
			lines(resultatMI2unif_Avg[,1],resultatMI2unif_Avg[,2],pch=10, lty=4,lwd=2)
			lines(resultatMI3unif_Avg[,1],resultatMI3unif_Avg[,2],pch=10, lty=3,lwd=2)

			lines(resultatMI1unif_SD[,1],resultatMI1unif_Avg[,2]+0.96*resultatMI1unif_SD[,2], type='l',lty=1,lwd=2,col='gray')
			lines(resultatMI2unif_SD[,1],resultatMI2unif_Avg[,2]+0.96*resultatMI2unif_SD[,2],pch=10, lty=4,lwd=2,col='gray')
			lines(resultatMI3unif_SD[,1],resultatMI3unif_Avg[,2]+0.96*resultatMI3unif_SD[,2],pch=10, lty=3,lwd=2,col='gray')

			lines(resultatMI1unif_SD[,1],resultatMI1unif_Avg[,2]-0.96*resultatMI1unif_SD[,2], type='l',lty=1,lwd=2,col='gray')
			lines(resultatMI2unif_SD[,1],resultatMI2unif_Avg[,2]-0.96*resultatMI2unif_SD[,2],pch=10, lty=4,lwd=2,col='gray')
			lines(resultatMI3unif_SD[,1],resultatMI3unif_Avg[,2]-0.96*resultatMI3unif_SD[,2],pch=10, lty=3,lwd=2,col='gray')
		}
	}
	if (Criterion == "DT") 
	{
		resultatDT1unif_Stock<- matrix(0,MatrixSize,Loop)
		resultatDT2unif_Stock<- matrix(0,MatrixSize,Loop)
		resultatDT3unif_Stock<- matrix(0,MatrixSize,Loop)

		for (rank in seq(1,Loop, by = 1)) #rank = loop number
		{
			resultat1 <- matrix(0,MatrixSize,2)
			resultat2 <- matrix(0,MatrixSize,2)
			resultat3 <- matrix(0,MatrixSize,2)

			j=0
			SmallXNoise <- rnorm(Size,mean = 0,sd = 0.05)

			######################################################################

			for (outX in seq(outX_Min,outX_MAX,by=StepSize))
			{				
				j = j + 1
				
				if (outX > 0)
				{
					###
					outlier_prop = outX							    #Proportion d'outliers
					outlier_nbr = round(Size*outlier_prop)			#Nombre total d'outliers
					outlier_pos = sample(seq(1,Size,1),outlier_nbr)	#Position des outliers au hasard dans le dataset
						
					outlying_X = runif(outlier_nbr,min = -1,max = 1)	#Importance de l'erreur d'outlier en Y
						
					dataset1_outliers <- matrix(0,nrow = Size, ncol = 2)	#Dataset 1 corrompu
					dataset2_outliers <- matrix(0,nrow = Size, ncol = 2)	#Dataset 2 corrompu
					dataset3_outliers <- matrix(0,nrow = Size, ncol = 2)	#Dataset 3 corrompu
						
					dataset1_outliers <- dataset1
					dataset2_outliers <- dataset2
					dataset3_outliers <- dataset3

					for (i in seq(1,outlier_nbr,by=1))
					{
						dataset1_outliers[outlier_pos[i],1]= dataset1_outliers[outlier_pos[i],1] + outlying_X[i]		
						dataset2_outliers[outlier_pos[i],1]= dataset2_outliers[outlier_pos[i],1] + outlying_X[i]		
						dataset3_outliers[outlier_pos[i],1]= dataset3_outliers[outlier_pos[i],1] + outlying_X[i]
					}
				}
				if (outX ==0)
				{
					dataset1_outliers <- dataset1
					dataset2_outliers <- dataset2
					dataset3_outliers <- dataset3
					dataset1_outliers[,1] <- dataset1[,1] + SmallXNoise
					dataset2_outliers[,1] <- dataset2[,1] + SmallXNoise
					dataset3_outliers[,1] <- dataset3[,1] + SmallXNoise
				}				
				
				X1 <- dataset1_outliers[1:Size,1]
				Y1 <- dataset1_outliers[1:Size,2]
				NewX1 <- matrix(0,Size,1)
				NewX1[,1] <- X1
				knn1<-get.knn(NewX1, k=1, algorithm=c("kd_tree")) #POUR CHAQUE X TROUVE LE PLUS PROCHE VOISIN
				NN1 <- knn1$nn.index #DONNE L'INDEX DU POINT LE PLUS PROCHE DANS DATASET_EXTRACT
				M <- Size #M INPUTS POINTS = TAILLE DU DATASET
				delta_test1 <-(sum((Y1- Y1[NN1])^2))/(2*M)
				resultat1[j,1]<-outX
				resultat1[j,2]<-delta_test1

				X2 <- dataset2_outliers[1:Size,1]
				Y2 <- dataset2_outliers[1:Size,2]
				NewX2 <- matrix(0,Size,1)
				NewX2[,1] <- X2
				knn2<-get.knn(NewX2, k=1, algorithm=c("kd_tree")) #POUR CHAQUE X TROUVE LE PLUS PROCHE VOISIN
				NN2 <- knn2$nn.index #DONNE L'INDEX DU POINT LE PLUS PROCHE DANS DATASET_EXTRACT
				M <- Size #M INPUTS POINTS = TAILLE DU DATASET
				delta_test2 <-(sum((Y2- Y2[NN2])^2))/(2*M)
				resultat2[j,1]<-outX
				resultat2[j,2]<-delta_test2

				X3 <- dataset3_outliers[1:Size,1]
				Y3 <- dataset3_outliers[1:Size,2]
				NewX3 <- matrix(0,Size,1)
				NewX3[,1] <- X3
				knn3<-get.knn(NewX3, k=1, algorithm=c("kd_tree")) #POUR CHAQUE X TROUVE LE PLUS PROCHE VOISIN
				NN3 <- knn3$nn.index #DONNE L'INDEX DU POINT LE PLUS PROCHE DANS DATASET_EXTRACT
				M <- Size #M INPUTS POINTS = TAILLE DU DATASET
				delta_test3 <-(sum((Y3- Y3[NN3])^2))/(2*M)
				resultat3[j,1]<-outX
				resultat3[j,2]<-delta_test3
			}	
			resultatDT1unif_Stock[,rank] <-resultat1[,2]
			resultatDT2unif_Stock[,rank] <-resultat2[,2]
			resultatDT3unif_Stock[,rank] <-resultat3[,2]
		}
		resultatDT1unif_Avg <- matrix(0,MatrixSize,2)
		resultatDT2unif_Avg <- matrix(0,MatrixSize,2)
		resultatDT3unif_Avg <- matrix(0,MatrixSize,2)

		resultatDT1unif_SD <- matrix(0,MatrixSize,2)
		resultatDT2unif_SD <- matrix(0,MatrixSize,2)
		resultatDT3unif_SD <- matrix(0,MatrixSize,2)

		resultatDT1unif_Avg[,1] <- resultat1[,1]
		resultatDT2unif_Avg[,1] <- resultat2[,1]
		resultatDT3unif_Avg[,1] <- resultat3[,1]
		resultatDT1unif_SD[,1] <- resultat1[,1]
		resultatDT2unif_SD[,1] <- resultat2[,1]
		resultatDT3unif_SD[,1] <- resultat3[,1]

		for (i in seq(1,MatrixSize,by=1))
		{
			resultatDT1unif_Avg[i,2]<-mean(resultatDT1unif_Stock[i,])
			resultatDT2unif_Avg[i,2]<-mean(resultatDT2unif_Stock[i,])
			resultatDT3unif_Avg[i,2]<-mean(resultatDT3unif_Stock[i,])

			resultatDT1unif_SD[i,2]<-sd(resultatDT1unif_Stock[i,])
			resultatDT2unif_SD[i,2]<-sd(resultatDT2unif_Stock[i,])
			resultatDT3unif_SD[i,2]<-sd(resultatDT3unif_Stock[i,])
		}	
	
		if (DatasetName == "poland_30.mat")
		{
			plot(resultatDT1unif_Avg[,1],resultatDT1unif_Avg[,2], ylim=c(0,0.8),type='l',lty=1,lwd=2,cex.axis=2)
			lines(resultatDT2unif_Avg[,1],resultatDT2unif_Avg[,2],pch=10, lty=4,lwd=2)
			lines(resultatDT3unif_Avg[,1],resultatDT3unif_Avg[,2],pch=10, lty=3,lwd=2)

			lines(resultatDT1unif_SD[,1],resultatDT1unif_Avg[,2]+0.96*resultatDT1unif_SD[,2], type='l',lty=1,lwd=2,col='gray')
			lines(resultatDT2unif_SD[,1],resultatDT2unif_Avg[,2]+0.96*resultatDT2unif_SD[,2],pch=10, lty=4,lwd=2,col='gray')
			lines(resultatDT3unif_SD[,1],resultatDT3unif_Avg[,2]+0.96*resultatDT3unif_SD[,2],pch=10, lty=3,lwd=2,col='gray')

			lines(resultatDT1unif_SD[,1],resultatDT1unif_Avg[,2]-0.96*resultatDT1unif_SD[,2], type='l',lty=1,lwd=2,col='gray')
			lines(resultatDT2unif_SD[,1],resultatDT2unif_Avg[,2]-0.96*resultatDT2unif_SD[,2],pch=10, lty=4,lwd=2,col='gray')
			lines(resultatDT3unif_SD[,1],resultatDT3unif_Avg[,2]-0.96*resultatDT3unif_SD[,2],pch=10, lty=3,lwd=2,col='gray')
		}
		
		if (DatasetName == "anthrokidsV2.dat")
		{
			plot(resultatDT1unif_Avg[,1],resultatDT1unif_Avg[,2], ylim=c(0,1.1),type='l',lty=1,lwd=2,cex.axis=2)
			lines(resultatDT2unif_Avg[,1],resultatDT2unif_Avg[,2],pch=10, lty=4,lwd=2)
			lines(resultatDT3unif_Avg[,1],resultatDT3unif_Avg[,2],pch=10, lty=3,lwd=2)

			lines(resultatDT1unif_SD[,1],resultatDT1unif_Avg[,2]+0.96*resultatDT1unif_SD[,2], type='l',lty=1,lwd=2,col='gray')
			lines(resultatDT2unif_SD[,1],resultatDT2unif_Avg[,2]+0.96*resultatDT2unif_SD[,2],pch=10, lty=4,lwd=2,col='gray')
			lines(resultatDT3unif_SD[,1],resultatDT3unif_Avg[,2]+0.96*resultatDT3unif_SD[,2],pch=10, lty=3,lwd=2,col='gray')

			lines(resultatDT1unif_SD[,1],resultatDT1unif_Avg[,2]-0.96*resultatDT1unif_SD[,2], type='l',lty=1,lwd=2,col='gray')
			lines(resultatDT2unif_SD[,1],resultatDT2unif_Avg[,2]-0.96*resultatDT2unif_SD[,2],pch=10, lty=4,lwd=2,col='gray')
			lines(resultatDT3unif_SD[,1],resultatDT3unif_Avg[,2]-0.96*resultatDT3unif_SD[,2],pch=10, lty=3,lwd=2,col='gray')					
		}
		
		if (DatasetName == "santafe.mat")
		{
			plot(resultatDT1unif_Avg[,1],resultatDT1unif_Avg[,2], type='l',lty=1,lwd=2,cex.axis=2)
			lines(resultatDT2unif_Avg[,1],resultatDT2unif_Avg[,2],pch=10, lty=4,lwd=2)
			lines(resultatDT3unif_Avg[,1],resultatDT3unif_Avg[,2],pch=10, lty=3,lwd=2)

			lines(resultatDT1unif_SD[,1],resultatDT1unif_Avg[,2]+0.96*resultatDT1unif_SD[,2], type='l',lty=1,lwd=2,col='gray')
			lines(resultatDT2unif_SD[,1],resultatDT2unif_Avg[,2]+0.96*resultatDT2unif_SD[,2],pch=10, lty=4,lwd=2,col='gray')
			lines(resultatDT3unif_SD[,1],resultatDT3unif_Avg[,2]+0.96*resultatDT3unif_SD[,2],pch=10, lty=3,lwd=2,col='gray')

			lines(resultatDT1unif_SD[,1],resultatDT1unif_Avg[,2]-0.96*resultatDT1unif_SD[,2], type='l',lty=1,lwd=2,col='gray')
			lines(resultatDT2unif_SD[,1],resultatDT2unif_Avg[,2]-0.96*resultatDT2unif_SD[,2],pch=10, lty=4,lwd=2,col='gray')
			lines(resultatDT3unif_SD[,1],resultatDT3unif_Avg[,2]-0.96*resultatDT3unif_SD[,2],pch=10, lty=3,lwd=2,col='gray')	
		}
				
	}

	if (Criterion == "NLR2")
	{	
		K_Initial <- 6
		K_Final <-round(Size*0.4)
		Step_K <- 80
		Nbr_Boucles_K <- ((K_Final - K_Initial)/Step_K)+1

		resultatR1unif_Stock<- matrix(0,MatrixSize,Loop)
		resultatR2unif_Stock<- matrix(0,MatrixSize,Loop)
		resultatR3unif_Stock<- matrix(0,MatrixSize,Loop)

		for (rank in seq(1,Loop, by = 1)) #rank = loop number
		{
			f=0

			Best_Radj_N <- matrix(0,MatrixSize,Nbr_Features+1)
			SelectK_adj <-matrix(0,Nbr_Features,MatrixSize)
			SmallXNoise <- rnorm(Size,mean = 0,sd = 0.05)

			for (outX in seq(outX_Min,outX_MAX,by=StepSize))
			{
				f=f+1
				
				if (outX > 0)
				{
					outlier_prop = outX							    #Proportion d'outliers
					outlier_nbr = round(Size*outlier_prop)			#Nombre total d'outliers
					outlier_pos = sample(seq(1,Size,1),outlier_nbr)	#Position des outliers au hasard dans le dataset
						
					outlying_X = runif(outlier_nbr,min = -1,max = 1)	#Importance de l'erreur d'outlier en Y
						
					dataset1_outliers <- matrix(0,nrow = Size, ncol = 2)	#Dataset 1 corrompu
					dataset2_outliers <- matrix(0,nrow = Size, ncol = 2)	#Dataset 2 corrompu
					dataset3_outliers <- matrix(0,nrow = Size, ncol = 2)	#Dataset 3 corrompu
						
					dataset1_outliers <- dataset1
					dataset2_outliers <- dataset2
					dataset3_outliers <- dataset3

					for (i in seq(1,outlier_nbr,by=1))
					{
						dataset1_outliers[outlier_pos[i],1]= dataset1_outliers[outlier_pos[i],1] + outlying_X[i]		
						dataset2_outliers[outlier_pos[i],1]= dataset2_outliers[outlier_pos[i],1] + outlying_X[i]		
						dataset3_outliers[outlier_pos[i],1]= dataset3_outliers[outlier_pos[i],1] + outlying_X[i]
					}						
				}
				if (outX ==0)
				{
					dataset1_outliers <- dataset1
					dataset2_outliers <- dataset2
					dataset3_outliers <- dataset3
					dataset1_outliers[,1] <- dataset1[,1] + SmallXNoise
					dataset2_outliers[,1] <- dataset2[,1] + SmallXNoise
					dataset3_outliers[,1] <- dataset3[,1] + SmallXNoise
				}

				dataset_X <- matrix(0,Size,Nbr_Features)
				dataset_X[,1]<-dataset1_outliers[,1]
				dataset_X[,2]<-dataset2_outliers[,1]
				dataset_X[,3]<-dataset3_outliers[,1]

				dataset_Y <- matrix(0,Size,Nbr_Features)
				dataset_Y[,1]<-dataset1_outliers[,2]
				dataset_Y[,2]<-dataset2_outliers[,2]
				dataset_Y[,3]<-dataset3_outliers[,2]
					
				##### Initialisation des tableaux de variables
				NewY1 <-matrix(0,Size,Nbr_Features)
				NewY1 <- dataset_Y
				R_Carre_adj  <- matrix(0,Size,Nbr_Features)
				Mean_R_Carre_adj <- matrix(0,Nbr_Boucles_K,Nbr_Features)
				VecteurK <-matrix(0,Nbr_Boucles_K,1)

				r=0
				for (K in seq(K_Initial, K_Final, by=Step_K)) #Boucle sur les K voisins
				{
					r=r+1
					VecteurK[r,1]<-K

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
						Mean_R_Carre_adj[r,k]=mean(R_Carre_adj[,k])
					}
								
				}					
				Best_Radj_N[f,1]<- outX	
				Best_Radj_N[f,2]<-max(Mean_R_Carre_adj[,1])
				Best_Radj_N[f,3]<-max(Mean_R_Carre_adj[,2])
				Best_Radj_N[f,4]<-max(Mean_R_Carre_adj[,3])
				# Worst Rsquared = 0
				# Best Rsquared = 1
			}
			
			resultatR1unif_Stock[,rank] <-Best_Radj_N[,2]
			resultatR2unif_Stock[,rank] <-Best_Radj_N[,3]
			resultatR3unif_Stock[,rank] <-Best_Radj_N[,4]
		}

		resultatR1unif_Avg <- matrix(0,MatrixSize,2)
		resultatR2unif_Avg <- matrix(0,MatrixSize,2)
		resultatR3unif_Avg <- matrix(0,MatrixSize,2)

		resultatR1unif_SD <- matrix(0,MatrixSize,2)
		resultatR2unif_SD <- matrix(0,MatrixSize,2)
		resultatR3unif_SD <- matrix(0,MatrixSize,2)

		resultatR1unif_Avg[,1] <- Best_Radj_N[,1]
		resultatR2unif_Avg[,1] <- Best_Radj_N[,1]
		resultatR3unif_Avg[,1] <- Best_Radj_N[,1]
		resultatR1unif_SD[,1] <- Best_Radj_N[,1]
		resultatR2unif_SD[,1] <- Best_Radj_N[,1]
		resultatR3unif_SD[,1] <- Best_Radj_N[,1]

		for (i in seq(1,MatrixSize,by=1))
		{
			resultatR1unif_Avg[i,2]<-mean(resultatR1unif_Stock[i,])
			resultatR2unif_Avg[i,2]<-mean(resultatR2unif_Stock[i,])
			resultatR3unif_Avg[i,2]<-mean(resultatR3unif_Stock[i,])

			resultatR1unif_SD[i,2]<-sd(resultatR1unif_Stock[i,])
			resultatR2unif_SD[i,2]<-sd(resultatR2unif_Stock[i,])
			resultatR3unif_SD[i,2]<-sd(resultatR3unif_Stock[i,])
		}
	
		if (DatasetName == "poland_30.mat")
		{
			plot(resultatR1unif_Avg[,1],resultatR1unif_Avg[,2], ylim=c(0,0.7),type='l',lty=1,lwd=2,cex.axis=2)
			lines(resultatR2unif_Avg[,1],resultatR2unif_Avg[,2],pch=10, lty=4,lwd=2)
			lines(resultatR3unif_Avg[,1],resultatR3unif_Avg[,2],pch=10, lty=3,lwd=2)

			lines(resultatR1unif_SD[,1],resultatR1unif_Avg[,2]+0.96*resultatR1unif_SD[,2], type='l',lty=1,lwd=2,col='gray')
			lines(resultatR2unif_SD[,1],resultatR2unif_Avg[,2]+0.96*resultatR2unif_SD[,2],pch=10, lty=4,lwd=2,col='gray')
			lines(resultatR3unif_SD[,1],resultatR3unif_Avg[,2]+0.96*resultatR3unif_SD[,2],pch=10, lty=3,lwd=2,col='gray')

			lines(resultatR1unif_SD[,1],resultatR1unif_Avg[,2]-0.96*resultatR1unif_SD[,2], type='l',lty=1,lwd=2,col='gray')
			lines(resultatR2unif_SD[,1],resultatR2unif_Avg[,2]-0.96*resultatR2unif_SD[,2],pch=10, lty=4,lwd=2,col='gray')
			lines(resultatR3unif_SD[,1],resultatR3unif_Avg[,2]-0.96*resultatR3unif_SD[,2],pch=10, lty=3,lwd=2,col='gray')
		}
		
		if (DatasetName == "anthrokidsV2.dat")
		{
			plot(resultatR1unif_Avg[,1],resultatR1unif_Avg[,2], ylim=c(0,0.7),type='l',lty=1,lwd=2,cex.axis=2)
			lines(resultatR2unif_Avg[,1],resultatR2unif_Avg[,2],pch=10, lty=4,lwd=2)
			lines(resultatR3unif_Avg[,1],resultatR3unif_Avg[,2],pch=10, lty=3,lwd=2)

			lines(resultatR1unif_SD[,1],resultatR1unif_Avg[,2]+0.96*resultatR1unif_SD[,2], type='l',lty=1,lwd=2,col='gray')
			lines(resultatR2unif_SD[,1],resultatR2unif_Avg[,2]+0.96*resultatR2unif_SD[,2],pch=10, lty=4,lwd=2,col='gray')
			lines(resultatR3unif_SD[,1],resultatR3unif_Avg[,2]+0.96*resultatR3unif_SD[,2],pch=10, lty=3,lwd=2,col='gray')

			lines(resultatR1unif_SD[,1],resultatR1unif_Avg[,2]-0.96*resultatR1unif_SD[,2], type='l',lty=1,lwd=2,col='gray')
			lines(resultatR2unif_SD[,1],resultatR2unif_Avg[,2]-0.96*resultatR2unif_SD[,2],pch=10, lty=4,lwd=2,col='gray')
			lines(resultatR3unif_SD[,1],resultatR3unif_Avg[,2]-0.96*resultatR3unif_SD[,2],pch=10, lty=3,lwd=2,col='gray')		
		}
		
		if (DatasetName == "santafe.mat")
		{
			plot(resultatR1unif_Avg[,1],resultatR1unif_Avg[,2], type='l',lty=1,lwd=2,cex.axis=2)
			lines(resultatR2unif_Avg[,1],resultatR2unif_Avg[,2],pch=10, lty=4,lwd=2)
			lines(resultatR3unif_Avg[,1],resultatR3unif_Avg[,2],pch=10, lty=3,lwd=2)

			lines(resultatR1unif_SD[,1],resultatR1unif_Avg[,2]+0.96*resultatR1unif_SD[,2], type='l',lty=1,lwd=2,col='gray')
			lines(resultatR2unif_SD[,1],resultatR2unif_Avg[,2]+0.96*resultatR2unif_SD[,2],pch=10, lty=4,lwd=2,col='gray')
			lines(resultatR3unif_SD[,1],resultatR3unif_Avg[,2]+0.96*resultatR3unif_SD[,2],pch=10, lty=3,lwd=2,col='gray')

			lines(resultatR1unif_SD[,1],resultatR1unif_Avg[,2]-0.96*resultatR1unif_SD[,2], type='l',lty=1,lwd=2,col='gray')
			lines(resultatR2unif_SD[,1],resultatR2unif_Avg[,2]-0.96*resultatR2unif_SD[,2],pch=10, lty=4,lwd=2,col='gray')
			lines(resultatR3unif_SD[,1],resultatR3unif_Avg[,2]-0.96*resultatR3unif_SD[,2],pch=10, lty=3,lwd=2,col='gray')			
		}			
	}
}	