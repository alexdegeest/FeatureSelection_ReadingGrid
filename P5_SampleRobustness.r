
P5_SampleRobustness <- function(DatasetName,Criterion)
{
########################################################################################
# Test a criterion on a dataset DatasetName for property5 "Estimator Sample Robustness"
#
# Inputs:
#   - DatasetName
#	- Criterion
#
# Outputs:
#   - figure of the results
#   
# Author: A. Degeest, 2020 (alexandra.degeest@gmail.com)
##########################################################################################
	if (DatasetName == "anthrokidsV2.dat")
	{
		dataset <- read.csv(file="anthrokidsV2.dat",head=FALSE,sep=" ") 
		dataset_X_init = matrix(c(dataset[,1],dataset[,11],dataset[,19]), ncol=3)
		dataset_Y_init = matrix(c(dataset[,21]),ncol=1) 
		Nbr_Features <- ncol(dataset_X_init)
		Size_init <- length(dataset_Y_init)
	}
	
	if (DatasetName == "poland_30.mat")
	{
		dataset <- readMat(con="poland_30.mat",head=FALSE) 
		dataset_X1 <- dataset$x
		#dataset_X = matrix(c(dataset_X1[,1],dataset_X1[,23],exp(dataset_X1[,24])), ncol=3)
		dataset_X = matrix(c(dataset_X1[,1],dataset_X1[,23],((exp(dataset_X1[,24]))-1.58)/1.5), ncol=3)
		dataset_Y_init = dataset$y
		Size <- length(dataset_Y_init)
		Nbr_Features <- ncol(dataset_X_init)
		Size_init <-Size
		NewY1 <-matrix(0,Size_init,1)
		NewY1 <- dataset_Y_init
	}

	if (DatasetName == "santafe.mat")
	{
		dataset <- readMat(con="santafe.mat",head=FALSE)#,sep=",") 
		dataset_X1 <- dataset$x
		dataset_X_init = matrix(c(dataset_X1[,1],dataset_X1[,4],16*sqrt(dataset_X1[,5])), ncol=3)
		dataset_Y_init = dataset$y
		Size <- length(dataset_Y_init)
		Nbr_Features <- ncol(dataset_X_init)
		Size_init <-Size
	}
	
	eps = rnorm(Size_init,mean=0,sd=0.002)
	dataset_X_init[,2]=dataset_X_init[,2]+eps
	dataset_X_init[,3]=dataset_X_init[,3]+eps
	
	Extract_Size_Init <- 20
	Extract_Size_Fin <- Size_init
	Extract_Size_Step <- 50
	Nbr_Boucles_Extract <- ((Extract_Size_Fin-Extract_Size_Init)/Extract_Size_Step) + 1
	
	Loop = 10

	if (Criterion == "MI") 
	{	
		K = 6
		
		resultatFeat1D_N <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI1
		resultatFeat2D_N <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI2 
		resultatFeat3D_N <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI3

		resultatFeat1D_N_Stock1 <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI1
		resultatFeat2D_N_Stock1<- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI2 
		resultatFeat3D_N_Stock1 <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI3

		resultatFeat1D_N_Stock2 <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI1
		resultatFeat2D_N_Stock2 <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI2 
		resultatFeat3D_N_Stock2 <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI3

		resultatFeat1D_N_Stock3 <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI1
		resultatFeat2D_N_Stock3 <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI2 
		resultatFeat3D_N_Stock3 <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI3

		resultatFeat1D_N_Stock4 <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI1
		resultatFeat2D_N_Stock4 <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI2 
		resultatFeat3D_N_Stock4 <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI3

		resultatFeat1D_N_Avg <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI1
		resultatFeat2D_N_Avg <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI2 
		resultatFeat3D_N_Avg <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI3

		resultatFeat1D_N_StockFeat1 <- matrix(0,4,Nbr_Boucles_Extract)
		resultatFeat1D_N_StockFeat2 <- matrix(0,4,Nbr_Boucles_Extract)
		resultatFeat1D_N_StockFeat3 <- matrix(0,4,Nbr_Boucles_Extract)
		resultatFeat1D_N_StockFeat4 <- matrix(0,4,Nbr_Boucles_Extract)

		resultatFeat1D_N_SD_Feat1 <- matrix(0,1,Nbr_Boucles_Extract)
		resultatFeat1D_N_SD_Feat2 <- matrix(0,1,Nbr_Boucles_Extract)
		resultatFeat1D_N_SD_Feat3 <- matrix(0,1,Nbr_Boucles_Extract)

		for (rank in seq(1,Loop, by = 1)) #rank = loop number
		{ 

			v = 0
			for (Extract_Size in seq(Extract_Size_Init, Extract_Size_Fin, by=Extract_Size_Step))
			{
				#ici extraire une partie du dataset initial pour pouvoir faire varier la taille de dataset
				dataset1 <- matrix(0,Size_init,2)
				dataset2 <- matrix(0,Size_init,2)
				dataset3 <- matrix(0,Size_init,2)
				dataset1_extract <- matrix(0,Extract_Size,2)
				dataset2_extract <- matrix(0,Extract_Size,2)
				dataset3_extract <- matrix(0,Extract_Size,2)
				dataset1[,1] <- dataset_X_init[,1]
				dataset1[,2] <- dataset_Y_init
				dataset1_extract <- dataset1[sample(1:nrow(dataset1),Extract_Size,replace=FALSE),]
				dataset2[,1] <- dataset_X_init[,2]
				dataset2[,2] <- dataset_Y_init
				dataset2_extract <- dataset2[sample(1:nrow(dataset2),Extract_Size,replace=FALSE),]
				dataset3[,1] <- dataset_X_init[,3]
				dataset3[,2] <- dataset_Y_init
				dataset3_extract <- dataset3[sample(1:nrow(dataset3),Extract_Size,replace=FALSE),]

				dataset_X = matrix(0,Extract_Size,3)
				dataset_Y = matrix(0,Extract_Size,3)
				dataset_X[,1]<- dataset1_extract[,1]
				dataset_X[,2]<- dataset2_extract[,1]
				dataset_X[,3]<- dataset3_extract[,1]
				dataset_Y[,1]<- dataset1_extract[,2]
				dataset_Y[,2]<- dataset2_extract[,2]
				dataset_Y[,3]<- dataset3_extract[,2]
				Size <- Extract_Size


				resultatMI1 <- matrix(0,1,Nbr_Features)
				################################################
				#####BOUCLE SUR LE NOMBRE DE FEATURES
				################################################
				for (IndexFeature in seq(1,Nbr_Features,by=1))
				{
					NewX1 <-matrix(0,Size,1)
					NewX1 <- dataset_X[,IndexFeature]
					NewY1 <-matrix(0,Size,1)
					NewY1 <- dataset_Y[,IndexFeature]
					
					MI_test1 <- MIxnyn4(matrix(NewX1,ncol=1), matrix(NewY1,ncol=1),K)
					resultatMI1[1,IndexFeature]<- MI_test1
				}

				FeatureSelectedMI<- max(resultatMI1)
				IndexSelectedFeat <- which (resultatMI1 == FeatureSelectedMI)


				resultatMI2 <- matrix(0,1,Nbr_Features)
				for (IndexFeature in seq(1,Nbr_Features,by=1))
				{
					NewX1 <- matrix(0,Size,2)
					if (IndexFeature != IndexSelectedFeat) #On met 2 features ensemble, toujours le premier sélectionné et puis un différent!
					{
						NewX1[,1] <- dataset_X[,IndexSelectedFeat]
						NewX1[,2] <- dataset_X[,IndexFeature]
						NewY1 <-matrix(0,Size,1)
						NewY1 <- dataset_Y[,IndexFeature]
						
						MI_test1 <- MIxnyn4(matrix(NewX1,ncol=2), matrix(NewY1,ncol=1),K)
						resultatMI2[1,IndexFeature]<- MI_test1
					}
				}
				FeatureSelectedMI2 <- max(resultatMI2)
				IndexSelectedFeat2 <- which (resultatMI2 == FeatureSelectedMI2)


				if (Nbr_Features >2)
				{
					resultatMI3 <- matrix(0,1,Nbr_Features)
					for (IndexFeature in seq(1,Nbr_Features,by=1))
					{
						NewX1 <- matrix(0,Size,3)
						if (IndexFeature != IndexSelectedFeat) #On met 2 features ensemble, toujours le premier sélectionné et puis un différent!
						{
							if (IndexFeature != IndexSelectedFeat2)
							{
								NewX1[,1] <- dataset_X[,IndexSelectedFeat]
								NewX1[,2] <- dataset_X[,IndexSelectedFeat2]
								NewX1[,3] <- dataset_X[,IndexFeature]
								NewY1 <-matrix(0,Size,1)
								NewY1 <- dataset_Y[,IndexFeature]
								
								MI_test1 <- MIxnyn4(matrix(NewX1,ncol=3), matrix(NewY1,ncol=1),K)
								resultatMI3[1,IndexFeature]<- MI_test1	
							}
						}
					}

					FeatureSelectedMI3 <- max(resultatMI3)
					IndexSelectedFeat3 <- which (resultatMI3 == FeatureSelectedMI3)
				}

				if (Nbr_Features >3)
				{
					resultatMI3 <- matrix(0,1,Nbr_Features)
					for (IndexFeature in seq(1,Nbr_Features,by=1))
					{
						NewX1 <- matrix(0,Size,4)
						if (IndexFeature != IndexSelectedFeat) #On met 2 features ensemble, toujours le premier sélectionné et puis un différent!
						{
							if (IndexFeature != IndexSelectedFeat2)
							{
								if (IndexFeature != IndexSelectedFeat3)
								{
									NewX1[,1] <- dataset_X[,IndexSelectedFeat]
									NewX1[,2] <- dataset_X[,IndexSelectedFeat2]
									NewX1[,3] <- dataset_X[,IndexSelectedFeat3]
									NewX1[,4] <- dataset_X[,IndexFeature]
									NewY1 <-matrix(0,Size,1)
									NewY1 <- dataset_Y[,IndexFeature]
									
									MI_test1 <- MIxnyn4(matrix(NewX1,ncol=4), matrix(NewY1,ncol=1),K)
									resultatMI4[1,IndexFeature]<- MI_test1	
								}
							}
						}
					}
					
					FeatureSelectedMI4 <- max(resultatMI4)
					IndexSelectedFeat4 <- which (resultatMI4 == FeatureSelectedMI4)
				}
				
				v = v + 1
				resultatFeat1D_N[1,v] <- Extract_Size
				resultatFeat2D_N[1,v] <- Extract_Size
				resultatFeat3D_N[1,v] <- Extract_Size
				
				resultatFeat1D_N[2,v] <- resultatMI1[1,1] #STEP 1 FS - FEATURE 1
				resultatFeat2D_N[2,v] <- resultatMI2[1,1] #STEP 2 FS - FEATURE 1
				resultatFeat3D_N[2,v] <- resultatMI3[1,1] #STEP 3 FS - FEATURE 1
				
				resultatFeat1D_N[3,v] <- resultatMI1[1,2] #STEP 1 FS - FEATURE 2
				resultatFeat2D_N[3,v] <- resultatMI2[1,2] #STEP 2 FS - FEATURE 2
				resultatFeat3D_N[3,v] <- resultatMI3[1,2] #STEP 3 FS - FEATURE 2
				
				resultatFeat1D_N[4,v] <- resultatMI1[1,3] #STEP 1 FS - FEATURE 3
				resultatFeat2D_N[4,v] <- resultatMI2[1,3] #STEP 2 FS - FEATURE 3
				resultatFeat3D_N[4,v] <- resultatMI3[1,3] #STEP 3 FS - FEATURE 3
				
				

			} #Fin Boucle sur taille du dataset
			if (rank==1)
			{
				resultatFeat1D_N_Stock1 <- resultatFeat1D_N
				resultatFeat2D_N_Stock1 <- resultatFeat2D_N
				resultatFeat3D_N_Stock1 <- resultatFeat3D_N
			}
			
			if (rank == 2)
			{
				resultatFeat1D_N_Stock2 <- resultatFeat1D_N
				resultatFeat2D_N_Stock2 <- resultatFeat2D_N
				resultatFeat3D_N_Stock2 <- resultatFeat3D_N
			}
			
			if (rank == 3)
			{
				resultatFeat1D_N_Stock3 <- resultatFeat1D_N
				resultatFeat2D_N_Stock3 <- resultatFeat2D_N
				resultatFeat3D_N_Stock3 <- resultatFeat3D_N
			}
			
			if (rank == 4)
			{
				resultatFeat1D_N_Stock4 <- resultatFeat1D_N
				resultatFeat2D_N_Stock4 <- resultatFeat2D_N
				resultatFeat3D_N_Stock4 <- resultatFeat3D_N
			}
		}

		resultatFeat1D_N_Avg <- (resultatFeat1D_N_Stock1+resultatFeat1D_N_Stock2+resultatFeat1D_N_Stock3+resultatFeat1D_N_Stock4)/4 #STEP 1 FS
		resultatFeat2D_N_Avg <- (resultatFeat2D_N_Stock1+resultatFeat2D_N_Stock2+resultatFeat2D_N_Stock3+resultatFeat2D_N_Stock4)/4 #STEP 2 FS
		resultatFeat3D_N_Avg <- (resultatFeat3D_N_Stock1+resultatFeat3D_N_Stock2+resultatFeat3D_N_Stock3+resultatFeat3D_N_Stock4)/4 #STEP 3 FS

		#FS STEP 1 - FEATURE 1
		resultatFeat1D_N_StockFeat1[1,] <-(resultatFeat1D_N_Stock1[2,])
		resultatFeat1D_N_StockFeat1[2,] <-(resultatFeat1D_N_Stock2[2,])
		resultatFeat1D_N_StockFeat1[3,] <-(resultatFeat1D_N_Stock3[2,])
		resultatFeat1D_N_StockFeat1[4,] <-(resultatFeat1D_N_Stock4[2,])

		#FS STEP 1 - FEATURE 2
		resultatFeat1D_N_StockFeat2[1,] <-(resultatFeat1D_N_Stock1[3,])
		resultatFeat1D_N_StockFeat2[2,] <-(resultatFeat1D_N_Stock2[3,])
		resultatFeat1D_N_StockFeat2[3,] <-(resultatFeat1D_N_Stock3[3,])
		resultatFeat1D_N_StockFeat2[4,] <-(resultatFeat1D_N_Stock4[3,])

		#FS STEP 1 - FEATURE 3
		resultatFeat1D_N_StockFeat3[1,] <-(resultatFeat1D_N_Stock1[4,])
		resultatFeat1D_N_StockFeat3[2,] <-(resultatFeat1D_N_Stock2[4,])
		resultatFeat1D_N_StockFeat3[3,] <-(resultatFeat1D_N_Stock3[4,])
		resultatFeat1D_N_StockFeat3[4,] <-(resultatFeat1D_N_Stock4[4,])


		if (DatasetName == "anthrokidsV2.dat")
		{
			for (i in seq(1,20,by=1))
			{
				resultatFeat1D_N_SD_Feat1[1,i] <- sd(resultatFeat1D_N_StockFeat1[,i])
				resultatFeat1D_N_SD_Feat2[1,i] <- sd(resultatFeat1D_N_StockFeat2[,i])
				resultatFeat1D_N_SD_Feat3[1,i] <- sd(resultatFeat1D_N_StockFeat3[,i])
			}
		}
		
		if (DatasetName == "poland_30.mat")
		{
			for (i in seq(1,20,by=1))
			{
				resultatFeat1D_N_SD_Feat1[1,i] <- sd(resultatFeat1D_N_StockFeat1[,i])
				resultatFeat1D_N_SD_Feat2[1,i] <- sd(resultatFeat1D_N_StockFeat2[,i])
				resultatFeat1D_N_SD_Feat3[1,i] <- sd(resultatFeat1D_N_StockFeat3[,i])
			}
		}

		if (DatasetName == "santafe.mat")
		{		
			for (i in seq(1,Nbr_Boucles_Extract,by=1))
			{
				resultatFeat1D_N_SD_Feat1[1,i] <- sd(resultatFeat1D_N_StockFeat1[,i])
				resultatFeat1D_N_SD_Feat2[1,i] <- sd(resultatFeat1D_N_StockFeat2[,i])
				resultatFeat1D_N_SD_Feat3[1,i] <- sd(resultatFeat1D_N_StockFeat3[,i])
			}
		}
	}
	
	if (Criterion == "DT") 
	{
	
		resultatFeat1D_N <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI1
		resultatFeat2D_N <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI2 
		resultatFeat3D_N <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI3

		resultatFeat1D_N_Stock1 <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI1
		resultatFeat2D_N_Stock1<- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI2 
		resultatFeat3D_N_Stock1 <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI3

		resultatFeat1D_N_Stock2 <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI1
		resultatFeat2D_N_Stock2 <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI2 
		resultatFeat3D_N_Stock2 <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI3

		resultatFeat1D_N_Stock3 <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI1
		resultatFeat2D_N_Stock3 <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI2 
		resultatFeat3D_N_Stock3 <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI3

		resultatFeat1D_N_Stock4 <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI1
		resultatFeat2D_N_Stock4 <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI2 
		resultatFeat3D_N_Stock4 <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI3

		resultatFeat1D_N_Avg <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI1
		resultatFeat2D_N_Avg <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI2 
		resultatFeat3D_N_Avg <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) #Taille dataset | Resultat MI3

		resultatFeat1D_N_StockFeat1 <- matrix(0,4,Nbr_Boucles_Extract)
		resultatFeat1D_N_StockFeat2 <- matrix(0,4,Nbr_Boucles_Extract)
		resultatFeat1D_N_StockFeat3 <- matrix(0,4,Nbr_Boucles_Extract)
		resultatFeat1D_N_StockFeat4 <- matrix(0,4,Nbr_Boucles_Extract)

		resultatFeat1D_N_SD_Feat1 <- matrix(0,1,Nbr_Boucles_Extract)
		resultatFeat1D_N_SD_Feat2 <- matrix(0,1,Nbr_Boucles_Extract)
		resultatFeat1D_N_SD_Feat3 <- matrix(0,1,Nbr_Boucles_Extract)

		for (rank in seq(1,Loop, by = 1))
		{ 

			v = 0
			for (Extract_Size in seq(Extract_Size_Init, Extract_Size_Fin, by=Extract_Size_Step))
			{

				##### Initialisation des tableaux de variables
				# delta_test <- matrix(0,1,Nbr_Features)
				delta_test <- matrix(1000,1,Nbr_Features)

				#ici extraire une partie du dataset initial pour pouvoir faire varier la taille de dataset
				dataset1 <- matrix(0,Size_init,2)
				dataset2 <- matrix(0,Size_init,2)
				dataset3 <- matrix(0,Size_init,2)
				dataset1_extract <- matrix(0,Extract_Size,2)
				dataset2_extract <- matrix(0,Extract_Size,2)
				dataset3_extract <- matrix(0,Extract_Size,2)
				dataset1[,1] <- dataset_X_init[,1]
				dataset1[,2] <- dataset_Y_init
				dataset1_extract <- dataset1[sample(1:nrow(dataset1),Extract_Size,replace=FALSE),]
				dataset2[,1] <- dataset_X_init[,2]
				dataset2[,2] <- dataset_Y_init
				dataset2_extract <- dataset2[sample(1:nrow(dataset2),Extract_Size,replace=FALSE),]
				dataset3[,1] <- dataset_X_init[,3]
				dataset3[,2] <- dataset_Y_init
				dataset3_extract <- dataset3[sample(1:nrow(dataset3),Extract_Size,replace=FALSE),]

				dataset_X = matrix(0,Extract_Size,3)
				dataset_Y = matrix(0,Extract_Size,3)
				dataset_X[,1]<- dataset1_extract[,1]
				dataset_X[,2]<- dataset2_extract[,1]
				dataset_X[,3]<- dataset3_extract[,1]
				dataset_Y[,1]<- dataset1_extract[,2]
				dataset_Y[,2]<- dataset2_extract[,2]
				dataset_Y[,3]<- dataset3_extract[,2]
				Size <- Extract_Size



				################################################
				#####BOUCLE SUR LE NOMBRE DE FEATURES
				################################################
				for (IndexFeature in seq(1,Nbr_Features,by=1))
				{
					NewX1 <-matrix(0,Size,1)
					NewX1 <- dataset_X[,IndexFeature]
					NewY1 <-matrix(0,Size,1)
					NewY1 <- dataset_Y[,IndexFeature]
					
					X1 <- NewX1
					Y1 <- NewY1
					knn1<-get.knn(X1, k=1, algorithm=c("kd_tree")) #POUR CHAQUE X TROUVE LE PLUS PROCHE VOISIN
					NN1 <- knn1$nn.index #DONNE L'INDEX DU POINT LE PLUS PROCHE DANS DATASET_EXTRACT
					M <- Size #M INPUTS POINTS = TAILLE DU DATASET
					delta_test[1,IndexFeature] <-(sum((Y1- Y1[NN1])^2))/(2*M)

				}

				FeatureSelectedDT<- min(delta_test)
				IndexSelectedFeat <- which (delta_test == FeatureSelectedDT)


				delta_test2 <- matrix(1000,1,Nbr_Features)

				for (IndexFeature in seq(1,Nbr_Features,by=1))
				{
					NewX1 <- matrix(0,Size,2)
					if (IndexFeature != IndexSelectedFeat) #On met 2 features ensemble, toujours le premier sélectionné et puis un différent!
					{
						NewX1[,1] <- dataset_X[,IndexSelectedFeat]
						NewX1[,2] <- dataset_X[,IndexFeature]
						NewY1 <-matrix(0,Size,1)
						NewY1 <- dataset_Y[,IndexFeature]
						
						Y1 <- NewY1
						knn1<-get.knn(NewX1, k=1, algorithm=c("kd_tree")) #POUR CHAQUE X TROUVE LE PLUS PROCHE VOISIN
						NN1 <- knn1$nn.index #DONNE L'INDEX DU POINT LE PLUS PROCHE DANS DATASET_EXTRACT
						M <- Size #M INPUTS POINTS = TAILLE DU DATASET
						delta_test2[1,IndexFeature] <-(sum((Y1- Y1[NN1])^2))/(2*M)
					}
				}

				FeatureSelectedDT2 <- min(delta_test2)
				IndexSelectedFeat2 <- which (delta_test2 == FeatureSelectedDT2)
		

				if (Nbr_Features >2)
				{
					delta_test3 <- matrix(0,1,Nbr_Features)

					for (IndexFeature in seq(1,Nbr_Features,by=1))
					{
						
						NewX1 <- matrix(0,Size,3)
						if (IndexFeature != IndexSelectedFeat) #On met 2 features ensemble, toujours le premier sélectionné et puis un différent!
						{
							if (IndexFeature != IndexSelectedFeat2)
							{
								NewX1[,1] <- dataset_X[,IndexSelectedFeat]
								NewX1[,2] <- dataset_X[,IndexSelectedFeat2]
								NewX1[,3] <- dataset_X[,IndexFeature]
								NewY1 <-matrix(0,Size,1)
								NewY1 <- dataset_Y[,IndexFeature]
								
								Y1 <- NewY1
								knn1<-get.knn(NewX1, k=1, algorithm=c("kd_tree")) #POUR CHAQUE X TROUVE LE PLUS PROCHE VOISIN
								NN1 <- knn1$nn.index #DONNE L'INDEX DU POINT LE PLUS PROCHE DANS DATASET_EXTRACT
								M <- Size #M INPUTS POINTS = TAILLE DU DATASET
								delta_test3[1,IndexFeature] <-(sum((Y1- Y1[NN1])^2))/(2*M)	
							}
						}
					}

					FeatureSelectedDT3 <- min(delta_test3)
					IndexSelectedFeat3 <- which (delta_test3 == FeatureSelectedDT3)

				}

				if (Nbr_Features >3)
				{
					delta_test4 <- matrix(1000,1,Nbr_Features)


					for (IndexFeature in seq(1,Nbr_Features,by=1))
					{
						NewX1 <- matrix(0,Size,4)
						if (IndexFeature != IndexSelectedFeat) #On met 2 features ensemble, toujours le premier sélectionné et puis un différent!
						{
							if (IndexFeature != IndexSelectedFeat2)
							{
								if (IndexFeature != IndexSelectedFeat3)
								{
									NewX1[,1] <- dataset_X[,IndexSelectedFeat]
									NewX1[,2] <- dataset_X[,IndexSelectedFeat2]
									NewX1[,3] <- dataset_X[,IndexSelectedFeat3]
									NewX1[,4] <- dataset_X[,IndexFeature]
									NewY1 <-matrix(0,Size,1)
									NewY1 <- dataset_Y[,IndexFeature]
									
									Y1 <- NewY1
									knn1<-get.knn(NewX1, k=1, algorithm=c("kd_tree")) #POUR CHAQUE X TROUVE LE PLUS PROCHE VOISIN
									NN1 <- knn1$nn.index #DONNE L'INDEX DU POINT LE PLUS PROCHE DANS DATASET_EXTRACT
									M <- Size #M INPUTS POINTS = TAILLE DU DATASET
									delta_test4[1,IndexFeature] <-(sum((Y1- Y1[NN1])^2))/(2*M)	
								}
							}
						}
					}
					
					FeatureSelectedDT4 <- min(delta_test4)
					IndexSelectedFeat4 <- which (delta_test4 == FeatureSelectedDT4)
				}
				
				v = v + 1
				resultatFeat1D_N[1,v] <- Extract_Size
				resultatFeat2D_N[1,v] <- Extract_Size
				resultatFeat3D_N[1,v] <- Extract_Size
				
				resultatFeat1D_N[2,v] <- delta_test[1,1]
				resultatFeat2D_N[2,v] <- delta_test2[1,1]
				resultatFeat3D_N[2,v] <- delta_test3[1,1]
				
				resultatFeat1D_N[3,v] <- delta_test[1,2]
				resultatFeat2D_N[3,v] <- delta_test2[1,2]
				resultatFeat3D_N[3,v] <- delta_test3[1,2]
				
				resultatFeat1D_N[4,v] <- delta_test[1,3]
				resultatFeat2D_N[4,v] <- delta_test2[1,3]
				resultatFeat3D_N[4,v] <- delta_test3[1,3]

			} #Fin Boucle sur taille du dataset
			
			if (rank==1)
			{
				resultatFeat1D_N_Stock1 <- resultatFeat1D_N
				resultatFeat2D_N_Stock1 <- resultatFeat2D_N
				resultatFeat3D_N_Stock1 <- resultatFeat3D_N
			}
			
			if (rank == 2)
			{
				resultatFeat1D_N_Stock2 <- resultatFeat1D_N
				resultatFeat2D_N_Stock2 <- resultatFeat2D_N
				resultatFeat3D_N_Stock2 <- resultatFeat3D_N
			}
			
			if (rank == 3)
			{
				resultatFeat1D_N_Stock3 <- resultatFeat1D_N
				resultatFeat2D_N_Stock3 <- resultatFeat2D_N
				resultatFeat3D_N_Stock3 <- resultatFeat3D_N
			}
			
			if (rank == 4)
			{
				resultatFeat1D_N_Stock4 <- resultatFeat1D_N
				resultatFeat2D_N_Stock4 <- resultatFeat2D_N
				resultatFeat3D_N_Stock4 <- resultatFeat3D_N
			}
		}

		resultatFeat1D_N_Avg <- (resultatFeat1D_N_Stock1+resultatFeat1D_N_Stock2+resultatFeat1D_N_Stock3+resultatFeat1D_N_Stock4)/4
		resultatFeat2D_N_Avg <- (resultatFeat2D_N_Stock1+resultatFeat2D_N_Stock2+resultatFeat2D_N_Stock3+resultatFeat2D_N_Stock4)/4
		resultatFeat3D_N_Avg <- (resultatFeat3D_N_Stock1+resultatFeat3D_N_Stock2+resultatFeat3D_N_Stock3+resultatFeat3D_N_Stock4)/4

		#FS STEP 1 - FEATURE 1
		resultatFeat1D_N_StockFeat1[1,] <-(resultatFeat1D_N_Stock1[2,])
		resultatFeat1D_N_StockFeat1[2,] <-(resultatFeat1D_N_Stock2[2,])
		resultatFeat1D_N_StockFeat1[3,] <-(resultatFeat1D_N_Stock3[2,])
		resultatFeat1D_N_StockFeat1[4,] <-(resultatFeat1D_N_Stock4[2,])

		#FS STEP 1 - FEATURE 2
		resultatFeat1D_N_StockFeat2[1,] <-(resultatFeat1D_N_Stock1[3,])
		resultatFeat1D_N_StockFeat2[2,] <-(resultatFeat1D_N_Stock2[3,])
		resultatFeat1D_N_StockFeat2[3,] <-(resultatFeat1D_N_Stock3[3,])
		resultatFeat1D_N_StockFeat2[4,] <-(resultatFeat1D_N_Stock4[3,])

		#FS STEP 1 - FEATURE 3
		resultatFeat1D_N_StockFeat3[1,] <-(resultatFeat1D_N_Stock1[4,])
		resultatFeat1D_N_StockFeat3[2,] <-(resultatFeat1D_N_Stock2[4,])
		resultatFeat1D_N_StockFeat3[3,] <-(resultatFeat1D_N_Stock3[4,])
		resultatFeat1D_N_StockFeat3[4,] <-(resultatFeat1D_N_Stock4[4,])

		if (DatasetName == "anthrokidsV2.dat")
		{
			for (i in seq(1,20,by=1))
			{
				resultatFeat1D_N_SD_Feat1[1,i] <- sd(resultatFeat1D_N_StockFeat1[,i])
				resultatFeat1D_N_SD_Feat2[1,i] <- sd(resultatFeat1D_N_StockFeat2[,i])
				resultatFeat1D_N_SD_Feat3[1,i] <- sd(resultatFeat1D_N_StockFeat3[,i])
			}
		}
		
		if (DatasetName == "poland_30.mat")
		{
			for (i in seq(1,20,by=1))
			{
				resultatFeat1D_N_SD_Feat1[1,i] <- sd(resultatFeat1D_N_StockFeat1[,i])
				resultatFeat1D_N_SD_Feat2[1,i] <- sd(resultatFeat1D_N_StockFeat2[,i])
				resultatFeat1D_N_SD_Feat3[1,i] <- sd(resultatFeat1D_N_StockFeat3[,i])
			}
		}

		if (DatasetName == "santafe.mat")
		{		
			for (i in seq(1,Nbr_Boucles_Extract,by=1))
			{
				resultatFeat1D_N_SD_Feat1[1,i] <- sd(resultatFeat1D_N_StockFeat1[,i])
				resultatFeat1D_N_SD_Feat2[1,i] <- sd(resultatFeat1D_N_StockFeat2[,i])
				resultatFeat1D_N_SD_Feat3[1,i] <- sd(resultatFeat1D_N_StockFeat3[,i])
			}
		}
		
	}

	if (Criterion == "NLR2")
	{
	
		resultatFeat1D_N <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) 
		resultatFeat1D_N_Stock1 <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) 
		resultatFeat1D_N_Stock2 <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) 
		resultatFeat1D_N_Stock3 <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) 
		resultatFeat1D_N_Stock4 <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) 
		resultatFeat1D_N_Stock5 <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) 
		resultatFeat1D_N_Stock6 <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) 
		resultatFeat1D_N_Stock7 <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) 
		resultatFeat1D_N_Stock8 <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) 
		resultatFeat1D_N_Stock9 <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) 
		resultatFeat1D_N_Stock10 <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract)
		resultatFeat1D_N_Avg <- matrix(0,Nbr_Features+1,Nbr_Boucles_Extract) 

		resultatFeat1D_N_StockFeat1 <- matrix(0,10,Nbr_Boucles_Extract)
		resultatFeat1D_N_StockFeat2 <- matrix(0,10,Nbr_Boucles_Extract)
		resultatFeat1D_N_StockFeat3 <- matrix(0,10,Nbr_Boucles_Extract)

		resultatFeat1D_N_SD_Feat1 <- matrix(0,1,Nbr_Boucles_Extract)
		resultatFeat1D_N_SD_Feat2 <- matrix(0,1,Nbr_Boucles_Extract)
		resultatFeat1D_N_SD_Feat3 <- matrix(0,1,Nbr_Boucles_Extract)
		
		for (rank in seq(1,Loop, by = 1))
		{ 

			v = 0
			for (Extract_Size in seq(Extract_Size_Init, Extract_Size_Fin, by=Extract_Size_Step))
			{


				#ici extraire une partie du dataset initial pour pouvoir faire varier la taille de dataset
				dataset1 <- matrix(0,Size_init,2)
				dataset2 <- matrix(0,Size_init,2)
				dataset3 <- matrix(0,Size_init,2)
				dataset1_extract <- matrix(0,Extract_Size,2)
				dataset2_extract <- matrix(0,Extract_Size,2)
				dataset3_extract <- matrix(0,Extract_Size,2)
				dataset1[,1] <- dataset_X_init[,1]
				dataset1[,2] <- dataset_Y_init
				dataset1_extract <- dataset1[sample(1:nrow(dataset1),Extract_Size,replace=FALSE),]
				dataset2[,1] <- dataset_X_init[,2]
				dataset2[,2] <- dataset_Y_init
				dataset2_extract <- dataset2[sample(1:nrow(dataset2),Extract_Size,replace=FALSE),]
				dataset3[,1] <- dataset_X_init[,3]
				dataset3[,2] <- dataset_Y_init
				dataset3_extract <- dataset3[sample(1:nrow(dataset3),Extract_Size,replace=FALSE),]

				dataset_X = matrix(0,Extract_Size,3)
				dataset_Y = matrix(0,Extract_Size,3)
				dataset_X[,1]<- dataset1_extract[,1]
				dataset_X[,2]<- dataset2_extract[,1]
				dataset_X[,3]<- dataset3_extract[,1]
				dataset_Y[,1]<- dataset1_extract[,2]
				dataset_Y[,2]<- dataset2_extract[,2]
				dataset_Y[,3]<- dataset3_extract[,2]
				Size <- Extract_Size
				
				###### Choix du nombre de K voisins 
				K_Initial <- 6
				K_Final <-round(Size*0.45)
				# if (Size > 500)
					# {K_Final <- 250}#pour accélerer un peu le travail (et plus que 250 n'a pas de sens)
				Step_K <- 20
				Nbr_Boucles_K <- ((K_Final - K_Initial)/Step_K)+1

				##### Initialisation des tableaux de variables
				R_Carre_adj  <- matrix(0,Size,Nbr_Features)
				Mean_R_Carre_adj <- matrix(0,Nbr_Boucles_K,Nbr_Features)
				VecteurK <-matrix(0,Nbr_Boucles_K,1)
				Best_Radj_N <- matrix(0,1,Nbr_Features)
				IndexK <- matrix(0,1,Nbr_Features)
				SelectK_adj <-matrix(0,1,Nbr_Features)

				################################################
				#####BOUCLE SUR LE NOMBRE DE FEATURES
				################################################
				for (IndexFeature in seq(1,Nbr_Features,by=1))
				{
					NewX1 <-matrix(0,Size,1)
					NewX1 <- dataset_X[,IndexFeature]
					NewY1 <-matrix(0,Size,1)
					NewY1 <- dataset_Y[,IndexFeature]
						
					#BOUCLE SUR LE CHOIX DU NOMBRE DE VOISINS K
					############################################
					r = 0
					for (ValeurK in seq(K_Initial, K_Final, by=Step_K)) 
					{
						r = r + 1 #index du K choisi
						VecteurK[r,1] <- ValeurK
						
						NN1_X <- matrix(0,Size,ValeurK)	
						NN1_Y <- matrix(0,Size,ValeurK)

						knn <-get.knn(NewX1, k = ValeurK, algorithm=c("kd_tree")) 	#Trouve les K voisins les plus proches en Y
						NN1 <- knn$nn.index 										#Donne l'index des K points les plus proches 

						for (i in seq(1,ValeurK,by = 1))
						{
							NN1_Y[,i]<-NewY1[NN1[,i]]
							NN1_X[,i]<-NewX1[NN1[,i]]
						}
						
						#BOUCLE SUR CHAQUE POINT DE LA FONCTION
						########################################
						for (Point in seq(1,Size,by = 1))
						{
							results <- lm(NN1_Y[Point,]~NN1_X[Point,])
							R_Carre_adj[Point,IndexFeature] <- summary(results)$adj.r.squared
						}
						Mean_R_Carre_adj[r,IndexFeature]=mean(R_Carre_adj[,IndexFeature])
					}
				}

				for (IndexFeature in seq(1,Nbr_Features,by=1))
				{
					Best_Radj_N[1,IndexFeature] <- max(Mean_R_Carre_adj[,IndexFeature]) #Donne le meilleur R² pour chaque feature
					IndexK[1,IndexFeature] <- which(Mean_R_Carre_adj[,IndexFeature]== Best_Radj_N[1,IndexFeature])
					SelectK_adj[1,IndexFeature] <- K_Initial+((IndexK[1,IndexFeature]-1)*Step_K) 
				}

				FeatureSelectedR2 <- max(Best_Radj_N)
				IndexSelectedFeat <- which (Best_Radj_N == FeatureSelectedR2)
				AssociatedK <-SelectK_adj[,IndexSelectedFeat]
							
				v = v + 1
				resultatFeat1D_N[1,v] <- Extract_Size
				resultatFeat1D_N[2,v] <- Best_Radj_N[1,1]
				resultatFeat1D_N[3,v] <- Best_Radj_N[1,2]
				resultatFeat1D_N[4,v] <- Best_Radj_N[1,3]

			} #Fin Boucle sur taille du dataset
			
			if (rank==1)
			{
				resultatFeat1D_N_Stock1 <- resultatFeat1D_N
			}
			
			if (rank == 2)
			{
				resultatFeat1D_N_Stock2 <- resultatFeat1D_N
			}
			
			if (rank == 3)
			{
				resultatFeat1D_N_Stock3 <- resultatFeat1D_N
			}
			
			if (rank == 4)
			{
				resultatFeat1D_N_Stock4 <- resultatFeat1D_N
			}

			
			if (rank == 5)
			{
				resultatFeat1D_N_Stock5 <- resultatFeat1D_N
			}
			
			if (rank == 6)
			{
				resultatFeat1D_N_Stock6 <- resultatFeat1D_N
			}
			
			if (rank == 7)
			{
				resultatFeat1D_N_Stock7 <- resultatFeat1D_N
			}
			if (rank == 8)
			{
				resultatFeat1D_N_Stock8 <- resultatFeat1D_N
			}
			
			if (rank == 9)
			{
				resultatFeat1D_N_Stock9 <- resultatFeat1D_N
			}
			
			if (rank == 10)
			{
				resultatFeat1D_N_Stock10<- resultatFeat1D_N
			}
		}

			if (Loop==1)
			{
			resultatFeat1D_N_Avg <- (resultatFeat1D_N_Stock1)
			}
			
			if (Loop==2)
			{
			resultatFeat1D_N_Avg <- (resultatFeat1D_N_Stock1+resultatFeat1D_N_Stock2)/2
			}
			
			if (Loop==3)
			{
			resultatFeat1D_N_Avg <- (resultatFeat1D_N_Stock1+resultatFeat1D_N_Stock2+resultatFeat1D_N_Stock3)/3
			}
			
			if (Loop==4)
			{
			resultatFeat1D_N_Avg <- (resultatFeat1D_N_Stock1+resultatFeat1D_N_Stock2+resultatFeat1D_N_Stock3+resultatFeat1D_N_Stock4)/4
			}
			
			if (Loop==5)
			{
			resultatFeat1D_N_Avg <- (resultatFeat1D_N_Stock1+resultatFeat1D_N_Stock2+resultatFeat1D_N_Stock3+resultatFeat1D_N_Stock4+resultatFeat1D_N_Stock5)/5
			}
			if (Loop==10)
			{
			resultatFeat1D_N_Avg <- (resultatFeat1D_N_Stock1+resultatFeat1D_N_Stock2+resultatFeat1D_N_Stock3+resultatFeat1D_N_Stock4+resultatFeat1D_N_Stock5+resultatFeat1D_N_Stock6+resultatFeat1D_N_Stock7+resultatFeat1D_N_Stock8+resultatFeat1D_N_Stock9+resultatFeat1D_N_Stock10)/10
			}

		#FS STEP 1 - FEATURE 1
		resultatFeat1D_N_StockFeat1[1,] <-(resultatFeat1D_N_Stock1[2,])
		resultatFeat1D_N_StockFeat1[2,] <-(resultatFeat1D_N_Stock2[2,])
		resultatFeat1D_N_StockFeat1[3,] <-(resultatFeat1D_N_Stock3[2,])
		resultatFeat1D_N_StockFeat1[4,] <-(resultatFeat1D_N_Stock4[2,])
		resultatFeat1D_N_StockFeat1[5,] <-(resultatFeat1D_N_Stock5[2,])
		resultatFeat1D_N_StockFeat1[6,] <-(resultatFeat1D_N_Stock6[2,])
		resultatFeat1D_N_StockFeat1[7,] <-(resultatFeat1D_N_Stock7[2,])
		resultatFeat1D_N_StockFeat1[8,] <-(resultatFeat1D_N_Stock8[2,])
		resultatFeat1D_N_StockFeat1[9,] <-(resultatFeat1D_N_Stock9[2,])
		resultatFeat1D_N_StockFeat1[10,] <-(resultatFeat1D_N_Stock10[2,])

		#FS STEP 1 - FEATURE 2
		resultatFeat1D_N_StockFeat2[1,] <-(resultatFeat1D_N_Stock1[3,])
		resultatFeat1D_N_StockFeat2[2,] <-(resultatFeat1D_N_Stock2[3,])
		resultatFeat1D_N_StockFeat2[3,] <-(resultatFeat1D_N_Stock3[3,])
		resultatFeat1D_N_StockFeat2[4,] <-(resultatFeat1D_N_Stock4[3,])
		resultatFeat1D_N_StockFeat2[5,] <-(resultatFeat1D_N_Stock5[3,])
		resultatFeat1D_N_StockFeat2[6,] <-(resultatFeat1D_N_Stock6[3,])
		resultatFeat1D_N_StockFeat2[7,] <-(resultatFeat1D_N_Stock7[3,])
		resultatFeat1D_N_StockFeat2[8,] <-(resultatFeat1D_N_Stock8[3,])
		resultatFeat1D_N_StockFeat2[9,] <-(resultatFeat1D_N_Stock9[3,])
		resultatFeat1D_N_StockFeat2[10,] <-(resultatFeat1D_N_Stock10[3,])

		#FS STEP 1 - FEATURE 3
		resultatFeat1D_N_StockFeat3[1,] <-(resultatFeat1D_N_Stock1[4,])
		resultatFeat1D_N_StockFeat3[2,] <-(resultatFeat1D_N_Stock2[4,])
		resultatFeat1D_N_StockFeat3[3,] <-(resultatFeat1D_N_Stock3[4,])
		resultatFeat1D_N_StockFeat3[4,] <-(resultatFeat1D_N_Stock4[4,])
		resultatFeat1D_N_StockFeat3[5,] <-(resultatFeat1D_N_Stock5[4,])
		resultatFeat1D_N_StockFeat3[6,] <-(resultatFeat1D_N_Stock6[4,])
		resultatFeat1D_N_StockFeat3[7,] <-(resultatFeat1D_N_Stock7[4,])
		resultatFeat1D_N_StockFeat3[8,] <-(resultatFeat1D_N_Stock8[4,])
		resultatFeat1D_N_StockFeat3[9,] <-(resultatFeat1D_N_Stock9[4,])
		resultatFeat1D_N_StockFeat3[10,] <-(resultatFeat1D_N_Stock10[4,])


		for (i in seq(1,Nbr_Boucles_Extract,by=1))
		{
			resultatFeat1D_N_SD_Feat1[1,i] <- sd(resultatFeat1D_N_StockFeat1[,i])
			resultatFeat1D_N_SD_Feat2[1,i] <- sd(resultatFeat1D_N_StockFeat2[,i])
			resultatFeat1D_N_SD_Feat3[1,i] <- sd(resultatFeat1D_N_StockFeat3[,i])
		}
		

	}

	if (DatasetName == "anthrokidsV2.dat")
	{
		if (Criterion == "MI")
		{
			par(mfrow=c(1,1))
			plot(resultatFeat1D_N_Avg[1,],resultatFeat1D_N_Avg[2,],type='l',log='x',lty=1,lwd=2,cex.axis=1.5,ylim=c(0,1.5))
			lines(resultatFeat1D_N_Avg[1,],resultatFeat1D_N_Avg[3,],pch=10, lty=4)
			lines(resultatFeat1D_N_Avg[1,],resultatFeat1D_N_Avg[4,],pch=10, lty=3,lwd=2)

			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[2,]+0.96*resultatFeat1D_N_SD_Feat1),lty=1,lwd=2,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[3,]+0.96*resultatFeat1D_N_SD_Feat2),pch=10, lty=4,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[4,]+0.96*resultatFeat1D_N_SD_Feat3),pch=10, lty=3,lwd=2,col='gray')

			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[2,]-0.96*resultatFeat1D_N_SD_Feat1),lty=1,lwd=2,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[3,]-0.96*resultatFeat1D_N_SD_Feat2),pch=10, lty=4,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[4,]-0.96*resultatFeat1D_N_SD_Feat3),pch=10, lty=3,lwd=2,col='gray')

		}
		
		if (Criterion == "DT") 
		{
			par(mfrow=c(1,1))
			plot(resultatFeat1D_N_Avg[1,],resultatFeat1D_N_Avg[2,],type='l',log='x',lty=1,lwd=2,cex.axis=1.5,ylim=c(0,1.2))
			lines(resultatFeat1D_N_Avg[1,],resultatFeat1D_N_Avg[3,],pch=10, lty=4)
			lines(resultatFeat1D_N_Avg[1,],resultatFeat1D_N_Avg[4,],pch=10, lty=3,lwd=2)

			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[2,]+0.96*resultatFeat1D_N_SD_Feat1),lty=1,lwd=2,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[3,]+0.96*resultatFeat1D_N_SD_Feat2),pch=10, lty=4,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[4,]+0.96*resultatFeat1D_N_SD_Feat3),pch=10, lty=3,lwd=2,col='gray')

			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[2,]-0.96*resultatFeat1D_N_SD_Feat1),lty=1,lwd=2,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[3,]-0.96*resultatFeat1D_N_SD_Feat2),pch=10, lty=4,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[4,]-0.96*resultatFeat1D_N_SD_Feat3),pch=10, lty=3,lwd=2,col='gray')

		}

		if (Criterion == "NLR2") 
		{
			par(mfrow=c(1,1))
			plot(resultatFeat1D_N_Avg[1,],resultatFeat1D_N_Avg[2,],type='l',log='x',lty=1,lwd=2,cex.axis=1.5,ylim=c(-0.07,0.8))
			lines(resultatFeat1D_N_Avg[1,],resultatFeat1D_N_Avg[3,],pch=10, lty=4)
			lines(resultatFeat1D_N_Avg[1,],resultatFeat1D_N_Avg[4,],pch=10, lty=3,lwd=2)

			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[2,]+0.96*resultatFeat1D_N_SD_Feat1),lty=1,lwd=2,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[3,]+0.96*resultatFeat1D_N_SD_Feat2),pch=10, lty=4,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[4,]+0.96*resultatFeat1D_N_SD_Feat3),pch=10, lty=3,lwd=2,col='gray')

			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[2,]-0.96*resultatFeat1D_N_SD_Feat1),lty=1,lwd=2,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[3,]-0.96*resultatFeat1D_N_SD_Feat2),pch=10, lty=4,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[4,]-0.96*resultatFeat1D_N_SD_Feat3),pch=10, lty=3,lwd=2,col='gray')

		}
	}

	if (DatasetName == "poland_30.mat")
	{
		if (Criterion == "MI") 
		{
			par(mfrow=c(1,1))
			plot(resultatFeat1D_N_Avg[1,],resultatFeat1D_N_Avg[2,],type='l',log='x',lty=1,lwd=2,cex.axis=1.5,ylim=c(0,1.5))
			lines(resultatFeat1D_N_Avg[1,],resultatFeat1D_N_Avg[3,],pch=10, lty=4)
			lines(resultatFeat1D_N_Avg[1,],resultatFeat1D_N_Avg[4,],pch=10, lty=3,lwd=2)

			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[2,]+0.96*resultatFeat1D_N_SD_Feat1),lty=1,lwd=2,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[3,]+0.96*resultatFeat1D_N_SD_Feat2),pch=10, lty=4,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[4,]+0.96*resultatFeat1D_N_SD_Feat3),pch=10, lty=3,lwd=2,col='gray')

			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[2,]-0.96*resultatFeat1D_N_SD_Feat1),lty=1,lwd=2,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[3,]-0.96*resultatFeat1D_N_SD_Feat2),pch=10, lty=4,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[4,]-0.96*resultatFeat1D_N_SD_Feat3),pch=10, lty=3,lwd=2,col='gray')

		}
		
		if (Criterion == "DT") 
		{
			par(mfrow=c(1,1))
			plot(resultatFeat1D_N_Avg[1,],resultatFeat1D_N_Avg[2,],type='l',log='x',lty=1,lwd=2,cex.axis=1.5,ylim=c(0,1))
			lines(resultatFeat1D_N_Avg[1,],resultatFeat1D_N_Avg[3,],pch=10, lty=4)
			lines(resultatFeat1D_N_Avg[1,],resultatFeat1D_N_Avg[4,],pch=10, lty=3,lwd=2)

			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[2,]+0.96*resultatFeat1D_N_SD_Feat1),lty=1,lwd=2,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[3,]+0.96*resultatFeat1D_N_SD_Feat2),pch=10, lty=4,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[4,]+0.96*resultatFeat1D_N_SD_Feat3),pch=10, lty=3,lwd=2,col='gray')

			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[2,]-0.96*resultatFeat1D_N_SD_Feat1),lty=1,lwd=2,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[3,]-0.96*resultatFeat1D_N_SD_Feat2),pch=10, lty=4,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[4,]-0.96*resultatFeat1D_N_SD_Feat3),pch=10, lty=3,lwd=2,col='gray')
			
		}

		if (Criterion == "NLR2")
		{
			par(mfrow=c(1,1))
			plot(resultatFeat1D_N_Avg[1,],resultatFeat1D_N_Avg[2,],type='l',log='x',lty=1,lwd=2,cex.axis=1.5,ylim=c(0,0.7))
			lines(resultatFeat1D_N_Avg[1,],resultatFeat1D_N_Avg[3,],pch=10, lty=4)
			lines(resultatFeat1D_N_Avg[1,],resultatFeat1D_N_Avg[4,],pch=10, lty=3,lwd=2)

			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[2,]+0.96*resultatFeat1D_N_SD_Feat1),lty=1,lwd=2,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[3,]+0.96*resultatFeat1D_N_SD_Feat2),pch=10, lty=4,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[4,]+0.96*resultatFeat1D_N_SD_Feat3),pch=10, lty=3,lwd=2,col='gray')

			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[2,]-0.96*resultatFeat1D_N_SD_Feat1),lty=1,lwd=2,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[3,]-0.96*resultatFeat1D_N_SD_Feat2),pch=10, lty=4,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[4,]-0.96*resultatFeat1D_N_SD_Feat3),pch=10, lty=3,lwd=2,col='gray')
		}
	}

	if (DatasetName == "santafe.mat")
	{
		if (Criterion == "MI") 
		{
			par(mfrow=c(1,1))
			plot(resultatFeat1D_N_Avg[1,],resultatFeat1D_N_Avg[2,],type='l',log='x',lty=1,lwd=2,cex.axis=1.5,ylim=c(0,1.2))
			lines(resultatFeat1D_N_Avg[1,],resultatFeat1D_N_Avg[3,],pch=10, lty=4)
			lines(resultatFeat1D_N_Avg[1,],resultatFeat1D_N_Avg[4,],pch=10, lty=3,lwd=2)

			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[2,]+0.96*resultatFeat1D_N_SD_Feat1),lty=1,lwd=2,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[3,]+0.96*resultatFeat1D_N_SD_Feat2),pch=10, lty=4,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[4,]+0.96*resultatFeat1D_N_SD_Feat3),pch=10, lty=3,lwd=2,col='gray')

			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[2,]-0.96*resultatFeat1D_N_SD_Feat1),lty=1,lwd=2,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[3,]-0.96*resultatFeat1D_N_SD_Feat2),pch=10, lty=4,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[4,]-0.96*resultatFeat1D_N_SD_Feat3),pch=10, lty=3,lwd=2,col='gray')

		}
		
		if (Criterion == "DT") 
		{
			par(mfrow=c(1,1))
			plot(resultatFeat1D_N_Avg[1,],resultatFeat1D_N_Avg[2,],type='l',log='x',lty=1,lwd=2,cex.axis=1.5,ylim=c(200,2700))
			lines(resultatFeat1D_N_Avg[1,],resultatFeat1D_N_Avg[3,],pch=10, lty=4)
			lines(resultatFeat1D_N_Avg[1,],resultatFeat1D_N_Avg[4,],pch=10, lty=3,lwd=2)

			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[2,]+0.96*resultatFeat1D_N_SD_Feat1),lty=1,lwd=2,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[3,]+0.96*resultatFeat1D_N_SD_Feat2),pch=10, lty=4,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[4,]+0.96*resultatFeat1D_N_SD_Feat3),pch=10, lty=3,lwd=2,col='gray')

			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[2,]-0.96*resultatFeat1D_N_SD_Feat1),lty=1,lwd=2,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[3,]-0.96*resultatFeat1D_N_SD_Feat2),pch=10, lty=4,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[4,]-0.96*resultatFeat1D_N_SD_Feat3),pch=10, lty=3,lwd=2,col='gray')

		}

		if (Criterion == "NLR2")
		{
			par(mfrow=c(1,1))
			plot(resultatFeat1D_N_Avg[1,],resultatFeat1D_N_Avg[2,],type='l',log='x',lty=1,lwd=2,cex.axis=1.5,ylim=c(-0.13,0.095))
			lines(resultatFeat1D_N_Avg[1,],resultatFeat1D_N_Avg[3,],pch=10, lty=4)
			lines(resultatFeat1D_N_Avg[1,],resultatFeat1D_N_Avg[4,],pch=10, lty=3,lwd=2)

			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[2,]+0.96*resultatFeat1D_N_SD_Feat1),lty=1,lwd=2,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[3,]+0.96*resultatFeat1D_N_SD_Feat2),pch=10, lty=4,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[4,]+0.96*resultatFeat1D_N_SD_Feat3),pch=10, lty=3,lwd=2,col='gray')

			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[2,]-0.96*resultatFeat1D_N_SD_Feat1),lty=1,lwd=2,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[3,]-0.96*resultatFeat1D_N_SD_Feat2),pch=10, lty=4,col='gray')
			lines(resultatFeat1D_N_Avg[1,],(resultatFeat1D_N_Avg[4,]-0.96*resultatFeat1D_N_SD_Feat3),pch=10, lty=3,lwd=2,col='gray')

		}
	}
}
