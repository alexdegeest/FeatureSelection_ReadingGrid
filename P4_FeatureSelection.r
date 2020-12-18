
P4_FeatureSelection <- function(DatasetName,Criterion)
{
#############################################################################
# Test a criterion on a dataset DatasetName for property4 "Feature Selection"
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
	
	if (Criterion == "NLR2")
	{
		if (DatasetName == "santafe.mat")
		{
			Extract_Prop = 0.6
			Extract_Size = round(Size*Extract_Prop)
			dataset_Xextract <- dataset_X[sample(1:nrow(dataset_X),Extract_Size,replace=FALSE),]
			dataset_Yextract <- dataset_Y[sample(1:nrow(dataset_Y),Extract_Size,replace=FALSE),]
			Size <- Extract_Size
			dataset_X <-dataset_Xextract
			dataset_Y <-dataset_Yextract
		}

		###### Choix du nombre de K voisins 
		K_Initial <-20
		K_Final <-round(Size*0.5)
		Step_K <- 20
		Nbr_Boucles_K <- ((K_Final - K_Initial)/Step_K)+1	
	}	

	NewY1 <-matrix(0,Size,1)
	NewY1 <- dataset_Y


	if (Criterion == "MI") 
	{
		K = 6
		resultatMI1 <- matrix(0,1,Nbr_Features)

		for (IndexFeature in seq(1,Nbr_Features,by=1))
		{
			NewX1 <-matrix(0,Size,1)
			NewX1 <- dataset_X[,IndexFeature]
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
							
							MI_test1 <- MIxnyn4(matrix(NewX1,ncol=4), matrix(NewY1,ncol=1),K)
							resultatMI4[1,IndexFeature]<- MI_test1	
						}
					}
				}
			}
			
			FeatureSelectedMI4 <- max(resultatMI4)
			IndexSelectedFeat4 <- which (resultatMI4 == FeatureSelectedMI4)
		}
	}
	
	if (Criterion == "DT")
	{	
		delta_test <- matrix(1000,1,Nbr_Features)

		for (IndexFeature in seq(1,Nbr_Features,by=1))
		{
			NewX1 <-matrix(0,Size,1)
			NewX1 <- dataset_X[,IndexFeature]
			
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
			delta_test3 <- matrix(1000,1,Nbr_Features)

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

		
	}

	if (Criterion == "NLR2")
	{
	
		R_Carre_adj  <- matrix(0,Size,Nbr_Features)
		Mean_R_Carre_adj <- matrix(0,Nbr_Boucles_K,Nbr_Features)
		VecteurK <-matrix(0,Nbr_Boucles_K,1)
		Best_Radj_N <- matrix(0,1,Nbr_Features)
		IndexK <- matrix(0,1,Nbr_Features)
		SelectK_adj <-matrix(0,1,Nbr_Features)
		
		for (IndexFeature in seq(1,Nbr_Features,by=1))
		{
			NewX1 <-matrix(0,Size,1)
			NewX1 <- dataset_X[,IndexFeature]
			
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


		R_Carre_adj2  <- matrix(0,Size,Nbr_Features)
		Mean_R_Carre_adj2 <- matrix(0,Nbr_Boucles_K,Nbr_Features)
		Best_Radj_N2 <- matrix(0,1,Nbr_Features)
		IndexK2 <- matrix(0,1,Nbr_Features)
		SelectK_adj2 <-matrix(0,1,Nbr_Features)

		for (IndexFeature in seq(1,Nbr_Features,by=1))
		{
			NewX1 <- matrix(0,Size,2)
			if (IndexFeature != IndexSelectedFeat) #On met 2 features ensemble, toujours le premier sélectionné et puis un différent!
			{
				NewX1[,1] <- dataset_X[,IndexSelectedFeat]
				NewX1[,2] <- dataset_X[,IndexFeature]
			}

			#BOUCLE SUR LE CHOIX DU NOMBRE DE VOISINS K
			############################################
			r = 0
			for (ValeurK in seq(K_Initial, K_Final, by = Step_K)) 
			{
				r = r + 1 #index du K choisi
				VecteurK[r,1] <- ValeurK
				
				NN1_Y <- matrix(0,Size,ValeurK)	
				NN1_X1 <- matrix(0,Size,ValeurK)
				NN1_X2 <- matrix(0,Size,ValeurK)

				knn <-get.knn(NewX1, k = ValeurK, algorithm=c("kd_tree")) 	#Trouve les K voisins les plus proches en X
				NN1 <- knn$nn.index 										#Donne l'index des K points les plus proches 
				
				for (i in seq(1,ValeurK,by = 1))
				{
					NN1_Y[,i]<-NewY1[NN1[,i]]
					NN1_X1[,i]<-NewX1[NN1[,i],1]
					NN1_X2[,i]<-NewX1[NN1[,i],2]
				}
				
				#BOUCLE SUR CHAQUE POINT DE LA FONCTION
				########################################
				
				for (Point in seq(1,Size,by = 1))
				{
					results2 <- lm(NN1_Y[Point,]~NN1_X1[Point,]+NN1_X2[Point,])
					R_Carre_adj2[Point,IndexFeature] <- summary(results2)$adj.r.squared
				}
				Mean_R_Carre_adj2[r,IndexFeature]=mean(R_Carre_adj2[,IndexFeature])
			}	
		}

		for (IndexFeature in seq(1,Nbr_Features,by=1))
		{
			if (IndexFeature != IndexSelectedFeat)
			{
				Best_Radj_N2[1,IndexFeature] <- max(Mean_R_Carre_adj2[,IndexFeature]) #Donne le meilleur R² pour chaque feature
				IndexK2[1,IndexFeature] <- which(Mean_R_Carre_adj2[,IndexFeature]== Best_Radj_N2[1,IndexFeature])
				SelectK_adj2[1,IndexFeature] <- K_Initial+((IndexK2[1,IndexFeature]-1)*Step_K) 
			}
		}

		FeatureSelectedR2_2 <- max(Best_Radj_N2)
		IndexSelectedFeat2 <- which (Best_Radj_N2 == FeatureSelectedR2_2)
		AssociatedK2 <-SelectK_adj2[,IndexSelectedFeat2]



		if (Nbr_Features >2)
		{
			R_Carre_adj3  <- matrix(0,Size,Nbr_Features)
			Mean_R_Carre_adj3 <- matrix(0,Nbr_Boucles_K,Nbr_Features)
			Best_Radj_N3 <- matrix(0,1,Nbr_Features)
			IndexK3 <- matrix(0,1,Nbr_Features)
			SelectK_adj3 <-matrix(0,1,Nbr_Features)

			# Refaire les groupes de travail : prendre le feature selectionné et le coller aux features non sélectionnés pour obtenir des groupes de 2

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
					}
				}

				#BOUCLE SUR LE CHOIX DU NOMBRE DE VOISINS K
				############################################
				r = 0
				for (ValeurK in seq(K_Initial, K_Final, by = Step_K)) 
				{
					r = r + 1 #index du K choisi
					VecteurK[r,1] <- ValeurK
					
					NN1_Y <- matrix(0,Size,ValeurK)	
					NN1_X1 <- matrix(0,Size,ValeurK)
					NN1_X2 <- matrix(0,Size,ValeurK)
					NN1_X3 <- matrix(0,Size,ValeurK)

					knn <-get.knn(NewX1, k = ValeurK, algorithm=c("kd_tree")) 	#Trouve les K voisins les plus proches en Y
					NN1 <- knn$nn.index 										#Donne l'index des K points les plus proches 
					
					for (i in seq(1,ValeurK,by = 1))
					{
						NN1_Y[,i]<-NewY1[NN1[,i]]
						NN1_X1[,i]<-NewX1[NN1[,i],1]
						NN1_X2[,i]<-NewX1[NN1[,i],2]
						NN1_X3[,i]<-NewX1[NN1[,i],3]
					}
					
					#BOUCLE SUR CHAQUE POINT DE LA FONCTION
					########################################
					
					for (Point in seq(1,Size,by = 1))
					{
						results3 <- lm(NN1_Y[Point,]~NN1_X1[Point,]+NN1_X2[Point,]+NN1_X3[Point,])
						R_Carre_adj3[Point,IndexFeature] <- summary(results3)$adj.r.squared
					}
					Mean_R_Carre_adj3[r,IndexFeature]=mean(R_Carre_adj3[,IndexFeature])
				}	
			}

			for (IndexFeature in seq(1,Nbr_Features,by=1))
			{
				if (IndexFeature != IndexSelectedFeat)
				{
					if (IndexFeature != IndexSelectedFeat2)
					{
						Best_Radj_N3[1,IndexFeature] <- max(Mean_R_Carre_adj3[,IndexFeature]) #Donne le meilleur R² pour chaque feature
						IndexK3[1,IndexFeature] <- which(Mean_R_Carre_adj3[,IndexFeature]== Best_Radj_N3[1,IndexFeature])
						SelectK_adj3[1,IndexFeature] <- K_Initial+((IndexK3[1,IndexFeature]-1)*Step_K) 
					}
				}
			}

			FeatureSelectedR2_3 <- max(Best_Radj_N3)
			IndexSelectedFeat3 <- which (Best_Radj_N3 == FeatureSelectedR2_3)
			AssociatedK3 <-SelectK_adj3[,IndexSelectedFeat3]


		}

		if (Nbr_Features >3)
		{
			R_Carre_adj4  <- matrix(0,Size,Nbr_Features)
			Mean_R_Carre_adj4 <- matrix(0,Nbr_Boucles_K,Nbr_Features)
			Best_Radj_N4 <- matrix(0,1,Nbr_Features)
			IndexK4 <- matrix(0,1,Nbr_Features)
			SelectK_adj4 <-matrix(0,1,Nbr_Features)

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
						}
					}
				}

				#BOUCLE SUR LE CHOIX DU NOMBRE DE VOISINS K
				############################################
				r = 0
				for (ValeurK in seq(K_Initial, K_Final, by = Step_K)) 
				{
					r = r + 1 #index du K choisi
					VecteurK[r,1] <- ValeurK
					
					NN1_Y <- matrix(0,Size,ValeurK)	
					NN1_X1 <- matrix(0,Size,ValeurK)
					NN1_X2 <- matrix(0,Size,ValeurK)
					NN1_X3 <- matrix(0,Size,ValeurK)
					NN1_X4 <- matrix(0,Size,ValeurK)

					knn <-get.knn(NewX1, k = ValeurK, algorithm=c("kd_tree")) 	#Trouve les K voisins les plus proches en Y
					NN1 <- knn$nn.index 										#Donne l'index des K points les plus proches 
					
					for (i in seq(1,ValeurK,by = 1))
					{
						NN1_Y[,i]<-NewY1[NN1[,i]]
						NN1_X1[,i]<-NewX1[NN1[,i],1]
						NN1_X2[,i]<-NewX1[NN1[,i],2]
						NN1_X3[,i]<-NewX1[NN1[,i],3]
						NN1_X4[,i]<-NewX1[NN1[,i],4]
					}
					
					#BOUCLE SUR CHAQUE POINT DE LA FONCTION
					########################################
					
					for (Point in seq(1,Size,by = 1))
					{
						results4 <- lm(NN1_Y[Point,]~NN1_X1[Point,]+NN1_X2[Point,]+NN1_X3[Point,]+ NN1_X4[Point,])
						R_Carre_adj4[Point,IndexFeature] <- summary(results4)$adj.r.squared
					}
					Mean_R_Carre_adj4[r,IndexFeature]=mean(R_Carre_adj4[,IndexFeature])
				}	
			}

			for (IndexFeature in seq(1,Nbr_Features,by=1))
			{
				if (IndexFeature != IndexSelectedFeat)
				{
					if (IndexFeature != IndexSelectedFeat2)
					{
						if (IndexFeature != IndexSelectedFeat3)
						{
							Best_Radj_N4[1,IndexFeature] <- max(Mean_R_Carre_adj4[,IndexFeature]) #Donne le meilleur R² pour chaque feature
							IndexK4[1,IndexFeature] <- which(Mean_R_Carre_adj4[,IndexFeature]== Best_Radj_N4[1,IndexFeature])
							SelectK_adj4[1,IndexFeature] <- K_Initial+((IndexK4[1,IndexFeature]-1)*Step_K) 
						}
					}
				}
			}
			FeatureSelectedR2_4 <- max(Best_Radj_N4)
			IndexSelectedFeat4 <- which (Best_Radj_N4 == FeatureSelectedR2_4)
			AssociatedK4 <-SelectK_adj4[,IndexSelectedFeat4]
		}		
	}

	if (DatasetName == "anthrokidsV2.dat")
	{
		if (Criterion == "MI")
		{
			par(mfrow=c(1,1))
			MIscore = matrix(0,3,2)
			MIscore[1,1]=resultatMI1[IndexSelectedFeat]
			MIscore[2,1]=resultatMI2[IndexSelectedFeat2]
			MIscore[3,1]=resultatMI3[IndexSelectedFeat3]
			MIscore[1,2]=1
			MIscore[2,2]=2
			MIscore[3,2]=3
			plot(MIscore[,2],MIscore[,1],ylim=c(0,2.5),type='o',lty=2,lwd=2,cex.axis=2,xaxt = "n")
			axis(1, at = 1:3,cex.axis=2)
		}


		if (Criterion == "DT")
		{
			DTscore = matrix(0,3,2)
			DTscore[1,1]=delta_test[IndexSelectedFeat]
			DTscore[2,1]=delta_test2[IndexSelectedFeat2]
			DTscore[3,1]=delta_test3[IndexSelectedFeat3]
			DTscore[1,2]=1
			DTscore[2,2]=2
			DTscore[3,2]=3
			plot(DTscore[,2],DTscore[,1],ylim=c(0,0.1),type='o',lty=2,lwd=2,cex.axis=2,xaxt = "n")
			axis(1, at = 1:3,cex.axis=2)			
		}

		if (Criterion == "NLR2") 
		{
			par(mfrow=c(1,1))
			Rscore = matrix(0,3,2)
			Rscore[1,1]=Best_Radj_N[IndexSelectedFeat]
			Rscore[2,1]=Best_Radj_N2[IndexSelectedFeat2]
			Rscore[3,1]=Best_Radj_N3[IndexSelectedFeat3]
			Rscore[1,2]=1
			Rscore[2,2]=2
			Rscore[3,2]=3
			plot(Rscore[,2],Rscore[,1],ylim=c(0.7,0.95),type='o',lty=2,lwd=2,cex.axis=2,xaxt = "n")
			axis(1, at = 1:3,cex.axis=2)

		}
	}

	if (DatasetName == "poland_30.mat")
	{
		if (Criterion == "MI") 
		{
			par(mfrow=c(1,1))
			MIscore = matrix(0,3,2)
			MIscore[1,1]=resultatMI1[IndexSelectedFeat]
			MIscore[2,1]=resultatMI2[IndexSelectedFeat2]
			MIscore[3,1]=resultatMI3[IndexSelectedFeat3]
			MIscore[1,2]=1
			MIscore[2,2]=2
			MIscore[3,2]=3
			plot(MIscore[,2],MIscore[,1],ylim=c(1,1.4),type='o',lty=2,lwd=2,cex.axis=2,xaxt = "n")
			axis(1, at = 1:3,cex.axis=2)
		}
		
		if (Criterion == "DT")
		{
			par(mfrow=c(1,1))
			DTscore = matrix(0,3,2)
			DTscore[1,1]=delta_test[IndexSelectedFeat]
			DTscore[2,1]=delta_test2[IndexSelectedFeat2]
			DTscore[3,1]=delta_test3[IndexSelectedFeat3]
			DTscore[1,2]=1
			DTscore[2,2]=2
			DTscore[3,2]=3
			plot(DTscore[,2],DTscore[,1],ylim=c(0,0.2),type='o',lty=2,lwd=2,cex.axis=2,xaxt = "n")
			axis(1, at = 1:3,cex.axis=2)			
		}

		if (Criterion == "NLR2")
		{
			par(mfrow=c(1,1))
			Rscore = matrix(0,3,2)
			Rscore[1,1]=Best_Radj_N[IndexSelectedFeat]
			Rscore[2,1]=Best_Radj_N2[IndexSelectedFeat2]
			Rscore[3,1]=Best_Radj_N3[IndexSelectedFeat3]
			Rscore[1,2]=1
			Rscore[2,2]=2
			Rscore[3,2]=3
			plot(Rscore[,2],Rscore[,1],ylim=c(0.5,0.75),type='o',lty=2,lwd=2,cex.axis=2,xaxt = "n")
			axis(1, at = 1:3,cex.axis=2)
			

		}
	}

	if (DatasetName == "santafe.mat")
	{
		par(mfrow=c(1,1))
		MIscore = matrix(0,3,2)
		
		if (Criterion == "MI") 
		{
			MIscore[1,1]=resultatMI1[IndexSelectedFeat]
			MIscore[2,1]=resultatMI2[IndexSelectedFeat2]
			MIscore[3,1]=resultatMI3[IndexSelectedFeat3]
			MIscore[1,2]=1
			MIscore[2,2]=2
			MIscore[3,2]=3
			plot(MIscore[,2],MIscore[,1],ylim=c(0,2.5),type='o',lty=2,lwd=2,cex.axis=2,xaxt = "n")
		}
		
		if (Criterion == "DT")
		{
			DTscore[1,1]=delta_test[IndexSelectedFeat]
			DTscore[2,1]=delta_test2[IndexSelectedFeat2]
			DTscore[3,1]=delta_test3[IndexSelectedFeat3]
			DTscore[1,2]=1
			DTscore[2,2]=2
			DTscore[3,2]=3
			plot(DTscore[,2],DTscore[,1],ylim=c(0,1500),type='o',lty=2,lwd=2,cex.axis=2,xaxt = "n")
		}

		if (Criterion == "NLR2")
		{
			Rscore[1,1]=Best_Radj_N[IndexSelectedFeat]
			Rscore[2,1]=Best_Radj_N2[IndexSelectedFeat2]
			Rscore[3,1]=Best_Radj_N3[IndexSelectedFeat3]
			Rscore[1,2]=1
			Rscore[2,2]=2
			Rscore[3,2]=3
			plot(Rscore[,2],Rscore[,1],ylim=c(0,0.007),type='o',lty=2,lwd=2,cex.axis=2,xaxt = "n")
		}
		
		axis(1, at = 1:3,cex.axis=2)
	}
}
