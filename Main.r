###########################
##### INITIALISATION ######
###########################

library(R.matlab)
library(FNN)

#3°) MILCA FROM KRASKOV
####################################################
MIxnyn4 <- function(X,Y,kneig)
{
# Copyright 2009 Alexander Kraskov, Harald Stoegbauer, Peter Grassberger
#--------------------------------------------------------------------------
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or 
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should receive a copy of the GNU General Public License
# along with this program.  See also <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------- 
# Contacts:
#
# Harald Stoegbauer <h.stoegbauer@gmail.com>
# Alexander Kraskov <alexander.kraskov@gmail.com>
#--------------------------------------------------------------------------
# Please reference
# 
# A. Kraskov, H. Stogbauer, and P. Grassberger,
# Estimating mutual information.
# Phys. Rev. E 69 (6) 066138, 2004
#
# in your published research.
# R Translation : Alexandra Degeest, 2014 (alexandra.degeest@gmail.com)
# Calculate MI value between 2 vector of any dimension (rectangular
# version)
# x....input data mxn   m...channelnummer  n...sampling points  m<<n
# kneig... k nearest neigbor for MI algorithm
#default-values
if (exists("kneig")==FALSE)
{kneig <- 6}
Ndx <- ncol(X)
Ndy <- ncol(Y)
N <- nrow(X)
zwsp <- cbind(X,Y)
write.table(zwsp,file="zwspMIxnyn.txt", row.names = FALSE, col.names = FALSE)  
unout<-system(paste('MIxnyn.exe zwspMIxnyn.txt',Ndx, Ndy, N, kneig),intern=TRUE)
miout <- as.numeric(unout)
}


ReadingGrid <- function(DatasetName,Criterion,PropertyNumber)
{
# Copyright 2020 Alexandra Degeest
#--------------------------------------------------------------------------
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or 
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should receive a copy of the GNU General Public License
# along with this program.  See also <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------- 
# Contacts:
#
# Alexandra Degeest - alexandra.degeest@gmail.com
#--------------------------------------------------------------------------
# Please reference
# 
# Alexandra Degeest, Benoît Frénay, Michel Verleysen
# Reading grid for feature selection relevance criteria in regression
# Pattern Recognition Letter, 2021
#
# in your published research.

	if (PropertyNumber == "1")
	{
		print("Property 1 : Multivariate Behaviour : No test for P1")
	}
	if (PropertyNumber == "2")
	{
		print("Property 2 : Nonlinearity : No test for P2")
	}
	if (PropertyNumber == "3")
	{
		print("Property 3 : k stability")
		if (Criterion == "MI")
		{
			if (DatasetName == "anthrokidsV2.dat")	{P3_KStability("anthrokidsV2.dat","MI")}
			if (DatasetName == "poland_30.mat")		{P3_KStability("poland_30.mat","MI")}
			if (DatasetName == "santafe.mat")		{P3_KStability("santafe.mat","MI")}
		}		
		if (Criterion == "NLR2")
		{
			if (DatasetName == "anthrokidsV2.dat")	{P3_KStability("anthrokidsV2.dat","NLR2")}
			if (DatasetName == "poland_30.mat")		{P3_KStability("poland_30.mat","NLR2")}
			if (DatasetName == "santafe.mat")		{P3_KStability("santafe.mat","NLR2")}
		}
		if (Criterion == "DT")
		{
			print("k is set to 1 for DT by definition")
		}
	}

	if (PropertyNumber == "4")
	{
		print("Property 4 : Feature Selection")
		if (Criterion == "MI")
		{
			if (DatasetName == "anthrokidsV2.dat")	{P4_FeatureSelection("anthrokidsV2.dat","MI")}
			if (DatasetName == "poland_30.mat")		{P4_FeatureSelection("poland_30.mat","MI")}
			if (DatasetName == "santafe.mat")		{P4_FeatureSelection("santafe.mat","MI")}
		}
		if (Criterion == "DT")
		{
			if (DatasetName == "anthrokidsV2.dat")	{P4_FeatureSelection("anthrokidsV2.dat","DT")}
			if (DatasetName == "poland_30.mat")		{P4_FeatureSelection("poland_30.mat","DT")}
			if (DatasetName == "santafe.mat")		{P4_FeatureSelection("santafe.mat","DT")}
		}
		if (Criterion == "NLR2")
		{
			if (DatasetName == "anthrokidsV2.dat")	{P4_FeatureSelection("anthrokidsV2.dat","NLR2")}
			if (DatasetName == "poland_30.mat")		{P4_FeatureSelection("poland_30.mat","NLR2")}
			if (DatasetName == "santafe.mat")		{P4_FeatureSelection("santafe.mat","NLR2")}
		}
	}
	
	if (PropertyNumber == "5")
	{
		print("Property 5 : Sample Robustness")
		if (Criterion == "MI")
		{
			if (DatasetName == "anthrokidsV2.dat")	{P5_SampleRobustness("anthrokidsV2.dat","MI")}
			if (DatasetName == "poland_30.mat")		{P5_SampleRobustness("poland_30.mat","MI")}
			if (DatasetName == "santafe.mat")		{P5_SampleRobustness("santafe.mat","MI")}
		}
		if (Criterion == "DT")
		{
			if (DatasetName == "anthrokidsV2.dat")	{P5_SampleRobustness("anthrokidsV2.dat","DT")}
			if (DatasetName == "poland_30.mat")		{P5_SampleRobustness("poland_30.mat","DT")}
			if (DatasetName == "santafe.mat")		{P5_SampleRobustness("santafe.mat","DT")}
		}
		if (Criterion == "NLR2")
		{
			if (DatasetName == "anthrokidsV2.dat")	{P5_SampleRobustness("anthrokidsV2.dat","NLR2")}
			if (DatasetName == "poland_30.mat")		{P5_SampleRobustness("poland_30.mat","NLR2")}
			if (DatasetName == "santafe.mat")		{P5_SampleRobustness("santafe.mat","NLR2")}
		}
	}

	if (PropertyNumber == "6X3")
	{
		print("Property 6 : X Noise Robustness - 3 levels of noise")
		if (Criterion == "MI")
		{		
			if (DatasetName == "anthrokidsV2.dat")	{P6_XnoiseRobustness_3levels("anthrokidsV2.dat","MI")}
			if (DatasetName == "poland_30.mat")		{P6_XnoiseRobustness_3levels("poland_30.mat","MI")}
			if (DatasetName == "santafe.mat")		{P6_XnoiseRobustness_3levels("santafe.mat","MI")}
		}
		if (Criterion == "DT")
		{
			if (DatasetName == "anthrokidsV2.dat")	{P6_XnoiseRobustness_3levels("anthrokidsV2.dat","DT")}
			if (DatasetName == "poland_30.mat")		{P6_XnoiseRobustness_3levels("poland_30.mat","DT")}
			if (DatasetName == "santafe.mat")		{P6_XnoiseRobustness_3levels("santafe.mat","DT")}
		}
		if (Criterion == "NLR2")
		{
			if (DatasetName == "anthrokidsV2.dat")	{P6_XnoiseRobustness_3levels("anthrokidsV2.dat","NLR2")}
			if (DatasetName == "poland_30.mat")		{P6_XnoiseRobustness_3levels("poland_30.mat","NLR2")}
			if (DatasetName == "santafe.mat")		{P6_XnoiseRobustness_3levels("santafe.mat","NLR2")}
		}
	}
	if (PropertyNumber == "6XSTD")
	{
		print("Property 6 : X Noise Robustness + STD")
		if (Criterion == "MI")
		{
		
			if (DatasetName == "anthrokidsV2.dat")	{P6_XNoiseRobustness_1levelSTD("anthrokidsV2.dat","MI")}
			if (DatasetName == "poland_30.mat")		{P6_XNoiseRobustness_1levelSTD("poland_30.mat","MI")}
			if (DatasetName == "santafe.mat")		{P6_XNoiseRobustness_1levelSTD("santafe.mat","MI")}
		}
		if (Criterion == "DT")
		{
			if (DatasetName == "anthrokidsV2.dat")	{P6_XNoiseRobustness_1levelSTD("anthrokidsV2.dat","DT")}
			if (DatasetName == "poland_30.mat")		{P6_XNoiseRobustness_1levelSTD("poland_30.mat","DT")}
			if (DatasetName == "santafe.mat")		{P6_XNoiseRobustness_1levelSTD("santafe.mat","DT")}
		}
		if (Criterion == "NLR2")
		{
			if (DatasetName == "anthrokidsV2.dat")	{P6_XNoiseRobustness_1levelSTD("anthrokidsV2.dat","NLR2")}
			if (DatasetName == "poland_30.mat")		{P6_XNoiseRobustness_1levelSTD("poland_30.mat","NLR2")}
			if (DatasetName == "santafe.mat")		{P6_XNoiseRobustness_1levelSTD("santafe.mat","NLR2")}
		}
	}
	if (PropertyNumber == "6Y3")
	{
		print("Property 6 : Y Noise Robustness")
		if (Criterion == "MI")
		{	
			if (DatasetName == "anthrokidsV2.dat")	{P6_YnoiseRobustness_3levels("anthrokidsV2.dat","MI")}
			if (DatasetName == "poland_30.mat")		{P6_YnoiseRobustness_3levels("poland_30.mat","MI")}
			if (DatasetName == "santafe.mat")		{P6_YnoiseRobustness_3levels("santafe.mat","MI")}
		}
		if (Criterion == "DT")
		{
			if (DatasetName == "anthrokidsV2.dat")	{P6_YnoiseRobustness_3levels("anthrokidsV2.dat","DT")}
			if (DatasetName == "poland_30.mat")		{P6_YnoiseRobustness_3levels("poland_30.mat","DT")}
			if (DatasetName == "santafe.mat")		{P6_YnoiseRobustness_3levels("santafe.mat","DT")}
		}
		if (Criterion == "NLR2")
		{
			if (DatasetName == "anthrokidsV2.dat")	{P6_YnoiseRobustness_3levels("anthrokidsV2.dat","NLR2")}
			if (DatasetName == "poland_30.mat")		{P6_YnoiseRobustness_3levels("poland_30.mat","NLR2")}
			if (DatasetName == "santafe.mat")		{P6_YnoiseRobustness_3levels("santafe.mat","NLR2")}
		}
	}
	if (PropertyNumber == "7")
	{
		print("Only 6 properties - No Property 7")
	}
}

print("ReadingGrid(DatasetName,Criterion,PropertyNumber)")