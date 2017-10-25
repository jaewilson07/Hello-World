#How to write code and get help

#Write Reporducible Code

#OPEN YOUR DOCUMENT WITH a decent heading 
#ALL your libraries
	#if you're feeling generous include a commented out install command
	#make it easy for me to help you ;)
#ALL your data import commands so they can be executed all at once.
	#Put your datasets in an online source so we don't have to manually modify read() statements.
	#In my example you can LITERALLY copy paste the code and run it b/c the datasource is stored on github
	#I will post any dataset you need in my github repository if you ask me to.	


#eg:

#Project 1

#install.packages(dplyr, dependencies = TRUE)
library(ggplot)
library(dplyr)

exchange_df <- read.csv("https://raw.githubusercontent.com/jaewilson07/Hello-World/master/Datasets/DM%20and%20A/Exchange.csv")

#put the rest of your code here up to the point of error.

#PRO TIP:  CLEAR YOUR WORKSPACE BEFORE YOU SEND US YOUR BROKEN CODE
#if you execute commands out of order, sometimes you don't realize that a variable has not been initialized until a later step and it's still lingering in your workspace

#This command will clear your workspace.
#rm(list=ls())
