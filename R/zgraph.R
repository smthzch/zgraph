
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

glmnet_coefs <- function(cvfit, s="lambda.min") {
	ind <- which(coef(cvfit, s=s) != 0)
	df <- data.frame(
		feature=rownames(coef(cvfit, s=s))[ind],
		coeficient=coef(cvfit, s=s)[ind]
	)
	df
}

dependenceGraph <- function(m, s="lambda.1se"){
	set.seed(711)
	m = as.matrix(m)
	N = ncol(m)

	depG = matrix(0, nrow=N, ncol=N)
	colnames(depG) = colnames(m)
	rownames(depG) = colnames(m)
	for(i in 1:N){
		tfit = cv.glmnet(x=m[,-i], y=m[,i], grouped=FALSE, nfolds=(N-1))
		depG[i,-i] = coef(tfit, s=s)[-1]
	}
	depG
}

relevanceGraph <- function(m){
	analCor = cor(m)
	#analRel = -.5*log(1-analCor^2)
	analCor
}

pointwiseDependence <- function(y, x, s="lambda.1se"){
	set.seed(711)
	y = as.matrix(y)
	x = as.matrix(x)
	k = ncol(x)
	r = ncol(y)
	N = nrow(y)

	depG = matrix(0, nrow=r, ncol=k)
	colnames(depG) = colnames(x)
	rownames(depG) = colnames(y)
	for (i in 1:r){
		tfit = cv.glmnet(x=x, y=y[,i], grouped=FALSE, nfolds=(N-1))
		depG[i,] = coef(tfit, s=s)[-1]
	}
	depG
}

graphGroup <- function(G, thresh=0){ #G is some sort of adjencency matrix
	grps = rep(0, ncol(G))
	availgrp = 2:ncol(G) #because directed graph will need to jump numbers
	names(grps) = colnames(G)
	queue = c()
	g = 1
	#bfs - modified to accomadate directed graphs
	for (i in 1:ncol(G)){
		if (grps[i]==0){
			queue = c(queue,i)
			while(length(queue)!=0){
				v = queue[1]
				queue = queue[-1]
				grps[v] = g
				for(e in 1:ncol(G)){
					if(e!=v){
						if(abs(G[v,e])>=thresh){
							if(grps[e]==0){
								queue = c(queue,e)
							}
							#adjacent node already grouped but in different group than current
							#reassign all current group memebers to adjacent group
							else if(grps[e]!=g){
								grps[grps==g] = grps[e]
								g = grps[e]
							}
						}
					}
				}
			}
			g = availgrp[1]
			availgrp = availgrp[-1]
		}
	}
	grps
}
