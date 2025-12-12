## This function simulates the effects of bird attack in terms of predation and fear/dispersal
## four types of arthropods are considered: tasty+dispersive, tasty+non-dispersive, nasty+dispersive, nasty+non-dispersive; they are always given in that order
## the landscape contains a series of bushes/patches, each with x-y coordinations and a volume
## each bush has a degree of maladptation (0-1), this is constant for a given simulation (the Timema population does not evolve)
## predation is proportional to volume (option 1), volume + maladaptation (option 2), or volume + maladptation + arthropod density (option 3)
## each arthropod group gets a rule that defines the proportion of arthrpods eaten, the proportion that are not eaten that disperse
## there is also an option for whether dispersal or predation happens first
## dispersal depends on a normal kernel density with a SD (bigger SD means more dispersal on average, scale is relative to the x-y coordinate system)

## landscape = dataframe with one row per bush and the following named columns: x, y, volume, mal, Ntd, Ntn, Nnd, Nnn. x and y are the coordinates, volume is volume, mal is proportion maladapted Timema, and the N variables are the number of tasty (t), nasty (n), dispersive (d) and non-dispersive (n) in their various combinations
## disp = standard deviation of the normal kernel
## predation = predation option 1, 2 or 3, as define above
## propEaten = rule, proportion eaten for each type, in same order given above (tn, td, nd, nn)
## propDisp = rule, proportion dispersing for each type, in same order given above (tn, td, nd, nn)
## dispFirst = boolean, dispersal before predation
## steps = number of bird attacks to simulate (one at a time)
## psteps = boolean print each step
sim<-function(landscape=NA, disp=1, predation=1,propEaten=c(.5,.5,.5,.5),
	propDisp=c(.5,.5,.5,.5), dispFirst=TRUE,steps=1,psteps=TRUE){
		
	Nb<-dim(landscape)[1]
	arth<-as.matrix(cbind(landscape$Ntd,landscape$Ntn,landscape$Nnd,landscape$Nnn))
	if(psteps==TRUE){
		typ=c("tasty-disp","tasty-non-disp","nasty-disp","nasty-non-disp")
		par(mfrow=c(2,2))
		for(j in 1:4){
			plot(landscape$x,landscape$y,pch=19,col="gray80",
			cex=landscape$volume,xlab=x,ylab=y)
			text(landscape$x,landscape$y,arth[,j])
			title(main=typ[j])
		}
	}
			
	## loop over steps
	for(i in 1:steps){
		## choose bush to attack
		if(predation==1){ ## volume only
			pp<-landscape$volume
		} else if(predation==2){ ## volume + maladaptation
			pp<-landscape$volume * landscape$mal
		} else if (predation==3){ ## volume + mal + arth
			landscape$volume * landscape$mal * apply(arth,1,mean)
		} else{
			print("Predation type not defined")
		}
		bb<-sample(1:Nb,1,prob=pp)
		## if disp first
		if(dispFirst==TRUE){
			## disperse
			nDisp<-floor(propDisp*arth[bb,])
			arth[bb,]<-arth[bb,]-nDisp ## subtract dispersers
			d<-sqrt((landscape$x[bb]-landscape$x[-bb])^2+(landscape$y[bb]-landscape$y[-bb])^2)
			di<-max(d)-d ## closeness
			di<-dnorm(di,mean=0,sd=disp) ## convert to normal kernel
			pp<-di*landscape$volume[-bb]
			bn<-(1:Nb)[-bb]
			for(j in 1:4){ ## four types
				newb<-sample(bn,size=nDisp[j],replace=TRUE,prob=pp)
				tnewb<-table(newb)
				ti<-as.numeric(names(tnewb))
				arth[ti,j]<-arth[ti,j]+tnewb
			}	
			## eat
			nEaten<-floor(propEaten*arth[bb,])
			arth[bb,]<-arth[bb,]-nEaten ## subtract eaten			
		} else{ ## eat first	
			## eat
			nEaten<-floor(propEaten*arth[bb,])
			arth[bb,]<-arth[bb,]-nEaten ## subtract eaten	
			
			## disperse			
						## disperse
			nDisp<-floor(propDisp*arth[bb,])
			arth[bb,]<-arth[bb,]-nDisp ## subtract dispersers
			d<-sqrt((landscape$x[bb]-landscape$x[-bb])^2+(landscape$y[bb]-landscape$y[-bb])^2)
			di<-max(d)-d ## closeness
			di<-dnorm(di,mean=0,sd=disp) ## convert to normal kernel
			pp<-di*landscape$volume[-bb]
			bn<-(1:Nb)[-bb]
			for(j in 1:4){ ## four types
				newb<-sample(bn,size=nDisp[j],replace=TRUE,prob=pp)
				tnewb<-table(newb)
				ti<-as.numeric(names(tnewb))
				arth[ti,j]<-arth[ti,j]+tnewb
			}
		}
		if(psteps==TRUE){
			Sys.sleep(1)
			par(mfrow=c(2,2))
			for(j in 1:4){
				plot(landscape$x,landscape$y,pch=19,col="gray80",
				cex=landscape$volume,xlab="x",ylab="y")
				text(landscape$x,landscape$y,arth[,j])
				title(main=typ[j])
			}
		}
	}
	return(arth)
}			
			
			
			
			
			
			
			
