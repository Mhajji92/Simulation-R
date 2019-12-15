## Algorithme de Metropolis pour le coloriage des departements ##


## Le But de cette exercice est de colorer la carte de France, avec 4
## couleurs différentes sans que deux départements voisins soient colorés
## de la même couleur
## en  L = 60 niveaux de gris ##

## Nombre de departements

N = 95 

## Matrice de voisinage des departements 
## V[i,j] = 1 si les departements i et j se touchent

V <- array(0,dim=c(N,N)) 

V[1,39]=1; V[1,74]=1; V[1,73]=1; V[1,38]=1; V[1,69]=1; V[1,71]=1;
V[2,8]=1; V[2,51]=1; V[2,77]=1; V[2,60]=1; V[2,80]=1; V[2,59]=1;
V[3,71]=1; V[3,42]=1; V[3,63]=1; V[3,23]=1; V[3,18]=1; V[3,58]=1;
V[4,5]=1; V[4,6]=1; V[4,83]=1; V[4,84]=1; V[4,26]=1;
V[5,26]=1; V[5,38]=1; V[5,73]=1; 
V[6,83]=1;
V[7,26]=1; V[7,30]=1; V[7,38]=1; V[7,43]=1; V[7,48]=1; 
V[7,42]=1; V[7,84]=1; 
V[8,51]=1; V[8,55]=1;
V[9,11]=1; V[9,66]=1; V[9,31]=1;
V[10,21]=1; V[10,89]=1; V[10,77]=1; V[10,51]=1; V[10,52]=1;
V[11,66]=1; V[11,31]=1; V[11,81]=1; V[11,34]=1;
V[12,15]=1; V[12,48]=1; V[12,30]=1; V[12,34]=1; V[12,81]=1; 
V[12,82]=1; V[12,46]=1;
V[13,83]=1; V[13,84]=1; V[13,30]=1;
V[14,50]=1; V[14,61]=1; V[14,27]=1;
V[15,19]=1; V[15,63]=1; V[15,43]=1; V[15,48]=1; V[15,46]=1;
V[16,17]=1; V[16,79]=1; V[16,86]=1; V[16,87]=1; V[16,24]=1;
V[17,85]=1; V[17,79]=1; V[17,24]=1; V[17,33]=1; 
V[18,23]=1; V[18,36]=1; V[18,41]=1; V[18,45]=1; V[18,58]=1;
V[19,24]=1; V[19,87]=1; V[19,23]=1; V[19,63]=1; V[19,46]=1;
V[21,52]=1; V[21,70]=1; V[21,39]=1; V[21,71]=1; V[21,58]=1; 
V[21,89]=1;
V[22,29]=1; V[22,56]=1; V[22,35]=1;
V[23,36]=1; V[23,63]=1; V[23,87]=1;
V[24,33]=1; V[24,87]=1; V[24,46]=1; V[24,47]=1;
V[25,39]=1; V[25,70]=1; V[25,90]=1; V[25,68]=1;
V[26,38]=1; V[26,84]=1;
V[27,28]=1; V[27,61]=1; V[27,76]=1; V[27,60]=1; V[27,95]=1; 
V[27,78]=1; 
V[28,61]=1; V[28,41]=1; V[28,45]=1; V[28,91]=1; V[28,78]=1; 
V[28,72]=1;
V[29,56]=1;
V[30,34]=1; V[30,48]=1; V[30,84]=1;
V[31,32]=1; V[31,82]=1; V[31,81]=1; V[31,65]=1;
V[32,40]=1; V[32,47]=1; V[32,82]=1; V[32,65]=1; V[32,64]=1;
V[33,40]=1; V[33,47]=1;
V[34,81]=1;
V[35,53]=1; V[35,50]=1; V[35,44]=1; V[35,56]=1;
V[36,37]=1; V[36,41]=1; V[36,87]=1; V[36,86]=1;
V[37,41]=1; V[37,49]=1; V[37,72]=1; V[37,86]=1; 
V[38,42]=1; V[38,69]=1; V[38,73]=1;
V[39,71]=1; V[39,70]=1;
V[40,47]=1; V[40,64]=1;
V[41,45]=1; V[41,72]=1;
V[42,63]=1; V[42,43]=1; V[42,63]=1; V[42,69]=1; V[42,71]=1;
V[43,63]=1; V[43,48]=1;
V[44,49]=1; V[44,85]=1; V[44,56]=1;
V[45,91]=1; V[45,77]=1; V[45,89]=1 ; V[45,58]=1;
V[46,47]=1; V[46,82]=1;
V[47,82]=1;
V[49,53]=1; V[49,72]=1; V[49,86]=1; V[49,79]=1; V[49,85]=1;
V[50,61]=1; V[50,53]=1;
V[51,52]=1; V[51,55]=1; V[51,77]=1;
V[52,55]=1; V[52,88]=1; V[52,70]=1;
V[53,61]=1; V[53,72]=1;
V[54,55]=1; V[54,57]=1; V[54,88]=1;
V[55,88]=1;
V[57,67]=1;
V[58,71]=1; V[58,89]=1;
V[59,62]=1; V[59,80]=1;
V[60,76]=1; V[60,95]=1; V[60,77]=1; V[60,80]=1;
V[61,72]=1;
V[62,80]=1;
V[64,65]=1;
V[67,68]=1; V[67,88]=1;
V[68,90]=1; V[68,88]=1;
V[69,71]=1;
V[70,90]=1; V[70,88]=1;
V[73,74]=1;
V[75,94]=1; V[75,93]=1; V[75,92]=1;
V[76,80]=1;
V[77,89]=1; V[77,91]=1; V[77,93]=1; V[77,94]=1; V[77,95]=1; 
V[78,91]=1; V[78,92]=1; V[78,95]=1; 
V[79,85]=1; V[79,86]=1;
V[81,82]=1;
V[83,84]=1;
V[86,87]=1;
V[88,90]=1;
V[91,92]=1; V[91,94]=1;
V[92,93]=1; V[92,94]=1; V[92,95]=1;
V[93,94]=1; V[93,95]=1;

V = t(V) +V

## Nombre de niveaux de gris L souhaite

#L = 60
L=4

## Nombre d'iterations de l'algorithme

nmax = 100000

## Initialissation aleatoire des couleurs (ou niveaux de gris) des departements

G<-sample(1:L,N,replace=T)

###############################################################
###                                                         ###
###                                                         ###
###                  Algorithme de Metropolis               ###
###                                                         ###
###                                                         ###
###############################################################
## algo de metropolis nmax fois

####### TRANSITION METROPOLIS

transition1 = function(g,H,beta) {
  result=c(g,H)
  gnew=g
  k0=sample(1:N,1)
  gnew[k0]=sample(c(max(g[k0]-1,1),min(g[k0]+1,L)),1)
  delta=sum(abs(gnew[k0]==gnew)*V[k0,])-sum(abs(g[k0]==g)*V[k0,])
  alpha=min(exp(-beta*delta),1)
  u=runif(1)
  if(u<alpha){
    result=c(gnew,H+delta)}
  return(result)
}
beta=1
beta0=1
L=4
H=rep(0,nmax)
G=sample(1:L,N,replace=TRUE)
H[1]=0
for (i in 1:(N-1)) {
  for (j in (i+1):N){
    H[1]=H[1]+1*(G[i]==G[j])*V[i,j]
  }
}
Gbest=G

Hbest=H[1]
for (i in 1:(nmax-1)){
  #beta=beta0*sqrt(i)
  totoc=transition1(G,H[i],beta)
  H[i+1]=totoc[N+1]
  G=totoc[1:N]
  if (H[i+1]<Hbest) {
    Hbest=H[i+1]
    Gbest=G
  }
}
plot(H)

####### TRANSITION AVEC GIBBS
transition2 = function(g,H,beta) {
  result=c(g,H)
  gnew=g
  k0=sample(1:N,1)
  Num=rep(0,L)
  for (j in 1:L) {
    Num[j]=exp(-beta*sum(abs(j==g)*V[k0,]))    #V(k0,l)
  }
  PGibbs=Num/sum(Num)
  gnew[k0]=sample(1:L,1,prob=PGibbs)
  delta=sum(abs(gnew[k0]==gnew)*V[k0,])-sum(abs(g[k0]==g)*V[k0,])
  #alpha=min(exp(beta*delta),1)
  result=c(gnew,H+delta)
  return(result)
}

######## 4 couleurs
beta=1
beta0=1
L=4
H=rep(0,nmax)
G=sample(1:L,N,replace=TRUE)
H[1]=0
for (i in 1:(N-1)) {
  for (j in (i+1):N){
    H[1]=H[1]+1*(G[i]==G[j])*V[i,j]
  }
}
Gbest=G

Hbest=H[1]
for (i in 1:(nmax-1)){
  beta=beta0*sqrt(i)
  totoc=transition2(G,H[i],beta)
  H[i+1]=totoc[N+1]
  G=totoc[1:N]
  if (H[i+1]<Hbest) {
    Hbest=H[i+1]
    Gbest=G
  }
}

plot(H,ylim=c(0,95))
#print(Hbest)




###############################################################
###                                                         ###
###                                                         ###
###         Affichage de la carte avec un G                 ###
###                                                         ###
###                                                         ###
###############################################################




## recuperation du fond de carte et affichage

setwd("R_files")
library(maptools)
fdc = readShapeSpatial("DEPARTEMENT")


A=fdc
A$CODE_DEPT=as.numeric(as.character(A$CODE_DEPT))
ligne=which(is.na(A$CODE_DEPT))  
A$CODE_DEPT[ligne]=20
A = A[order(A$CODE_DEPT),]
fdc=A[order(A$CODE_DEPT),]

couleurs=c("blue","red","green","white")
vect_couleurs=couleurs[G]
vect_couleurs=c(vect_couleurs[1:20],vect_couleurs[20],vect_couleurs[21:95])
plot(fdc,col=vect_couleurs)
