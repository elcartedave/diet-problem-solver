  source("draft.r")
  
  
  #this function accepts raw data as parameter and it returns a laballed list
  #containing the objective function, constraints, and the augmented coefficient matrix
  #to be used in the simplex convert method
  
  simplexSet_up=function(data){  
    if(!is.na(data[1,1])){
      totalCost=c()
      objectiveMatrix=matrix(NA, nrow=1, ncol=nrow(data)+1)
      for(i in 1:nrow(data)){
        totalCost=paste(totalCost, " + (", data[i,2]," * ","x",i,")", sep="")
        objectiveMatrix[1,i]=as.numeric(data[i,2])
        
      }
      totalCost=substring(totalCost,4) #to remove '+' sign in the beginning of the function
      totalCost=paste(totalCost,"= 1")
      objectiveMatrix[1,nrow(data)+1]=1
      
      
      constraints=c()
      constraints2=c()
      constraintTable=matrix()
      augCoeffMatrix=matrix(NA,ncol=(nrow(data)+1),nrow=(nrow(nutritionalRequirements)*2))
      augRow=1
      augCol=1
      col_num=3
      flag=1
      for(j in 1:nrow(nutritionalRequirements)){
        for(k in 1:nrow(data)){
          if(flag==1){
            constraints=paste(constraints, "-(", data[k,col_num], " * x",k,")",sep="")
            constraints2=paste(constraints2, "(", data[k,col_num], " * x",k,")",sep="")
            augCoeffMatrix[augRow,augCol]=-1*as.numeric(data[k,col_num])
            augCoeffMatrix[augRow+1,augCol]=as.numeric(data[k,col_num])
            augCol=augCol+1
            flag=0
          }else{
            constraints=paste(constraints, " + -(", data[k,col_num], " * x",k,")", sep="")
            constraints2=paste(constraints2, " + (", data[k,col_num], " * x",k,")", sep="")
            augCoeffMatrix[augRow,augCol]=-1*as.numeric(data[k,col_num])
            augCoeffMatrix[augRow+1,augCol]=as.numeric(data[k,col_num])
            augCol=augCol+1
          }
        }
        constraints2=paste(constraints2," > ", nutritionalRequirements[j,2], sep="")
        constraints=paste(constraints," > ","-",nutritionalRequirements[j,3],sep="")
        
        augCoeffMatrix[augRow,augCol]=-1*as.numeric(nutritionalRequirements[j,3])
        augCoeffMatrix[augRow+1,augCol]=as.numeric(nutritionalRequirements[j,2])
        
        augCol=1
        augRow=augRow+2
        
        constraintTable=rbind(constraintTable,constraints)
        constraintTable=rbind(constraintTable,constraints2)
        constraints=c()
        constraints2=c()
        augCoeffVector1=c()
        augCoeffvector2=c()
        flag=1
        col_num=col_num+1
      }
      
      newMatrix=matrix(NA,nrow=ncol(augCoeffMatrix)-1,ncol=ncol(augCoeffMatrix))
      mark=1
      for(i in 1:(ncol(augCoeffMatrix)-1)){
        for(j in 1:ncol(augCoeffMatrix)){
          newMatrix[i,j]=0
        }
        newMatrix[i,mark]=-1
        newMatrix[i,ncol(augCoeffMatrix)]=-10
        mark=mark+1
      }
      augCoeffMatrix=rbind(augCoeffMatrix,newMatrix)
      
      augCoeffMatrix=rbind(augCoeffMatrix,objectiveMatrix)
      rownames(augCoeffMatrix)=c(1:(23+nrow(newMatrix)))
      constraintTable=matrix(as.vector(constraintTable[-1]), nrow=(nrow(nutritionalRequirements)*2), dimname=list(1:22,"Constraints"))
      colName=c()
      for(i in 1:(ncol(augCoeffMatrix)-1)){
        colName[i]=paste("x",i,sep="")
      }
      colName[ncol(augCoeffMatrix)]="RHS"
      colnames(augCoeffMatrix)=colName
      list=list(objectiveFXN=totalCost, constraints=constraintTable, augCoeffMatrix=augCoeffMatrix)
      return(list)
    }
  }
  
  
  #this method accepts the data output by simplex_setUp and it basically sets-up the augmented coefficient matrix into a
  #tableau that will later on be used in the actual simplex implementation. It returns the initial tableau before proceeding
  #to simplex method
  
  simplex_Convert=function(data){
    matrixTranspose=t(data$augCoeffMatrix)
  
    initialTableau=matrix(NA, nrow=nrow(matrixTranspose), ncol=ncol(matrixTranspose)+nrow(matrixTranspose))
    names_col=c()
    xsub=1
    for(i in 1:ncol(initialTableau)){
      if(i<=ncol(matrixTranspose)-1){
        names_col[i]=paste("S",i, sep="")
      }else{
        if(i<=ncol(initialTableau)-2){
          names_col[i]=paste("x",xsub,sep="")
          xsub=xsub+1
        }else{
          if(i<=ncol(initialTableau)-1){
            names_col[i]="z"
          }
          if(i==ncol(initialTableau)){
            names_col[i]="Solution" 
          }
        }
      }
    }
    colnames(initialTableau)=names_col
    for(i in 1:nrow(matrixTranspose)){
      for(j in 1:ncol(matrixTranspose)){
        if(j==ncol(matrixTranspose)){
          if(i==nrow(matrixTranspose)){ #if following values are in the last row ( meaning, it is the objective function)
            initialTableau[i,ncol(initialTableau)-1]=matrixTranspose[i,j] #instead we copy the value to the last column of the initial tableau, it copies to the z column
          }else{
            initialTableau[i,ncol(initialTableau)]=matrixTranspose[i,j] #just copies the value of the last column in the transposeMatrix to the last column of the initial tableau
          }
        }else{
          if(i==nrow(matrixTranspose)){ #if following values are in the last row ( meaning, it is the objective function)
            initialTableau[i,j]= -1*matrixTranspose[i,j] #negates the values for that row and assigns it the initial tableau
          }else{
            initialTableau[i,j]=matrixTranspose[i,j] #copies the value of the transposed matrix to the initial tableau
          }
          
        }
      }
    }
    k=0
    for(i in 1:nrow(matrixTranspose)){
      for(j in ncol(matrixTranspose):(ncol(initialTableau)-1)){
        if(j==ncol(matrixTranspose)+k){
          initialTableau[i,j]=1
        }else{
          initialTableau[i,j]=0
        }
      }
      k=k+1
    }
    initialTableau[nrow(matrixTranspose),ncol(initialTableau)]=0
    return(initialTableau)
  }
  
  #accepts the initialTableau as input and it now implements the actual simplex method.
  #it also creates the table which contains all the table per iterations. It also returns the
  #basic solutions per iteration and the final solution (if applicable)
  simplexMethod<-function(data){
    iterationMatrix=matrix(NA, ncol=ncol(data)-1)
    columns=(colnames(data)[-c(ncol(data)-1)])
    iteration=0
    while(T){
      bind=matrix("", nrow=1,ncol=ncol(data))
      binder=rbind(bind,round(data,2))
      if(iteration==0){
        binderer=rbind(bind,data)
      }else{
        binderer=rbind(binderer,binder)
      }
      
      
      min=0
      flag=0
      for(i in 1:(ncol(data)-1)){
        if(data[nrow(data),i]<0){ #if there is a negative number in the last row
          flag=1
          if(data[nrow(data),i]<min){
            min=data[nrow(data),i]
            pivotColumn=i
          }
        }
      }
      if(flag==0){ #stops when there is no negative number in the last row
        break
      }
      # min=data[1,ncol(data)]/data[1,pivotColumn]
      flag=1
      for(i in 1:(nrow(data)-1)){
        if(flag==1 && data[i,pivotColumn]>0){
          TR=data[i,ncol(data)]/data[i,pivotColumn]
          min=TR
          flag=0
          pivotRow=i
        }else{
          TR=data[i,ncol(data)]/data[i,pivotColumn]
          if(TR<min && TR>0 && data[i,pivotColumn]>0){
            min=TR
            pivotRow=i
          }
        }
      }
      if(flag==1){ #if there is no solution anymore
        break
      }
      pivotElement=data[pivotRow,pivotColumn]
      
      nPR=(1/pivotElement)*data[pivotRow,]
      
      for(i in 1:nrow(data)){
        if(i!=pivotRow){  #eliminate
          data[i,]=data[i,]-(nPR*data[i,pivotColumn])
        }else{
          data[i,]=nPR
        }
      }
      basicSol=matrix(NA, nrow=1, ncol=ncol(data)-1)
      for(i in 1:(ncol(data)-1)){
        detect=0
        var=1
        for(j in 1:nrow(data)){
          if(data[j,i]==1) {
            var=j
          }
          else{
            if(data[j,i]!=0){
              detect=1
              break
            }
          }
        }
        if(detect==1 || data[var,i]!=1) {
          basicSol[1,i]=0
        }
        else {
          basicSol[1,i]=data[var,ncol(data)]
        }
      }
      iterationMatrix=rbind(iterationMatrix,basicSol)
      iteration=iteration+1
    }
  
    counter=c()
    counterMatrix=matrix("", nrow=nrow(binderer), dimnames=list(NULL,"Iteration"))
    iterationCounter=0
    for(i in 1:nrow(binderer)){
      if(i==1){
        counterMatrix[i,1]=paste("Iteration",iterationCounter)
        iterationCounter=iterationCounter+1
      }else{
        if(binderer[i,1]==""){
          counterMatrix[i,1]=paste("Iteration",iterationCounter)
          iterationCounter=iterationCounter+1
        }
      }
    }
    
    superFinalMatrix=cbind(counterMatrix,binderer)

    
    iterationMatrix=matrix(as.vector(iterationMatrix[-1,]),ncol=ncol(basicSol))
    if(flag==1){
      data=iterationMatrix
      rownames(data)=1:nrow(data)
      colnames(data)=columns
    }
    else{
      iterationMatrix=matrix(as.vector(iterationMatrix[-nrow(iterationMatrix),]),ncol=ncol(basicSol))
      data=matrix(data[nrow(data),-(ncol(data)-1)],nrow=1)
      data=rbind(iterationMatrix,data)
      rownames(data)=1:nrow(data)
      colnames(data)=columns
    }
   
    listMatrix=list(data=data, tableaus=superFinalMatrix)
    return(listMatrix)
  }
  

  
  
