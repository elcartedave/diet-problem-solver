GaussJordanMethod1<-function(a){
  b=a
  n=nrow(a)   #sets n as the number of variables in a
  for(i in 1:n){ #loop that iterates n times
    if(i!=n){  
      indexCounter=i
      max=max(abs(b[i:n,i])) #sets the maximum abs value through the specific numbers in the column
      for(j in c(b[i:n,i])){ #loop that gets the row number of the maximum value in the column
        indexCounter=indexCounter+1
        if(max==abs(j)){
          break
        }
      }
      temp=b[indexCounter-1,] #temporarily holds the row with the max value (in this case, swapping is done)
      b[indexCounter-1,]=b[i,] #swaps the values of the row with the max value and the current row
      b[i,]=temp #thus, making the row with max value as the pivot row
      if(b[i,i]==0){ # #returns NA is the pivot element is 0
        return(NA)
      }
    }
    b[i,]=b[i,]/b[i,i] #divides the current row by the pivot element to get the multiplier
    for(j in 1:n){ #loop that iterates n times
      if(i!=j){
        temp=b[i,]*b[j,i] #multiplies the multiplier by the succeeding row
        b[j,]=b[j,]-temp #sets the current row as the difference of the original row and the temp
      }
    }
  }

  solution=matrix(round(c(b[,n+1]),4), nrow=1) #creates a matrix that store the values of (n+1)th column
  if(b[,n+1][1]==Inf || b[,n+1][1]==-Inf){ #returns NA if the solution contains and INF and -INF (I initialized it as the first element since
    solution=NA                           # an infinity answer in the first element also generalizes the solution for the entire solution array
  }

  return(solution)
}


#THIS METHOD RUNS THE ENTIRE QUADRATIC SPLINE PROCEDURE. IT ACCEPTS A RAW MATRIX WITH 2 COLUMNS AS AN INPUT
#AND RETURNS THE FUNCTIONS (IN EXPRESSION AND IN STRING FORM), AND ALSO THE INITIAL SET-UP FOR THE EQUATIONS
#BEFORE INVOKING THE GAUSS JORDAN METHOD
quadraticSpline<-function(data){
  n=nrow(data)
  intervals=n-1
  numberOfEquations=3*intervals
  equation=c()
  
  
  names=c()
  index=1
  index1=2
  solutionName=c()
  solutionName[1]="a1"
  for(i in 1:intervals){
    if(i==1){
      names[index]=paste("b",i,sep="")
      names[index+1]=paste("c",i,sep="")
      solutionName[index1]=paste("b",i,sep="")
      solutionName[index1+1]=paste("c",i,sep="")
      index=index+2
      index1=index1+2
    }
    else{
      names[index]=paste("a",i,sep="")
      names[index+1]=paste("b",i,sep="")
      names[index+2]=paste("c",i,sep="")
      solutionName[index1]=paste("a",i,sep="")
      solutionName[index1+1]=paste("b",i,sep="")
      solutionName[index1+2]=paste("c",i,sep="")
      index=index+3
      index1=index1+3
    }
    
  }
  names1=names
  names1[index]="RHS"
  finalMatrix=matrix(0, ncol=numberOfEquations, nrow=numberOfEquations-1, dimnames=list(names,names1))
  
  index=1
  j=1
  k=1
  for(i in 2:intervals){
    equation[index]=paste(data[i,1]^2," * a",i-1, " + ", data[i,1]," * b", i-1, " + c",i-1," + -",data[i,2],sep="")
    equation[index+1]=paste(data[i,1]^2," * a",i, " + ", data[i,1]," * b", i, " + c",i," + -",data[i,2],sep="")
    index=index+2
  
    if(i==2){
      finalMatrix[j,k]=data[i,1]
      finalMatrix[j,k+1]=1
      finalMatrix[j,numberOfEquations]=data[i,2]
      k=k+2
    }
    else{
      finalMatrix[j,k]=data[i,1]^2
      finalMatrix[j,k+1]=data[i,1]
      finalMatrix[j,k+2]=1
      finalMatrix[j,numberOfEquations]=data[i,2]
      k=k+3
    }
    finalMatrix[j+1,k]=data[i,1]^2
    finalMatrix[j+1,k+1]=data[i,1]
    finalMatrix[j+1,k+2]=1
    finalMatrix[j+1,numberOfEquations]=data[i,2]
    j=j+2
    
  }
  k=1
  equation[index]=paste(data[1,1]^2," * a1"," + ",data[1,1]," * b1 + c1 + -",data[1,2],sep="")
  equation[index+1]=paste(data[n,1]^2," * a",n-1," + ",data[n,1]," * b",n-1," + c",n-1," + -",data[n,2],sep="")
  finalMatrix[j,k]=data[1,1]
  finalMatrix[j,k+1]=1
  finalMatrix[j,numberOfEquations]=data[1,2]
  
  
  finalMatrix[j+1,numberOfEquations-3]=data[n,1]^2
  finalMatrix[j+1,numberOfEquations-2]=data[n,1]
  finalMatrix[j+1,numberOfEquations-1]=1
  finalMatrix[j+1,numberOfEquations]=data[n,2]
  index=index+2
  j=j+2
  
  k=1
  for(i in 2:intervals){
    equation[index]=paste(2*data[i,1]," * a",i-1," + b",i-1, " + -", 2*data[i,1]," * a",i," + -b",i, sep="")
    index=index+1
    if(i==2){
      finalMatrix[j,k]=1
      k=k+2
    }
    else{
      finalMatrix[j,k]=2*data[i,1]
      finalMatrix[j,k+1]=1
      k=k+3
    }
    finalMatrix[j,k]=-2*data[i,1]
    finalMatrix[j,k+1]=-1
    finalMatrix[j,numberOfEquations]=0
    j=j+1
  }

  solution=GaussJordanMethod1(finalMatrix)
 
  solution=cbind(0,solution)
  colnames(solution)=solutionName

  j=1
 
  fxn=c()
  finalEqn=list()
  textForm=c()
  for(i in 1:intervals){
    result=paste("function(x) ",solution[1,j]," * x^2 + ",solution[1,j+1]," * x + ",solution[1,j+2], sep="")
    textForm[i]=result
    fxn=eval(parse(text=result))
    j=j+3
    finalEqn=append(finalEqn,fxn)
    result=c()
  }
  list=list(fxn=finalEqn, string=textForm, matrix=finalMatrix)


  return(list)
}


