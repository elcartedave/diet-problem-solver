PolynomialRegression <-function(n, list){
  numRow=n+1 #arbitrarily sets the number of rows as n+1
  numCol=n+2  #arbitrarily sets the number of columns as n+2
  i=1 #acts as the counter while iterating over the rows
  j=1 #acts as the counter while iterating over the columns
  index=1 #will be manipulated in order to put value to the vector
  exponent=0 #acts as the exponent
  vector1=c() #stores the values of the summation
  length=length(list[[1]]) #counts the number of numbers in the list and acts as its length
  if(n>length) return (NA)  #returns NA when the degree is larger than the number of data
  while(i<=numRow){ #loop that iterates over the total number of rows
    j=1 #resets the value of j, in order to go back to the first element of the succeeding rows
    while(j<=numCol-1){ #loop that iterates through the whole number of column-1
      summation=0 #sets the summation to 0 first
      for(k in 1:(length)){ #loop that goes through the values of the independent variable  
        summation=summation+list[[1]][k]^exponent #gets the summation of the variables raised to the corresponding exponent
      }
      vector1[index]=summation #assigns the resulting value to the vector
      index=index+1 #increases the index by 1
      j=j+1 #increases j by 1
      exponent=exponent+1 #increases the exponent by 1
    }
    exponent=i; #resets the exponent based on what is the current row
    summ=0 #resets the summ as 0
    for(l in 1:(length)){ #loop that goes through the values of the independent variables
      summ=summ+(list[[1]][l]^(exponent-1))*list[[2]][l] #gets the summation of the xiyi raised to the corresponding exponent
    }
    vector1[index]=summ #assigns the resulting value to the vector
    index=index+1 #increases the index by 1
    i=i+1 #increases i by 1
  }
  t<-matrix(vector1, nrow=numRow, byrow=T) #converts the vector into a matrix with numRow no. of rows
  a<-c(GaussJordanMethod(numRow,t)) #calls the function that will solve for the system of linear equations using gauss jordan elimination
  result="function(x)" #string concatenation (will be concatenated with the other strings and coefficients)
  for(m in 1:(n+1)){ #loop that iterates through n+1 times
    result=paste(result, round(a[m],6), sep=" ") #concatenates the solution vector into the string
    if(m>1){                                     #in the form of f(x) ax^0 + ax^1 + ... ax^n
      result=paste(result, " * x ^ ")
      result=paste(result, (m-1))
    }
    if(m!=(n+1)) result=paste(result," + ")
  }
  func=eval(parse(text = result)) #converts the string into an actual function (polynomial function)
  list1<-list(augcoeffmatrix=t, coefficients=a, polynomial_string=result, polynomial_function=func) #creates a labelled list, which consists of the augcoeffmatrix, solution vector, polynomial string, and polynomial function
  return (list1)
}

GaussJordanMethod<-function(n,a){
  print(a)
  for(i in 1:n){ #loop that iterates n times
    if(i!=n){  
      indexCounter=i
      max=max(abs(a[i:n,i])) #sets the maximum abs value through the specific numbers in the column
      for(j in c(a[i:n,i])){ #loop that gets the rwo number of the maximum value in the column
        indexCounter=indexCounter+1
        if(max==abs(j)){
          break
        }
      }
      temp=a[indexCounter-1,] #temporarily holds the row with the max value (in this case, swapping is done)
      a[indexCounter-1,]=a[i,] #swaps the values of the row with the max value and the current row
      a[i,]=temp #thus, making the row with max value as the pivot row
      if(a[i,i]==0){ # #returns NA is the pivot element is 0
        return(NA)
      }
    }
    a[i,]=a[i,]/a[i,i] #divides the current row by the pivot element to get the multiplier
    for(j in 1:n){ #loop that iterates n times
      if(i!=j){
        temp=a[i,]*a[j,i] #multiplies the multiplier by the succeeding row
        a[j,]=a[j,]-temp #sets the current row as the difference of the original row and the temp
      }
    }
  }
  solution=matrix(c(a[,n+1]), nrow=1) #creates a matrix that store the values of (n+1)th column
  if(a[,n+1][1]==Inf || a[,n+1][1]==-Inf){ #returns NA if the solution contains and INF and -INF (I initialized it as the first element since 
    solution=NA                           # an infinity answer in the first element also generalizes the solution for the entire solution array
  }
  return(c(solution)) #returns the solution as the vector
}

#THIS IS JUST A SAMPLE RUN, FEEL FREE TO EDIT HERE!
a <- c(20,20,25,27,30,30,33,35,35,40)
b <- c(8.75,9.43,12.87,14.24,16.89,18.94,25.48,30.11,36.07,51.27)
print(PolynomialRegression(2, list(a,b)))

