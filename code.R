dataSet<-read.csv("./sestari.csv")
#dataSet<-read.csv("~/Google Drive/SimulationPE/NEW/P1/output/sestari.csv")


#tempo
tempo = 3

# so che la media della sinusoide e' zero, e che la media di Z e' zero, allora la media della Logistica e'
meanDataSet = mean(dataSet$x);

#rimuovo la media di Y dal dataset
datasetSinusoide =dataSet$x - meanDataSet

#Uso la funzione di auto correlazione per scoprire la quantita di points della sinusoide
acf(x=datasetSinusoide,lag.max=32,xlab = "points",ylab = "ACF", type="correlation", main = "Auto Correlation" ,plot = TRUE)

# com acf so che la sinusoide ne ha 32 points
sinusoidePoints = 32

#ritorna la quantita di sinusoide nel dataset
sinusoideCount = (length(datasetSinusoide) /sinusoidePoints )

#frequenza della sinusoide
sinusoideFrequenza = sinusoideCount / tempo


#creo una matrix per mettere tutte le sinusoidi
v = matrix(ncol = sinusoidePoints, nrow = sinusoideCount)
for (i in 1 : sinusoideCount) {
  dim1 = (sinusoidePoints * (i-1) + 1);
  dim2 = (sinusoidePoints * i);
  v[i,] =  datasetSinusoide[dim1 : dim2];
}



#extrago la media della sinusoide
meanSinusoide = double();
for (i in 1 : sinusoidePoints){
  meanSinusoide[i] = mean(v[,i]) ;
}
plot(meanSinusoide)


#crea una sequenza diagonale per rapresentare una sinuzione nella dimenzione della sinusoide
x <- seq(from=pi/sinusoidePoints ,to=2*pi, by=2*pi/sinusoidePoints);


#faccio la regressione trigonometrica, usando la funzione della sinusoide
m = nls(formula =meanSinusoide~value * sin(x), start = c(value=1))


#la media della sinuzione con quella della regressione
plot(x=x,y=meanSinusoide,main="Sinuoside",xlab = "points",ylab = "media");   
lines(x=x, y=m$m$predict(),col="red",lwd=2);

# il coefficiente della regrezzione rapresenta la ampiezzza della sinusoide
A  = summary(m)$coefficients[,1];

# rimuovo la sinusoide dal mio dataset
newDataSet = double();
for (i in 1:length(dataSet$x)){
  newDataSet[i] =  dataSet$x[i] - A*sin((2*pi) * (i/sinusoidePoints));
}

#Moments function
moment = function(x,order=1) {
  T = length(x)
  xtransform =  x-mean(x);
  m = xtransform^order
  mean(m);
}

#Kurtosis function
kurtosis = function(x) {
  kurt = moment(x,order=4)/(moment(x, order=2)^2);
  kurt;
}

#varianza e kurtosis del dataset senza la sinuzione
v = var(newDataSet)
k = kurtosis(newDataSet) -3

#calcolo della varianza Y
a = -24/5
b = 6 * v
c =-3*v^2 - (k-3) * v^2
t1 = (-b + sqrt(b^2 - 4*a*c)  )/(2*a)
t2 = (-b - sqrt(b^2 - 4*a*c)  )/(2*a)
sigma_z_1  = v -t1
sigma_z_2  = v -t2
vz =sqrt(sigma_z_1)

#calcolo della varianza z
sigma_y_1 = v -sigma_z_1;
sigma_y_2 = v -sigma_z_2;

s1 = ( sigma_y_1  * 3) / (pi^2) 
s2 = ( sigma_y_2 * 3) / (pi^2)
sy1 =sqrt(s1)
sy2 =sqrt(s2)


#viene stima lintervalo di confidenza
#numero di sample per ogni campione
numSamples =75
#di modo ad avere la sinusoide completa
dimSamples = (sinusoideCount / numSamples) * sinusoidePoints


## divido il numero il dataset in campioni
samples = c();
samplesSenzaSinusoide = c();
for (i in 1:numSamples){
  dim1 =  dimSamples * (i - 1) + 1;
  dim2 =  dimSamples *(i);
  samples[i]               = mean(dataSet$x[dim1 : dim2]);
}


##Stima con la sinuzionde
limit1 <- mean(samples) - (1.96 * sd(samples))/sqrt(numSamples);
limit2 <- mean(samples) + (1.96 * sd(samples))/sqrt(numSamples);

limit1;
mean(samples);
limit2;

#fine risultati
print("A")
print(A)
print("f")
print(sinusoideFrequenza)
print("u")
print(meanDataSet)
print("s Logistic")
print(sy1)
print(sy2)
print("v Gaussian")
print(vz)

print("Stima con Sinuzionide")
print("limit dow")
print(limit1);
print("mean")
print(mean(samples));
print("limit up");
print(limit2);

print("End");



