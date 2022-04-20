#zaladowanie bilbiotek do animacji 
library(animation)

#pomiar czasu
start <- Sys.time()

#inicjalizacja paska postępu
##prog_bar <- txtProgressBar(min = 0, max = niter, style = 3)

#wymiary macierzy
N = 200

#fuzycyjnosc osrodka w którym rozchodzi się ciepło
a = 2 

#węzeł siatki
h = 5

#czas
t = (h^2)/(4*a)

L_max <- max(L)
dt <- h^2/(4*L_max)

pos <- round(runif(2)*(N-2)+1)
#utworzenie macierzy dla kroku nowego i terazniejszego oraz zainicajlizowanie jej zerami
L <- matrix(nrow = N, ncol = N, 0)
Lnew <- matrix(nrow = N, ncol = N, 0)

#warunki brzegowe
L[,1] <- rep(0, N)
L[,N] < rep(0,N) 
L[1,] <- rep(0,N) 
L[N,] <- rep(50,N) #temperatura 50 stopni na gloebokosci 1000m (otatni wiersz, koszlumny sie zmienaija)

#zainicjalizowanie trzech losowo rozmieszczonych niejednorodności
#cos takieg da nam trzy różne spolrzędne srodków
center1 <- round(runif(2)*(N-2)+1)
center2 <- round(runif(2)*(N-2)+1)
center3 <- round(runif(2)*(N-2)+1)

#zmiana dyfuzyjnosci cieplnej niejednosrodnosci
for(i in 1:200){
  for (j in 1:200){
    
    if ((i - center1[1])^2 + (j - center1[2])^2 <= 10^2){
      Lnew[i,j] <- 3*a
    }
    
    if ((i - center2[1])^2 + (j - center2[2])^2 <= 10^2){
      Lnew[i,j] <- 3*a
    }
    
    if ((i - center3[1])^2 + (j - center3[2])^2 <= 10^2){
      Lnew[i,j] <- 3*a
    }
    
  }
  
}

#odwrocenie
Lcircleimg <- apply(Lnew, 2, rev)
image(t(Lcircleimg))

#główna pętla

licznik = 0

saveGIF({
    
  while(TRUE){
    
    #pasek postepu
    ##stepi<- (-1)
    ##for(k in 1:licznik)
    ##{
##d
      ##stepi <- stepi +1
      ##setTxtProgressBar(prog_bar, stepi)
    #}
    
    for(i in 2:199){
      for(j in 2:199){
        #równanie opisujące przewodnitwo cieplne w naszym modelu
        Lnew[i,j] <- (1-(4*t*a)/(h^2))*L[i,j] + t*a*((L[i, j-1] + L[i-1, j] + L[i+1, j] + L[i, j+1])/(h^2))
      }
    }
    
    pom <- pos+round(runif(2)*2-1)
    
    
    #while(pom[1] <= 0 || pom[1] >= N || pom[2] <= 0 || pom[2] >= N){
     # pom <- pos + round(runif(2)*2-1)
    #}
    
    #II wersja
    #while(TRUE){
     # pom <- pos + round(runif(2)*2-1)
    #}
    
    
    Lnew[pos[1], pos[2]] <- 1
    
    L <- Lnew
    
    licznik = licznik +1
    
    if(licznik %% 10 == 1){
      Limg <- apply(L, 2, rev)
      image(t(Limg))
      text(0, 1, licznik)
    }
    
    if(licznik == 1000){
      break
    }
      
    
  }
  
  
  
  
  
  #koniecPmiaruCzasu 
  stop <- Sys.time()
  
  #wypisanie czasu trwania
  stop - start
  
})














