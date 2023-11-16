#===============================================================================
# Título : Medidas Estatísticas - Tendência central e posição (ou separatrizes)
# Autores: Gabriel Figueiredo, Matheus Almeida e Raí Bizerra
# Objetivo: Cálculo das principais medidas estatísticas
# Correções e adições: 2023-05-23 16:11:22 - J.C.Faria
#===============================================================================

#. Tendência central
#  - Média
#  - Mediana
#  - Moda

#. Posição ou separatrizes
#  - Quartis
#  - Decis
#  - Percentis

#. Carregando os pacotes necessários
# install.packages('fdth')
# install.packages('psych')
library(fdth)
library(psych)

#--------------------------------------------------
#. Função           Medida             Pacote
#--------------------------------------------------
#  mean             média aritmética   base, fdth
#  geometric.mean   média geométrica   psych
#  harmonic.mean    média harmônica    psych
#  mfv              moda               fdth
#  median           mediana            base, fdth
#  quantile         quantis            base, fdth
#--------------------------------------------------

#. Média
#.. Media aritimética
# Não agrupado
(y <- c(6.5, 10, 8, 9.4, 8, 6.4, 7))
mean(y)
  
# Dados previamente agrupados
# Vetor
(y <- c(15, 16, 17, 18, 22, 26))
(f <- c(2, 9, 7, 6, 5, 6))
sum(y*f)/sum(f)

# Agrupado Classes - reconstituindo uma fdt
(tb1 <- make.fdt(f=c(70, 20, 15, 15, 12, 18, 50),
                 start=0,
                 end=70))

methods(mean)
mean(tb1)  # nesse casso é da mean.fdt
    

#.. Média geral
(y1 <- 4:8)
(y2 <- 1:3)
(y3 <- 9:13)

medGeral <- function(y1,
                     y2,
                     y3)
{
	(length(y1)*mean(y1) +
  length(y2)*mean(y2) +
  length(y3)*mean(y3)) /
  (length(y1) +
   length(y2) +
   length(y3))
}
   
medGeral(y1,
         y2,
         y3)
    

#.. Média geométrica
y <- c(1.10, 1.20)
geometric.mean(y)       # library(psych)
(prod(y)^(1/length(y))) # formula média geométrica


#.. Média harmônica
y <- c(30, 20)
harmonic.mean(y)     # library(psych)
length(y)/(sum(1/y)) # formula Média harmônica


#. Moda
# Distribuição sem agrupamento de classes:
(y <- c(rep(15, 2),
        rep(16, 9),
        rep(17, 7),
        rep(18, 6),
        rep(22, 5),
        rep(26, 6)))

mfv(y) # mfv.default - library(fdth)
table(y)

# Distribuição com agrupamento de classes
(tb3 <- make.fdt(f=c(5, 7, 4, 3, 1),
                 start=4,
                 end=24))

methods(mfv)
mfv(tb3) # mfv.fdt


#. Mediana
#.. Variável discreta
#... Vetor: impar
(y <- c(rep(15, 2),
        rep(16, 9),
        rep(17, 7),
        rep(18, 6),
        rep(22, 5),
        rep(26, 6)))

length(y)
median(y)

#... Vetor: par
(y <- c(rep(15, 2),
        rep(16, 9),
        rep(17, 7),
        rep(18, 6),
        rep(22, 5),
        rep(26, 6),
        rep(27, 1)))

length(y)
median(y)
 
#... Variável contínua
(tb2 <- make.fdt(f=c(70, 20, 15, 15, 12, 18, 50),
                 start=0,
                 end=70))
methods(median)
median(tb2)  # median.fdt


#. Medidas de posição ou separatrizes
#.. Quartis
#... Vetor
(k <- c(1, 1, 2, 3, 5, 5, 6, 7, 9, 9, 10, 13))

# observar que o resultado são 5 valores: 0%   25%   50%   75%  100%
quantile(k)

# podar extremos: 3 quartis
quantile(k)[2:4]

# Observar:
(q3 <- quantile(k)[4])
(q1 <- quantile(k)[2])

# Amplitude inter-quartílica
(aiq <- as.numeric(q3 - q1))

#... A partir de uma fdt
(tb <- make.fdt(f=c(4, 9, 11, 8, 5, 3),
                start=50,
                end=74))

methods(quantile)
str(quantile.fdt)

# quantile.fdt
quantile(tb)

#... Todos os quartis
vq <- numeric()
for(n in 1:3)
  vq[n] <- quantile(tb,
                    i=n,
                    probs=seq(0, 1, .25))

names(vq) <- paste0(c(25, 50, 75),
                    '%')
round(vq, 2)


#.. Decis
#... Vetor
(k <- c( 1,  3, 5,  6,  6,  9,  10, 13, 15, 20,
        22, 24, 25, 29, 31, 31, 39, 40, 55, 90, 91))

quantile(k,
         p=seq(0, 1, .1))[2:10]   # todos os decis

#... A partir de uma fdt
(tb <- make.fdt(f=c(4, 9, 11, 8, 5, 3),
                start=50,
                end=74))

methods(quantile)
str(quantile.fdt)
d3 <- quantile(tb,
               i=3,  # decil 3
               probs=seq(0, 1, .1))      # quantile.fdt
round(d3, 2)

#... Todos os decis
vd <- numeric()
for(n in 1:9)
  vd[n] <- quantile(tb,
                    i=n,
                    probs=seq(0, 1, .1))

names(vd) <- paste0(1:9,
                    '%')
round(vd, 2)


#.. Percentis
#... Vetor
k <- c(31, 31, 37, 40, 48, 50, 51, 51, 60, 62,
       64, 65, 65, 65, 66, 74, 74, 88, 91, 92)

quantile(k,
         p=seq(0, 1, .01))[2:100]  # todos os percentis

quantile(k,
         p=seq(0, 1, .01))[71]     # percentil 70

#... A partir de uma fdt
(tb <- make.fdt(f=c(5, 7, 4, 2, 2),
                start=4,
                end=24))

methods(quantile)
str(quantile.fdt)
quantile(tb,
         i=75,  # percentil 75
         probs=seq(0, 1, .01))  # quantile.fdt

#... Todos os percentis
vp <- numeric()
for(n in 1:99)
  vp[n] <- quantile(tb,
                    i=n,
                    probs=seq(0, 1, .01))

names(vp) <- paste0(1:99,
                    '%')
round(vp, 2)
