par(mfrow=c(1,1), mai = c(0.85,1,0.3,0.1))
s = 1000
x = rnorm(s)
y = rnorm(s)
plot(x,y, 
     xlim = c(-4,4), ylim = c(-4,4),
     main = 'Muestreo Aleatorio',
     xlab = 'Variable 1 \n (e.g. Educación)',
     ylab = 'Variable 2 \n (e.g. Ideología política)')
legend('topright', c('Población', 'Muestra'), bty = 'n', 
       pch = c(1,16), col = c('black','red'))
idx = round(runif(round(0.05*s),1,s))
points(x[idx], y[idx], col = 'red', pch = 16)

plot(x,y, 
     xlim = c(-4,4), ylim = c(-4,4),
     main = 'Muestreo Estratificado',
     xlab = 'Variable 1 \n (e.g. Educación)',
     ylab = 'Variable 2 \n (e.g. Ideología política)')
legend('topright', c('Población'), bty = 'n', 
       pch = c(1,16), col = c('black'))
idx = x<0 & y<0
points(x[idx], y[idx], col = 'forestgreen')
idx = x<0 & y>=0
points(x[idx], y[idx], col = 'purple')


plot(x,y, 
     xlim = c(-4,4), ylim = c(-4,4),
     main = 'Muestreo Estratificado',
     xlab = 'Variable 1 \n (e.g. Educación)',
     ylab = 'Variable 2 \n (e.g. Ideología política)')
legend('topright', c('Población', 'Muestra'), bty = 'n', 
       pch = c(1,16), col = c('black','red'))
idx = x<0 & y<0
points(x[idx], y[idx], col = 'forestgreen')
idx = x<0 & y>=0
points(x[idx], y[idx], col = 'purple')
tempx = x[idx]
tempy = y[idx]
idx = round(runif(round(0.05*sum(idx)),1,sum(idx)))
points(tempx[idx], tempy[idx], col = 'red', pch = 16)


plot(x,y, 
     xlim = c(-4,4), ylim = c(-4,4),
     main = 'Muestreo sin sesgo',
     xlab = 'Variable 1 \n (e.g. Educación)',
     ylab = 'Variable 2 \n (e.g. Ideología política)')
legend('topright', c('Población', 'Muestra'), bty = 'n', 
       pch = c(1,16), col = c('black','black'))
idx = x<0 & y<0
tempx = x[idx]
tempy = y[idx]
idx = round(runif(round(0.05*sum(idx)),1,sum(idx)))
points(tempx[idx], tempy[idx], col = 'forestgreen', pch=16)
idx = x<0 & y>=0
tempx = x[idx]
tempy = y[idx]
idx = round(runif(round(0.05*sum(idx)),1,sum(idx)))
points(tempx[idx], tempy[idx], col = 'purple', pch = 16)
idx = x>0
tempx = x[idx]
tempy = y[idx]
idx = round(runif(round(0.05*sum(idx)),1,sum(idx)))
points(tempx[idx], tempy[idx], col = 'red', pch = 16)



plot(x,y, 
     xlim = c(-4,4), ylim = c(-4,4),
     main = 'Muestreo con sesgo',
     xlab = 'Variable 1 \n (e.g. Educación)',
     ylab = 'Variable 2 \n (e.g. Ideología política)')
legend('topright', c('Población', 'Muestra'), bty = 'n', 
       pch = c(1,16), col = c('black','black'))
idx = x<0 & y<0
tempx = x[idx]
tempy = y[idx]
idx = round(runif(round(0.05*sum(idx)),1,sum(idx)))
points(tempx[idx], tempy[idx], col = 'forestgreen', pch=16)
idx = x<0 & y>=0
tempx = x[idx]
tempy = y[idx]
idx = round(runif(round(0.8*sum(idx)),1,sum(idx)))
points(tempx[idx], tempy[idx], col = 'purple', pch = 16)
idx = x>0
tempx = x[idx]
tempy = y[idx]
idx = round(runif(round(0.025*sum(idx)),1,sum(idx)))
points(tempx[idx], tempy[idx], col = 'red', pch = 16)

