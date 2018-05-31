x = airquality$Wind
ph = 6
n =147

plot(x,type = "l")
data=x[1:(n-ph)]
acf(x)
pacf(x)
ar100 = arima(data,order = c(1,0,0))
ar200 = arima(data,order = c(1,0,10))
ar500 = arima(data,order = c(2,0,0))
ar208 = arima(data,order = c(2,0,10))
pred100=predict(ar100,n.ahead=ph)
pred200=predict(ar200,n.ahead=ph)
pred500=predict(ar500,n.ahead=ph)
pred208=predict(ar208,n.ahead=ph)
plot(x,type = "l",xlim = c(140,153))
lines(pred100$pred,type = "o",col="red")
lines(pred200$pred,type = "o",col="green")
lines(pred500$pred,type = "o",col="blue")
lines(pred208$pred,type = "o",col="yellow")
vec1 = x[(n-ph+1):n]-pred100$pred
vec2 = x[(n-ph+1):n]-pred200$pred
vec3 = x[(n-ph+1):n]-pred500$pred
vec4 = x[(n-ph+1):n]-pred208$pred
mse1=mean(vec1*vec1)
mse2=mean(vec2*vec2)
mse3=mean(vec3*vec3)
mse4=mean(vec4*vec4)
model = c("arima100","arima200","arima500","arima208")
err2=c(mse1,mse2,mse3,mse4)
err=data.frame(model,err2)
print(err2)

