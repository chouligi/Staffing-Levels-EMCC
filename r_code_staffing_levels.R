require(xtable); require(tikzDevice) # Latex packages
library(queueing)
library(lubridate)
library(plyr)


data = read.csv('ASM_data_csv.csv',sep = ';')

#format the data

data$arrival = as.POSIXct(data$arrival, format = "%d/%m/%Y %H:%M:%S")
data$pick.up = as.POSIXct(data$pick.up, format = "%d/%m/%Y %H:%M:%S")
data$hang.up = as.POSIXct(data$hang.up, format = "%d/%m/%Y %H:%M:%S")


#calculate response time and duration call
#time until pick up
data$response = as.numeric(difftime(data$pick.up, data$arrival,units = 'secs') )

#service time = call duration
data$service = as.numeric(difftime(data$hang.up, data$pick.up,units = 'secs') )

#mean response time

avg_resp = mean(data$response) #mean waiting time EWq (TARGET = 6 seconds)

#mean service time

avg_serv = mean(data$service)  # E[S]

var_serv = var(data$service)


#squared coefficient of variation for service time
scv_serv = var(data$service)/avg_serv^2

# Average connection time and call duration per day of the week
for (i in 1:7) { print(mean(data[wday(data$arrival) == i, "response"])) }
for (i in 1:7) { print(mean(data[wday(data$arrival) == i, 'service'])) }


serv_per_hour = matrix(nrow=24,ncol=1)
#average service time per hour  
for (i in 0:23) { print(mean(data[hour(data$arrival) == i, 'service'])) }
for (i in 0:23) { serv_per_hour[i+1] = (mean(data[hour(data$arrival) == i, 'service'])) }
plot(serv_per_hour, type ='l',main = 'Average service time per hour')


#Plot total arrivals per time interval
# Set time interval 00:00:00" "00:15:00" "00:30:00" "00:45:00 ...
int <- 60 

# Count number of arriving calls and plot 
dt <- strftime(data$arrival, format="%H:%M:%S") #give specific date format ("18:01:47") to the observations
dt <- as.numeric(difftime(as.POSIXct(dt, format = "%H:%M:%S"),   #format to seconds
                          as.POSIXct("00:00:00", format = "%H:%M:%S")))
round15 <- dt - (dt %% (int*60))   # int*60 is the number of seconds in 15 minutes!
round15 <- table(round15) #count arriving calls per 15 minutes

#strftime convert objects from the classes "POSIXlt" and "POSIXct" to character vectors.

labels <- strftime(seq(as.POSIXct("00:00:00", format = "%H:%M:%S"), by = int*60,
                       to = as.POSIXct("24:00:00", format = "%H:%M:%S")), 
                   format = "%H:%M:%S")   #fix labels per 15 mins "00:00:00" "00:15:00" "00:30:00" "00:45:00"..
names(round15) <- labels[1:length(round15)] #give the labels to round15 

plot(round15, main = "Number of arriving calls per hour",
     xlab = "Time", ylab = "Average number of arriving calls")


# Count number of arriving calls and plot per day
for (i in 1:7) {
  dt <- strftime(data$arrival[wday(data$arrival) == i], format = "%H:%M:%S")
  dt <- as.numeric(difftime(as.POSIXct(dt, format = "%H:%M:%S"), 
                            as.POSIXct("00:00:00", format = "%H:%M:%S"),
                            units = "secs"))
  round15 <- dt - (dt %% (int*60))
  round15 <- table(round15)
  
  labels <- strftime(seq(as.POSIXct("00:00:00", format = "%H:%M:%S"), by = int*60,
                         to = as.POSIXct("24:00:00", format = "%H:%M:%S")), 
                     format = "%H:%M:%S")
  names(round15) <- labels[1:length(round15)]
  
  plot(round15, xlab = "Time", ylab = "# arriving calls",
       main = paste("Number of arriving calls per time interval at", 
                    wday(i, label = TRUE, abbr = FALSE)))
}  
  
  ### Service Time distribution
  
  #draft plot
  S= table(data$service)
  
  plot(S, xlab = 'Call duration',ylab ='Frequency',main='Service time distribution')
  


#find the number of arrivals per hour for every day
arrivals_perhour <- table(cut(data$arrival, breaks="hour"))


mat = matrix(nrow = 24, ncol = 61)

#arrivals for one hour for EVERY DAY
for (i in 1:24){
  for (j in 0:60){
    mat[i,j+1] = arrivals_perhour[i + j*24]
  }
}
for (i in 1:23){
  plot(mat[i,],type='l', main = 'number of arrivals per one hour for every day ', xlab = 'day',ylab ='number of arrivals')
}

#plot arrivals per day and per hour
monday = matrix(nrow=24,ncol=8)#rows = hours, col = week
thusday = matrix(nrow=24,ncol=8)#rows = hours, col = week
wednesday = matrix(nrow=24,ncol=8)#rows = hours, col = week
thursday = matrix(nrow=24,ncol=8)#rows = hours, col = week
friday = matrix(nrow=24,ncol=8)#rows = hours, col = week
saturday = matrix(nrow=24,ncol=8)#rows = hours, col = week
sunday = matrix(nrow=24,ncol=8)#rows = hours, col = week



for (i in (1:24)){
  for (j in 0:7){
    monday[i,j+1] = arrivals_perhour[i+1 + 7*j*24]
    thusday[i,j+1] = arrivals_perhour[i + 24 + 7*j*24]
    wednesday[i,j+1] = arrivals_perhour[i+ 2*24 + 7*j*24]
    thursday[i,j+1] = arrivals_perhour[i+ 3*24 +7*j*24 ]
    friday[i,j+1] = arrivals_perhour[i+4*24 +7*j*24 ]
    saturday[i,j+1] = arrivals_perhour[i+5*24 +7*j*24 ]
    sunday[i,j+1] = arrivals_perhour[i+6*24 +7*j*24 ]
    
  }
}

avg_monday = matrix(nrow = 24,ncol=1)
avg_thusday = matrix(nrow = 24,ncol=1)
avg_wednesday = matrix(nrow = 24,ncol=1)
avg_thursday = matrix(nrow = 24,ncol=1)
avg_friday = matrix(nrow = 24,ncol=1)
avg_saturday = matrix(nrow = 24,ncol=1)
avg_sunday = matrix(nrow = 24,ncol=1)
#mean number of arrivals on Monday (of all weeks)
for (i in 1:24){
  avg_monday[i] = mean(monday[i,])
  avg_thusday[i] = mean(thusday[i,])
  avg_wednesday[i] = mean(wednesday[i,])
  avg_thursday[i] = mean(thursday[i,])
  avg_friday[i] = mean(friday[i,])
  avg_saturday[i] = mean(saturday[i,])
  avg_sunday[i] = mean(sunday[i,])
}

x = c(1:24)

plot (avg_monday, type = 'l', col = 'red', main = 'Arrivals per hour and per day',ylab = 'Average number of arrivals',xlab = 'Hour of the day', xlim = c(1,24), xaxt = 'n')
axis(x,at = x)
lines (avg_thusday, type = 'l', col = 'blue')
lines (avg_wednesday, type = 'l', col = 'green')
lines (avg_thursday, type = 'l', col = 'black')
lines (avg_friday, type = 'l', col = 'cyan')
lines (avg_saturday, type = 'l', col = 'yellow')
lines (avg_sunday, type = 'l', col = 'purple')
legend("topright", legend = c("Monday","Thusday",'Wednesday','Thursday','Friday','Saturday','Sunday'),pch =c(15,15),lwd=1,lty=c(NA,NA),cex = 0.6, col=c("red","blue",'green','black','cyan','yellow','purple'),bty = 'n')

#plot original waiting times!
#response
response  = matrix(nrow = 24,ncol = 7)
for (i in 0:23){
  for (j in 1:7){
    response[i+1,j] = mean(data[hour(data$arrival) == i & wday(data$arrival) == j, 'response'])
  }  
}
y = c(1:14)

plot(response[,1],type = 'l', col = 'red', main = 'Mean waiting time per hour and per day',ylab = 'Mean Waiting time (seconds)',xlab = 'Hour of the day', xlim = c(1,24),ylim=c(1,14), xaxt = 'n',yaxt ='n')
axis(x,at = x)
axis(side =2,at=y)
lines (response[,2], type = 'l', col = 'blue')
lines (response[,3], type = 'l', col = 'green')
lines (response[,4], type = 'l', col = 'black')
lines (response[,5], type = 'l', col = 'cyan')
lines (response[,6], type = 'l', col = 'yellow')
lines (response[,7], type = 'l', col = 'purple')
legend("bottomright", legend = c("Sunday","Monday",'Thusday','Wednesday','Thursday','Friday','Saturday'),pch =c(15,15),lwd=1,lty=c(NA,NA),cex = 0.5, col=c("red","blue",'green','black','cyan','yellow','purple'),bty = 'n')
abline(6,0,lty=5,col = 'black')
#make matrix arrivals per day and per hour
arrivals = matrix (nrow = 24,ncol =7)
arrivals_monday = apply(monday,1,mean) # 1 to take average over the rows
arrivals_thusday = apply(thusday,1,mean)
arrivals_wednesday = apply(wednesday,1,mean)
arrivals_thursday = apply(thursday,1,mean)
arrivals_friday = apply(friday,1,mean)
arrivals_saturday = apply(saturday,1,mean)
arrivals_sunday = apply(sunday,1,mean)


#lambdas for every hour and for every day (means)
arrivals2 = (cbind(arrivals_monday,arrivals_thusday,arrivals_wednesday,arrivals_thursday,arrivals_friday,arrivals_saturday,arrivals_sunday))/3600

arrivals = (cbind(arrivals_sunday,arrivals_monday,arrivals_thusday,arrivals_wednesday,arrivals_thursday,arrivals_friday,arrivals_saturday))/3600



#now make matrix with service times per hour for every day


#service time per day and per hour
service  = matrix(nrow = 24,ncol = 7)

for (i in 0:23){
  for (j in 1:7){
    service[i+1,j] = mean(data[hour(data$arrival) == i & wday(data$arrival) == j, 'service'])
  }  
}


#plot service time per day and per hour
y=seq(from=30,to=130,by=10)
x= c(1:24)

plot(service[,1],type = 'l', col = 'red', main = 'Mean service time per hour and per day',ylab = 'Mean Service time (seconds)',xlab = 'Hour of the day', xlim = c(1,24),ylim=c(30,130), xaxt = 'n',yaxt ='n')
axis(x,at = x)
axis(side =2,at=y)
lines (service[,2], type = 'l', col = 'blue')
lines (service[,3], type = 'l', col = 'green')
lines (service[,4], type = 'l', col = 'black')
lines (service[,5], type = 'l', col = 'cyan')
lines (service[,6], type = 'l', col = 'yellow')
lines (service[,7], type = 'l', col = 'purple')
legend("topright", legend = c("Sunday","Monday",'Thusday','Wednesday','Thursday','Friday','Saturday'),pch =c(15,15),lwd=1,lty=c(NA,NA),cex = 0.5, col=c("red","blue",'green','black','cyan','yellow','purple'),bty = 'n')


#load
load = matrix(nrow=24,ncol=7)

load= arrivals * service

mu = matrix(nrow = 24,ncol = 7)
mu = 1/service



#CALCULATIONS!!!
#number of servers
s = 5
a = load[1,1]
ro = a/s  #load per server


sum=0
#probability of delay
for(j in 0:(s-1)){
  sum=(a^j)/factorial(j) + sum
}
C = (a^s)/(factorial(s-1)*(s-a)) * (1/(sum+ a^s/((factorial(s-1)*(s-a))))) #Correct

#theoretical mean waiting time in queue
EWq_MMC = C/(s*mu[1,1]-arrivals[1,1]) #with s=1 it is equal to mean waiting time for M/M/1 so it is correct!


EWq_MMC
EWq_MM1 = load[1,1]/(mu[1,1]*( 1- load[1,1]))
EWq_MM1


input_MMC = NewInput.MMC(lambda = arrivals[1,1], mu = mu[1,1] , n=0,method = 0 , c=5)

MMC = QueueingModel(input_MMC)
summary(MMC)




MMC$Wq #mean time spent in queue (CHECK IT USING THE ACTUAL FORMULA) 
#calculate r (load per server) and a(load)

ro = matrix(nrow=24, ncol=7)
a = matrix(nrow=24, ncol=7)
for (i in 1:24){
  for (j in 1:7){
    a[i,j] =load[i,j]
  }
}
#calculate the optimal agents for target level EWq = 6s
optimal = matrix(nrow=24, ncol=7)
input_MMC  = matrix(nrow=24, ncol=7)
MMC= matrix(nrow=24, ncol=7)
Wq = matrix(nrow=24, ncol=7)
for (i in 1:24){
    for (j in 1:7){
        for (k in 1:10){
          load2 = arrivals[i,j]/(mu[i,j]*k)
          c = k
          if (load2 >= 1) {
            c = k+1
            x = NewInput.MMC(lambda = arrivals[i,j], mu = mu[i,j] , n=1,method = 0 , c=c)
          }
          else {
            x = NewInput.MMC(lambda = arrivals[i,j], mu = mu[i,j] , n=1,method = 0 , c=c)
          }
          MMC= QueueingModel(x)
          if  (MMC$Wq < 6 ){
            optimal[i,j] = c
            Wq[i,j] = MMC$Wq
            break
          }
        }
      }  
}
    
    
    plot(Wq[,1],type='l')
    
    
    
    new_inp = NewInput.MMC(lambda = arrivals[9,6], mu = mu[9,6] , n=1,method = 0 , c=2)
    MMC2= QueueingModel(new_inp)
    MMC2$Wq
    
    #overall mean waiting time after the model implementation
    
    mean(Wq) #2.1436
    
    
    ###plot mean waiting time with the optimal agents
    y = c(1:7)
x= c(1:24)


    plot(Wq[,1],type = 'l', col = 'red', main = 'Mean waiting time per hour and per day',ylab = 'Mean Waiting time (seconds)',xlab = 'Hour of the day', xlim = c(1,24),ylim=c(1,7), xaxt = 'n',yaxt ='n')
    axis(x,at = x)
    axis(side =2,at=y)
    lines (Wq[,2], type = 'l', col = 'blue')
    lines (Wq[,3], type = 'l', col = 'green')
    lines (Wq[,4], type = 'l', col = 'black')
    lines (Wq[,5], type = 'l', col = 'cyan')
    lines (Wq[,6], type = 'l', col = 'yellow')
    lines (Wq[,7], type = 'l', col = 'purple')
    legend("topleft", legend = c("Sunday","Monday",'Thusday','Wednesday','Thursday','Friday','Saturday'),pch =c(15,15),lwd=1,lty=c(NA,NA),cex = 0.6, col=c("red","blue",'green','black','cyan','yellow','purple'),bty = 'n')
    abline(6,0,lty=5,col = 'black')

