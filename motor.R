#IMPORTING DATASET
data <- read.csv("pmsm_temperature_data.csv")

#SETTING THE AMOUNT OF THE DATASET
mysample <- data[sample(1:nrow(data), 3000,replace=FALSE),]

#CHECKING THE HEAD OF THE DATASET 
head(data)

#CHECKING THE STRUCTURE OF THE DATASET 
str(data)

#FORMING TABLE
table(is.na(data))

#CREATING THE HISTOGRAM TAKING THE ATTRIBUTE MOTOR_SPEED
hist(data$motor_speed)
line(data$torque)

#CRAETING A BAR-PLOT
barplot(t,a,xlab="torque",ylab="ambient",col="blue",main="TORQUE VS AMBIENT",border="red")

#TAKING COLUMN DATA INTO VARIABLE 
t <-data$torque
a <-data$ambient

head(t)

#MAXIMUN ELEMENT FOR THE ATTRIBUTE AMBIENT
max(data$ambient)

#CHECKING THE TAIL OF THE DATA
tail(data)

#TAKING COLUMN DATA INTO VARIABLE 
c<-data$coolant

#CRAETING A BAR-PLOT
barplot(t,c,xlab="torque",ylab="coolant",main="TORQUE VS COOLANT",border="blue")
plot(mean(data$motor_speed))
plot(data$ambient,data$coolant)

#mysample <- data[sample(1:nrow(data), 3000,replace=FALSE),]
#CHECKING THE HEAD OF MYSAMPLE
head(mysample)  
table(is.na(mysample))

#TAKING COLUMN DATA INTO VARIABLE 
t1 <- mysample$torque
speed <- mysample$motor_speed

#CREATING THE BAR-PLOT
barplot(t1,speed,xlab="torque",ylab="motor_speed",main="TORQUE VS MOTOT_SPEED",border="blue")
d2<-mysample

# CREATING THE HEATMAP OF MYSAMPLE 
motor_temp.heatmap <- ggplot(data = mysample, mapping = aes(x = profile_id, y =torque , )) +
geom_line("orange") + xlab(label = "Sample")
motor_temp.heatmap

#PLOTING PLOT AND FACIDGRID
plot(t1,type="o")+FacetGrid

#PLOTING HISTOGRAM
hist(t1,xlab = "torque",col = "yellow",border = "blue")

#CREATING BAR-PLOT
barplot(speed,names.arg=t1,xlab="torque",ylab="speed",col="blue",main="torque vs speed",border="red")

#IMPORTING GGPLOT2
library(ggplot2)

#CREATING PLOT USING GGPLOT-2
ggplot(mysample, aes(x = profile_id, y =torque  )) + geom_line(colour = "green",size=1.5)+ theme_dark()
ggplot(mysample, aes(x = i_d, y =torque  )) + geom_line(colour = "purple",size=1.5)+ theme_dark()
a <- ggplot(mysample, aes(x = i_q, y =torque  )) + geom_line(colour = "",size=1.5)+ theme_dark()
p<-ggplot(data=mysample)
p+geom_line(aes(x=torque,y=i_q,color=profile_id))

#CREATING PLOT USING GGPLOT-2
ggplot(mysample, aes(x = u_q, y =torque  )) + geom_line(colour = "cyan",size=1.5)+ theme_dark()
a <- ggplot(mysample, aes(x = u_d, y =torque  )) + geom_line(colour = "brown",size=1.5)+ theme_dark()

plot(df1,type="o")
df1 <- list(mysample$pm,mysample$stator_yoke,mysample$stator_tooth,mysample$stator_winding)
head(df1,limit=6)
p+geom_line(aes(x=,y=speed,color="brown",size=1.5))+theme_dark()

#CREATING PLOT USING GGPLOT-2
ggplot(mysample, aes(x = u_q, y =i_q )) + geom_line(colour = "violet",size=1.5)+ theme_light()
ggplot(mysample, aes(y = stator_yoke, stator_winding )) + geom_line(colour = "yellow",size=1.5)+ theme_dark()
p+geom_line(aes(x=ambient,y=coolant,color=torque)) 
p+geom_line(aes(x=ambient,y=coolant,color=speed)) 

#CREATING PLOT USING GGPLOT-2
p+geom_line(aes(x=ambient,y=coolant,color=stator_yoke))
p+geom_line(aes(x=ambient,y=coolant,color=stator_winding))
p+geom_line(aes(x=ambient,y=coolant,color=stator_tooth))
p+geom_line(aes(x=ambient,y=coolant,color=pm))

#CREATING PLOT USING GGPLOT-2
p+geom_line(aes(x=torque,y=speed,color=stator_yoke,size=1.5))+theme_dark()
p+geom_line(aes(x=torque,y=speed,color=stator_winding,size=1.5))+theme_dark()
p+geom_line(aes(x=torque,y=speed,color=stator_tooth,size=1.5))+theme_dark()
p+geom_line(aes(x=torque,y=speed,color=pm,size=1.5))+theme_dark()+facet_grid()

#CREATING PLOT USING GGPLOT-2
p+geom_line(aes(x=ambient,y=coolant,color=u_q))
p+geom_line(aes(x=ambient,y=coolant,color=u_d))
p+geom_line(aes(x=ambient,y=coolant,color=i_d))
p+geom_line(aes(x=ambient,y=coolant,color=i_q))

hist(df1,xlab = "torque",ylab = speed,col = "yellow",border = "blue")


