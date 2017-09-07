student<-data.frame(Name=c(LETTERS[1:5]),
                    Age=c(23,22,25,26,27),
                    Maths_marks=c(78,89,90,88,78),
                    Science_marks=c(67,76,78,87,89))

student$total_marks<-student$Maths_marks+student$Science_marks

student$pct_Maths_marks<-student$Maths_marks/student$total_marks

?round

student$pct_Maths_marks<-round(100*(student$Maths_marks/student$total_marks))

student1<-student[,c(2:6)]

student1<-student1[,-c(5)]

student2<-student1[,c("Age","total_marks")]

student3<-student1[,seq(2,ncol(student1),2)] #only alternate columns

names(student)

colnames(student)

colnames_student<-names(student)
colnames_student
colnames_student<-c("A","B","C","D","E","F")
names(student)<-colnames_student

names(student)[3]<-"New_Maths"

student[3,2]<-100



student[3,2]<-24

student1$Maths_marks_log<-log(student1$Maths_marks)
student1$Maths_marks_square<-(student1$Maths_marks*student1$Maths_marks/1000)
student1$Maths_marks_inv<-(1/student1$Maths_marks*100)
student1$Maths_marks_exp<-exp(student1$Maths_marks/mean(student1$Maths_marks))

class(student1$Maths_marks)

nn<-c("a",1)
class(nn)
nn.numeric<-as.numeric(nn)
is.na(nn.numeric)
table(is.na(nn.numeric)) #display of summary of number of T and F


#Pick up rows based on a condition, time-series analysis, apply filter condition in R
student
s1<-student[student$B>=24,]
s1<-student[!student$B>=24,]
s2<-student[!s1,] #not working!
s2
s3<-student[student$B>=24 & student$D>=80,c(1:5)]
s4<-student[!s3$A,] #not working!


sampleStudent<-sample(student,3,replace=T)
sampleStudent
sample_index<-sample(1:nrow(student),3,replace=F)
sample_index
student<-student[sample_index,]
view(student)

?subset









