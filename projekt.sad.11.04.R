table=read.csv("https://mimuw.edu.pl/~dot/resources/sad/laptops.csv",sep=";")

#a)
ram1=table[table$ram==1,]
ram11=nrow(ram1[ram1$company==1,])
ram12=nrow(ram1[ram1$company==2,])
ram13=nrow(ram1[ram1$company==3,])
ram14=nrow(ram1[ram1$company==4,])
ram15=nrow(ram1[ram1$company==5,])
ram16=nrow(ram1[ram1$company==6,])
ram17=nrow(ram1[ram1$company==7,])

ram2=table[table$ram==2,]
ram21=nrow(ram2[ram2$company==1,])
ram22=nrow(ram2[ram2$company==2,])
ram23=nrow(ram2[ram2$company==3,])
ram24=nrow(ram2[ram2$company==4,])
ram25=nrow(ram2[ram2$company==5,])
ram26=nrow(ram2[ram2$company==6,])
ram27=nrow(ram2[ram2$company==7,])

ram3=table[table$ram==3,]
ram31=nrow(ram3[ram3$company==1,])
ram32=nrow(ram3[ram3$company==2,])
ram33=nrow(ram3[ram3$company==3,])
ram34=nrow(ram3[ram3$company==4,])
ram35=nrow(ram3[ram3$company==5,])
ram36=nrow(ram3[ram3$company==6,])
ram37=nrow(ram3[ram3$company==7,])

ram4=table[table$ram==4,]
ram41=nrow(ram4[ram4$company==1,])
ram42=nrow(ram4[ram4$company==2,])
ram43=nrow(ram4[ram4$company==3,])
ram44=nrow(ram4[ram4$company==4,])
ram45=nrow(ram4[ram4$company==5,])
ram46=nrow(ram4[ram4$company==6,])
ram47=nrow(ram4[ram4$company==7,])

daneram=c(ram11,ram12,ram13,ram14,ram15,ram16,ram17,
          ram21,ram22,ram23,ram24,ram25,ram26,ram27,
          ram31,ram32,ram33,ram34,ram35,ram36,ram37,
          ram41,ram42,ram43,ram44,ram45,ram46,ram47)

#wykres stosowania pamiêci ram w firmach
library(tidyverse)
ggplot(data=table,aes(x=company))+
  geom_bar(aes(fill=ram))

rownames=c("com1","com2","com3","com4","com5","com6","com7")
colnames=c("ram1","ram2","ram3","ram4")
ramcom=matrix(daneram,nrow=7,4,dimnames = list(rownames,colnames))

wynik <- chisq.test(ramcom)

#odrzucamy hipotezê zerow¹ o niezale¿noœci
#b)

note_hp=table[table$typename==4 & table$company==4,]
note_len=table[table$typename==4 & table$company==5,]
ram_note_hp=c(nrow(note_hp[note_hp$ram==1,]),nrow(note_hp[note_hp$ram==2,]),
              nrow(note_hp[note_hp$ram==3,]),nrow(note_hp[note_hp$ram==4,]))
ram_note_len=c(nrow(note_len[note_len$ram==1,]),nrow(note_len[note_len$ram==2,]),
               nrow(note_len[note_len$ram==3,]),nrow(note_len[note_len$ram==4,]))
table$company=as.character(table$company)
table$ram=as.character(table$ram)

a1=tibble(company="HP",n=note_hp$ram)
a2=tibble(company="Lenovo",n=note_len$ram)

library(tidyverse)
#wykres rozk³adu ram
ggplot(rbind(a1,a2), aes(x=n,fill=company)) + 
  geom_histogram(position='dodge',bins=10) +
  labs(x="ram")

#lenovo wyznacza prob
observed1 <- ram_note_hp  		 
prob1 <- ram_note_len/sum(ram_note_len) 
chisq.test(observed1,p=prob1)
#p=0.595, nie odrzucamy hipotezy zerowej, 
#ram mo¿e pochodziæ z tego samego rozk³adu (multinomial)

#hp wyznacza prob
observed2 <- ram_note_len  		 
prob2 <- ram_note_hp/sum(ram_note_hp)
chisq.test(observed2,p=prob2)
#nie wychodzi, bo prob(4)=0

#œrednia wyznacza prob
chisq.test(observed1,p=(prob1+prob2)/2) #p=0.856
chisq.test(observed2,p=(prob1+prob2)/2) #p=0.862
#zostaje H0, mog¹ pochodziæ z tego samego rozk³adu p=(prob1+prob2)

#c)

ceny_del=table[table$company==3 & table$typename==4,"price_euros"]
ceny_hp=table[table$company==4 & table$typename==4,"price_euros"]

table[table$company==3 & table$typename==4,]
table[table$company==4 & table$typename==4,]

ceny_hp_ln=log(ceny_hp)
ceny_del_ln=log(ceny_del)
var(ceny_del_ln)
var(ceny_hp_ln)
t.test(ceny_del_ln,ceny_hp_ln,alternative = "two.sided", var.equal = FALSE)
#p=0.1396

ceny_del_ln_gg=tibble(price=ceny_del_ln,company="del")
ceny_hp_ln_gg=tibble(price=ceny_hp_ln,company="hp")
#wykres cen zlogarytmowanych
ggplot(rbind(ceny_del_ln_gg,ceny_hp_ln_gg),aes(x=price,fill=company))+
  geom_histogram(bins=20)

