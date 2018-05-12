library(plotrix)
library(stringr)
library(ggplot2)
library(dplyr)
library(scales)
library(reshape)
###########
##fixing pol column
upitnik_data_unknown = as.data.frame(upitnik_data[,-c(1:3)])

n_occur <- data.frame(table(ime_prezime))
n_occur[n_occur$Freq > 1,]
#remove duplicates
#upitnik_data_unknown = upitnik_data_unknown[-251,]
#upitnik_filtered = upitnik_filtered[-c(151,82,95,252,163),]

#change level 
polFactor = factor()
polFactor = as.character(upitnik_filtered$Pol)
polFactor[115]=polFactor[119]
polFactor = as.factor(polFactor)
upitnik_filtered$Pol = polFactor

upitnik_filtered$Pol

##fixed
########
########
##fixing navedi drzavu i ime uni
drzaveUnis = as.character(upitnik_filtered$Navedite.državu.i.ime.univerziteta.fakulteta.gde.ste.bili.na.razmeni.studijama.u.inostranstvu..)
drzaveUnis = as.data.frame(drzaveUnis)
drzaveUnis$Drzava1 = ""
drzaveUnis$uni1 = ""
drzaveUnis$Drzava4 = ""
drzaveUnis$uni4 = ""
########
##godine pocetka
godinePocetka = as.character(upitnik_filtered$Koje.godine.ste.zapoceli.studije.razmenu.u.inostranstvu..)
godinePocetka = as.data.frame(godinePocetka)
godinePocetka$God2=""
godinePocetka$God3=""
edit(godinePocetka)
godinePocetka_ = edit(godinePocetka_)
View(godinePocetka_)
View(godinePocetka_)
godinePocetka_ = edit(godinePocetka_)
View(godinePocetka_)
View(godinePocetka_)
View(upitnik_data)
View(godinePocetka_)
godinePocetka_$godinePocetka = as.factor(godinePocetka_$godinePocetka)
godinePocetka_$God2 = as.factor(godinePocetka_$God2)
godinePocetka_$God3 = as.factor(godinePocetka_$God3)
View(godinePocetka_)
upitnik_filtered$godPocetka1=godinePocetka_$godinePocetka
upitnik_filtered$godPocetka2=godinePocetka_$God2
upitnik_filtered$godPocetka3=godinePocetka_$God3
View(upitnik_filtered)

##trajanje mobilnosti
trajanjeMobilnosti = as.data.frame(as.character(upitnik_filtered$Koliko.dugo.je.trajao.period.mobilnosti..))
trajanjeMobilnosti$trajanje = ""
trajanjeMobilnosti_ = edit(trajanjeMobilnosti_)

trajanjeMobilnosti = factor()
trajanjeMobilnosti = as.factor(trajanjeMobilnosti_$trajanjeChar)
upitnik_filtered$trajanjeMobilnosti = trajanjeMobilnosti

  
levels(u_f$Pol)[levels(u_f$Pol)==levels(u_f$Pol)[2]]<- "Muški"
levels(u_f$Pol)
#get muski, zenski
pol = levels(u_f$Pol)[u_f$Pol]
# replace NA with Muski
pol[117]=unique(pol)[1]
#replacing first factor
u_f$Pol=factor(pol)
# get strings from 2nd column
str_drz_uni = levels(u_f$Navedite.državu.i.ime.univerziteta.fakulteta.gde.ste.bili.na.razmeni.studijama.u.inostranstvu..)[u_f$Navedite.državu.i.ime.univerziteta.fakulteta.gde.ste.bili.na.razmeni.studijama.u.inostranstvu..]
#if loop if value in string


drzaveUnis_ = drzaveUnis
drzaveUnis_$uni1 = ""
drzaveUnis_$uni2 = ""
drzaveUnis_$uni3 = ""
drzaveUnis_$uni4 = ""
drzaveUnis_$uni5 = ""

drzaveUnis_$drz1 = ""
drzaveUnis_$drz2 = ""
drzaveUnis_$drz3 = ""
drzaveUnis_$drz4 = ""
drzaveUnis_$drz5 = ""


drzave = c('Poljska','USA','Nemacka','Austrija', 'Francuska', 'Spanija', 'Portugalija','Velika Britanija','Italija', 'Belgija', 
          'Holandija', 'Rumunija', 'Bugarska', 'Slovenija', 'Hrvatska', 'Ceska', 'Slovacka', 'Irska', 'Svedska', 
          'Norveska', 'Finska', 'Litvanija', "Španija","Švedska", "Grcka")



for (drzava in drzave) {
  idx = 0
  for (variable in drzaveUnis_$drzaveUnis) {
    idx = idx + 1
    #u_f$Drzava.Mobilnosti = ""
    if (grepl(drzava, variable)) {
      if(drzaveUnis_$drz1[idx]==""){
        drzaveUnis_$drz1[idx] = drzava
      }
      else if (drzaveUnis_$drz2[idx]==""){
        drzaveUnis_$drz2[idx] = drzava
        }
      else if(drzaveUnis_$drz3[idx]=="")
      {
        drzaveUnis_$drz3[idx]==drzava
      }
      else {
        drzaveUnis_$drz4[idx]==drzava
      }
      }
  }
  
}
drzaveUnis_ = edit(drzaveUnis_)
unique(drzaveUnis_$uni1)

##append to upitnik_filtered, mob unis and countries

mob_uni1 = character()
mob_uni1 = drzaveUnis_$uni1
upitnik_filtered$mob_uni1 = mob_uni1

mob_uni2 = character()
mob_uni2 = drzaveUnis_$uni2
upitnik_filtered$mob_uni2 = mob_uni2

mob_uni3 = character()
mob_uni3 = drzaveUnis_$uni3
upitnik_filtered$mob_uni3 = mob_uni3

mob_uni4 = character()
mob_uni4 = drzaveUnis_$uni4
upitnik_filtered$mob_uni4 = mob_uni4

mob_uni5 = character()
mob_uni5 = drzaveUnis_$uni5
upitnik_filtered$mob_uni5 = mob_uni5


mob_drz1 = character()
mob_drz1 = drzaveUnis_$drz1
upitnik_filtered$mob_drz1 = mob_drz1

mob_drz2 = character()
mob_drz2 = drzaveUnis_$drz2
upitnik_filtered$mob_drz2 = mob_drz2

mob_drz3 = character()
mob_drz3 = drzaveUnis_$drz3
upitnik_filtered$mob_drz3 = mob_drz3

mob_drz4 = character()
mob_drz4 = drzaveUnis_$drz4
upitnik_filtered$mob_drz4 = mob_drz4

mob_drz5 = character()
mob_drz5 = drzaveUnis_$drz5
upitnik_filtered$mob_drz5 = mob_drz5

##stefan filtered

stefan_filtered = read.csv(file="Stefan_WBAA anket3.csv",header = T,sep=";", encoding = "UTF-8")
stefan_filtered=stefan_filtered[-164,]
View(stefan_filtered[c(152,83,95,252),])
stefan_filtered = stefan_filtered[-c(152,83,95,252),]
View(stefan_filtered[,22])
stefan_filtered = stefan_filtered[,-c(22)]
stefan_filtered=stefan_filtered[-1,]
#stefan_filtered=stefan_filtered[-c(254,255),]

##maticni univerzitet

maticni_univ = factor()
maticni_univ = as.character(stefan_filtered$X.U.FEFF.Navedite.ime.maticnog.univerziteta.fakulteta.sa.kog.ste.otišli.na.razmenu.studije.u.inostranstvu.)
maticni_univ = as.data.frame(maticni_univ)
maticni_univ = edit(maticni_univ)
maticni_univ = as.character(maticni_univ$maticni_univ)
unique(maticni_univ)
maticni_univ = edit(maticni_univ)
maticni_univ_ = as.factor(maticni_univ)
upitnik_filtered$maticni_uni = maticni_univ_

##kako ste culi

nacinSaznanja = as.character(stefan_filtered$Na.koji.nacin.ste.pronašli.studijski.program.koji.ste.pohadali.u.inostranstvu.)
nacinSaznanja = as.data.frame(nacinSaznanja)
unique(nacinSaznanja)
nacinSaznanja = edit(nacinSaznanja)
nacinSaznanja = as.character(nacinSaznanja$nacinSaznanja)
nacinSaznanja_ = as.factor(nacinSaznanja)
upitnik_filtered$nacinSaznanja = nacinSaznanja_


## dodatak diplome

vidljivDodatkuDiplome = as.character(stefan_filtered$Da.li.je.period.mobilnosti..predmeti..ocene..jasno.vidljiv.u.dodatku.Vaše.diplome..Diploma.Supplement..)
vidljivDodatkuDiplome = as.data.frame(vidljivDodatkuDiplome)
vidljivDodatkuDiplome = edit(vidljivDodatkuDiplome)
vidljivDodatkuDiplome = as.character(vidljivDodatkuDiplome$vidljivDodatkuDiplome)
vidljivDodatkuDiplome_ = as.factor(vidljivDodatkuDiplome)
upitnik_filtered$VidljivDodatkuDiplome = vidljivDodatkuDiplome_
##########################
dodatakDiplKomentar = as.character(stefan_filtered$dodatak.diplome.komentar)
#dodatakDiplKomentar = as.data.frame(dodatakDiplKomentar) 
#dodatakDiplKomentar = edit(dodatakDiplKomentar)
upitnik_filtered$dodatakDiplKomentar = dodatakDiplKomentar

## programi mobilnosti

programMobilnostiPre = as.character(stefan_filtered$U.kojim.programima.mobilnosti.ste.ucestvovali.u.prethodnom.periodu.)
programMobilnostiPre = as.data.frame(programMobilnostiPre)
programMobilnostiPre = edit(programMobilnostiPre)
unique(programMobilnostiPre)
programMobilnostiPre_ = as.character(programMobilnostiPre$programMobilnostiPre)
upitnik_filtered$mobilnostPre = programMobilnostiPre_ 

#####################

mobilnostPreOstalo = as.character(stefan_filtered$programi.mobilnosti.ostalo)
upitnik_filtered$mobilnostOstalo = mobilnostPreOstalo

##koliko judi poznajete
ljudiPoznajete = as.character(stefan_filtered$Koliko.poznajete.ljudi.koji.su.išli.na.razmenu.u.inostranstvo.sa.Vašeg.univerziteta.fakulteta.)
#ljudiPoznajete = as.data.frame(ljudiPoznajete)
unique(ljudiPoznajete)
ljudiPoznajete = edit(ljudiPoznajete)
ljudiPoznajete_ = as.factor(ljudiPoznajete)
upitnik_filtered$ljudiPOznajete = ljudiPoznajete_

##jezici

jezici = as.character(stefan_filtered$Koje.jezike.ste.tokom.studija.na.maticnom.univerzitetu.fakultetu.slušali.kao.obavezne.izborne.predmete.)
jezici = as.data.frame(jezici)
install.packages('stringr')

jezici = str_split_fixed(jezici$jezici, ", ", 4)
jezici = as.data.frame(jezici)
jezici = edit(jezici)
upitnik_filtered$maticni_jezik1 = jezici$jezik1
upitnik_filtered$maticni_jezik2 = jezici$jezik2
upitnik_filtered$maticni_jezik3 = jezici$jezik3
upitnik_filtered$maticni_jezik4 = jezici$jezik4
save.image()
##kako se ulaze za strane jezike

ulaganjeJezici = as.character(stefan_filtered$Koliko.se.ulaže.u.ucenje.stranih.jezika.na.vašem.univerzitetu.fakultetu.)
ulaganjeJezici = as.data.frame(ulaganjeJezici)
ulaganjeJezici$ulaganjeJeziciCopy = "" 
ulaganjeJezici$ulaganjeJeziciKomentar = "" 
ulaganjeJezici$ulaganjeJeziciCopy = ulaganjeJezici$ulaganjeJezici
ulaganjeJezici = edit(ulaganjeJezici)

ulaganjeJezici_ = as.character(ulaganjeJezici$ulaganjeJeziciCopy)
unique(ulaganjeJezici_)
ulaganjeJezici_ = as.factor(ulaganjeJezici_)
ulaganjeJeziciKomentar = ulaganjeJezici$ulaganjeJeziciKomentar

upitnik_filtered$ulaganjJezici = ulaganjeJezici_
upitnik_filtered$ulaganjeJeziciKomentar = ulaganjeJeziciKomentar
save.image()
##nivo podrske
upitnik_filtered$nivoPodrskeMaticni= upitnik_filtered$Koliko.ste.zadovoljni.kvalitetom.podrške.kancelarije.za.medunarodnu.saradnju.na.Vašem.maticnom.fakultetu.
## probleme u komunikaciji sa maticnim fakultetom
problemMaticni = as.character(upitnik_filtered$Da.li.ste.imali.probleme.u.komunikaciji.sa.osobljem.iz.kancelarije.za.medunarodnu.saradnju.)
problemMaticni = as.data.frame(problemMaticni)
problemMaticni = edit(problemMaticni)
problemMaticneKancelarije = as.character(problemMaticni$problemMaticneMedjKancel)
unique(problemMaticneKancelarije)
problemMaticneKancelarije = as.factor(problemMaticneKancelarije)
upitnik_filtered$problemMaticneMedjunKancel = problemMaticneKancelarije
problemMaticMedjKancKomentar = as.character(problemMaticni$problemMaticneMedjKancKomentar)
upitnik_filtered$problemMaticneMedjunKancelKomentar = problemMaticMedjKancKomentar

##izazovi sa medjunarodnom kancelarijom
izazoviMedjKanc = as.character(upitnik_filtered$Koji.su.najveci.izazovi..prema.Vašem.mišljenju..sa.kojima.se.studenti.suocavaju.tokom.saradnje.sa.maticnim.univerzitetom.u.procesu.razmene..Odaberite.tri.najbitnije.opcije..)
izazoviMedjKanc = as.data.frame(izazoviMedjKanc)
izazoviMedjKanc = as.data.frame(str_split_fixed(izazoviMedjKanc$izazoviMedjKanc, ";", 7))
izazoviMedjKanc_ = izazoviMedjKanc
izazoviMedjKanc_ = edit(izazoviMedjKanc_)
save.image()

izazoviMedjKanc1 = as.character(izazoviMedjKanc_$izazovMaticniUni1)
izazoviMedjKanc2 = as.character(izazoviMedjKanc_$izazovMaticniUni2)
izazoviMedjKanc3 = as.character(izazoviMedjKanc_$izazovMaticniUni3)
izazoviMedjKanc4 = as.character(izazoviMedjKanc_$izazovMaticniUni4)
izazoviMedjKanc5 = as.character(izazoviMedjKanc_$izazovMaticniUni5)
izazoviMedjKanc6 = as.character(izazoviMedjKanc_$izazovMaticniUni6)
izazoviMedjKancKomentar = as.character(izazoviMedjKanc_$izazovMaticniUnikomentar)


izazoviMedjKanc1[izazoviMedjKanc3==" "] = NA
unique(izazoviMedjKanc3)

upitnik_filtered$izazovMedjKanc1 = as.factor(izazoviMedjKanc1)
upitnik_filtered$izazovMedjKanc2 = as.factor(izazoviMedjKanc2)
upitnik_filtered$izazovMedjKanc3 = as.factor(izazoviMedjKanc3)
upitnik_filtered$izazovMedjKanc4 = as.factor(izazoviMedjKanc4)
upitnik_filtered$izazovMedjKanc5 = as.factor(izazoviMedjKanc5)
upitnik_filtered$izazovMedjKanc6 = as.factor(izazoviMedjKanc6)
upitnik_filtered$izazovMedjKomentar = as.factor(izazoviMedjKancKomentar)
save.image()
##nivo studija

nivoStudija = as.character(upitnik_filtered$Na.kom.nivou.studija.ste.bili.na.razmeni.u.inostranstvu.)
nivoStudija = as.data.frame(nivoStudija)
nivoStudija = as.data.frame(str_split_fixed(nivoStudija$nivoStudija, ";", 3))

nivoStudija_ = edit(nivoStudija)

upitnik_filtered$nivoStudija1 = as.factor(nivoStudija_$studije1)
upitnik_filtered$nivoStudija2 = as.factor(nivoStudija_$studije2)
upitnik_filtered$nivoStudija3 = as.factor(nivoStudija_$studije3)
save.image()

##medijska kamapanja

upitnik_filtered$medijskaKamanja = upitnik_filtered$Da.li.ste.ikada.videli.medijsku.kampanju.za.mobilnost.na.svom.maticnom.univerzitetu.fakultetu..

####################
##prvi deo upitnike filtriran - o user-u


informacije_ucesnik = upitnik_filtered[,c(1,42:51,52,38:41,73:76,56:57,54:55)]
View(informacije_ucesnik)
##
####################
## drugi deo upitnika filtriran - informisanost i priprema za mobilnost

informisanost_I_priprema = upitnik_filtered[,c(53,58,77,59:62,63,65,66:67,78,68:73)]
View(informisanost_I_priprema)

save.image()
write.csv(informacije_ucesnik,file = "informacije_o_ucesniku.csv",fileEncoding = "UTF-8")
##
####################
## survey



pie(table(informacije_ucesnik$Pol),labels=paste(informacije_ucesnik$Pol,table(informacije_ucesnik$Pol)),main="Informacije o polu")
#pie()
##qplot(informacije_ucesnik$Pol,aes (x=factor(1),),main = "Informacije o polu") + coord_polar(theta="y") 
bar <- ggplot(informacije_ucesnik,aes(x = factor(1), fill = informacije_ucesnik$Pol)) + geom_bar(width = 1)
pie <- bar + coord_polar(theta = "y")+ theme_void() + labs(x="",y="",title="", fill="Pol")
pie

bar = ggplot(informacije_ucesnik, aes(x = factor(informacije_ucesnik$Pol)),fill = informacije_ucesnik$Pol) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  ## scale_y_continuous(labels = percent_format()) #version 3.0.9
  scale_y_continuous(labels = percent) #version 3.1.0
bar
pie <- bar + coord_polar(theta = "y")  + labs(x="",y="",title="", fill="Pol")
pie

pol_table = as.data.frame(table(informacije_ucesnik$Pol))
slices = pol_table$Freq
lbls = c("Muski","Zenski")
pct = round(slices/sum(slices)*100)
lbls = paste(lbls,pct)
lbls = paste(lbls,"%",sep=" ")
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pol") 

#
pie3D(slices,labels=lbls,explode=0.1,theta = 1,
      main=paste("Пол, укупан број:" , sum(slices),sep = " "))
## gde su studirali - drzave i univerziteti
save.image()
drz1 = as.data.frame(table(informacije_ucesnik$mob_drz1))
drz1 = edit(drz1)
drz1 = drz1[-c(25),]
drz2 = as.data.frame(table(informacije_ucesnik$mob_drz2))
drz3 = as.data.frame(table(informacije_ucesnik$mob_drz3))
drz4 = as.data.frame(table(informacije_ucesnik$mob_drz4))
drz5 = as.data.frame(table(informacije_ucesnik$mob_drz5))

drz5$Freq = as.numeric(drz5$Freq)

drzave_count <- merge(drz1, drz2, by.x="Var1", by.y="Var1", all.x=T, all.y=T)
drzave_count <- merge(drzave_count, drz3, by.x="Var1", by.y="Var1", all.x=T, all.y=T)
drzave_count <- merge(drzave_count, drz4, by.x="Var1", by.y="Var1", all.x=T, all.y=T)
drzave_count <- merge(drzave_count, drz5, by.x="Var1", by.y="Var1", all.x=T, all.y=T)
#drzave_count <- edit(drzave_count)
#write.csv(drzave_count,file = "countries_count.csv")
drzave_count$drzaveSums=rowSums(drzave_count[2:6],na.rm = T)
drzave_count = drzave_count[-c(1),]
drzave_count[-c(1),]
#write.csv(drzave_count,file = "countries_count.csv")
theme_set(theme_classic())
g <- ggplot(drzave_count, aes(Var1, drzaveSums))
g + geom_bar(stat="identity", width = 0.5, fill="tomato2") + geom_text(aes(label = drzave_count$drzaveSums), position = position_dodge(0.9),
                                                                       hjust = -1,colour = "black", fontface = "bold")+  
  labs(title="Ступчани график држава мобилности, број мобилности: 325, број учесника: 253", 
       subtitle="Анкета за студенте и алумнисте разних програма мобилности, април 2018.", 
       caption="Извор: Асоцијација алумниста Балкана - Србија", y="Број", x="Државе")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + coord_flip()



drzave = c(informacije_ucesnik$mob_drz1,informacije_ucesnik$mob_drz2,informacije_ucesnik$mob_drz3,
  informacije_ucesnik$mob_drz4,informacije_ucesnik$mob_drz5)

univerziteti = c(informacije_ucesnik$mob_uni1,informacije_ucesnik$mob_uni2,informacije_ucesnik$mob_uni3,
                informacije_ucesnik$mob_uni4,informacije_ucesnik$mob_uni5)
drzave_i_univerzitet = as.data.frame(drzave)
drzave_i_univerzitet$univerzitet = univerziteti
View(drzave_i_univerzitet)
##drzave i univerziteti
drzave_i_univerzitet = edit(drzave_i_univerzitet
                          )
drzave_i_univerzitet[drzave_i_univerzitet==""]=NA
drzave_i_univerzitet[is.na(drzave_i_univerzitet)]

drzave_i_univerzitet_omit_na = drzave_i_univerzitet[complete.cases(drzave_i_univerzitet[,1]),]

drzave_i_univerzitet_omit_na = edit(drzave_i_univerzitet_omit_na)
##obrisi sve null vrednosti
drzave_i_univerzitet_omit_na = as.data.frame(drzave_i_univerzitet[complete.cases(drzave_i_univerzitet), ])
#drzave_i_univerzitet_omit_na = drzave_i_univerzitet[complete.cases(drzave_i_univerzitet), ]
drzave_i_univerzitet_omit_na = data.frame()
g <- ggplot(drzave_i_univerzitet_omit_na, aes(drzave_i_univerzitet_omit_na$drzave))
g + geom_bar(aes(fill=drzave_i_univerzitet_omit_na$univerzitet), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Stupčani grafik država i univerziteta", 
       subtitle="Anketa za studente i alumniste raznih programa mobilnosti, April 2018.", 
       caption="Izvor: Asocijacija alumnista sa Balkana - Srbija", y="Ukupan Broj", x="Države")

#unis = unique(drzave_i_univerzitet_omit_na$univerzitet)

##try with geocode

unis_u_celosti = drzave_i_univerzitet_omit_na[drzave_i_univerzitet_omit_na$univerzitet!="/",]

#install.packages('ggmap')
#library(ggmap)
result <- geocode(paste(unis_u_celosti$univerzitet,sep=", ",unis_u_celosti$drzave), output = "latlona", source = "google")
origAddress <- cbind(unis_u_celosti$univerzitet, result)


##univerziteti suma

uni1 = as.data.frame(table(informacije_ucesnik$mob_uni1))
uni1 = edit(uni1)
uni1=uni1[-c(1,24,30,84,101,132),]
#uni1 = drz1[-c(25),]
uni2 = as.data.frame(table(informacije_ucesnik$mob_uni2))
uni2 = edit(uni2)
uni3 = as.data.frame(table(informacije_ucesnik$mob_uni3))
uni3 = edit(uni3)
uni4 = as.data.frame(table(informacije_ucesnik$mob_uni4))
uni4 = edit(uni4)
uni5 = as.data.frame(table(informacije_ucesnik$mob_uni5))
uni5 = edit(uni5)

uni5$Freq = as.numeric(uni5$Freq)

unis_count <- merge(uni1, uni2, by.x="Var1", by.y="Var1", all.x=T, all.y=T)
unis_count <- merge(unis_count, uni3, by.x="Var1", by.y="Var1", all.x=T, all.y=T)
unis_count <- merge(unis_count, uni4, by.x="Var1", by.y="Var1", all.x=T, all.y=T)
unis_count <- merge(unis_count, uni5, by.x="Var1", by.y="Var1", all.x=T, all.y=T)

unis_count = edit(unis_count)
unis_count$uniSums=rowSums(unis_count[2:6],na.rm = T)

unis_veci_od_1 = unis_count[unis_count$uniSums>1 & unis_count$uniSums<20,]
dim(unis_count[unis_count$uniSums==1,])

g <- ggplot(unis_veci_od_1, aes(university, uniSums))
g + geom_bar(stat="identity", width = 0.5, fill="tomato2") + geom_text(aes(label = unis_veci_od_1$uniSums), position = position_dodge(0.9),
                                                                       hjust = -1,colour = "black", fontface = "bold")+  
  labs(title="Stupčani grafik država mobilnosti, broj mobilnosti: 325, broj ucesnika: 153", 
       subtitle="Anketa za studente i alumniste raznih programa mobilnosti, April 2018.", 
       caption="Izvor: Asocijacija alumnista sa Balkana - Srbija", y="Broj", x="Države")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + coord_flip()

write.csv(unis_count,file="universities.csv")
unis_count$uniSums
unis_count = unis_count[-c(1),]
unis_count[order(unis_count$uniSums),]
dim(unique(unis_count))

###maticni fakultet

pol_table = as.data.frame(table(informacije_ucesnik$maticni_uni))
pol_table_=edit(pol_table)
pol_table_=pol_table_[c(1:6),]

pol_table_
slices = pol_table_$Freq
lbls = pol_table_$Var1
pct = round(slices/sum(slices)*100,1)
lbls = paste(lbls,pct)
lbls = paste(lbls,"%",sep=" ")
#pie(slices,labels = lbls, col=rainbow(length(lbls)),
#    main="Матични универзитети") 

pie3D(slices,labels=lbls,explode=0.1,theta = pi/3,start=pi/10,labelcex=1,
      main="Матични универзитети: 9, број учесника анкете: 253", col=grey.colors(length(lbls)))
title(main = "*Универзитети Мегатренд, Едуконс, Уметности, Нови Пазар и Приштина имају по једно или два учесника анкете",
      cex.main = 0.7, font.main = 1, line=-23)
title(main = "Анкета за студенте и алумнисте разних програма мобилности, април 2018.",line=-24,cex.main = 0.75)
#write.csv(pol_table_,file = "maticni_univerziteti.csv")


##godine upisa



godine_upisa = c(as.character(informacije_ucesnik$godPocetka1),as.character(informacije_ucesnik$godPocetka2),as.character(informacije_ucesnik$godPocetka3))

nivo_studija = c(as.character(informacije_ucesnik$nivoStudija1),as.character(informacije_ucesnik$nivoStudija2),as.character(informacije_ucesnik$nivoStudija3))
upise_i_studije = as.data.frame(godine_upisa)
upise_i_studije$nivo_studija = nivo_studija
View(upise_i_studije)
##drzave i univerziteti

upise_i_studije[upise_i_studije==""]=NA
upise_i_studije[is.na(upise_i_studije)]
upise_i_studije = upise_i_studije[complete.cases(upise_i_studije[,1])]

##obrisi sve null vrednosti
upise_i_studije$godine_upisa = as.data.frame(upise_i_studije[complete.cases(upise_i_studije), ])



length(upise_i_studije$godine_upisa)
g <- ggplot(upise_i_studije, aes(godine_upisa))
g + geom_bar(aes(fill=nivo_studija), width = 0.8) +scale_fill_manual("legend", values = c("#e5e5e5", "#cccccc", "#b2b2b2", "#999999","#7f7f7f","#666666","#4c4c4c","#323232","#191919"))+
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
labs(title="Hиво студија по години уписа", 
     subtitle="Анкета за студенте и алумнисте разних програма мобилности, април 2018.", 
     caption="Извор: Асоцијација алумниста Балкана - Србија", y="Број", x="Године уписа") +
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  coord_flip()




#trajanje mobilnosti

trajanjeMobilnosti = as.character((informacije_ucesnik$trajanjeMobilnosti))
nivo_studija = as.character(informacije_ucesnik$nivoStudija1)
trajanje_i_studije = as.data.frame(trajanjeMobilnosti)
trajanje_i_studije$nivo_studija = nivo_studija 

trajanje_i_studije[trajanje_i_studije==" "]=NA
trajanje_i_studije[is.na(trajanje_i_studije)]

##obrisi sve null vrednosti
trajanje_i_studije = trajanje_i_studije[complete.cases(trajanje_i_studije[,1]),]
##obrisi sve null vrednosti
#trajanje_i_studije = as.data.frame(upise_i_studije[complete.cases(upise_i_studije), ])

#trajanje_i_studije$trajanjeMobilnosti= factor(as.character(trajanje_i_studije$trajanjeMobilnosti), levels = trajanje_i_studije$trajanjeMobilnosti[order(trajanje_i_studije$trajanje)])


g <- ggplot(trajanje_i_studije, aes(trajanjeMobilnosti))
g + geom_bar(aes(fill=nivo_studija), width = 0.8)+ scale_fill_manual("legend", values = c("#e5e5e5", "#cccccc", "#b2b2b2", "#999999","#7f7f7f","#666666","#4c4c4c","#323232","#191919"))+
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Hивоа студија по трајању мобилности - 250 учесника анкете", 
       subtitle="Анкета за студенте и алумнисте разних програма мобилности, април 2018.", 
       caption="Извор: Асоцијација алумниста Балкана - Србија", y="Укупан број", x="Године уписа")+
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  coord_flip()
View(trajanje_i_studije)

#dodatak diplomi

sve_vidljivost = as.character(upitnik_filtered$Da.li.je.period.mobilnosti..predmeti..ocene..jasno.vidljiv.u.dodatku.Vaše.diplome..Diploma.Supplement..)
sve_vidljivost=sve_vidljivost[sve_vidljivost!="Ne znam"]


write.csv(sve_vidljivost, "vidljivost_ostalo.csv")
vidljivost_mobilnosti = as.character(informacije_ucesnik$VidljivDodatkuDiplome)
vidljivost_i_studije = as.data.frame(vidljivost_mobilnosti)
vidljivost_i_studije$nivo_studija = nivo_studija
View(vidljivost_i_studije)

View(vidljivost_mobilnosti)
vidljivost_i_studije[vidljivost_i_studije==""]=NA
vidljivost_i_studije[is.na(vidljivost_i_studije)]

vidljivost_i_studije = vidljivost_i_studije[complete.cases(vidljivost_i_studije[,1]),]

#nastavno osoblje
vidljivost_i_studije[vidljivost_i_studije=="Nastavno osoblje"]=NA
vidljivost_i_studije[is.na(vidljivost_i_studije)]
##obrisi sve nastavno osoblje vrednosti
vidljivost_i_studije = vidljivost_i_studije[complete.cases(vidljivost_i_studije[,1]),]
dim(vidljivost_i_studije)
vidljivost_i_studije = edit(vidljivost_i_studije)
View(vidljivost_i_studije)
g <- ggplot(vidljivost_i_studije, aes(vidljivost_mobilnosti))
g + geom_bar(aes(fill=nivo_studija), width = 0.8) + scale_fill_manual("legend", values = c("#e5e5e5", "#cccccc", "#b2b2b2", "#999999","#7f7f7f","#666666","#4c4c4c","#323232","#191919"))+

  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Питање о видљивости мобилности у 'Diploma Supplement' по нивоу студија на 246 учесника", 
       subtitle="Анкета за студенте и алумнисте разних програма мобилности, април 2018.\n*Учесници су и дање студенти или процес признања није започет.\nНаставно особље је изузето са графика", 
       caption="Извор: Асоцијација алумниста Балкана - Србија", y="Збир", x="")+ coord_flip()+
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black"))
##programi mobilnosti

pol_table = as.data.frame(table(informacije_ucesnik$mobilnostPre))
pol_table_=edit(pol_table)
pol_table_=pol_table_[-c(2),]
pol_table_=edit(pol_table_)
slices = pol_table_$Freq
lbls = pol_table_$Var1
pct = round(slices/sum(slices)*100,1)
lbls = paste(lbls,pct)
lbls = paste(lbls,"%",sep=" ")
pie(slices,labels = lbls,cex = 0.8,radius=1,col=c("#e5e5e5", "#cccccc", "#b2b2b2", "#999999","#7f7f7f","#666666","#4c4c4c","#323232","#191919"),
    main="Предходни програми мобилности")
title(main = "*По један учесник на програму мобилности: \nThe Junior Faculty Developing Program, KMM-VIN, A-SMYLE, CAMPUS EUROPAE,MAEC AECID, TAMU, HESP, CEU\nRon Brown Fellowship, OeAD,OeAD, Стипендија фонда 'др З. Ђинђић', MOR-PSU",
      cex.main = 0.6, font.main = 1, line=-25)
title(main = "Извор: Асоцијација алумниста Балкана - Србија, Анкета о мобилности, april 2018",line=0,cex.main = 0.6)
sum(slices)


##nacin informisanosti


pol_table = as.data.frame(table(informisanost_I_priprema$nacinSaznanja))
pol_table_=edit(pol_table)
pol_table_=pol_table_[c(3:10),]
View(pol_table_)
slices = pol_table_$Freq
lbls = pol_table_$Var1
pct = round(slices/sum(slices)*100,1)
#lbls = paste(lbls," ~ ")
#lbls=""
lbls = paste(lbls,slices)
#lbls = paste(lbls,"(")
lbls = paste(lbls," ~ ")
lbls = paste(lbls,pct)
lbls = paste(lbls,"%")
#lbls = paste(lbls,")")
edit(lbls)

pl=pie3D(slices,edges=NA,radius=.9,height=0.1,theta=pi/2,start=pi/3,border=par("fg"),
      col=c("#e5e5e5", "#cccccc", "#b2b2b2", "#999999","#7f7f7f","#666666","#4c4c4c","#323232","#191919"),
      labels=lbls,labelpos=NULL,labelcol="black",labelcex=1.2,
      sector.order=NULL,explode=0.1,shade=.75)

title(main="Начин проналаска програма мобилности. Број учесника анкете: 255", font.main=1, cex.main=.9)
title(main = "Извор: Асоцијација алумниста Балкана - Србија. Анкета студената и алумниста о мобилности, април 2018",line=-23,cex.main = .8, font.main=1)


pie(slices,labels = lbls, col=c("#e5e5e5", "#cccccc", "#b2b2b2", "#999999","#7f7f7f","#666666","#4c4c4c","#323232","#191919"),
    main="",radius=.8) 

#poznanstvo ljudi

pol_table = as.data.frame(table(informisanost_I_priprema$ljudiPOznajete))
pol_table_=edit(pol_table)
pol_table_=pol_table_[c(2:5),]
pol_table_
slices = pol_table_$Freq
lbls = pol_table_$Var1
pct = round(slices/sum(slices)*100,1)
lbls = paste(lbls,pct)
lbls = paste(lbls,"%",sep=" ")
pie(slices,labels = lbls, col=c("#e5e5e5", "#cccccc", "#b2b2b2", "#999999","#7f7f7f","#666666","#4c4c4c","#323232","#191919"),
    main="Број особа које познајете са матичног универзитета\n који су користили програме мобилности - 251 учесник анкетe", cex.main = 1, font.main = 1.5)
title(main = "Извор: Асоцијација алумниста са Балкана - Србија. Анкета студената и алумниста разних програма мобилности. Април 2018",
      cex.main = 0.7, font.main = 1, line=-22)
##medijska kampanja po univerzitetima


mediji= informisanost_I_priprema$medijskaKamanja
unis = informacije_ucesnik$maticni_uni

mediji_i_unis = as.data.frame(mediji)
mediji_i_unis$unis=unis
View(mediji_i_unis)
##ciscenje NA
mediji_i_unis[mediji_i_unis==""]=NA
mediji_i_unis[is.na(mediji_i_unis)]

mediji_i_unis = mediji_i_unis[complete.cases(mediji_i_unis),]

dim(mediji_i_unis)


unique(mediji_i_unis$mediji)
unique(mediji_i_unis$unis)

mediji_i_unis = edit(mediji_i_unis)
mediji_i_unis_ = mediji_i_unis

ggplot(data = mediji_i_unis) +
  geom_bar(aes(x =  mediji_i_unis$unis, fill = mediji_i_unis$mediji), position = "fill")
#  geom_text(aes(y=label_ypos, label=len), vjust=1.6, color="white", size=3.5)

g <- ggplot(mediji_i_unis, aes(unis))
g + geom_bar(aes(fill=mediji), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Ступчани график присуства медијске кампање по универзитетима", 
       subtitle="Анкета за студенте и алумнисте разних програма мобилности, април 2018", 
       caption="Извор: Асоцијација алумниста са Балкана - Србија", y="Збир", x="")+ scale_fill_manual(values = c("#e5e5e5","#999999","#4c4c4c"))+
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black"))
  



pol_table = as.data.frame(table(mediji_i_unis$mediji))
pol_table_=edit(pol_table)
pol_table_=pol_table_[c(2:4),]
pol_table_
slices = pol_table_$Freq
lbls = pol_table_$Var1
pct = round(slices/sum(slices)*100,1)
lbls = paste(lbls,pct)
lbls = paste(lbls,"%",sep=" ")
pie(slices,labels = lbls, col=c("#e5e5e5","#999999","#4c4c4c"))
    #main="Koliko poznajete osoba sa maticnog univerziteta\n koji su koristili programe mobilnosti - 251 učesnik ankete", cex.main = 1, font.main = 1.5)
title(main = "Извор: Асоцијација алумниста са Балкана - Србија. Анкета за студенте и алумнисте разник програма мобилности, април 2018",
      cex.main = 0.7, font.main = 1, line=-16)
sum(slices)

#jezici na maticnom univerzitetu

jezici_po_unis = c(as.character(informisanost_I_priprema$maticni_jezik1),
                   as.character(informisanost_I_priprema$maticni_jezik2),
                   as.character(informisanost_I_priprema$maticni_jezik3),
                   as.character(informisanost_I_priprema$maticni_jezik4))
univerziteti_jezici = c(as.character(informacije_ucesnik$maticni_uni),
                        as.character(informacije_ucesnik$maticni_uni),
                        as.character(informacije_ucesnik$maticni_uni),
                        as.character(informacije_ucesnik$maticni_uni))
jezici_unis = as.data.frame(jezici_po_unis)
jezici_unis$univerziteti = univerziteti_jezici

##ciscenje NA
jezici_unis[jezici_unis==""]=NA
jezici_unis[is.na(jezici_unis)]

jezici_unis = jezici_unis[complete.cases(jezici_unis),]

jezici_unis = edit(jezici_unis)


ostalo_univertiteti = function(vector){
  idx=0
  for(i in vector)
  {
    idx=idx+1
    if (i == "Megatrend" | i == "Univerzitet u Novom Pazaru" | i == "Univerzitet Educons" | i=="Univerzitet Umetnosti u Beogradu" | i == "Univerzitet u Prištini")
    {
      vector[idx]="Ostalo*"
    }
  }
  
}

ostalo_jezici = function(vector){
  idx=0
  new_vector = vector
  for(i in new_vector)
  {
    
    if (i == "bugarski" | i == "flamanski" | i == "mađarski" | i=="norveški" | i == "poljski" | i=="portugalski")
    {
      new_vector[idx]="Ostalo*"
      print(new_vector[idx])
    }
    else if(i=="česki")
    {
      new_vector[idx]="ceski"
      print(new_vector[idx])
    }
    idx=idx+1 
  } 
    return (new_vector)
}
  


#spineplot(as.factor(jezici_unis$univerziteti),as.factor(jezici_unis$jezici_po_unis),xlab="x axis", ylab="y axis",  pch=19,
#          col.lab="red", cex.lab=0.5,    #  for the xlab and ylab
#          col="green")

jezici_unis = edit(jezici_unis)
jezici_table =table(jezici_unis$jezici_po_unis)
jezici_table = as.data.frame(jezici_table)
jezici_table = edit(jezici_table)
jezici_table_ = jezici_table[jezici_table$Freq>2,]
jezici_table_ = edit(jezici_table_)

#korekcija ostalih jezika
#korekcija_jezika = ostalo_jezici(as.character(jezici_unis$jezici_po_unis))

jezici_unis_bez_ost_ino = jezici_unis
jezici_unis_bez_ost_ino[jezici_unis_bez_ost_ino=="Ostalo*"]=NA
jezici_unis_bez_ost_ino[is.na(jezici_unis_bez_ost_ino)]

jezici_unis_bez_ost_ino = jezici_unis_bez_ost_ino[complete.cases(jezici_unis_bez_ost_ino),]




g <- ggplot(jezici_unis_bez_ost_ino, aes(univerziteti))
g + geom_bar(aes(fill=jezici_po_unis), width = 0.7) + scale_fill_manual(values = c("#e5e5e5", "#cccccc", "#b2b2b2", "#999999","#7f7f7f","#666666","#4c4c4c","#323232","#191919","#000000"))+
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="График обавезних/изборних језика на матичном факултету", 
       subtitle="Анкета за студенте и алумнисте разних програма мобилности, април 2018", 
       caption="Извор: Асоцијација алумниста са Балкана - Србија", y="Збир", x="Одговори")+
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black"))

grey_style = c("#e5e5e5", "#cccccc", "#b2b2b2", "#999999","#7f7f7f","#666666","#4c4c4c","#323232","#191919","#000000")
pol_table = as.data.frame(table(jezici_unis$jezici_po_unis))
pol_table_=edit(pol_table)
pol_table_=pol_table_[-c(10),]
pol_table_
slices = pol_table_$Freq
lbls=""
lbls = pol_table_$Var1
pct = round(slices/sum(slices)*100,1)
lbls = paste(lbls," - ")
lbls = paste(lbls,slices)
lbls = paste(lbls,"(")
lbls = paste(lbls,pct)
lbls = paste(lbls,"%")
lbls = paste(lbls,")")
pie(slices,labels = lbls,cex=0.7, col=grey_style,
main="Укупан број курсева језика", cex.main = 1, font.main = 1.5)
#title(main = "Извор: Асоцијација алумниста са Балкана - Србија. Анкета за студенте и алумнисте разник програма мобилности, април 2018",
#      cex.main = 0.7, font.main = 1, line=-16)


#koliko se ulaze na vasem fakutlet


typeof(informisanost_I_priprema$ulaganjJezici)
typeof(unis)

ulaganja_jez_i_uni = as.data.frame(informisanost_I_priprema$ulaganjJezici)
ulaganja_jez_i_uni$univerziteti = unis
View(ulaganja_jez_i_uni)

##ciscenje NA
ulaganja_jez_i_uni[ulaganja_jez_i_uni==""]=NA
ulaganja_jez_i_uni[is.na(ulaganja_jez_i_uni)]

ulaganja_jez_i_uni = ulaganja_jez_i_uni[complete.cases(ulaganja_jez_i_uni),]



ulaganja_jez_i_uni_ = edit(ulaganja_jez_i_uni_)
ulaganja_jez_i_uni_bez = ulaganja_jez_i_uni_
ulaganja_jez_i_uni_bez[ulaganja_jez_i_uni_bez=="Inostranstvo"]=NA
ulaganja_jez_i_uni_bez[is.na(ulaganja_jez_i_uni_bez)]

ulaganja_jez_i_uni_bez = ulaganja_jez_i_uni_bez[complete.cases(ulaganja_jez_i_uni_bez),]

ulaganja_jez_i_uni_bez=edit(ulaganja_jez_i_uni_bez)
g <- ggplot(ulaganja_jez_i_uni_bez, aes(ulaganjJezici))
g + geom_bar(aes(fill=univerziteti), width = 0.8) + scale_fill_manual(values = grey_style)+
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="График улагања у језике на матичном факултету према учесницима", 
       subtitle="Анкета за студенте и алумнисте разних програма мобилности, април 2018.", 
       caption="Извор: Асоцијација алумниста са Балкана - Србија", y="Број", x="")+coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

pie(slices,labels = lbls,cex=0.7, col=rainbow(length(lbls)),
    main="Укупан број курсева језика", cex.main = 1, font.main = 1.5)
#title(main = "Извор: Асоцијација алумниста са Балкана - Србија. Анкета за студенте и алумнисте разник програма мобилности, април 2018",
#      cex.main = 0.7, font.main = 1, line=-16)
##kvalitet podrzske na maticnom fakultetu


ocene_kancelarije = informisanost_I_priprema$nivoPodrskeMaticni
maticni_univerziteti = informacije_ucesnik$maticni_uni
ocene_kancelarije_i_unis = data.frame()

ocene_kancelarije_i_unis = as.data.frame(as.character(unis))
ocene_kancelarije_i_unis$ocene= ocene_kancelarije
ocene_kancelarije_i_unis = edit(ocene_kancelarije_i_unis)
univerziteti_i_ostali = as.character(ocene_kancelarije_i_unis$`as.character(unis)`)

##ciscenje NA
ocene_kancelarije_i_unis[ocene_kancelarije_i_unis==""]=NA
ocene_kancelarije_i_unis[is.na(ocene_kancelarije_i_unis)]

ocene_kancelarije_i_unis = ocene_kancelarije_i_unis[complete.cases(ocene_kancelarije_i_unis),]

bg_ocene = ocene_kancelarije_i_unis[ocene_kancelarije_i_unis$univerziteti=="Univerzitet u Beogradu",]
ns_ocene =ocene_kancelarije_i_unis[ocene_kancelarije_i_unis$univerziteti=="Univerzitet u Novom Sadu",]
kg_ocene =ocene_kancelarije_i_unis[ocene_kancelarije_i_unis$univerziteti=="Univerzitet u Kragujevcu",]
nis_ocene =ocene_kancelarije_i_unis[ocene_kancelarije_i_unis$univerziteti!="Univerzitet u Kragujevcu",]
nis_ocene =nis_ocene[nis_ocene$univerziteti!="Univerzitet u Novom Sadu",]
nis_ocene=nis_ocene[nis_ocene$univerziteti!="Univerzitet u Beogradu",]
nis_ocene=nis_ocene[nis_ocene$univerziteti!="Inostranstvo",]
nis_ocene=nis_ocene[nis_ocene$univerziteti!="Ostalo*",]

#par(mfrow=c(2,2))
df = kg_ocene
df_table=as.data.frame(table(df$ocene))
mean(df$ocene)
broj=length(df$univerziteti)
g <- ggplot(df, aes(ocene))
g + geom_bar(aes(fill=univerziteti), width = 0.6) + scale_fill_manual(values = c("#b2b2b2"))+
 # + geom_text(aes(label = df_table$Freq), position = position_dodge(0.9),
  #            hjust = -1,colour = "black", fontface = "bold")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title=paste("Oцена међународне канцеларије на матичном факултету.\nБрој учесника анкете: ",broj), 
       subtitle="Анкета за студенте и алумнисте разних програма мобилности, април 2018.", 
       caption="Извор: Асоцијација алумниста са Балкана - Србија", y="Глас", x="Оцене")+
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black"))+
theme(legend.position="none")
#problem sa komunijacijom
problem = informisanost_I_priprema$problemMaticneMedjunKancel
problem_i_unis = as.data.frame(as.character(problem))
problem_i_unis$univerziteti = as.character(univerziteti_i_ostali)


##ciscenje NA
problem_i_unis[problem_i_unis==""]=NA
problem_i_unis[is.na(problem_i_unis)]

problem_i_unis = problem_i_unis[complete.cases(problem_i_unis),]
#po univerzitetima

problem_i_unis=edit(problem_i_unis)
bg_problemi = problem_i_unis[problem_i_unis$univerziteti=="Univerzitet u Beogradu",]
ns_problemi = problem_i_unis[problem_i_unis$univerziteti=="Univerzitet u Novom Sadu",]
kg_problemi = problem_i_unis[problem_i_unis$univerziteti=="Univerzitet u Kragujevcu",]
nis_problemi =problem_i_unis[problem_i_unis$univerziteti!="Univerzitet u Kragujevcu",]
nis_problemi =nis_problemi[nis_problemi$univerziteti!="Univerzitet u Novom Sadu",]
nis_problemi=nis_problemi[nis_problemi$univerziteti!="Univerzitet u Beogradu",]
nis_problemi=nis_problemi[nis_problemi$univerziteti!="Inostranstvo",]
nis_problemi=nis_problemi[nis_problemi$univerziteti!="Ostalo*",]

df = kg_problemi
df_table = as.data.frame(table(df$`as.character(problem)`))
df_table=df_table[-c(3),]
broj=length(df$univerziteti)
uni = "Унивезитету у Крагујевцу"
title=""
title=paste("Oцена при комуникацији са међународном канцеларијом на\n",uni )
title=paste(title,". Број учесника анкете: ")
title = paste(title,broj)
g <- ggplot(df, aes(`as.character(problem)`))
g + geom_bar(aes(fill=univerziteti), width = 0.5) + scale_fill_manual(values = c("#b2b2b2"))+
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title=title,
       subtitle="Анкета за студенте и алумнисте разних програма мобилности, април 2018.", 
       caption="Извор: Асоцијација алумниста са Балкана - Србија", y="Број", x="")+ coord_flip()+ 
  theme(plot.title = element_text(size=12))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position="none")
#izazovi

izazovi=c(as.character(informisanost_I_priprema$izazovMedjKanc1),
          as.character(informisanost_I_priprema$izazovMedjKanc2),
                       as.character(informisanost_I_priprema$izazovMedjKanc3))
 #                      as.character(informisanost_I_priprema$izazovMedjKanc4),
#                       as.character(informisanost_I_priprema$izazovMedjKanc5))
izazovi_univerziteti = c(as.character(informacije_ucesnik$maticni_uni),
                         as.character(informacije_ucesnik$maticni_uni),
                         as.character(informacije_ucesnik$maticni_uni))
#                         as.character(informacije_ucesnik$maticni_uni),
#                         as.character(informacije_ucesnik$maticni_uni))          
izazovi_i_uni = as.data.frame(izazovi)
izazovi_i_uni$univerziteti = izazovi_univerziteti
izazovi_i_uni = edit(izazovi_i_uni)
##ciscenje NA
izazovi_i_uni[izazovi_i_uni=="Megatrend"]=NA
izazovi_i_uni[is.na(izazovi_i_uni)]
izazovi_i_uni = izazovi_i_uni[complete.cases(izazovi_i_uni),]
izazovi_i_uni=edit(izazovi_i_uni)
len=seq(1,36,1)
ggplot(data = izazovi_i_uni) +
  geom_bar(aes(x =  univerziteti, fill = izazovi), position = "fill") +  coord_flip()+
  scale_fill_manual("legend", values = c("#e5e5e5", "#cccccc", "#b2b2b2", "#999999","#7f7f7f","#666666","#4c4c4c","#323232","#191919"))+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                panel.background = element_blank(), axis.line = element_line(colour = "black"))

#theme(legend.position="none")+

#c("grey", "orange", "blue", "antiquewhite","green","darkgreen","lightblue","tan","gold")
#+  geom_text(aes(y=label_ypos, label=seq(1,36,1), vjust=0.6, 
#             color="white", size=3.5))
# 
# administra1 = as.character(unique(izazovi_i_uni$izazovi))[c(8)]
# administra2 = as.character(unique(izazovi_i_uni$izazovi))[c(11)]
# 
# izazovi_i_uni$izazovi[izazovi_i_uni$izazovi==administra2] = administra1
# unique(izazovi_i_uni$izazovi)
# 
# stud_sluz1 = as.character(unique(izazovi_i_uni$izazovi))[c(5)]
# stud_sluz2 = as.character(unique(izazovi_i_uni$izazovi))[c(10)]
# izazovi_i_uni$izazovi[izazovi_i_uni$izazovi==stud_sluz2] = stud_sluz1
# 
# unique(izazovi_i_uni$izazovi)
# 
# nedovoljni_kap1 =as.character(unique(izazovi_i_uni$izazovi))[c(6)]
# nedovoljni_kap2 =as.character(unique(izazovi_i_uni$izazovi))[c(9)]
# izazovi_i_uni$izazovi[izazovi_i_uni$izazovi==nedovoljni_kap2] = nedovoljni_kap1
# #prvi =strsplit(as.character(unique(izazovi_i_uni$izazovi))[c(8,11)][1], NULL)
# #drugi= strsplit(as.character(unique(izazovi_i_uni$izazovi))[c(8,11)][2], NULL)
# #unlist(prvi) %in% unlist(drugi)
pol_table = as.data.frame(table(izazovi_i_uni$izazovi))
pol_table_=edit(pol_table)
pol_table_=pol_table_[-c(2,4,11),]
pol_table_
slices = pol_table_$Freq
#lbls = pol_table_$Var1
lbls=""
pct = round(slices/sum(slices)*100,1)
lbls = paste(lbls," - ")
lbls = paste(lbls,slices)
lbls = paste(lbls,"(")
lbls = paste(lbls,pct)
lbls = paste(lbls,"%")
lbls = paste(lbls,")")
pl=pie(slices,labels = lbls,cex=0.9, col=c("#e5e5e5", "#cccccc", "#b2b2b2", "#999999","#7f7f7f","#666666","#4c4c4c","#323232","#191919"),
    main="Broj selektovaniih odgovora: 537", cex.main = 1, font.main = 1.5, line=-1)
#title(main = "Извор: Асоцијација алумниста са Балкана - Србија. Анкета за студенте и алумнисте разник програма мобилности, април 2018",
#      cex.main = 0.7, font.main = 1, line=-16)
dev.off()
sum(pol_table_$Freq)
####filtered stefan  period mobilnosti

stefan = read.csv(file="Stefan_WBAA anket.csv",header = T,sep=";", encoding = "UTF-8")
stefan=stefan[-164,]
View(stefan[c(152,83,95,252),])
stefan = stefan[-c(152,83,95,252),]
View(stefan[,22])

############
stefan$Da.li.ste.pohadali.kurseve.jezika.zemlje.u.kojoj.ste.bili.na.razmeni.
upitnik_filtered$da_li_kurseve_pohadjali = stefan$Da.li.ste.pohadali.kurseve.jezika.zemlje.u.kojoj.ste.bili.na.razmeni.
##problem u komunikaciji sa gostujucim
problem_komunikacija = as.data.frame(upitnik_filtered$Da.li.ste.imali.probleme.u.komunikaciji.sa.osobljem.na.gostujucem.fakultetu.i.ili.sa.kancelarijom.za.medunarodnu.saradnju.u.zemlji.u.kojoj.ste.bili.)
problem_komunikacija$problem_komun_gostujuci = (upitnik_filtered$Da.li.ste.imali.probleme.u.komunikaciji.sa.osobljem.na.gostujucem.fakultetu.i.ili.sa.kancelarijom.za.medunarodnu.saradnju.u.zemlji.u.kojoj.ste.bili.)
#problem_komunikacija = as.data.frame(problem_komunikacija)
problem_komunikacija = edit(problem_komunikacija)
upitnik_filtered$problem_komunikac_gost = problem_komunikacija$problem_komun_gostujuci
upitnik_filtered$problem_komunikac_gost_komentar = problem_komunikacija$problem_gost_komentar

##
##vrste diskusija
vrste_diskusija = as.data.frame(stefan$Koje.vrste.diskusija..radionica.ili.ostalih.aktivnosti.su.vam.najviše.bile.od.koristi.prilikom.studijskog.boravka.)
vrste_diskusija$vrste_diskusija = vrste_diskusija$`stefan$Koje.vrste.diskusija..radionica.ili.ostalih.aktivnosti.su.vam.najviše.bile.od.koristi.prilikom.studijskog.boravka.`
#vrste_diskusija = edit(vrste_diskusija)


vrste_diskusija = str_split_fixed(vrste_diskusija$vrste_diskusija, ", ", 4)
vrste_diskusija = as.data.frame(vrste_diskusija)
vrste_diskusija = edit(vrste_diskusija)
unique(vrste_diskusija$radionice4)
upitnik_filtered$diskusije_radionice1= vrste_diskusija$radionice1
upitnik_filtered$diskusije_radionice2= vrste_diskusija$radionice2
upitnik_filtered$diskusije_radionice3= vrste_diskusija$radionice3
upitnik_filtered$diskusije_radionice4= vrste_diskusija$radionice4
upitnik_filtered$diskusije_radionice_kom= as.character(vrste_diskusija$komentar)


## izazovi kao studentu
izazovi_studenta = as.data.frame(stefan$Kao.studentu.na.razmeni.šta.Vam.je.bio.najveci.izazov..Odaberite.tri.najbitnije.opcije.)
izazovi_studenta = str_split_fixed(izazovi_studenta$`stefan$Kao.studentu.na.razmeni.šta.Vam.je.bio.najveci.izazov..Odaberite.tri.najbitnije.opcije.`,", ",12)
izazovi_studenta = as.data.frame(izazovi_studenta)
izazovi_studenta = edit(izazovi_studenta)


upitnik_filtered$izazovi_studenata_mob1=izazovi_studenta$V1
upitnik_filtered$izazovi_studenata_mob2=izazovi_studenta$V2
upitnik_filtered$izazovi_studenata_mob3=izazovi_studenta$V3
upitnik_filtered$izazovi_studenata_mob4=izazovi_studenta$V4
upitnik_filtered$izazovi_studenata_mob5=izazovi_studenta$V5
upitnik_filtered$izazovi_studenata_mob_kom=izazovi_studenta$V8


unique(izazovi_studenta$V8)

##vrednosti

vrednosti_cistiti = as.character(stefan$Šta.je.po.Vašem.mišljenju.najveca.vrednost.koju.student.donosi.u.svoju.maticnu.zemlju.po.povratku.sa.mobilnosti.iz.inostranstva..Odaberite.tri.najbitnije.opcije.)
write.csv(vrednosti_cistiti,file = "vrednosi_cistiti.csv")
vrednosti_odvojeno = read.csv("vrednosi_cistiti.csv", header = T)

vrednosti = str_split_fixed(vrednosti_odvojeno$x, "; ", 8)

vrednosti_ = edit(vrednosti)

vrednosti_ = as.data.frame(vrednosti_)
unique(vrednosti_$col1)

##merging the same
#vrednosti1 = as.character(unique(vrednosti_$col1))[c(10)]
#vrednosti2 = as.character(unique(vrednosti_$col1))[c(6)]

#vrednosti_ = edit(vrednosti_)
#vrednosti_$col1[vrednosti_$col1==vrednosti2] = vrednosti1
unique(vrednosti_$col8)

upitnik_filtered$najveca_vrednost1 = vrednosti_$col1
upitnik_filtered$najveca_vrednost2 = vrednosti_$col2
upitnik_filtered$najveca_vrednost3 = vrednosti_$col3
upitnik_filtered$najveca_vrednost4 = vrednosti_$col4
upitnik_filtered$najveca_vrednost5 = vrednosti_$col5
upitnik_filtered$najveca_vrednost6 = vrednosti_$col6
upitnik_filtered$najveca_vrednost7 = vrednosti_$col7
upitnik_filtered$najveca_vrednost_kom = vrednosti_$col8



##da li ste sa mentorima u kontaktu
kontakt_s_mentorima = as.data.frame(stefan$Da.li.ste.još.uvek.u.kontaktu.sa.svojim.akademskim.mentorima.iz.inostranstva.)
kontakt_s_mentorima$kontakt_s_mentorom = stefan$Da.li.ste.još.uvek.u.kontaktu.sa.svojim.akademskim.mentorima.iz.inostranstva.
kontakt_s_mentorima=edit(kontakt_s_mentorima)
upitnik_filtered$kontakt_s_mentorom= kontakt_s_mentorima$kontakt_s_mentorom
##publikacija
stefan$Da.li.ste.objavili.neku.publikaciju.ili.istraživacki.rad.tokom.mobilnosti.u.inostranstvu.
publikacija = as.data.frame(stefan$Da.li.ste.objavili.neku.publikaciju.ili.istraživacki.rad.tokom.mobilnosti.u.inostranstvu.)
publikacija$publikacija = stefan$Da.li.ste.objavili.neku.publikaciju.ili.istraživacki.rad.tokom.mobilnosti.u.inostranstvu.
publikacija = edit(publikacija)
upitnik_filtered$publikacija = publikacija$publikacija
##stecena znanja da prenesete na maticni fakultet
prenos_znanja = as.data.frame(stefan$Da.li.ste.imali.šansu.da.stecena.znanja.i.veštine.prenesete.na.Vaš.maticni.fakultet.po.povratku.sa.mobilnosti.)
prenos_znanja$prenos_znanja = stefan$Da.li.ste.imali.šansu.da.stecena.znanja.i.veštine.prenesete.na.Vaš.maticni.fakultet.po.povratku.sa.mobilnosti.
prenos_znanja = edit(prenos_znanja)
upitnik_filtered$prenoz_znanja_maticni = prenos_znanja$prenos_znanja
###sansa za zaposlenje 
zaposlenje = as.data.frame(stefan$Smatrate.li.da.student.koji.se.vrati.sa.razmene.ili.studija.u.inostranstvu.ima.bolje.šanse.za.zaposlenje.u.Srbiji.)
zaposlenje$zaposlenje = stefan$Smatrate.li.da.student.koji.se.vrati.sa.razmene.ili.studija.u.inostranstvu.ima.bolje.šanse.za.zaposlenje.u.Srbiji.
zaposlenje = edit(zaposlenje)
upitnik_filtered$sansa_zaposlenja = zaposlenje$zaposlenje

###kursevi napolju
kursevi_napolju = as.data.frame(stefan$Da.li.ste.pohadali.kurseve.jezika.zemlje.u.kojoj.ste.bili.na.razmeni.)

kursevi_napolju = edit(kursevi_napolju)
upitnik_filtered$kursevi_napolju=as.factor(as.character(kursevi_napolju$`stefan$Da.li.ste.pohadali.kurseve.jezika.zemlje.u.kojoj.ste.bili.na.razmeni.`))
###printanje grafika
View(upitnik_filtered)
##kursevi napolju
pol_table_kursevi_tamo = as.data.frame(table(upitnik_filtered$kursevi_napolju))
pol_table_kursevi_tamo=edit(pol_table_kursevi_tamo)
pol_table_kursevi_tamo_=pol_table_kursevi_tamo[-c(1),]
View(pol_table_kursevi_tamo_)
slices = pol_table_kursevi_tamo_$Freq
lbls = ""
lbls = pol_table_kursevi_tamo_$Var1
pct = round(slices/sum(slices)*100,1)
lbls = paste(lbls," - ")
lbls = paste(lbls,slices)
lbls = paste(lbls,"(")
lbls = paste(lbls,pct)
lbls = paste(lbls,"%")
lbls = paste(lbls,")")
#lbls = edit(lbls)
broj = sum(pol_table_kursevi_tamo_$Freq)
pie3D(slices,edges=NA,radius=.9,height=0.15,theta=pi/3,start=pi/3,border=par("fg"),
      col=c("#e5e5e5","#4c4c4c"),labels=lbls,labelpos=NULL,labelcol="black",labelcex=1.2,
      sector.order=NULL,explode=0.1,shade=.75)
title(main=paste("Да ли сте похађали курсеве језика у земљи у којој сте студирали? Број анкетираних ",broj), font.main=1, cex.main=1, line=-2)
title(main = "Извор: Асоцијација алумниста Балкана - Србија. Анкета студената и алумниста о мобилности, април 2018",line=-23,cex.main = .8, font.main=1)


##koliko ste zadovoljni profesora o jeziku


upitnik_filtered$jezik_strani_prof = upitnik_filtered$Koliko.ste.zadovoljni.nivoom.engleskog.jezika.profesora.koji.su.predavali.na.fakultetima.na.kojima.ste.bili.na.razmeni.studirali.
ocene_profa  = as.data.frame(upitnik_filtered$jezik_strani_prof)
ocene_profa$`upitnik_filtered$jezik_strani_prof` = as.factor(upitnik_filtered$jezik_strani_prof)

counts <- table(ocene_profa$`upitnik_filtered$jezik_strani_prof`)
df_counts = as.data.frame(counts)
df_counts$perc =  round(df_counts$Freq/sum(df_counts$Freq)*100,2)
df_counts$lbl = paste(paste(df_counts$Freq,"-", df_counts$perc),"%")
broj= length(ocene_profa$`upitnik_filtered$jezik_strani_prof`)
title=""
title="Оцена квалитета језика професора на страним универзитетима"
##title=paste(title,paste(title,"\nБрој учесника анкете:"),broj)
title=paste(title,"\nПросечна оцена:")
prosek=round(mean(as.numeric(as.character(upitnik_filtered$jezik_strani_prof))),2)
title=paste(title,prosek)
barplot(counts,col="#4c4c4c", main=title,xlab="Извор: Асоцијација алумниста са Балкана - Србија",ylim=c(0,160),
     sub="Анкета за студенте и алумнисте разних програма мобилности, април 2018.")+
  text(x=c(.8,2,3.1,4.3,5.5), y=c(10,15,45,80,140), df_counts$lbl, cex=1, srt=0, xpd=TRUE)
##prijem na gostujuci fakultet

upitnik_filtered$prijem_na_gost = upitnik_filtered$Koliko.ste.zadovoljni.prijemom.i.podrškom.na.gostujucem.fakultetu.

prijem_na_gostujuci= table(upitnik_filtered$prijem_na_gost)
df_counts = as.data.frame(prijem_na_gostujuci)
df_counts$perc =  round(df_counts$Freq/sum(df_counts$Freq)*100,2)
df_counts$lbl = paste(paste(df_counts$Freq,"-", df_counts$perc),"%")
broj= length(ocene_profa$`upitnik_filtered$jezik_strani_prof`)
title=""
title="Oцена пријема на гостујућем факултету/универзитету"
title=paste(title,"\nПросечна оцена:")
prosek=round(mean(as.numeric(as.character(upitnik_filtered$prijem_na_gost))),2)
title=paste(title,prosek)
barplot(prijem_na_gostujuci,col="#4c4c4c", main=title,xlab="Извор: Асоцијација алумниста са Балкана - Србија",ylim=c(0,190),
        sub="Анкета за студенте и алумнисте разних програма мобилности, април 2018.")+
  text(x=c(.8,2,3.1,4.3,5.5), y=c(10,15,45,80,195), df_counts$lbl, cex=1, srt=0, xpd=TRUE)


##problem na gostujucem

pol_table_problem_gost = as.data.frame(table(as.character(upitnik_filtered$problem_komunikac_gost)))
pol_table_problem_gost=edit(pol_table_problem_gost)
pol_table_problem_gost_=pol_table_problem_gost[-c(1),]
View(pol_table_problem_gost_)
slices = pol_table_problem_gost_$Freq
lbls = ""
lbls = pol_table_problem_gost_$Var1
pct = round(slices/sum(slices)*100,1)
lbls = paste(lbls," - ")
lbls = paste(lbls,slices)
lbls = paste(lbls,"(")
lbls = paste(lbls,pct)
lbls = paste(lbls,"%")
lbls = paste(lbls,")")
broj = sum(pol_table_problem_gost_$Freq)
pl=pie3D(slices,edges=NA,radius=1.1,height=0.15,theta=pi/3,start=pi/1,border=par("fg"),
      col=c("#e5e5e5","#999999","#4c4c4c"),labels=lbls,labelpos=c(3,3.2,8),labelcol="black",labelcex=1.2,
      sector.order=NULL,explode=0,shade=.75)
title(main=paste("Проблем са особљем на гостујућем универзитету?\n Број анкетираних ",broj), font.main=1, cex.main=1)
title(main = "Извор: Асоцијација алумниста Балкана - Србија.\nАнкета студената и алумниста о мобилности, април 2018",line=-15,cex.main = .8, font.main=1)


##da li ste u kontaktu sa mentorom


##problem na gostujucem

mentor_contact =as.character(anketa_procisceno$kontakt_s_mentorom[anketa_procisceno$nivoStudija1=="doktorske studije" |
                                                                                       anketa_procisceno$nivoStudija2=="doktorske studije" |
                                                                                       anketa_procisceno$nivoStudija3=="doktorske studije" |
                                                                                       anketa_procisceno$nivoStudija1=="post-doktorske studije" |
                                                                                       anketa_procisceno$nivoStudija2=="post-doktorske studije" |
                                                                                       anketa_procisceno$nivoStudija3=="post-doktorske studije" | 
                                                                                       anketa_procisceno$nivoStudija1=="istraživanje" |
                                                                                       anketa_procisceno$nivoStudija2=="istraživanje" |
                                                                                       anketa_procisceno$nivoStudija3=="istraživanje" |
                                                                                       anketa_procisceno$nivoStudija1=="trening" |
                                                                                       anketa_procisceno$nivoStudija2=="trening" |
                                                                                       anketa_procisceno$nivoStudija3=="trening"])

mentor_contact = as.data.frame(table(mentor_contact))
mentor_contact_=edit(mentor_contact)
mentor_contact_=mentor_contact_[-c(1),]
#View(mentor_contact_)
slices = mentor_contact_$Freq
lbls = ""
lbls = mentor_contact_$Var1
pct = round(slices/sum(slices)*100,1)
lbls = paste(lbls,mentor_contact_$mentor_contact)
lbls = paste(lbls,slices)
lbls = paste(lbls,"(")
lbls = paste(lbls,pct)
lbls = paste(lbls,"%")
lbls = paste(lbls,")")
broj = sum(mentor_contact_$Freq)
pie3D(slices,edges=NA,radius=1.1,height=0.15,theta=pi/3,start=pi/1,border=par("fg"),
      col=c("#e5e5e5","#4c4c4c"),labels=lbls,labelpos=NULL,labelcol="black",labelcex=1.2,
      sector.order=NULL,explode=0.1,shade=.75)
title(main=paste("Одржавање контакта са ментором анкетираних \nca докт., пост-докт... студија. Број анкетираних ",broj), font.main=1, cex.main=1)
title(main = "Извор: Асоцијација алумниста Балкана - Србија. \nАнкета студената и алумниста о мобилности, април 2018",line=-15,cex.main = .8, font.main=1)


##publikacija

publikacija_phd_stud =c(as.character(upitnik_filtered$publikacija[anketa_procisceno$nivoStudija1=="doktorske studije"]),
                        as.character(upitnik_filtered$publikacija[anketa_procisceno$nivoStudija2=="doktorske studije"]),
                        as.character(upitnik_filtered$publikacija[anketa_procisceno$nivoStudija3=="doktorske studije"]))
publikacija_post_phd_stud=c(as.character(upitnik_filtered$publikacija[anketa_procisceno$nivoStudija1=="post-doktorske studije"]),
                            as.character(upitnik_filtered$publikacija[anketa_procisceno$nivoStudija2=="post-doktorske studije"]),
                            as.character(upitnik_filtered$publikacija[anketa_procisceno$nivoStudija3=="post-doktorske studije"]))
publikacija_phd_post_stud = c(publikacija_phd_stud,publikacija_post_phd_stud)

publikacija_master_stud=c(as.character(upitnik_filtered$publikacija[anketa_procisceno$nivoStudija1=="master studije"]),
                            as.character(upitnik_filtered$publikacija[anketa_procisceno$nivoStudija2=="master studije"]),
                            as.character(upitnik_filtered$publikacija[anketa_procisceno$nivoStudija3=="master studije"]))
publikacija_osnovne_stud=c(as.character(upitnik_filtered$publikacija[anketa_procisceno$nivoStudija1=="osnovne studije"]),
                          as.character(upitnik_filtered$publikacija[anketa_procisceno$nivoStudija2=="osnovne studije"]),
                          as.character(upitnik_filtered$publikacija[anketa_procisceno$nivoStudija3=="osnovne studije"]))



  
unique(as.character(anketa_procisceno$nivoStudija1))

publikacija_osnovne_stud = publikacija_phd_stud
publikacija_osnovne_stud = as.data.frame(table(publikacija_osnovne_stud))
publikacija_osnovne_stud=edit(publikacija_osnovne_stud)
#publikacija_master_stud=publikacija_master_stud[-c(1),]
slices = publikacija_osnovne_stud$Freq
lbls = ""
lbls = publikacija_osnovne_stud$Var1
pct = round(slices/sum(slices)*100,1)
broj = sum(publikacija_osnovne_stud$Freq)
lbls = paste(lbls,publikacija_osnovne_stud$publikacija_osnovne_stud)
lbls = paste(lbls,slices)
lbls = paste(lbls,"(")
lbls = paste(lbls,pct)
lbls = paste(lbls,"%")
lbls = paste(lbls,")")
#c("lightblue","red"),
lp=pie3D(slices,edges=NA,radius=1.1,height=0.15,theta=pi/3,start=pi/1,border=par("fg"),
      col=c("#e5e5e5","#4c4c4c"),labels=lbls,labelpos=c(3.5,14),labelcol="black",labelcex=1.2,
      sector.order=NULL,explode=0,shade=.75)
title(main=paste("Да ли сте објавили неку публикацији или истраживачки рад?\n Број анкетираних кандидата основних студија",broj), font.main=1, cex.main=1,line=.3)
title(main = "Извор: Асоцијација алумниста Балкана - Србија. Анкета студената и алумниста \nпрограма мобилности, април 2018",line=-15.5,cex.main = .8, font.main=1)

bsc_msc_phd = c(as.character(anketa_procisceno$nivoStudija1),as.character(anketa_procisceno$nivoStudija2),as.character(anketa_procisceno$nivoStudija3))
public3x =c(as.character(anketa_procisceno$publikacija),as.character(anketa_procisceno$publikacija),as.character(anketa_procisceno$publikacija))

bsc_msc_phd_publ = as.data.frame(bsc_msc_phd)
bsc_msc_phd_publ$publicaija = public3x
bsc_msc_phd_publ[bsc_msc_phd_publ$bsc_msc_phd=="trening",]=NA
bsc_msc_phd_publ[is.na(bsc_msc_phd_publ)]  
bsc_msc_phd_publ = bsc_msc_phd_publ[complete.cases(bsc_msc_phd_publ),]
bsc_msc_phd_publ = edit(bsc_msc_phd_publ)


bsc_msc_phd_publ$bsc_msc_phd[bsc_msc_phd_publ$bsc_msc_phd!="master studije"
                             & bsc_msc_phd_publ$bsc_msc_phd!="osnovne studije" 
                             & bsc_msc_phd_publ$bsc_msc_phd!="post-doktorske studije"
                             & bsc_msc_phd_publ$bsc_msc_phd!="doktorske studije"]="istrazivanje"
table(bsc_msc_phd_publ[bsc_msc_phd_publ$bsc_msc_phd=="post-doktorske studije",])

#bsc_msc_phd_publ$bsc_msc_phd= factor(as.character(bsc_msc_phd_publ$bsc_msc_phd), levels = bsc_msc_phd_publ$bsc_msc_phd[order(prioriteti_table_df_ed$Freq)])
bsc_msc_phd_publ$bsc_msc_phd <- factor(bsc_msc_phd_publ$bsc_msc_phd, levels = c("post-doktorske studije","istrazivanje","doktorske studije","master studije","osnovne studije"))
g <- ggplot(bsc_msc_phd_publ, aes(bsc_msc_phd))
g + geom_bar(aes(fill=publicaija), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + scale_fill_manual(values = c("white","#e5e5e5","#4c4c4c"))+
  labs(title="Ступчани график публикација по студијама", 
       subtitle="Анкета за студенте и алумнисте разних програма мобилности, април 2018.", 
       caption="Извор: Асоцијација алумниста са Балкана - Србија", y="Збир", x="Одговори")+coord_flip()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                               panel.background = element_blank(), axis.line = element_line(colour = "black"))
##stecena iskustva na maticni univerzitet
prenos_znanja_osnovne =as.character(anketa_procisceno$prenoz_znanja_maticni[anketa_procisceno$nivoStudija1=="osnovne studije"])
prenos_znanja_master =as.character(anketa_procisceno$prenoz_znanja_maticni[anketa_procisceno$nivoStudija1=="master studije" |
                                                                            anketa_procisceno$nivoStudija2=="master studije" |
                                                                             anketa_procisceno$nivoStudija3=="master studije"])
                                                                             
prenos_znanja_ostalo = as.character(anketa_procisceno$prenoz_znanja_maticni[anketa_procisceno$nivoStudija1!="osnovne studije" &
                                                                              anketa_procisceno$nivoStudija1!="master studije" &
                                                                              anketa_procisceno$nivoStudija2!="master studije" &
                                                                              anketa_procisceno$nivoStudija3!="master studije" &
                                                                              anketa_procisceno$nivoStudija2!="osnovne studije" &
                                                                              anketa_procisceno$nivoStudija3!="osnovne studije"])
                                                                                                                                                     

stedena_iskustva = prenos_znanja_ostalo

stedena_iskustva = as.data.frame(table(stedena_iskustva))
stedena_iskustva_=edit(stedena_iskustva)
#stedena_iskustva_=stedena_iskustva_[-c(1),]
slices = stedena_iskustva_$Freq
lbls = ""
lbls = stedena_iskustva_$Var1
pct = round(slices/sum(slices)*100,1)
broj = sum(stedena_iskustva_$Freq)
lbls = paste(lbls,stedena_iskustva_$stedena_iskustva)
lbls = paste(lbls,slices)
lbls = paste(lbls,"(")
lbls = paste(lbls,pct)
lbls = paste(lbls,"%")
lbls = paste(lbls,")")

pie3D(slices,edges=NA,radius=1.1,height=0.15,theta=pi/3,start=pi/1,border=par("fg"),
      col=c("#e5e5e5","#4c4c4c"),labels=lbls,labelpos=NULL,labelcol="black",labelcex=1.2,
      sector.order=NULL,explode=0,shade=.75)
title(main=paste("Пренос знања на матични универзитет - докторске/постдокторске/\n тренинг/истраживање. Број анкетираних ",broj), font.main=1, cex.main=1)
title(main = "Извор: Асоцијација алумниста Балкана - Србија.\nАнкета студената и алумниста о мобилности, април 2018",line=-15,cex.main = .8, font.main=1)

bsc_msc_phd = c(as.character(anketa_procisceno$nivoStudija1),as.character(anketa_procisceno$nivoStudija2),as.character(anketa_procisceno$nivoStudija3))
iskustva = c(as.character(anketa_procisceno$prenoz_znanja_maticni),as.character(anketa_procisceno$prenoz_znanja_maticni),as.character(anketa_procisceno$prenoz_znanja_maticni))


bsc_msc_phd_isk = as.data.frame(bsc_msc_phd)
bsc_msc_phd_isk$isk = iskustva
bsc_msc_phd_isk[bsc_msc_phd_isk$bsc_msc_phd=="",]=NA
bsc_msc_phd_isk[is.na(bsc_msc_phd_isk)]  
bsc_msc_phd_isk = bsc_msc_phd_isk[complete.cases(bsc_msc_phd_isk),]
bsc_msc_phd_isk = edit(bsc_msc_phd_isk)


bsc_msc_phd_isk$bsc_msc_phd[bsc_msc_phd_isk$bsc_msc_phd_isk!="master studije"
                             & bsc_msc_phd_isk$bsc_msc_phd_isk!="osnovne studije" 
                             & bsc_msc_phd_isk$bsc_msc_phd_isk!="post-doktorske studije"
                             & bsc_msc_phd_isk$bsc_msc_phd_isk!="doktorske studije"
                            & bsc_msc_phd_isk$bsc_msc_phd_isk!="trening"
                            & bsc_msc_phd_isk$bsc_msc_phd_isk!="ostalo"]="istrazivanje"
                            
table(bsc_msc_phd_isk[bsc_msc_phd_isk$bsc_msc_phd=="ostalo",])

#bsc_msc_phd_publ$bsc_msc_phd= factor(as.character(bsc_msc_phd_publ$bsc_msc_phd), levels = bsc_msc_phd_publ$bsc_msc_phd[order(prioriteti_table_df_ed$Freq)])
bsc_msc_phd_isk$bsc_msc_phd <- factor(bsc_msc_phd_isk$bsc_msc_phd, levels = c("ostalo","post-doktorske studije","trening","istrazivanje","doktorske studije","master studije","osnovne studije"))
g <- ggplot(bsc_msc_phd_isk, aes(bsc_msc_phd))
g + geom_bar(aes(fill=isk), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + scale_fill_manual(values = c("#e5e5e5","#4c4c4c"))+
  labs(title="Ступчани график о могућности преноса знања на матични универзитет", 
       subtitle="Анкета за студенте и алумнисте разних програма мобилности, април 2018.", 
       caption="Извор: Асоцијација алумниста са Балкана - Србија", y="Збир", x="Одговори")+coord_flip()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                       panel.background = element_blank(), axis.line = element_line(colour = "black"))
                                                                                                                                                                                                                           








##bolje sanze za zaposlenje
sansa_zaposlenja = as.character(upitnik_filtered$sansa_zaposlenja[upitnik_filtered$nivoStudija1=="doktorske studije" |
                                                                    upitnik_filtered$nivoStudija2=="doktorske studije" |
                                                                    upitnik_filtered$nivoStudija3=="doktorske studije" |
                                                                    upitnik_filtered$nivoStudija1=="post-doktorske studije" |
                                                                    upitnik_filtered$nivoStudija2=="post-doktorske studije" |
                                                                    upitnik_filtered$nivoStudija3=="post-doktorske studije"])
sansa_zaposlenja = as.data.frame(table(sansa_zaposlenja))
sansa_zaposlenja_=edit(sansa_zaposlenja)
#sansa_zaposlenja_=sansa_zaposlenja_[-c(1),]
slices = sansa_zaposlenja_$Freq
lbls = ""
lbls = sansa_zaposlenja_$Var1
pct = round(slices/sum(slices)*100,1)
broj = sum(sansa_zaposlenja_$Freq)
lbls = paste(lbls,sansa_zaposlenja_$sansa_zaposlenja)
lbls = paste(lbls,slices)
lbls = paste(lbls,"(")
lbls = paste(lbls,pct)
lbls = paste(lbls,"%")
lbls = paste(lbls,")")

pie3D(slices,edges=NA,radius=.8,height=0.1,theta=pi/3,start=pi/2,border=par("fg"),
      col=c("#e5e5e5","#999999","#4c4c4c"),labels=lbls,labelpos=NULL,labelcol="black",labelcex=1.2,
      sector.order=NULL,explode=0.05,shade=.75)
title(main=paste("Оцена везано за запослење студента са мaстер студија у Србији\n Број анкетираних ",broj), font.main=1, cex.main=1, line=-.5)
title(main = "Извор: Асоцијација алумниста Балкана - Србија. \nАнкета студената и алумниста о мобилности, април 2018",line=-20,cex.main = .9, font.main=1)
#col=c("#e5e5e5", "#cccccc", "#b2b2b2", "#999999","#7f7f7f","#666666","#4c4c4c","#323232","#191919")

###################
##najveci izazov, tri odgovora
izzv = c(as.character(upitnik_filtered$izazovi_studenata_mob1),
         as.character(upitnik_filtered$izazovi_studenata_mob2),
         as.character(upitnik_filtered$izazovi_studenata_mob3))

##izbiris sve na ili Nije bilo problema.
izzv[izzv==""]=NA
izzv[izzv=="Nije bilo problema."]=NA

izzv[is.na(izzv)]
izzv = izzv[complete.cases(izzv)]
unique(izzv )
izzv[izzv=="Diskriminacija po rasnoj,verskoj nacionalnoj osnovi"]="Diskriminacija po rasnoj,verskoj,nacionalnoj osnovi"
izzv[izzv=="Primena stecenog znanja"]="Primena znanja"
izzv[izzv=="Problemi sa stipendijom"]="Problemi sa isplatama ili iznosom stipendije"
najveci_izazov= table(izzv)


izazovi_na_razmeni =as.factor(izzv)


df_izzv=as.data.frame(izzv)
df_izzv_table=as.data.frame(table(df_izzv$izzv))
label=df_izzv_table$Freq
label_proc = round(label/253*100,1)
label_proc = paste(label_proc,"%")
label = paste(label, "~",label_proc)

df_izzv_table[order(df_izzv_table$Freq,decreasing = T),]
df_izzv_table$Var1 = factor(df_izzv_table$Var1,levels = as.character(df_izzv_table$Var1[order(df_izzv_table$Freq,decreasing = F)]))

df_izzv_table=edit(df_izzv_table)
g <- ggplot(df_izzv_table, aes(Var1, Freq))
g + geom_bar(stat="identity", width =.8 , fill="#b2b2b2") + geom_text(aes(label = label), position = position_dodge(0.5),
                                                                      hjust = 1,vjust=-.2,colour = "black", fontface = "bold")+  
  labs(title="Изазови учесника током мобилности на основу три одговора.", 
       subtitle="Анкета за студенте и алумнисте програма мобилности. Април 2018", 
       caption="Извор: Асоцијација алумниста Балкана - Србија", y="", x="")+
  theme(axis.text.x = element_text(angle=0, vjust=1)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  coord_flip()
  #+scale_x_discrete(labels = abbreviate)

upise_i_studije
                                                                                                                                             

god_upisa = as.data.frame(as.character(anketa_procisceno$godPocetka1))
god_upisa$drugav =as.character(anketa_procisceno$godPocetka2)
god_upisa$treca=as.character(anketa_procisceno$godPocetka3)



programi = c(as.character(anketa_procisceno$mobilnostPre
                          ))

unique(god_upisa$drugav)
View(anketa_procisceno)

anketa_procisceno$publikacija
#izzv_ostalo = unique(as.character(upitnik_filtered$izazovi_studenata_mob_kom))
#write.csv(izzv_ostalo,"izzv_ostalo.csv")
##vrednost koju student donosi

vredn = c(as.character(upitnik_filtered$najveca_vrednost1),
          as.character(upitnik_filtered$najveca_vrednost2),
          as.character(upitnik_filtered$najveca_vrednost3))
vredn[vredn==""]=NA
#izzv[izzv=="Nije bilo problema."]=NA
vredn[is.na(vredn)]
vredn = vredn[complete.cases(vredn)]
unique(vredn)
vredn[vredn==unique(vredn)[6]]=unique(vredn)[8]
df_vredn = as.data.frame(vredn)
label=as.data.frame(table(as.character(df_vredn$vredn)))$Freq
label_proc = round(label/253*100,1)
label_proc = paste(label_proc,"%")
label = paste(label, "~",label_proc)
x = as.data.frame(table(as.character(df_vredn$vredn)))$Freq+40
y=seq(1,8,1)

vredn_tabela = as.data.frame(table(df_vredn$vredn))
View(vredn_tabela)

vredn_tabela[order(vredn_tabela$Freq,decreasing = T),]
vredn_tabela$Var1 = factor(vredn_tabela$Var1,levels = as.character(vredn_tabela$Var1[order(vredn_tabela$Freq,decreasing = T)]))

label=vredn_tabela$Freq
label_proc = round(label/252*100,1)
label_proc = paste(label_proc,"%")
label = paste(label, "~",label_proc)
vredn_tabela=edit(vredn_tabela)
g <- ggplot(vredn_tabela, aes(Var1, Freq))
g + geom_bar(stat="identity", width =.8 , fill="#b2b2b2") + geom_text(aes(label = label), position = position_dodge(0.5),
                                                                       hjust = .3,vjust=-.2,colour = "black", fontface = "bold")+  
  labs(title="Најзначајнији бенефити које су испитаници стекли у току периода мобилности на основу три селектована одговора.", 
       subtitle="Анкета за студенте и алумнисте програма мобилности. Април 2018", 
       caption="Извор: Асоцијација алумниста Балкана - Србија", y="", x="")+
  theme(axis.text.x = element_text(angle=0, vjust=1)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))#+scale_x_discrete(labels = abbreviate)


#g <- ggplot(ulaganja_jez_i_uni_bez, aes(ulaganjJezici))
g + geom_bar(aes(fill=univerziteti), width = 0.8) 

####internacionalizacija, izazovi i predlozi
## vaznost internacionalizacije visokog obrazovanja
upitnik_filtered$vaznost_internac_ocena = upitnik_filtered$Na.skali.od.1.do.5.ocenite.po.Vašem.mišljenju.važnost.internacionalizacije.visokog.obrazovanja.u.Republici.Srbiji.

prijem_na_gostujuci= table(upitnik_filtered$vaznost_internac_ocena)
df_counts = as.data.frame(prijem_na_gostujuci)
df_counts$perc =  round(df_counts$Freq/sum(df_counts$Freq)*100,2)
df_counts$lbl = paste(paste(df_counts$Freq,"-", df_counts$perc),"%")
broj= length(ocene_profa$`upitnik_filtered$jezik_strani_prof`)
title=""
title="Оцена о важности интернационализације високог образовања у Србији"
title=paste(title,"\nПросечна оцена:")
prosek=round(mean(as.numeric(as.character(upitnik_filtered$vaznost_internac_ocena))),2)
title=paste(title,prosek)
barplot(prijem_na_gostujuci,col="#b2b2b2", main=title,xlab="Извор: Асоцијација алумниста са Балкана - Србија",ylim=c(0,210),
        sub="Анкета за студенте и алумнисте разних програма мобилности, април 2018.")+
  text(x=c(.8,2,3.1,4.3,5.5), y=c(10,15,35,50,195), df_counts$lbl, cex=1,srt=0, xpd=TRUE)
##jasna strategija

upitnik_filtered$strategija = upitnik_filtered$Da.li.smatrate.da.Vaš.Univerzitet.ima.jasnu.strategiju.o.svojoj.internacionalizaciji..

bg = as.character(upitnik_filtered$strategija[upitnik_filtered$maticni_uni=="Univerzitet u Beogradu"])
ns = as.character(upitnik_filtered$strategija[upitnik_filtered$maticni_uni=="Univerzitet u Novom Sadu"])
kg = as.character(upitnik_filtered$strategija[upitnik_filtered$maticni_uni=="Univerzitet u Kragujevcu"])
nis= as.character(nis$strategija)
strategija = kg

strategija = as.data.frame(table(strategija))
strategija_=edit(strategija)
#strategija_=strategija_[-c(1),]
slices = strategija_$Freq
lbls = ""
lbls = strategija_$Var1
pct = round(slices/sum(slices)*100,1)
broj = sum(strategija_$Freq)
lbls = paste(lbls,strategija_$strategija)
lbls = paste(lbls,slices)
lbls = paste(lbls,"(")
lbls = paste(lbls,pct)
lbls = paste(lbls,"%")
lbls = paste(lbls,")")

labela = "Оцена о јасној стратегији о интернационализацији- Универзитет у Нишу" 
labela = paste(labela, "\n Број анкетираних")
labela = paste(labela,broj)

pl =pie3D(slices,edges=NA,radius=.7,height=0.1,theta=pi/3,start=pi/3,border=par("fg"),
      col=c("#e5e5e5","#4c4c4c","#999999"),labels=lbls,labelpos=c(1.2,2.5,5),labelcol="black",labelcex=1,
      sector.order=NULL,explode=0.05,shade=.75)
title(main=labela, font.main=1.5, cex.main=1)
title(main = "Извор: Асоцијација алумниста Балкана - Србија.\n Анкета студената и алумниста о мобилности, април 2018",line=-21,cex.main = .8, font.main=1)


##bar plot print

vector = seq(1,85,1)
vector[vector!=100]=NA
set.seed(1) # make reproducible
### 3x variables, 5x observations

df1 <- data.frame(bg)
vector[1:55]=ns
vector[vector!=100]=NA
vector[1:20]=nis
df1$nis = vector
vector[vector!=100]=NA
vector[1:74]=kg
df1$kg = vector

View(df1)
library(reshape2)
### convert to 'long form'
m1 <- melt(df1, measure.vars=c("bg","ns","kg","nis"))
### now use facets to give one plot per variable
library(ggplot2)
qplot(variable, data=m1, fill=value) + facet_wrap( facets= ~variable, scale="free_x")
#############
anketa_procisceno = read.csv("anketa_procisceno.csv")

ocene_strat= as.character(upitnik_filtered$Da.li.smatrate.da.Vaš.Univerzitet.ima.jasnu.strategiju.o.svojoj.internacionalizaciji..)
#maticni_univerziteti
ocene_i_uni= as.data.frame(ocene_strat)
ocene_i_uni$uni=maticni_univerziteti

ocene_i_uni = edit(ocene_i_uni)
##ciscenje#
ocene_i_uni[ocene_i_uni=="Megatrend"]=NA
ocene_i_uni[is.na(ocene_i_uni)]
ocene_i_uni = ocene_i_uni[complete.cases(ocene_i_uni),]

ocene_i_uni$ocene_strat = factor(ocene_i_uni$ocene_strat, levels=c("Ne","Nisam siguran/na","Da"))
#izazovi_i_uni=edit(izazovi_i_uni)
len=seq(1,36,1)
label=c("6 (7.1%)","32 (37.6%)","47 (55.3%)","33 (44.6%)","27 (36.5%)","14 (18.9%)","4 (20%)","6 (30%)","10 (50%)","9 (16.4%)","24 (43.6%)","22 (40%)")
ggplot(data = ocene_i_uni) + geom_bar(aes(x =  uni, fill = ocene_strat), position = "fill") +labs(x="",y="", title="Оцене стратегије интернационализације пропорционално по универзитетима") +scale_fill_manual("legend", values = c("#4c4c4c","#999999","#cccccc"))+ annotate(geom="text", label = label, x = c(1,1,1,2,2,2,3,3,3,4,4,4), y = c(0.05,0.3,.7,.3,.65,.9,.1,.35,.75,.1,.35,.8) , size = 4.5, colour = "white")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                                                                                                                                                                                                                                                                                                  panel.background = element_blank(), axis.line = element_line(colour = "black"))


##


prakse = as.character(upitnik_filtered$Koje.prakse.bi.univerziteti.fakulteti.u.Srbiji.trebalo.da.razviju.kako.bi.se.privukao.što.veci.broj.medunarodnih.studenata..Odaberite.tri.najbitnije.opcije.)
#unique(prakse)
prakse = as.data.frame(prakse)
#prakse[179,1]
#View(prakse)
#head(prakse,1)
prakse = str_split_fixed(prakse$prakse,";",17)
View(prakse)
prakse_=as.data.frame(prakse)
#prakse_V2=as.character(prakse_$V2)
#prakse_V3=as.character(prakse_$V3)
#prakse_V4=as.character(prakse_$V4)
#prakse_V5=as.character(prakse_$V5)
#prakse_V6=as.character(prakse_$V6)
#prakse_V7=as.character(prakse_$V7)
#prakse_V8=as.character(prakse_$V8)
#prakse_V9=as.character(prakse_$V8)
#prakse_V10=as.character(prakse_$V10)


#View(prakse)
#prakse_V2[prakse_V2==""]=as.character(prakse_$V3[prakse_$V2==""])
#prakse_V3[prakse_V3==""]=as.character(prakse_$V4[prakse_$V3==""])
#prakse_V4[prakse_V4==""]=as.character(prakse_$V5[prakse_$V4==""])
#prakse_V5[prakse_V5==""]=as.character(prakse_$V6[prakse_$V5==""])
#prakse_V6[prakse_V6==""]=as.character(prakse_$V7[prakse_$V6==""])
#prakse_V7[prakse_V7==""]=as.character(prakse_$V8[prakse_$V7==""])
#prakse_V8[prakse_V8==""]=as.character(prakse_$V9[prakse_$V8==""])
#prakse_V9[prakse_V9==""]=as.character(prakse_$V10[prakse_$V9==""])
#prakse_V10[prakse_V10==""]=as.character(prakse_$V11[prakse_$V10==""])

#prakse_$V2=prakse_V2
#prakse_$V3=prakse_V3
#prakse_$v4=prakse_V4
#prakse_$V5=prakse_V5
#prakse_$V6=prakse_V6
#prakse_$V7=prakse_V7
#prakse_$V8=prakse_V8
##prakse_$V9=prakse_V9
#prakse_$V10=prakse_V10

#=prakse_[prakse_$V2=="",]$V3
#length(prakse_[prakse_$V2=="",])
#length(prakse_[prakse_$V2=="",])
#View(prakse_)
prakse_=edit(prakse_)

unique(as.character(prakse_$V3))
#upitnik_filtered$prakse_privlacenje1=as.character(prakse_$V1)
#upitnik_filtered$prakse_privlacenje2=as.character(prakse_$V2)
#upitnik_filtered$prakse_privlacenje3=as.character(prakse_$V3)
#upitnik_filtered$prakse_privlacenje_kom=as.character(prakse_$V17)

prakse_kom = as.character(upitnik_filtered$prakse_privlacenje_kom)
prakse_kom[prakse_kom==""] = NA
prakse_kom[is.na(prakse_kom)]
prakse_kom = prakse_kom[complete.cases(prakse_kom)]

write.csv(prakse_kom,"prakse_komentari.csv")

prakse__ = prakse_
prakse__$V2
prakse__$V3
prakse_sve3 = c(as.character(prakse__$V1),as.character(prakse__$V2), as.character(prakse__$V3))#,as.character(prakse__$V4))
#uni_bg_ns_kg_nis = univerziteti_i_ostali#[univerziteti_i_ostali=="Inostranstvo"]
#uni_bg_ns_kg_nis[uni_bg_ns_kg_nis=="Inostranstvo"]=""
#uni_bg_ns_kg_nis[uni_bg_ns_kg_nis=="Ostalo*"]=""

#prakse_uni=c(uni_bg_ns_kg_nis,uni_bg_ns_kg_nis,uni_bg_ns_kg_nis)
#prakse_i_uni = as.data.frame(prakse_sve3)
#prakse_i_uni$uni=uni_bg_ns_kg_nis
#length(prakse_i_uni$prakse_sve3)
prakse_table = as.data.frame(table(as.character(prakse_sve3)))
prakse_table = edit(prakse_table)

prakse_table_na = prakse_table
prakse_table_na=prakse_table_na[,c(1,2)]
prakse_table_na[prakse_table_na==""]=NA
prakse_table_na[is.na(prakse_table_na)]
prakse_table_na = prakse_table_na[complete.cases(prakse_table_na),]
#View(prakse_table_na)#=prakse_table_na[,c(1,2)]
#prakse_table_na=edit(prakse_table_na)
new_Val= factor(prakse_table_na$Var1, levels = prakse_table_na$Var1[order(prakse_table_na$Freq)])

prakse_table_na$Var2=new_Val
label=prakse_table_na$Freq
label_proc = round(label/252*100,1)
label_proc = paste(label_proc,"%")
label = paste(label, "~",label_proc)

#View(as.data.frame(unlist(prakse_table_na)))
#prakse_table_na = edit(prakse_table_na)
g <- ggplot(prakse_table_na, aes(Var2, Freq))
g + geom_bar(stat="identity", width = 0.8, fill="#b2b2b2") + geom_text(aes(label =label), position = position_dodge(0.7),
                                                                       hjust = -.1,colour = "black", fontface = "bold")+  
  labs(title="Праксе да би се привукли међународни студенти. На основу три одговора 252 учесника", 
       subtitle="Анкета за студенте и алумнисте програма мобилности. Април 2018", 
       caption="Извор: Асоцијација алумниста Балкана - Србија", y="", x="
       ")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


save.image()
##prioriteti internacionalizacije
prioriteti = as.data.frame(upitnik_filtered$Šta.bi..po.Vašem.mišljenju..trebalo.da.budu.prioriteti.internacionalizacije.domacih.univerziteta..Odaberite.tri.najbitnije.opcije..)
colnames(prioriteti)[1]="prioriteti"
prioriteti = str_split_fixed(prioriteti$prioriteti,";",10)
prioriteti = as.data.frame(prioriteti)
prioriteti_ = edit(prioriteti_)
unique(as.character(prioriteti_$V3))

upitnik_filtered$prioriteti_internac1 = as.character(prioriteti_$V1)
upitnik_filtered$prioriteti_internac2 = as.character(prioriteti_$V2)
upitnik_filtered$prioriteti_internac3 = as.character(prioriteti_$V3)
upitnik_filtered$prioriteti_internac_kom = as.character(prioriteti_$var11)

prioriteti_kom = prioriteti_$var11[!is.na(prioriteti_$var11)]
write.csv(prioriteti_kom,"prioriteti_kom.csv")


prioriteti_tri = as.data.frame(c(as.character(prioriteti_$V1),as.character(prioriteti_$V2),as.character(prioriteti_$V3)))
#prioriteti_tri$uni =  uni_bg_ns_kg_nis
colnames(prioriteti_tri)[1]="prioriteti"
#View(prioriteti_tri)
prioriteti_table_df = as.data.frame(table(as.character(prioriteti_tri$prioriteti)))
prioriteti_table_df = edit(prioriteti_table_df)
prioriteti_table_df[prioriteti_table_df==""]=NA
prioriteti_table_df[is.na(prioriteti_table_df)]
prioriteti_table_df = prioriteti_table_df[complete.cases(prioriteti_table_df),]
View(prioriteti_table_df)
prioriteti_table_df_ed$Var1= factor(as.character(prioriteti_table_df_ed$Var1), levels = prioriteti_table_df_ed$Var1[order(prioriteti_table_df_ed$Freq)])

prioriteti_table_df_ed = edit(prioriteti_table_df)
prioriteti_table_df_ed = edit(prioriteti_table_df_ed)

label=prioriteti_table_df_ed$Freq
label_proc = round(label/252*100,1)
label_proc = paste(label_proc,"%")
label = paste(label, "~",label_proc)

g <- ggplot(prioriteti_table_df_ed, aes(Var1, Freq))
g + geom_bar(stat="identity", width = 0.8, fill="#b2b2b2") + geom_text(aes(label = label), position = position_dodge(0.5),
                                                                       hjust = 0,colour = "black", fontface = "bold")+  
  labs(title="Приоритети интернационализације домаћих универзитета. На основу три одговора 252 учесника", 
       subtitle="Анкета за студенте и алумнисте програма мобилности. Април 2018", 
       caption="Извор: Асоцијација алумниста Балкана - Србија", y="", x="")+
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + coord_flip()+
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank(), axis.line = element_line(colour = "black"))

##koje od usluga smatrate veoma dobrim

usluge_dobre = as.data.frame(as.character(upitnik_filtered$Koje.od.usluga.na.univerzitetu.fakultetu.na.kom.ste.boravili.na.razmeni.smatrate.veoma.dobrim..Odaberite.tri.najbitnije.opcije.))
colnames(usluge_dobre)[1]="usluge"
usluge_dobre = str_split_fixed(usluge_dobre$usluge,";",16)
usluge_dobre=as.data.frame(usluge_dobre)
usluge_dobre_ = edit(usluge_dobre_)
unique(as.character(usluge_dobre_$V3))

upitnik_filtered$usluge_smatrano_dobrim1=as.character(usluge_dobre_$V1)
upitnik_filtered$usluge_smatrano_dobrim2=as.character(usluge_dobre_$V2)
upitnik_filtered$usluge_smatrano_dobrim3=as.character(usluge_dobre_$V3)
upitnik_filtered$usluge_smatrano_dobrim_kom=as.character(usluge_dobre_$V16)


usluge_kom = usluge_dobre_$V16[(usluge_dobre_$V16)!=""]
write.csv(usluge_kom,"usluge_kom.csv")



usluge_tri = as.data.frame(c(as.character(usluge_dobre_$V1),as.character(usluge_dobre_$V2),as.character(usluge_dobre_$V3)))
colnames(usluge_tri)[1]="usluge"
usluge_table_df = as.data.frame(table(usluge_tri$usluge))
usluge_table_df = edit(usluge_table_df)
usluge_table_df[usluge_table_df==""]=NA
usluge_table_df[is.na(usluge_table_df)]
usluge_table_df = usluge_table_df[complete.cases(usluge_table_df),]
usluge_table_df$Var1= factor(as.character(usluge_table_df$Var1), levels = usluge_table_df$Var1[order(usluge_table_df$Freq)])


usluge_table_df$proc = round(usluge_table_df$Freq/251*100,1)
label=""
label=paste(usluge_table_df$Freq,"(")
label=paste(label, usluge_table_df$proc)
label=paste(label,"%)")

g <- ggplot(usluge_table_df, aes(Var1, Freq))
g + geom_bar(stat="identity", width = 0.7, fill="#4c4c4c") + geom_text(aes(label = label), position = position_dodge(.9),
                                                                       hjust = 1.5,colour = "#e5e5e5", size=3.5, fontface="bold")+  
  labs(title="Услуге на страном факултету/универзитету које се сматрају веома добрим.\nНа основу три одговора 252 учесника", 
       subtitle="Анкета за студенте и алумнисте програма мобилности. Април 2018", 
       caption="Извор: Асоцијација алумниста Балкана - Србија", y="", x="")+coord_flip() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                           panel.background = element_blank(), axis.line = element_line(colour = "black"))


##najveci izazov stranim kod nas
strani_izazov=as.data.frame(as.character(upitnik_filtered$Šta.bi.po.Vašem.mišljenju.bio.najveci.izazov.internacionalnim.studentima.koji.dodu.da.studiraju.u.Srbiju..))
colnames(strani_izazov)[1]="strani_izazov"
strani_izazov=str_split_fixed(strani_izazov$strani_izazov,";",5)
strani_izazov = as.data.frame(strani_izazov)
View(strani_izazov)
strani_izazov = edit(strani_izazov)
unique(as.character(strani_izazov$V3))
strani_izazov_ = strani_izazov

upitnik_filtered$strani_stud_izazovi1 = as.character(strani_izazov_$V1)
upitnik_filtered$strani_stud_izazovi2 = as.character(strani_izazov_$V2)
upitnik_filtered$strani_stud_izazovi3 = as.character(strani_izazov_$V3)
upitnik_filtered$strani_stud_izazovi4 = as.character(strani_izazov_$V4)
upitnik_filtered$strani_stud_izazovi_kom = as.character(strani_izazov_$V5)

##export dodatne komentar
write.csv(strani_izazov_$V5[strani_izazov_$V5!=""],"strani_student_izazov.csv")

strani_izazov_tri= as.data.frame(c(as.character(strani_izazov_$V1),as.character(strani_izazov_$V2),as.character(strani_izazov_$V3)))
colnames(strani_izazov_tri)[1]="strani_izazov"

strani_izazov_table_df = as.data.frame(table(strani_izazov_tri$strani_izazov))
strani_izazov_table_df = edit(strani_izazov_table_df)
strani_izazov_table_df[strani_izazov_table_df==""]=NA
strani_izazov_table_df[is.na(strani_izazov_table_df)]
strani_izazov_table_df = strani_izazov_table_df[complete.cases(strani_izazov_table_df),]
strani_izazov_table_df$Var1= factor(as.character(strani_izazov_table_df$Var1), levels = strani_izazov_table_df$Var1[order(strani_izazov_table_df$Freq)])

strani_izazov_table_df$proc = round(strani_izazov_table_df$Freq/251*100,1)
label=""
label=paste(strani_izazov_table_df$Freq,"(")
label=paste(label, strani_izazov_table_df$proc)
label=paste(label,"%)")
strani_izazov_table_df=edit(strani_izazov_table_df)
g <- ggplot(strani_izazov_table_df, aes(Var1, Freq))
g + geom_bar(stat="identity", width = 0.7, fill="#4c4c4c") + geom_text(aes(label = label), position = position_dodge(.9),
                                                                   vjust=1.5,hjust = .5,colour = "#e5e5e5", size=3.5, fontface="bold")+  
  labs(title="Изазови страних студената доласком у Србију. На основу три одговора 252 учесника", 
       subtitle="Анкета за студенте и алумнисте програма мобилности. Април 2018", 
       caption="Извор: Асоцијација алумниста Балкана - Србија", y="", x="") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                           panel.background = element_blank(), axis.line = element_line(colour = "black"))
##prednosti obrazovanja u Srbiji, a koje u inostranstvu

prednosti_ino = as.data.frame(as.character(upitnik_filtered$Koje.su.prednosti.visokog.obrazovanja.u.Srbiji..a.koje.u.obrazovanju.koje.ste.ostvarili.u.inostranstvu..Odaberite.tri.najbitnije.opcije.za.oba...inostranstvo.))
colnames(prednosti_ino)[1]="prednost"

prednosti_ino = str_split_fixed(prednosti_ino$prednost,";",16)
prednosti_ino = as.data.frame(prednosti_ino)
View(prednosti_ino)
#unique(as.character(prednosti_srb$V3))

prednosti_ino_16 = as.data.frame(c(as.character(prednosti_ino$V1),
                                    as.character(prednosti_ino$V2),
                                    as.character(prednosti_ino$V3),
                                    as.character(prednosti_ino$V4),
                                    as.character(prednosti_ino$V5),
                                    as.character(prednosti_ino$V6),
                                    as.character(prednosti_ino$V7),
                                    as.character(prednosti_ino$V8),
                                    as.character(prednosti_ino$V9),
                                    as.character(prednosti_ino$V10),
                                    as.character(prednosti_ino$V11),
                                    as.character(prednosti_ino$V12),
                                    as.character(prednosti_ino$V13),
                                    as.character(prednosti_ino$V14),
                                    as.character(prednosti_ino$V15),
                                    as.character(prednosti_ino$V16)))

colnames(prednosti_ino_16)[1]="prednost"
prednosti_ino_16[prednosti_ino_16==""]=NA
prednosti_ino_16[is.na(prednosti_ino_16)]
prednosti_ino_16 = prednosti_ino_16[complete.cases(prednosti_ino_16),]

prednosti_ino_16_tb_df = as.data.frame(table(prednosti_ino_16))
prednosti_ino_16_tb_df = edit(prednosti_ino_16_tb_df)
prednosti_ino_16_tb_df = prednosti_ino_16_tb_df[-c(1),]

upitnik_filtered$prednost_inostranstva1 = as.character(prednosti_ino$V1)
upitnik_filtered$prednost_inostranstva2 = as.character(prednosti_ino$V2)
upitnik_filtered$prednost_inostranstva3 = as.character(prednosti_ino$V3)
upitnik_filtered$prednost_inostranstva4 = as.character(prednosti_ino$V4)
upitnik_filtered$prednost_inostranstva5 = as.character(prednosti_ino$V5)
upitnik_filtered$prednost_inostranstva6 = as.character(prednosti_ino$V6)
upitnik_filtered$prednost_inostranstva7 = as.character(prednosti_ino$V7)
upitnik_filtered$prednost_inostranstva8 = as.character(prednosti_ino$V8)
upitnik_filtered$prednost_inostranstva9 = as.character(prednosti_ino$V9)
upitnik_filtered$prednost_inostranstva10 = as.character(prednosti_ino$V10)
upitnik_filtered$prednost_inostranstva11 = as.character(prednosti_ino$V11)
upitnik_filtered$prednost_inostranstva12 = as.character(prednosti_ino$V12)
upitnik_filtered$prednost_inostranstva13 = as.character(prednosti_ino$V13)
upitnik_filtered$prednost_inostranstva14 = as.character(prednosti_ino$V14)
upitnik_filtered$prednost_inostranstva15 = as.character(prednosti_ino$V15)
upitnik_filtered$prednost_inostranstva16 = as.character(prednosti_ino$V16)

##srbija

prednosti_srb = as.data.frame(as.character(upitnik_filtered$Koje.su.prednosti.visokog.obrazovanja.u.Srbiji..a.koje.u.obrazovanju.koje.ste.ostvarili.u.inostranstvu..Odaberite.tri.najbitnije.opcije.za.oba...Srbija.))
colnames(prednosti_srb)[1]="prednost"

prednosti_srb = str_split_fixed(prednosti_srb$prednost,";",16)
prednosti_srb = as.data.frame(prednosti_srb)
#View(prednosti_srb)
#unique(as.character(prednosti_srb$V3))

upitnik_filtered$prednosti_srb1 = as.character(prednosti_srb$V1)
upitnik_filtered$prednosti_srb2 = as.character(prednosti_srb$V2)
upitnik_filtered$prednosti_srb3 = as.character(prednosti_srb$V3)
upitnik_filtered$prednosti_srb4 = as.character(prednosti_srb$V4)
upitnik_filtered$prednosti_srb5 = as.character(prednosti_srb$V5)
upitnik_filtered$prednosti_srb6 = as.character(prednosti_srb$V6)
upitnik_filtered$prednosti_srb7 = as.character(prednosti_srb$V7)
upitnik_filtered$prednosti_srb8 = as.character(prednosti_srb$V8)
upitnik_filtered$prednosti_srb9 = as.character(prednosti_srb$V9)
upitnik_filtered$prednosti_srb10 = as.character(prednosti_srb$V10)
upitnik_filtered$prednosti_srb11 = as.character(prednosti_srb$V11)
upitnik_filtered$prednosti_srb12 = as.character(prednosti_srb$V12)
upitnik_filtered$prednosti_srb13 = as.character(prednosti_srb$V13)
upitnik_filtered$prednosti_srb14 = as.character(prednosti_srb$V14)
upitnik_filtered$prednosti_srb15 = as.character(prednosti_srb$V15)
upitnik_filtered$prednosti_srb16 = as.character(prednosti_srb$V16)


prednost_srb_16 = as.data.frame(c(as.character(prednosti_srb$V1),
                                   as.character(prednosti_srb$V2),
                                   as.character(prednosti_srb$V3),
                                   as.character(prednosti_srb$V4),
                                   as.character(prednosti_srb$V5),
                                   as.character(prednosti_srb$V6),
                                   as.character(prednosti_srb$V7),
                                   as.character(prednosti_srb$V8),
                                   as.character(prednosti_srb$V9),
                                   as.character(prednosti_srb$V10),
                                   as.character(prednosti_srb$V11),
                                   as.character(prednosti_srb$V12),
                                   as.character(prednosti_srb$V13),
                                   as.character(prednosti_srb$V14),
                                   as.character(prednosti_srb$V15),
                                   as.character(prednosti_srb$V16)))
colnames(prednost_srb_16)[1]="prednost"
prednost_srb_16[prednost_srb_16==""]=NA
prednost_srb_16[is.na(prednost_srb_16)]
prednost_srb_16 = prednost_srb_16[complete.cases(prednost_srb_16),]

prednost_16_tb_df = as.data.frame(table(prednost_srb_16))
prednost_16_tb_df = edit(prednost_16_tb_df)
prednost_16_tb_df = prednost_16_tb_df[-c(1),]
##merging both

unique(prednosti_ino_16_tb_df$prednosti_ino_16)
unique(prednost_16_tb_df$prednost_srb_16)

prednost_oba = prednost_16_tb_df
prednost_oba$FreqIno = prednosti_ino_16_tb_df$Freq


prednost_oba_3 = prednost_srb_tb_df
prednost_oba_3$FreqIno = prednosti_ino_tri_tb_df$Freq
View(prednost_oba_3)
View(prednost_oba)
plot(prednost_oba$Freq,prednost_oba$FreqIno)
##using reshape and melt
#library(reshape)
prednost_oba_reshape <- melt(prednost_oba, id.vars='prednost_srb_16')


prednost_oba_3_reshape <- melt(prednost_oba_3, id.vars='prednost_srb_tri')
prednost_oba_3_reshape = edit(prednost_oba_3_reshape)

prednost_oba_reshape$prednost_srb_16 = factor(prednost_oba_reshape$prednost_srb_16,levels=levels(prednost_oba_3_reshape$prednost_srb_tri))

prednost_oba_3_reshape$proc = round(prednost_oba_reshape$value/252*100,1)
prednost_oba_3_reshape = edit(prednost_oba_3_reshape)

prednost_oba_reshape$proc = round(prednost_oba_reshape$value/252*100,1)
prednost_oba_reshape = edit(prednost_oba_reshape)

prednost_oba_reshape$prop = round(c(prednost_oba_reshape$value[17:32],prednost_oba_reshape$value[17:32])/prednost_oba_reshape$value,0)

prednost_oba_reshape$prop_srb = round(c(prednost_oba_reshape$value[1:16],prednost_oba_reshape$value[1:16])/prednost_oba_reshape$value,0)
prednost_oba_reshape_order_by_prop = prednost_oba_reshape[order(prednost_oba_reshape$prop, decreasing = T),]
prednost_oba_reshape_by_propsrb=prednost_oba_reshape[order(prednost_oba_reshape$prop_srb, decreasing = T),]
View(prednost_oba_reshape_by_propsrb)

write.csv(prednost_oba_reshape_order_by_prop[1:11,c(1,4)],"prednosti_ino_vs_srbija.csv")
write.csv(prednost_oba_reshape_by_propsrb[1:2,c(1,5)],"prednosti_srb_vs_ino.csv")


View(prednost_oba_reshape)
label=""
label=paste(prednost_oba_3_reshape$value,"")
label=paste(label, prednost_oba_reshape$proc)
label=paste(label,"%)")

ggplot(prednost_oba_3_reshape, aes(x=prednost_srb_tri, y=value, fill=variable))+ scale_fill_manual(values = c("#cccccc","#323232"))+
  geom_bar(stat='identity', position='dodge')+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                     panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  coord_flip()+labs(title="Предности образовања у Србији и иностранству. На основу три одговора учесника анкете", 
      subtitle="Анкета за студенте и алумнисте програма мобилности. Април 2018", 
      caption="Извор: Асоцијација алумниста Балкана - Србија", y="", x="")+
   geom_text(aes(label = label), position = position_dodge(.9),
              hjust = -.2,colour = "black", size=3.5)
# ##export svih filtriranih podataka
# colnames(upitnik_filtered)
# kolone = colnames(upitnik_filtered)[-c(2:11,13:37)]
# 
# anketa_procisceno = subset(upitnik_filtered, select=kolone)
# colnames(anketa_procisceno)[2]="domace_mob_organizacije"
# View(anketa_procisceno)
# #View(upitnik_filtered)
# write.csv(anketa_procisceno,"anketa_procisceno.csv")
##proveriti KG i Spaniju

strani_unis=as.character(c(anketa_procisceno$mob_uni1,
               anketa_procisceno$mob_uni1,
               anketa_procisceno$mob_uni1,
               anketa_procisceno$mob_uni1))

strane_drzave = c(as.character(anketa_procisceno$mob_drz1),
                  as.character(anketa_procisceno$mob_drz2),
                  as.character(anketa_procisceno$mob_drz3),
                  as.character(anketa_procisceno$mob_drz4))
kg_uni = c(as.character(anketa_procisceno$maticni_uni),
           as.character(anketa_procisceno$maticni_uni),
           as.character(anketa_procisceno$maticni_uni),
           as.character(anketa_procisceno$maticni_uni))
drz_kg_uni = as.data.frame(strane_drzave)                      
drz_kg_uni$kg_uni = kg_uni

head(drz_kg_uni,20)
drz_kg_uni = drz_kg_uni[drz_kg_uni$kg_uni=="Univerzitet u Kragujevcu",]
View(drz_kg_uni)
kragujevcani = as.data.frame(table(drz_kg_uni$strane_drzave))
kragujevcani=kragujevcani[kragujevcani$Freq!=0,]
#kragujevcani = kragujevcani[-c(1),]
g <- ggplot(kragujevcani, aes(Var1, Freq))
g + geom_bar(stat="identity", width = 0.45, fill="tan")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                               panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  coord_flip()+labs(title="Универзитет у Крагујевцу", 
    
                    
                                    caption="Извор: Асоцијација алумниста Балкана - Србија", y="", x="")
###procenat po fakultetima koji je isao u spaniju

spanija = anketa_procisceno[(anketa_procisceno$mob_drz1=="Spanija" |
                              anketa_procisceno$mob_drz2=="Spanija" |
                              anketa_procisceno$mob_drz3=="Spanija" |
                              anketa_procisceno$mob_drz4=="Spanija" |
                               anketa_procisceno$mob_drz5=="Spanija"),]
dim(spanija)
View(spanija_tabela)
spanija_tabela = as.data.frame(table(spanija$maticni_uni))
spanija_tabela$proc = spanija_tabela$Freq/35*100

dim(spanija[spanija$maticni_uni=="Univerzitet u Beogradu",])              

#amerikanci

sad = anketa_procisceno[(anketa_procisceno$mob_drz1=="USA" |
                               anketa_procisceno$mob_drz2=="USA" |
                               anketa_procisceno$mob_drz3=="USA" |
                               anketa_procisceno$mob_drz4=="USA" |
                              anketa_procisceno$mob_drz5=="USA"),]
View(sad)
### broj osnovnih, master, doc
dim(anketa_procisceno[anketa_procisceno$nivoStudija1=="osnovne studije" |
                        anketa_procisceno$nivoStudija2=="osnovne studije" |
                        anketa_procisceno$nivoStudija3=="osnovne studije" ,])
View(anketa_procisceno[anketa_procisceno$nivoStudija1=="master studije" |
                        anketa_procisceno$nivoStudija2=="master studije" |
                        anketa_procisceno$nivoStudija3=="master studije" ,])
dim(anketa_procisceno[anketa_procisceno$nivoStudija1=="doktorske studije" |
                         anketa_procisceno$nivoStudija2=="doktorske studije" |
                         anketa_procisceno$nivoStudija3=="doktorske studije" |
                        anketa_procisceno$nivoStudija1=="post-doktorske studije" |
                        anketa_procisceno$nivoStudija2=="post-doktorske studije" |
                        anketa_procisceno$nivoStudija3=="post-doktorske studije" | 
                      anketa_procisceno$nivoStudija1=="istraživanje" |
                        anketa_procisceno$nivoStudija2=="istraživanje" |
                        anketa_procisceno$nivoStudija3=="istraživanje" |
                        anketa_procisceno$nivoStudija1=="trening" |
                        anketa_procisceno$nivoStudija2=="trening" |
                        anketa_procisceno$nivoStudija3=="trening",])
##broj onih na duplim
View(anketa_procisceno[anketa_procisceno$nivoStudija1="" |
                    anketa_procisceno$nivoStudija3!="",])
##problemi gostujuci
problemi_gost = upitnik_filtered$problem_komunikac_gost_komentar[!is.na(upitnik_filtered$problem_komunikac_gost_komentar)]
write.csv(problemi_gost,"problemi_gost.csv")
zaposlenje_kom=as.character(levels(upitnik_filtered$Smatrate.li.da.student.koji.se.vrati.sa.razmene.ili.studija.u.inostranstvu.ima.bolje.šanse.za.zaposlenje.u.Srbiji.))
write.csv(zaposlenje_kom,"zaposlenje_kom.csv")
save.image()
