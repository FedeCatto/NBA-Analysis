library(tidyverse)
library(mclust)
library(Rmixmod)
library(GGally)
library(ggcorrplot)
library(flexmix)

#####import e trasformazione datasets####
#data 97-22
#tutti i dataset sono importati utilizzando
#i nostri path, lasciamo i read.csv perchè contengono i
#nomi con cui abbiamo memorizzato i nostri dati
nba1=read.csv("Stat computaz/hw 2/NBA_Player_Stats_2.csv")
anyNA(nba1)


#Sistemo dopo gli NA perchè devo prima aggiungere i dati della stagione 22-23
#Essi avvengono quando gli attempts sono 0 inoltre
#tutte le percentage sono leggermente errate



#data 22-23
dati2223=read.csv("Stat computaz/hw 2/dati2223.txt")
#Il dataset sulla stagione 2022-23 è stato preso
#dalla stessa fonte con cui è stato creato quello con tutte le stagioni
#perciò i dati sono coerenti, c'è una variabile in più: player_additional
#che è l'ID univoco del player usato dal sito per memorizzarlo
dati22.23=dati2223%>%select(-Player.additional)
#Rimuoviamo questa variabile

#Nel dataset iniziale abbiamo due variabili in più:
# -Season: stagione a cui la u.s. fa riferimento
# -MVP: variabile dicotomica che indica se il giocatore è stato eletto MVP
#  in quella stagione (il premio MVP è uno solo per stagione ed è dato al 
# migliore giocatore della stagione)

#Quindi creiamo la variabile Season che è sempre ="2022-23"
#per questo dataset. La variabile MVP="False" per tutti tranne per il vincitore
#del premio in questa stagione, ovvero Joel Embiid
dati22.23=dati22.23%>%
  mutate(Season="2022-23",MVP=ifelse(Player=="Joel Embiid","True","False"))

#unisco i due dataset
datinba=rbind(nba1,dati22.23)

napos=which(is.na(datinba),arr.ind = T) 
#NA sono solo sulle percentage (X2P.,X3P. etc...) quando attempts=0 quindi sostituisco questi 
#e in più le percentuali sono leggermente sbagliate per errori di approssimazione
colnames(datinba)[as.numeric(levels(as.factor(napos[,2])))] #verifico che
#siano tutte percentage le variabili in cui ho NA
datinba=datinba%>%mutate(FG.=ifelse(FGA==0,0,round(FG/FGA,3)),X3P.=ifelse(X3PA==0,0,round(X3P/X3PA,3)),
                   X2P.=ifelse(X2PA==0,0,round(X2P/X2PA,3)),FT.=ifelse(FTA==0,0,round(FT/FTA,3)),
                   eFG.=ifelse(FGA==0,0,round((1.5*X3P+X2P)/FGA,3)))

anyNA(datinba) #risolti gli NA

#Data height
height=read.csv("Stat computaz/hw 2/all_seasons.csv")
#rendiamo compatibili i nomi delle variabili
height=height%>%rename(Player=player_name,Season=season,Net_rating=net_rating,
                       ORB.=oreb_pct,DRB.=dreb_pct,USG.=usg_pct,TS.=ts_pct,AST.=ast_pct) 

#Faremo un join sui due dataset basato sulle variabili
#Player e Season, ovviamente i nomi dei giocatori possono dar problemi
#in quanto possono essere stati memorizzati in maniera differente,
# vediamo con un anti_join i problemi nel join e cerchiamo di risolverli:
missjoin=anti_join(datinba, height,by=c("Player","Season"))
missjoin$Player 
#notiamo che alcuni giocatori vengono memorizzati 
#con una stellina (*), dopo alcuni approfondimenti
# abbiamo notato che sta a significare che questo giocatore 
#è nella Hall of Fame (ovvero è tra i migliori giocatori NBA della storia)

hof=grep("*",datinba$Player,fixed=T)
# in caso volessimo memorizzare i dati su hall of famers
datinba$Player=gsub('*', '', datinba$Player,fixed=T)

missjoin=anti_join(datinba,height,by=c("Player","Season"))
missjoin$Player
#join sbagliati dovuti a caratteri speciali, li fixo uno per volta
#con gsub
datinba$Player=gsub('č', 'c', datinba$Player,fixed=T)
datinba$Player=gsub('Ž', 'Z', datinba$Player,fixed=T)
datinba$Player=gsub('ć', 'c', datinba$Player,fixed=T)
datinba$Player=gsub('ü', 'u', datinba$Player,fixed=T)
datinba$Player=gsub('ò', 'o', datinba$Player,fixed=T)
datinba$Player=gsub('è', 'e', datinba$Player,fixed=T)
datinba$Player=gsub('é', 'e', datinba$Player,fixed=T)
datinba$Player=gsub('á', 'a', datinba$Player,fixed=T)
datinba$Player=gsub('ó', 'o', datinba$Player,fixed=T)
datinba$Player=gsub('í', 'i', datinba$Player,fixed=T)
datinba$Player=gsub('Š', 'S', datinba$Player,fixed=T)
datinba$Player=gsub('ä', 'a', datinba$Player,fixed=T)
datinba$Player=gsub('ö', 'o', datinba$Player,fixed=T)
datinba$Player=gsub('ï', 'i', datinba$Player,fixed=T)
datinba$Player=gsub('ô', 'o', datinba$Player,fixed=T)
datinba$Player=gsub('ô', 'o', datinba$Player,fixed=T)
datinba$Player=gsub('ô', 'o', datinba$Player,fixed=T)
datinba$Player=gsub('ô', 'o', datinba$Player,fixed=T)
datinba$Player=gsub('ô', 'o', datinba$Player,fixed=T)
datinba$Player=gsub('ģ', 'g', datinba$Player,fixed=T)
datinba$Player=gsub('ņ', 'n', datinba$Player,fixed=T)
datinba$Player=gsub('ã', 'a', datinba$Player,fixed=T)
datinba$Player=gsub('Ş', 'S', datinba$Player,fixed=T)
datinba$Player=gsub('ğ', 'g', datinba$Player,fixed=T)
datinba$Player=gsub('ū', 'u', datinba$Player,fixed=T)
datinba$Player=gsub('ř', 'r', datinba$Player,fixed=T)
datinba$Player=gsub('ž', 'z', datinba$Player,fixed=T)
datinba$Player=gsub('š', 's', datinba$Player,fixed=T)
datinba$Player=gsub('ë', 'e', datinba$Player,fixed=T)


#Alcuni giocatori hanno il nome diversamente memorizzato nel dataset
#Height,decido di sistemarli direttamente lì.
height$Player=gsub("Clar.","Clarence",height$Player,fixed=T)
height$Player=gsub("JR","J.R.",height$Player,fixed=T)
height$Player=gsub("PJ","P.J.",height$Player,fixed=T)
height$Player=gsub("JJ","J.J.",height$Player,fixed=T)
height$Player=gsub("TJ","T.J.",height$Player,fixed=T)
height$Player=gsub("DJ","D.J.",height$Player,fixed=T)
height$Player=gsub("CJ","C.J.",height$Player,fixed=T)
height$Player=gsub("KJ","K.J.",height$Player,fixed=T)


missjoin=anti_join(datinba,height,by=c("Player","Season"))
mj=missjoin%>%filter(MP>15)
#La perdita di alcune righe è dovuta sia alla mancanza di dati nel dataset height
#che alla differenza tra formati dei nomi di alcuni giocatori
#Tuttavia, poichè consideriamo nelle analisi solo giocatori con MP>15,
#la perdita di informazioni è trascurabile

dat=inner_join(datinba,height,by=c("Player","Season"))
#Il warning è dovuto a omonimie di giocatori nella stessa stagione,
#invece il dataset height può avere più corrispondnze con datinba
#perchè nel primo i giocatori posssono essere presenti in più righe
#per una stagione se cambiano team durante il corso di questa
dat%>%filter(Player=="Marcus Williams")%>%select(Player,Season,Tm,player_height,MP)
#duplica il dato, ma siccome considero solo quando MP>15
#mi interessa eliminare la terza riga di questo tibble appena creato
dat=dat%>%filter(!(Player=="Marcus Williams" & Tm=="NJN" & Season=="2007-08" & player_height>200 ))

#Poichè faremo delle analisi basate sui ruoli (Pos)
#rimuoviamo i ruoli ibridi (tipo "C-PF" è un ibrido tra Centro e Power Forward)
#mantendendo solo il primo dei due che è quello "principale"
#(da "C-PF" a "C")
dat$Pos=gsub("-PF","",dat$Pos,fixed=T)
dat$Pos=gsub("-PG","",dat$Pos,fixed=T)
dat$Pos=gsub("-SF","",dat$Pos,fixed=T)
dat$Pos=gsub("-SG","",dat$Pos,fixed=T)
dat$Pos=gsub("-C","",dat$Pos,fixed=T)
dat$Pos=(as.factor(dat$Pos))
#abbiamo solamente 5 levels adesso, che sono i 5 ruoli

#modifico le variabili comprese tra 0 e 1 in modo tale che siano su tutto l'insieme dei numeri reali
dat=dat%>%
  mutate(across(ends_with(".") , function(x) ifelse(x<1 & x>0,log(x/(1-x)),x*log(0.95/0.05))))
#dato che i valori estremi di 0 e 1 creano problemi,utilizzo queta trasformazione
#in moda da mantenere gli 0 invariati e da trattare gli 1 come 0.95 che riteniamo essere un valore
#consono all'analisi
#dà warning ma non crea effettivamente dei Nan, lo verifico qua sotto
sum(is.nan(dat$eFG.))
sum(is.nan(dat$FT.))
sum(is.nan(dat$X2P.))
sum(is.nan(dat$X3P.))
sum(is.nan(dat$FG.))
sum(is.nan(dat$ast_pct))
sum(is.nan(dat$oreb_pct))
sum(is.nan(dat$usg_pct))
sum(is.nan(dat$dreb_pct))
sum(is.nan(dat$ts_pct))

#ci sono variabili che si ripetono perchè presenti in entrambi i dataset
#quindi le rimuoviamo, togliamo anche le variabili riguardanti il draft e la provenienza
#che hanno ben poca rilevanza in questa analisi
dat=dat%>%
  select(-college,-country,-draft_year,-draft_round,-draft_number,-gp,-pts,-reb,-ast,-Rk,-age,-X)

########CLASSIFICAZIONE PER RUOLI (tutte stagioni)######
#Facciamo una classificazione di giocatori NBA per la loro posizione considerando prima
#tutte le stagioni presenti nel dataset.
#Successivamente proviamo a trovare le differenze di classificazione nei ruoli 
#in due periodi differenti, il più recente e il più remoto entrambi di 5 stagioni,
#per capire se è avvenuto qualche cambiamento
# per quanto riguarda le posizioni e quello che le caratterizza.
#Tutte queste tre classificazioni saranno fatte tramite EDDA, confrontando
#i modelli mediante cross-validation con un training set, un  selection set
#ed un test set
#Funzione moda
#verrà usata perchè quando raggrupperemo per giocatori
#selezioneremo come ruolo quello che hanno ricoperto 
#più volte negli anni considerati.
moda=function(x){
  names(which.max(table(x)))
}

#####Scelta Variabili####
dat$Pos=as.factor(dat$Pos)
#Creo un dataset in cui faccio un raggruppamento per giocatore
#e calcolo la media di ogni statistica nel corso di alcune stagioni
#Questo è utile per smussare eventuali stagioni anomale giocate
#da un singolo giocatore
#Seleziono solo le righe con MP>15 perchè se un giocatore ha giocato troppo poco
#le statistiche non saranno affidabili
dtgioctot=dat%>%filter(MP>15)%>%
  group_by(Player)%>%
  summarise(across(where(is.numeric),mean),Pos=as.factor(moda(Pos)))

#Facciamo correlogramma per avere un'idea iniziale sulle variabili
ggcorrplot(cor(dtgioctot[,c(6:27,30:35)]),lab=T, lab_size=2,type="upper")
#Sceglieremo le variabili utilizzando le nostre conoscenze nel settore,
#cercando di evitare le variabili eccessivamente
#correlate tra loro o che mostrano poche differenze tra i ruoli
ggpairs(dtgioctot[,c(6:16)],mapping=aes(col=dtgioctot$Pos))
#La variabile X3P. è multimodale e sembra distinguere bene tra almeno 2 
#dei 5 ruoli
ggpairs(dtgioctot[,17:24],mapping=aes(col=dtgioctot$Pos))
#Le variabili meglio differenziate per posizione che considererei nel cluster sono ORB,AST
ggpairs(dtgioctot[,c(25:27,30:35)],mapping = aes(col=dtgioctot$Pos))
#Abbiamo scelto anche player_height come variabile 
#caratterizzante della posizione come possiamo notare nel grafico.
#Scegliamo inoltre DRB.,perchè ben divide bene per posizione
varclust=c("player_height","X3P.","AST","DRB.","BLK","ORB") 
#Chiamo varclust e trsetclust nonostante sia classificazione poichè, per 
#prove inizialmente, era stata utilizzata per fare clustering 


#Poichè l'estrazione di campioni con slice_sample
#introduce una inevitabile casualità nella scelta del modello,
#facciamo un ciclo for per vedere quale modello
#viene selezionato più volte come migliore
#utilizzando il selection set
ggcorrplot(cor(dtgioctot%>%select(all_of(varclust))),lab=T)
#le correlazioni non sono eccessivamente alte
M=100
mbest=tibble(mod=c(NA))
for( i in  2:M){
  #TRAINING SET
#Training set preso come il 60% del dataset in maniera casuale
#Scelgo solo le variabili che userò per la classification
#Lo estraggo ogni volta dentro al ciclo for per 
  #utilizzarne diversi ed evitare che, selezionandone uno solo,
  #questo mi possa dare un "training" inadatto al classificatore e quindi
  #deviare i miei risulati.
  trset=dtgioctot%>%slice_sample(prop = 0.6) 
  trsetclst=trset%>%select(all_of(varclust))
  #memorizzo anche le posizioni
  trsetpos=trset%>%select(Pos)  
  lernmodtot=mixmodLearn(trsetclst, trsetpos$Pos, 
                         models=mixmodGaussianModel(family="all", equal.proportions = F),
                         criterion=c('CV'))
  #SELECTION SET
  #creo il set contenente il resto delle u.s. e da questo estraggo il selection
  #set, che verrà utilizzato per scegliere il modello.
  set40=dtgioctot%>%filter(!Player %in% trset$Player)
  selectset=set40%>%slice_sample(prop=0.5)
  selectclst=selectset%>%select(all_of(varclust))
  nsel=nrow(selectclst)
  #Calcolo per ogni modello il MER ottenuto dal selection set,
  #e poi estraggo il modello migliore in base a questo indicatore.
  mermod=sapply(lernmodtot@results,FUN=function(x){
    selpred=mixmodPredict(selectclst,x)
    MER=length(classError(selpred@partition, class=selectset$Pos)$misclassified)/nsel
    return(MER)
  })
  mbest=add_row(mbest,mod=lernmodtot@results[[which.min(mermod)]]@model)
  #memorizzo in ogni passo del ciclo il mod migliore, alla fine di questo
  #troverò quello che è stato più volte il migliore 
  #e lo userò per l'ultima fase, quella di testing
}
mtot=mbest%>%group_by(mod)%>%summarise(n=n())
#il modello migliore (trovato facendo questo ciclo 
#con M elevati) è il pk_L_Bk
tabmer=tibble(mod=apply(matrix(c(1:14),ncol=1),1,FUN=function(x)lernmodtot@results[[x]]@model))
nummod=which(tabmer$mod==mtot$mod[which.max(mtot$n)])


#TEST SET
#porto avanti modello scelto alla fase di selection,
#qua estrarremo il valore finale del MER

#il test set è l'ultimo 20% rimasto dal dataset
testset=set40%>%filter(!Player %in% selectset$Player)
testclst=testset%>%select(all_of(varclust))
testpred=mixmodPredict(testclst,lernmodtot@results[[nummod]])

(miss_class<-classError(testpred@partition, class=testset$Pos)$misclassified)
MERfin=length(miss_class)/length(testset$Player)


#Calcolo i vettori delle medie utilizzando le posizioni del training set, tanto 
#essendo un modello di classificazione essi vengoni calcolati con le medie
#solo delle u.s. del gruppo, non tramite le medie pesate usando gli zij.
#Grazie a questo posso anche inserire variabili che non ho
#utilizzato nella classificazione ma che sono interessanti per 
#il report per il confronto tra vecchie stagioni e recenti
medie=trset%>%
  mutate(across(ends_with("."),function(x) exp(x)/(1+exp(x))))%>%
  #siccome la media delle percentuali trasformate ritrasformata non è uguale
  #alla media delle percentuali originali, le trasformo prima poi calcolo le medie
  group_by(Pos)%>%
  select(X3PA,X2PA,all_of(varclust))%>%
  summarise_all(mean)
dtgioctot%>%filter(Pos=="C")%>%summarise(perc3=mean(exp(X3P.)/(1+exp(X3P.))),attempts3=mean(X3PA))
#una percentuale così alta di tiri da 3 sembrerebbe strana per i centri
#ma guardando i tentativi possiamo notare quanto siano bassi perciò 
#è plausibile una percentuale così alta con così pochi tiri.

#GRAFICO
dtgioctot2var<-dtgioctot%>%select(player_height,DRB.)
ng<-nrow(dtgioctot2var)
dtgioctot.pos<-dtgioctot$Pos

pkLBk<-mixmodLearn(dtgioctot2var, dtgioctot.pos ,
                  models=mixmodGaussianModel(listModels="Gaussian_pk_L_Bk")) #miglior modello da CV
precg <- 250 #precisione
Zg <- dtgioctot2var
#creo la griglia per colorare le aree dei gruppi
x1g<-seq(150,235 , length=precg)  
x2g<-seq(-4 ,0 , length=precg)   
sg<-expand.grid(x1g ,x2g)
sg <- as.data.frame(sg)
Pg<-mixmodPredict(sg,pkLBk@bestResult)@proba   #classifico la griglia
pastelg <- .7

colorsg<-c(rgb(1,pastelg ,pastelg),
          rgb(pastelg ,1,pastelg),
          rgb(pastelg ,pastelg ,1),
          rgb(0.5 ,pastelg ,1),
          rgb(0.9 ,pastelg ,1))[max.col(Pg)]


ggplot()+
  geom_point(alpha=0.5,aes(x=sg$Var1,y=sg$Var2,col=colorsg))+
  xlim(150,235)+
  geom_point(aes(x=dtgioctot2var$player_height,y=dtgioctot2var$DRB.,col=as.factor(dtgioctot.pos)))+
  labs(title="Classification boundaries by role")+
  xlab("Altezza giocatore")+
  ylab("% rimbalzi difensivi")+
  scale_color_manual(values=c("pink","lightgreen","cornflowerblue","yellow","orange","magenta","red","blue","chartreuse4","gold"),
                     name="Roles",labels=c("C.area","SF.area","PG.area","SG.area","PF.area","C","PF","PG","SF","SG"))+
  theme_minimal()

######CLASSIFICAZIONE PER RUOLI (STAGIONI SEPARATE)#####
#Adesso applicheremo la stessa classification ma utilizzando
#prima le stagioni più vecchie e poi le stagioni più recenti
#Durante questa analisi confronteremo:
# -le statistiche medie di ogni ruolo per vedere come sono cambiati 
#durante il corso degli anni
# -la differenza nella bontà della classificazione
#per confrontare quanto le differenze tra ruoli siano mutato.

prime5=c("1997-98","1998-99","1999-00","2000-01","2001-02")

dtgioctotpr5=dat%>%filter(MP>15 & Season %in% prime5)%>%
  group_by(Player)%>%
  summarise(across(where(is.numeric),mean),Pos=as.factor(Pos[1]))

ggcorrplot(cor(dtgioctotpr5%>%select(all_of(varclust))),lab=T)
#anche qua le correlazioni non sono troppo alte
Mpr5=100
mbestpr5=tibble(mod=c(NA))
minimo=0
for( i in  2:Mpr5){
  minimo=0
  while(minimo<0.00001){
    #Inserisco questo ciclo while per evitare problemi legati
    #alle matrici di varianza e covarianza dei gruppi, che a volte non sono invertibili
    #e quindi danno errori nel mixmodLearn e Predict
    #Le matrici di varianza e covarianza possono avere un determinante troppo basso 
    #nonostante le variabili non siano troppo correlate tra loro(come si può vedere dal ggcorrplot),
    #questo perchè nell'estrazione casuale del training set può succedere
    #che i campioni estratti abbiano gruppi dati dalle posizioni
    #in cui le variabili sono troppo correlate. Questo problema
    #infatti non si presenta sempre ma solo quando l'estrazione è particolarmente "sfortunata"
    #Con il ciclo while perciò
    #ci assicuriamo che il campione estratto non abbia problemi
    #di multicollinearità all'interno dei gruppi
    
    #TRAINING SET
    trsetpr5=dtgioctotpr5%>%slice_sample(prop = 0.6) 
    trsetclstpr5=trsetpr5%>%select(all_of(varclust))
    trsetpospr5=trsetpr5%>%select(Pos)
    minimo=min(apply(as.matrix(levels(trsetpr5$Pos)),1,FUN=function(x){
      covmat=cov(trsetpr5%>%filter(Pos==x)%>%select(all_of(varclust)))
      return(det(covmat))
    }))
    
  }
  lernmodtotpr5=mixmodLearn(trsetclstpr5, trsetpospr5$Pos, 
                              models=mixmodGaussianModel(family="all", equal.proportions = F),
                              criterion=c('CV'))
  set40pr5=dtgioctotpr5%>%filter(!Player %in% trsetpr5$Player)
  selectsetpr5=set40pr5%>%slice_sample(prop=0.5)
  selectclstpr5=selectsetpr5%>%select(all_of(varclust))
  nselpr5=nrow(selectclstpr5)
  
  mermodpr5=sapply(lernmodtotpr5@results,FUN=function(x){
    selpredpr5=mixmodPredict(selectclstpr5,x)
    MERpr5=length(classError(selpredpr5@partition, class=selectsetpr5$Pos)$misclassified)/nselpr5
    return(MERpr5)
  })
  mbestpr5=add_row(mbestpr5,mod=lernmodtotpr5@results[[which.min(mermodpr5)]]@model)
}

mtotpr5=mbestpr5%>%group_by(mod)%>%summarise(n=n())
#il modello migliore è pk_Lk_D_Ak_D
tabmerpr5=tibble(mod=apply(matrix(c(1:14),ncol=1),1,FUN=function(x)lernmodtotpr5@results[[x]]@model))
nummodpr5=which(tabmerpr5$mod==mtotpr5$mod[which.max(mtotpr5$n)])

#TEST SET
testsetpr5=set40pr5%>%filter(!Player %in% selectsetpr5$Player)
testclstpr5=testsetpr5%>%select(all_of(varclust))
testpredpr5=mixmodPredict(testclstpr5,lernmodtotpr5@results[[nummodpr5]])

(miss_classpr5<-classError(testpredpr5@partition, class=testsetpr5$Pos)$misclassified)
MERfinpr5=length(miss_classpr5)/length(testsetpr5$Player)
#Buon valore, tendenzialmente più basso di quello della classification
#comprendente tutte le seasons

#medie dei gruppi
mediepr5=trsetpr5%>%
  mutate(across(ends_with("."),function(x) exp(x)/(1+exp(x))))%>%
  group_by(Pos)%>%
  select(X3PA,X2PA,all_of(varclust))%>%
  summarise_all(mean)

dtgioctotpr5%>%filter(Pos=="C")%>%summarise(perc3=mean(exp(X3P.)/(1+exp(X3P.))),attempts3=mean(X3PA))
#anche qua vale lo stesso discorso per le percentuali di tiri da 3 dei centri
#Anzi in queste stagioni erano ancora meno i tiri da 3 

#########ultimi5########
#Analizziamo le ultime 5 stagioni,
#ci aspetteremo che la classificazione sia peggiore in termini
#di MER poichè i ruoli sono sempre meno definiti tra loro, 
#anche i vettori delle medie saranno diversi
ultimi5=c("2018-19","2019-20","2020-21","2021-22","2022-23")
dtgioctotul5=dat%>%filter(MP>15 & Season %in% ultimi5)%>%
  group_by(Player)%>%
  summarise(across(where(is.numeric),mean),Pos=as.factor(Pos[1]))

ggcorrplot(cor(dtgioctotul5%>%select(all_of(varclust))),lab=T)
#anche qua le correlazioni non sono troppo elevate
Mul5=100
mbestul5=tibble(mod=c(NA))
for( i in  2:Mul5){
  #TRAINING SET
  trsetul5=dtgioctotul5%>%slice_sample(prop = 0.6) 
  trsetclstul5=trsetul5%>%select(all_of(varclust))
  trsetposul5=trsetul5%>%select(Pos)  
  lernmodtotul5=mixmodLearn(trsetclstul5, trsetposul5$Pos, 
                            models=mixmodGaussianModel(family="all", equal.proportions = F),
                            criterion=c('CV'))
  set40ul5=dtgioctotul5%>%filter(!Player %in% trsetul5$Player)
  selectsetul5=set40ul5%>%slice_sample(prop=0.5)
  selectclstul5=selectsetul5%>%select(all_of(varclust))
  nselul5=nrow(selectclstul5)
  mermodul5=sapply(lernmodtotul5@results,FUN=function(x){
    selpredul5=mixmodPredict(selectclstul5,x)
    MERul5=length(classError(selpredul5@partition, class=selectsetul5$Pos)$misclassified)/nselul5
    return(MERul5)
  })
  mbestul5=add_row(mbestpr5,mod=lernmodtotpr5@results[[which.min(mermodpr5)]]@model)
}
mtotul5=mbestul5%>%group_by(mod)%>%summarise(n=n())
#come per le prime stagioni, il mod migliore è pk_Lk_D_Ak_D
tabmerul5=tibble(mod=apply(matrix(c(1:14),ncol=1),1,FUN=function(x)lernmodtotul5@results[[x]]@model))
nummodul5=which(tabmerul5$mod==names(which.max(table(mbestul5))))



#TEST SET
testsetul5=set40ul5%>%filter(!Player %in% selectsetul5$Player)
testclstul5=testsetul5%>%select(all_of(varclust))
testpredul5=mixmodPredict(testclstul5,lernmodtotul5@results[[nummodul5]])

(miss_classul5<-classError(testpredul5@partition, class=testsetul5$Pos)$misclassified)
MERfinul5=length(miss_classul5)/length(testsetul5$Player)
#Maggiore di quello di pr5 di circa il 10% (dipende dal seed)
#questo ci indica che nelle ultime stagioni
#è più difficile distinguere tra di loro i ruoli,
#mentre alla fine degli anni 90 inizio 2000 i ruoli erano 
#decisamente più definiti e differenti tra loro

medieul5=trsetul5%>%
  mutate(across(ends_with("."),function(x) exp(x)/(1+exp(x))))%>%
  group_by(Pos)%>%
  select(X3PA,X2PA,all_of(varclust))%>%
  summarise_all(mean)
dtgioctotul5%>%filter(Pos=="C")%>%summarise(perc3=mean(exp(X3P.)/(1+exp(X3P.))),attempts3=mean(X3PA))
#notiamo che i tiri da tre tentati sono 
#decisamente più alti di quelli nelle prime 5 stagioni

####Classification with MDA####

#Proviamo ad utilizzare il metodo MDA per vedere se 
#esso possa migliorare in maniera significativa i risultati in termini di MER, facciamo
#quindi una veloce analisi solo con un training set ed un test set
#per tutte le seasons
#Utilizziamo le stesse variabili della classification tramite EDDA
#TRAINING SET 
dtgioctotMDA<-dtgioctot%>%select(Player,all_of(varclust),Pos)
mm=c()
for(i in 1:10){
  #utilizziamo il ciclo for per ottenere molteplici valori
  #del MER, così da capire subito se questo
  #tipo di classificazione può portare sostanziali miglioramenti
  trsetMDA=dtgioctotMDA%>%slice_sample(prop=0.8)
  trsvarMDA=trsetMDA%>%select(-Pos,-Player)
  #MODEL BASED ON TRAINING SET
  modMDA <- MclustDA(trsvarMDA, trsetMDA$Pos)
  summary(modMDA)
  testsetMDA=anti_join(dtgioctotMDA,trsetMDA)
  testvarMDA=testsetMDA%>%select(-Pos,-Player)
  MERMDA=sum(predict(modMDA,testvarMDA)$classification!=testsetMDA$Pos)/nrow(testsetMDA)
  mm=rbind(mm,c(MERMDA))
}
(mean_mm<-mean(mm))
#Il risultato dimostra come in realtà il MER non si discosta tanto dalla classification fatta 
#precedentemente, perciò non approfondiamo ulteriormente l'analisi 
#restringendoci ad un insieme di stagioni come per il metodo EDDA



######Rockets D'Antoni####
#creiamo il dataset e il grafico che utilizzeremo nel report
#per illustrare l'anomalia di questa squadra per quanto riguarda
# tiri da 3 tentati
datihou=dat%>%filter(Season=="2018-19" & Tm!="TOT")%>%group_by(Tm)%>%
  mutate(X3Ptot=X3P*G,X3PAtot=X3PA*G)%>%
  summarise(across(where(is.numeric),sum))%>%
  select(Tm,X3Ptot,X3PAtot)

ggplot(datihou)+
  geom_col(aes(x=Tm, y=X3Ptot),col="darkblue",fill="blue",alpha=0.95)+
  geom_col(aes(x=Tm, y=X3PAtot),col=ifelse(datihou$Tm=="HOU","red","darkviolet"),fill="violet",alpha=0.3,linewidth=ifelse(datihou$Tm=="HOU",0.8,0.5))+
  theme(axis.text.x = element_text(angle=80,  hjust=1,colour = ifelse(datihou$Tm=="HOU","red","black"))) +
  labs(y="Tiri da 3 tentati e realizzati", x="Team",title = "I Rockets di D'Antoni")
  
#####3 role clust (ball-handler, center, off-ball guys)####
#Mettiamo a test la teoria di JJ Redick tramite
#cluster analysis

#Selezioniamo le stagioni più recenti 
stag3r=c( "2019-20","2020-21","2021-22","2022-23")

dt3r=dat%>%filter(MP>15)%>%
  filter(Season %in% stag3r)%>%
  group_by(Player)%>%
  summarise(across(where(is.numeric),mean),Pos=moda(Pos))

#SELEZIONE VARIABILI
#Siamo in ambito di clustering quindi seleziono più variabili rispetto a prima
#perchè non avendo le etichette servono più informazioni per formare
#un buon raggruppamento  

ggcorrplot(cor(dt3r%>%select(where(is.numeric))),lab = T,lab_size =2)
#lo usiamo per eviatre di selezionare coppie di variabili troppo correlate tra loro
ggpairs(dt3r[6:15])
#X3P la selezioniamo perchè sembra avere una distribuzione proveniente da una mistura
#e perchè sappiamo che i centri tentano molte meno triple degli altri due ruoli
ggpairs(dt3r[16:25])
#FT. è chiaramente multimodale perciò lo selezioniamo,
#FTA nei grafici bivariati distingue alcuni giocatori
#con molti tiri liberi tentati, solitamente gli off-ball guys che subiscono 
#spesso falli mentre stanno tirando (questi portano a tiri liberi, a differenza
#degli altri falli in situazioni offensive "classiche")
#Per lo stesso motivo selezioniamo AST (qua quelli che si diastinguono 
#sono i ball-handlers), DRB e BLK (qua i centri per entrambi)
ggpairs(dt3r[26:35])
#height e weight sono multimodali, non selezioniamo PTS perchè
#dipendono non dal ruolo, ma dall'abilità e dal minutaggio del giocatore.
#ORB. è multimodale come anche AST., USG. nell'NBA moderna
#è un dato che sappiamo essere solitamente più elevato
#per i ball-handlers,gli off-ball guys e i centri
#sono solitamente meno coinvolti e perciò hanno una USG. inferiore .
ggcorrplot(cor(dt3r%>%select(USG.,AST.,AST,ORB.,DRB.,DRB,FTA,FT.,X3P,BLK,
                         player_height,player_weight)),lab=T,lab_size=2.5)
#non notiamo correlazioni troppo elevate

varclst3role=dt3r%>%
  select(USG.,AST.,AST,ORB.,DRB,DRB.,FTA,FT.,X3P.,BLK,
         player_height,player_weight)


#CLUSTERING
clst3role=Mclust(varclst3role,G=(2:6))
plot(clst3role,what="BIC",ylim=c(-15500,-14800))
#il VVE 4 migliore per il BIC, ma VVV 3 è molto vicino
clst3role$BIC
#il BIC ci suggerisve un VVV,3 ma andiamo 
#a esaminare l'ICL che è il criterio che preferiamo 
(clst3roleicl=mclustICL(varclst3role,G=(2:6)) )
#ICL sceglie un VVV 3 
clst3rolbest=Mclust(varclst3role,G=3,modelNames = "VVV")
z3r=clst3rolbest$z
medie3r1=as_tibble(t(clst3rolbest$parameters$mean))%>%select(-ends_with("."))
#tratto a parte le variabili percentuali in quanto
#le medie devono essere calcolate sui valori orignali
#e pesate tramite gli zij come nell'M-step
medie3r2=apply(varclst3role%>%select(ends_with(".")),2,function(x){
    perc=exp(x)/(1+exp(x))
    m11=sum(perc*z3r[,1])/sum(z3r[,1])
    m22=sum(perc*z3r[,2])/sum(z3r[,2])
    m33=sum(perc*z3r[,3])/sum(z3r[,3])
    return(c(m11,m22,m33))
  })
medie3r=cbind(medie3r1,medie3r2)

#commento sulle medie nel report

#Entropia 
En=-sum(rowSums(z3r*log(z3r))) 
En_max=clst3rolbest$n*log(clst3rolbest$G)
En/En_max #7.9% dell'en max,
#considerando che i ruoli rilevati non sono ben definiti,
#è un buon valore

#GRAFICI
ggplot()+
  geom_point(aes(x=varclst3role$AST,y=varclst3role$player_height,
                 col=as.factor(clst3rolbest$classification),size=clst3rolbest$uncertainty),show.legend=c(col=T,size=F))+
  scale_color_manual(values=c("red","blue","green"),name="Role",labels=c("Off-Ball","Center","Ball-handler"))+
  labs(x="Assist",y="Player Height",title="Clustering 3 roles")+
  theme_minimal()
#La grandezza dei punti nel grafico è direttamente proporzionale all'incertezza 

#funzione che assegna ad un punto il gruppo in base
#al naive Bayes classifier
postprob3r=function(x) {
  d=matrix(x,ncol=2)
  f1=clst3rolbest$parameters$pro[1]*dmvnorm(d,clst3rolbest$parameters$mean[c(3,11),1],clst3rolbest$parameters$variance$sigma[c(3,11),c(3,11),1])
  f2=clst3rolbest$parameters$pro[2]*dmvnorm(d,clst3rolbest$parameters$mean[c(3,11),2],clst3rolbest$parameters$variance$sigma[c(3,11),c(3,11),2])
  f3=clst3rolbest$parameters$pro[3]*dmvnorm(d,clst3rolbest$parameters$mean[c(3,11),3],clst3rolbest$parameters$variance$sigma[c(3,11),c(3,11),3])
  return(which.max(c(f1,f2,f3)))}
postprob3r(c(2,200))

v3role2var=varclst3role%>%select(AST,player_height)
prec3r <- 250 #precisione
x13r<-seq(0 , 11, length=prec3r)  
x23r<-seq(170 , 225, length=prec3r)   
s3r<-expand.grid(x13r ,x23r)
s3r <- as.data.frame(s3r)
pastel <- .7
colclst3r=apply(s3r,1,postprob3r)

ggplot()+
  geom_point(alpha=0.5,aes(x=s3r$Var1,y=s3r$Var2,col=c(rgb(1,pastel ,pastel),
                                                   rgb(pastel ,1,pastel),
                                                   rgb(pastel ,pastel ,1))[colclst3r]))+
  geom_point(aes(x=v3role2var$AST,y=v3role2var$player_height,col=as.factor(clst3rolbest$classification)))+
  labs(title="Clusters' 3 roles")+
  xlab("Assist")+
  ylab("Altezza giocatore")+
  scale_color_manual(values=c("lightgreen","cornflowerblue","orange","red","blue","chartreuse4"),
                          name="3 Roles",labels=c("Ball-handler.area","Center.area","Off-Ball.area","Off-Ball","Center","Ball-handler"))+
  theme_minimal()
#I colori nel grafico potrebbero non essere coerenti. Il grafico corretto è nel report

######CLUSTERING STAGIONI#####
seasons=dat%>%group_by(Season)%>% filter(MP>15)%>%
  summarise(across(where(is.numeric),mean))%>%
  select(-c("G","GS","MP",
            "Net_rating","ORB.","DRB.","USG."))
pairs(seasons[,3:15],col=c(rep(1,8),rep(2,9),rep(3,9)))
#coloro le stagioni in tre gruppi di numerosità simili,
#ma da variabili come X3PA e X3P sembrano esssere due i gruppi.
#correlazione molto alta tra X3PA e X3P, metterò uno dei due perchè 
#sono entrambi multimodali, stesso discorso per X2PA e X2P.
varseason<-c("X3PA","X2PA")
seasvarclst=seasons%>%select(all_of(varseason))
(sclstICL=mclustICL(seasvarclst)) 
#secondo criterio ICL il mod migliore è un EEV con 2 gruppi
seasclst=Mclust(seasvarclst,G=2,modelNames = "EEV")
ggplot(seasvarclst,
       aes(x=X2PA,y=X3PA,col=as.factor(seasclst$classification),label=seasons$Season))+
  geom_text(show.legend = F)+
  labs(x="Tiri da 2 tentati",y="Tiri da 3 tentati",title="Clustering stagioni")+
  theme_minimal()

KL_S<-function(mu1,mu2,sigma1,sigma2) {
  d12<-t(mu1-mu2)%*%(solve(sigma1)+solve(sigma2))%*%(mu1-mu2)/2+sum(diag(sigma1%*%solve(sigma2)+solve(sigma1)%*%sigma2))/2-length(mu1)
  return(d12)
}
#distanza di Kullback-Leibler
KL_S(seasclst$parameters$mean[,1],seasclst$parameters$mean[,2],
     seasclst$parameters$variance$sigma[, ,1],seasclst$parameters$variance$sigma[, ,2]) 

seasclst$parameters$mean
zetaseason=seasclst$z
En=-sum(rowSums(zetaseason*log(zetaseason))) 
En_max=seasclst$n*log(seasclst$G)
En/En_max
#3.7% dell'entropia massima, buon valore nonostante abbiamo
#usato solo due variabili.

#Fine script
#Ringraziamo per l'attenzione

