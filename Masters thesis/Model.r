iter<-1 #Счётчик
info<-c() #Множество пар (номер итерации; количество информированных)
nstarts<-c("UCy1hRH78K-J3Jikk9QtBacw") #Стартовая вершина
activated<-length(nstarts) #Количество информированных в настоящий момент
isend<-FALSE #Конец=False
act<-c() #Множество информированных
act<-c(act,nstarts)
lastactivations<-c() #Множество информированных в момент времени
lastactivations<-c(lastactivations,act)
info[[iter]]<-activated #На начальном этапе фиксируем, что на 1ой итерации у нас уже есть n-ое число информированных 

while(!isend){ #Пока не конец
  nextla<-c() #Множество активированных соседей node
  for(node in lastactivations){ #Для каждой вершины node из множества:
    neigh<-neighbors(YTchannelsOK, node, mode = "out") #Вычисляем исходящих соседов node из сети YTchannelsOK
    for (dirnode in names(neigh)) { #Для каждого исходящего соседа dirnode:
      iter <- iter + 1 #Увеличиваем значение счётчика
      info[[iter]]<-activated #Фиксируем количество информированных в этот момент времени
      if(dirnode %in% act == FALSE){ #Если исходящий сосед dirnode не принадлежит множеству информированных, то:
        pth<-c(node,dirnode) #Формируем направленную дугу pth: node->dirnode
        probb<-E(YTchannelsOK,path = pth)$weight #Вытаскиваем значение вероятности для дуги pth
        if(runif(1,0,1)<=probb){ #Если сгенерированная вероятность <= вероятности дуги pth, то:
          nextla<-c(nextla,dirnode) #Добавляем dirnode во множество информированных
          activated<-length(union(act,union(lastactivations,nextla))) #Фиксируем количество информированных
        }
      }
    }
  }
  lastactivations<-c() #Обнуляем множество информированных в момент времени
  lastactivations<-c(lastactivations,nextla) #Добавляем во множество информированных в момент времени всех информированных соседей node
  act<-union(act,lastactivations) #Формируем множество информированных
  if(length(lastactivations)==0){#Если никакие вершины не были проинформированны, то:
    info[[iter]]<-activated #Фиксируем количество информированных в конечный момент времени
    isend<-TRUE #Конец=True
  }
}
