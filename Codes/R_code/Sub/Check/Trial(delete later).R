##
rm(Pr1) ## saigonijikkou

Pr1 <- prob(datalie2$alpha)
for(i in 1:nrow(datalie2)){
  if(datalie2[i, 24] == 1){ # AielineNo
    Demand[i] <- datalie2[i, 14]*Pr[i]                                            ### ���������������i�܂��j
    Frequencyrenewal[i] <- (Demand[i]/(datalie2[i, 20]*datalie2[i, 23]/100))          ### ���������������i�܂��j
  } else {
    Frequencyrenewal[i] <- Frequency[i]
  }
}
Frequency <- as.numeric(Frequency)
Frequencyrenewal <- as.numeric(Frequencyrenewal)
distance <- sqrt(sum(Frequency - Frequencyrenewal)^2)
Frequency <- Frequencyrenewal