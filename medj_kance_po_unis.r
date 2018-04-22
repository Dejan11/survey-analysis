##koje su to kancelarije

bg = as.data.frame(table(as.character(upitnik_filtered$Koje.su.to.organizacije.na.Vašem.univerzitetu.fakultetu.koje.se.bave.mobilnošcu.[upitnik_filtered$maticni_uni=="Univerzitet u Beogradu"])))
kg = as.data.frame(table(as.character(upitnik_filtered$Koje.su.to.organizacije.na.Vašem.univerzitetu.fakultetu.koje.se.bave.mobilnošcu.[upitnik_filtered$maticni_uni=="Univerzitet u Kragujevcu"])))
ns = as.data.frame(table(as.character(upitnik_filtered$Koje.su.to.organizacije.na.Vašem.univerzitetu.fakultetu.koje.se.bave.mobilnošcu.[upitnik_filtered$maticni_uni=="Univerzitet u Novom Sadu"])))

nis = upitnik_filtered[upitnik_filtered$maticni_uni!="Univerzitet u Beogradu",]
nis = nis[nis$maticni_uni!="Univerzitet u Novom Sadu",]
nis = nis[nis$maticni_uni!="Univerzitet u Kragujevcu",]
nis = nis[nis$maticni_uni!="Univerzitet u Pristini ",]
nis = nis[nis$maticni_uni!="Megatrend",]
nis = nis[nis$maticni_uni!="Univerzitet Educons ",]
nis = nis[nis$maticni_uni!="Univerzitet u Novom Pazaru",]
nis = nis[nis$maticni_uni!="Univerzitet Umetnosti u Beogradu",]
nis = nis[nis$maticni_uni!="Inostranstvo",]
nis = nis[nis$maticni_uni!="",]

nis$maticni_uni

nis_table = as.data.frame(table(unique(as.character(nis$Koje.su.to.organizacije.na.Vašem.univerzitetu.fakultetu.koje.se.bave.mobilnošcu.))))
nis_table
write.csv(bg, "bg_kancelarije.csv")
write.csv(ns,"ns_kancelarije.csv")
write.csv(kg, "kg_kancelarije.csv")
write.csv(nis_table, "nis_kancelarije.csv")
#nis
