library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)

#Kuidas benfordi seadus kehtib
df1 = data.frame(x=factor(c(1:9)),
                 y=c(32.62, 16.66, 11.8, 9.26, 7.63, 6.55, 5.76, 5.14, 4.56))
p1 = ggplot(df1, aes(x=x, weight=y)) + geom_bar() + ggtitle("Twitteri kasutajaid jälgijate arvu lõikes \n") +
  scale_x_discrete("Esimene number") + scale_y_continuous("") + theme(text = element_text(size=11))

df1 = data.frame(x=factor(c(1:9)),
                 y=c(31.57, 18.12, 11.88, 9.35, 7.84, 6.09, 5.78, 4.83, 4.53))
p2 = ggplot(df1, aes(x=x, weight=y)) + geom_bar() + ggtitle("Riikide SKP suurus \n") + 
  scale_x_discrete("Esimene number") + scale_y_continuous("") + theme(text = element_text(size=11))

grid.arrange(p1, p2, nrow=1)

#Kontrollisin maksude andmeid, kas on andmeid võltsitud või mitte 2014 andmestik
#kas Benfordi seaduse põhjal võib väita, et FIE-d on võltsinud maksunäitajaid
#kas Benfordi seaduse põhjal võib väita, et 2017. aasta kohaliku omavalitsuse volikogu valimistel toimus pettus.

maksud <- read.csv2("maksude_andmestik.csv", fileEncoding = "latin1")

joonis1 <- maksud %>%
  filter(!is.na(maakond)& maakond!= "") %>% 
  count(maakond) %>%
  ggplot(aes(x=reorder(maakond, n), y = n)) + geom_col(fill = "lightblue") + coord_flip() + theme_minimal() + labs(title = "Ettevõtete jaotus maakondade lõikes", x = "Maakond", y = "Ettevõtete arv")

joonis1 #näitab palju on erinevates maakondades juriidilisi isikuid.

maksud <- maksud %>%
  mutate(toojoumaksud_ja_maksed = as.numeric(toojoumaksud_ja_maksed),riiklikud_maksud = as.numeric(riiklikud_maksud))

joonis2 <- maksud %>% 
  filter(toojoumaksud_ja_maksed > 0) %>% 
  ggplot(aes(x=kaibemaksukohuslane, y=toojoumaksud_ja_maksed)) + geom_boxplot() + scale_y_log10() + theme_minimal() + labs(title ="Tööjõumaksud olenevalt käibemaksukohustusest ", x= "Käibemaksukohuslane", y= "Tööjõumaksud ja -maksed")

joonis2 #tundub, et graafikult vaadates käibemaksukohuslased maksavad keskmiselt rohkem tööjõumakse kui mitte käibemaksukohuslased.

maksude_osakaal <- maksud %>%
  filter(!is.na(liik) & liik != "") %>% 
  group_by(liik) %>%
  summarise(kogusumma = sum(riiklikud_maksud, na.rm = TRUE) + sum(toojoumaksud_ja_maksed, na.rm = TRUE)) %>%
  mutate(protsent = kogusumma / sum(kogusumma) * 100) %>%
  arrange(desc(protsent))

joonis3 <- ggplot(maksude_osakaal, aes(x = reorder(liik, protsent), y = protsent)) +
  geom_col(fill = "lightblue") +
  coord_flip() + geom_text(aes(label = sprintf("%.1f%%", protsent)), hjust = 0.3, size = 3) +
  theme_minimal() +
  labs(title = "Maksude laekumise osakaal juriidilise isiku liigi järgi", x = "Juriidilise isiku liik", y = "Osakaal kogulaekumisest (%)")

joonis3 #näitab palju makse laekus erinevatelt juriidilistelt isikutelt

#Kontrollisin visuaalselt Benfordi seaduse kehtimist tunnustel *riiklikud_maksud* ja *toojoumaksud_ja_maksed*. 
#Nulliga võrduvad väärtused jätsin kõrvale. Tegin vastava joonis ka FIE-de, äriühingute jne lõikes (vt tunnus *liik*).

benford_andmed <- maksud %>%
  select(liik, riiklikud_maksud, toojoumaksud_ja_maksed) %>%
  gather(key = "maksu_tuup", value = "summa", riiklikud_maksud, toojoumaksud_ja_maksed) %>%
  filter(summa > 0 & !is.na(summa)) %>%
  mutate(esimene_nr = factor(substr(as.character(summa), 1, 1), levels = 1:9)) %>% 
  filter(!is.na(esimene_nr))

benfordj1 <- ggplot(benford_andmed, aes(x = esimene_nr)) + geom_bar(fill = "lightblue", color = "black") + facet_wrap(~ maksu_tuup) + theme_minimal() + labs(title = "Benfordi seaduse kontroll", x = "Esimene number", y = "Sagedus")

benfordj1


suuremad <- c("Äriühing", "FIE", "Mittetulundusühing", "Valitsus- ja riigiasutus")

benford_s <- benford_andmed %>%
  filter(liik %in% suuremad)

benfordj2 <- ggplot(benford_s, aes(x = esimene_nr)) + geom_bar(fill = "lightblue", color = "black") + facet_wrap(maksu_tuup ~ liik, scales = "free_y", ncol = 4) + theme_minimal() + labs(title = "Benfordi seadus maksutüübi ja isiku liigi lõikes", x = "Esimene number", y = "Sagedus")

benfordj2 


#Valimised 2017

valimised2017 <- read.csv2("KOV_valimised_2017.csv")

suured_erakonnad <- c("Eesti Keskerakond", "Eesti Reformierakond", 
                      "Sotsiaaldemokraatlik Erakond", 
                      "Erakond Isamaa ja Res Publica Liit", 
                      "Eesti Konservatiivne Rahvaerakond")

valimised2017 <- valimised2017 %>%
  mutate(
    koguhaali = paberhaali + ehaali,
    erakond_grupp = ifelse(nimekiri %in% suured_erakonnad, nimekiri, "Muu")
  )

vjoonis1 <- ggplot(valimised2017, aes(x = reorder(erakond_grupp, table(erakond_grupp)[erakond_grupp]))) + geom_bar(fill = "darkgreen") + coord_flip() + theme_minimal() + labs(title = "Kandidaatide arv erakondade lõikes", x = "Erakond / Grupp", y = "Kandidaatide arv")

vjoonis1 # palju on erinevaid kanditaate suurtes erakondades ja palju valimisliitudes

vjoonis2 <- ggplot(valimised2017, aes(x = paberhaali, y = ehaali)) + geom_point(alpha = 0.3, color = "darkgreen") + stat_smooth(method = "lm", color = "black") + theme_minimal() + labs(title = "Paberhäälte ja e-häälte seos kandidaatidel", x = "Paberhääled", y = "E-hääled") + coord_cartesian(ylim = c(0, 500), xlim = c(0, 500)) # tegin väiksema vahemiku, et seost oleks näha, pigem saadakse rohkem paberhääli.

vjoonis2 #näitab kui palju iga kandidaat sai paberhääli ja ehääli.

haaled_erakonniti <- valimised2017 %>%
  group_by(erakond_grupp) %>%
  summarise(Paberhääled = sum(paberhaali, na.rm = TRUE),E_hääled = sum(ehaali, na.rm = TRUE)) %>%
  gather(key = "haale_tuup", value = "haalte_arv", Paberhääled, E_hääled)

vjoonis3 <- ggplot(haaled_erakonniti, aes(x = reorder(erakond_grupp, haalte_arv), y = haalte_arv, fill = haale_tuup)) + geom_bar(stat = "identity", position = "dodge", color = "black", alpha = 0.8) +coord_flip() + scale_fill_manual(values = c("Paberhääled" = "darkgreen", "E_hääled" = "lightblue")) + theme_minimal() + labs(title = "Paberhäälte ja e-häälte võrdlus erakondade lõikes",x = "Erakond", y = "Kogutud häälte arv", fill = "Hääle tüüp")

vjoonis3 #näitab erakonna järgi, palju saadi paberhääli ja palju ehääli

benford_valimised2017 <- valimised2017 %>%
  select(erakond_grupp, paberhaali, ehaali, koguhaali) %>%
  gather(key = "haale_tuup", value = "haali", paberhaali, ehaali, koguhaali) %>%
  filter(haali > 0 & !is.na(haali)) %>%
  mutate(esimene_nr = factor(substr(as.character(haali), 1, 1), levels = 1:9)) %>%
  filter(!is.na(esimene_nr))

joonis_benford_uldine <- ggplot(benford_valimised2017, aes(x = esimene_nr)) + geom_bar(fill = "darkgreen", color = "black") + facet_wrap(~ haale_tuup, scales = "free_y") + theme_minimal() + labs(title = "Benfordi seadus: e-hääled, paberhääled ja koguhääled", x = "Esimene number", y = "Sagedus")

joonis_benford_uldine

joonis_benford_erakonnad <- ggplot(benford_valimised2017, aes(x = esimene_nr)) + geom_bar(fill = "darkgreen", color = "black") + facet_grid(erakond_grupp ~ haale_tuup, scales = "free_y") + theme_minimal() + theme(strip.text.y = element_text(angle = 0)) + # pöörasin erakondade nimesid, et oleks loetav 
  labs(title = "Benfordi seadus erakondade lõikes", x = "Esimene number", y = "Sagedus")

joonis_benford_erakonnad

erakonnad_joonis <- valimised2017 %>%
  mutate(
    #teen pikkadest nimedest lühikesed
    erakond_luhike = case_when(
      nimekiri == "Eesti Keskerakond" ~ "KESK",
      nimekiri == "Eesti Reformierakond" ~ "REF",
      nimekiri == "Sotsiaaldemokraatlik Erakond" ~ "SDE",
      nimekiri == "Erakond Isamaa ja Res Publica Liit" ~ "IRL",
      nimekiri == "Eesti Konservatiivne Rahvaerakond" ~ "EKRE",
      TRUE ~ "Muu"), erakond_luhike = factor(erakond_luhike, levels = c("KESK", "REF", "SDE", "IRL", "EKRE", "Muu"))) %>%
  filter(koguhaali > 0 & ehaali > 0)


#värvid
erakonna_varvid <- c("KESK" = "#00983A", "REF"  = "#FFDE00", "SDE"  = "#E30613", "IRL"  = "#009FE3", "EKRE" = "#8B4513", "Muu"  = "#82368C")


#joonis
ggplot(erakonnad_joonis, aes(x = koguhaali, y = ehaali, color = erakond_luhike)) + geom_point(alpha = 0.5, size = 1.2) + scale_x_log10() + scale_y_log10() + scale_color_manual(values = erakonna_varvid) + facet_wrap(~ erakond_luhike, ncol = 3) + labs(x = "Hääli kokku",y = "E-hääli") + theme(legend.position = "none")
