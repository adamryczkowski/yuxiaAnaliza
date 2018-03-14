
annotate_db<-function(dt, flag_only_fix_attributes=FALSE, kodowanieECpath='shared/kodowanieEC.xlsx', zamiany_zmiennych_path='shared/Zamiany_zmiennych.ods')
{
  if (!flag_only_fix_attributes) {
    SlownikNazwRegionow<-  {
      PowiatNames=c('Powiat Taichung','Powiat Taoyuan','Obca Francja','Powiat Taipei','Powiat Jiayi',
                    'Miasto Taipei','Powiat Yunlin','Powiat Pingtung (Pingdong)','Powiat Chsinchu (Xinzhu)',
                    'Obca Chiny','Miasto Jiayi','Powiat Miaoli','Powiat Kaohsiung (Gaoxiong)','Miasto Tajchung (Taizhong)',
                    'Miasto Keelung (Jilong)','Powiat Changhua','Powiat Tainan','Miasto Tainan','Miasto Kaohsiung (Gaoxiong)',
                    'Powiat Kinmen (Jinmen)','Powiat Yilan','Obca Inne','Powiat Nantou','Powiat Penghu','Obca Wietnam',
                    'Miasto Hsinchu (Xinzhu)','Powiat Hualien (Hualian)','Powiat Taitung (Taidong)','Powiat Lianjiang','Obca Indonezja',
                    'Obca Malezja','Obca Japonia','Obca Singapur','Obca Filipiny','Obca Tajlandia','Obca Usa','Obca Makao','Obca Kanada',
                    'Obca Hongkong','Nieznana')
      PowiatSizes<-factor(plyr::laply(stringi::stri_split(PowiatNames, fixed=' '), function(x)x[[1]]))
      a<-unlist(stringi::stri_split(unique(unique(paste0(dt$m_preg_place2, ' = ', dt$m_preg_place)),unique(paste0(dt$m_reg_place2, ' = ', dt$m_reg_place))), fixed=' = '))
      dim(a)<-c(2,length(a)/2)
      odp<-data.table(
        kod=a[1,],
        oryg_nazwa=a[2,],
        label=suppressWarnings(PowiatNames[as.integer(a[1,])]),
        rozmiar=suppressWarnings(PowiatSizes[as.integer(a[1,])])
      )
      odp[!is.na(suppressWarnings(as.numeric(a[1,]))),]
    }

    encode_region<-function(var, SlownikNazwRegionowL)
    {
      pos_var<-match(var, SlownikNazwRegionow$oryg_nazwa)
      return(factor(pos_var, levels = seq(nrow(SlownikNazwRegionow)),  labels = SlownikNazwRegionow$label ))
    }




    #At the beginning we insert some compuations, that can be done in parallel,
    #so they have time to finish
    #

    labels<- c('Obecność niedokrwistości u matki',
               'Obecność chorób serca u matki',
               'Obecność chorób płuc u matki',
               'Obecność cukrzycy zdiagnozowanej przed ciążą (PGDM) u matki',
               'Obecność kiły u matki',
               'Obecność cukrzycy ciężarnych (GDM) u matki',
               'Obecność małowodzia lub wielowodzia u matki',
               'Obecność nieprawidłowości w budowie hemoglobiny u matki',
               'Obecność przewlekłego nadciśnienia tętniczego u matki',
               'Obecność nadciśnienia tętniczego indukowanego ciążą (PIH) u matki',
               'Obecność zatrucia ciążowego u matki',
               'Obecność niezamkniętej szyjki macicy u matki',
               'Historia porodu potomstwa o masie 4000 gramów lub wyższej u matki',
               'Historia porodu przedwczesnego lub noworodka z niską masą urodzeniową u matki',
               'Obecność choroby nerek u matki',
               'Obecność konfliktu serologicznego między matką a płodem',
               'Obecność innych czynników ryzyka opisanych w dokumentacji',
               'Obciążenie matki talasemią (zdiagnozowana choroba, nie nosicielstwo)',
               'Palenie przez matkę tytoniu w czasie ciąży',
               'Spożywanie przez matkę alkoholu w czasie ciąży',
               'Istnienie stwierdzonego u matki uzależnienia farmakologicznego')


    p_risk_f_<-launch_conv_to_wide(dt = dt,
                                   varnames = paste0('p_risk_f_',1:8),
                                   valid_levels = 1:21,
                                   valid_varnames = paste0('m_risk_f_',1:21),
                                   valid_labels = labels,
                                   labels = c('Nie ma','Jest'),
                                   flag_remove_original_fields = TRUE)

    m_proc_<-launch_conv_to_wide(dt = dt,
                                 varnames = paste0('p_proc_',1:5),
                                 valid_levels = 1:10,
                                 valid_varnames = paste0('m_proc_',1:10),
                                 valid_labels = paste0('Czy u matki zastosowano ',
                                                       c('USG',
                                                         'amniopunkcję',
                                                         'badanie kosmówki',
                                                         'płodowe EKG',
                                                         'poród indukowany',
                                                         'poród przyśpieszony',
                                                         'procedury mające na celu utrzymanie ciąży',
                                                         'szew okrężny szyjki macicy',
                                                         'laparotomię',
                                                         'inną procedurę')),
                                 labels = c('Nie zastosowano','Zastosowano'),
                                 flag_remove_original_fields = TRUE)

    b_cmpl_<-launch_conv_to_wide(dt = dt,
                                 varnames = paste0('b_compl_',1:8),
                                 valid_levels = 1:16,
                                 valid_varnames = paste0('b_cmpl_',1:16),
                                 valid_labels = paste0('Powikłanie porodu ',
                                                       c('gorączką',
                                                         'smółką w wodach płodowych',
                                                         'przedwczesnym pęknięciem błon płodowych (PROM)',
                                                         'wczesnem oddzieleniem łożyska',
                                                         'łożyskiem przodującym',
                                                         'innym dużym krwotokiem',
                                                         'skurczami mięśni szkieletowych podczas porodu',
                                                         'szybkim porodem',
                                                         'przedłużająm się porodem',
                                                         'nieprawidłowo rozwijającą się akcją porodową',
                                                         'nienormalną pozycja barków płodu',
                                                         'dysproporcją matczyno-płodową (CPD)',
                                                         'wypadnięciem pępowiny',
                                                         'skutkami ubocznymi znieczulenia',
                                                         'zagrażającą zamartwicą wewnątrzmaciczną płodu',
                                                         'innymi zaburzenia porodu')),
                                 # valid_labels = paste0('Czy u matki ',
                                 #                       c('wystąpiła gorączka podczas porodu',
                                 #                         'wystąpiła smółka w wodach płodowych',
                                 #                         'wystąpiło wczesne odejście wód',
                                 #                         'wystapiło wczesne oddzielenie łożyska',
                                 #                         'wystąpiło łożysko przodujące',
                                 #                         'wystąpił inny duży krwotok',
                                 #                         'wystąpiły skurcze mięśni szkieletowych podczas porodu',
                                 #                         'wystąpił szybki poród',
                                 #                         'wystąpił przedłużający się poród',
                                 #                         'wystąpiła nieprawidłowo rozwijająca się akcja porodowa',
                                 #                         'wystąpiła nienormalna pozycja barków płodu',
                                 #                         'wystąpiła dysproporcja matczyno-płodowa',
                                 #                         'wystąpiło wypadnięcie pępowiny',
                                 #                         'wystąpiły skutki uboczne znieczulenia',
                                 #                         'wystąpiło niedotlenienie płodu-stan zagrożenia płodu',
                                 #                         'wystąpiły inne zaburzenia porodu')),
                                 labels = c('Nie był','Był'),
                                 #                                 labels = c('Nie wystąpiła','Wystąpiła'),
                                 flag_remove_original_fields = TRUE)



    dt[,id:=seq(nrow(dt))]
  }


  if (!flag_only_fix_attributes) {
    dt[,death:=factor(dt$death, labels=c('Żywo urodzony', 'Martwo urodzony'))]
  }
  setattr(dt$death, 'label', 'Martwe urodzenie')


  if (!flag_only_fix_attributes) {
    dt[,birth:=
         as.Date(ifelse(is.na(dt$birth),
                        NA,  #Niepotrzebny warunek, bo nigdy się nie zdarza w naszej bd
                        paste0(as.numeric(substring(dt$birth, 1,3))+1911,'.',substring(dt$birth,4,6),'.15')),
                 format='%Y.%m.%d')]
    dt[,birth_year_gr:=as.ordered(year(dt$birth))]

    dt[,birth_month_gr:=factor(month(dt$birth), levels=1:12, labels=as.roman(1:12))]

    dt[,birth_year:=as.numeric(difftime(birth,as.Date('0-1-1')))/365.24]
  }
  setattr(dt$birth, 'label', 'Rok i miesiąc urodzenia dziecka')
  setattr(dt$birth_year, 'label', 'Rok i miesiąc urodzenia dziecka')
  setattr(dt$birth_year, 'units', 'lata')
  setattr(dt$birth_month_gr, 'label', 'Miesiąc urodzenia dziecka')
  setattr(dt$birth_year_gr, 'f.o.b', 2) # zmienna porządkowa
  setattr(dt$birth_month_gr, 'f.o.b', 1) # zmienna porządkowa

  setattr(dt$birth_year_gr, 'label', 'Rok urodzenia dziecka')


  if (!flag_only_fix_attributes) {
    f <- dt$birth < '2002-01-01'
    dt[f, xlsid:=seq(sum(f))]
    f <- dt$birth >= '2002-01-01' & dt$birth < '2003-01-01'
    dt[f, xlsid:=seq(sum(f))]
    f <- dt$birth >= '2003-01-01' & dt$birth < '2004-01-01'
    dt[f, xlsid:=seq(sum(f))]
    f <- dt$birth >= '2004-01-01' & dt$birth < '2005-01-01'
    dt[f, xlsid:=seq(sum(f))]
    f <- dt$birth >= '2005-01-01' & dt$birth < '2006-01-01'
    dt[f, xlsid:=seq(sum(f))]
    f <- dt$birth >= '2006-01-01' & dt$birth < '2007-01-01'
    dt[f, xlsid:=seq(sum(f))]
    f <- dt$birth >= '2007-01-01' & dt$birth < '2008-01-01'
    dt[f, xlsid:=seq(sum(f))]
    f <- dt$birth >= '2008-01-01' & dt$birth < '2009-01-01'
    dt[f, xlsid:=seq(sum(f))]
    f <- dt$birth >= '2009-01-01' & dt$birth < '2010-01-01'
    dt[f, xlsid:=seq(sum(f))]
    rm(f)
  }
  setattr(dt$xlsid, 'label', 'Numer wiersza w pliku źródłowym')




  if (!flag_only_fix_attributes) {
    dt[,preg_weight:=as.numeric(dt$preg_weight)]
  }

  setattr(dt$preg_weight, 'label', 'Masa ciała noworodka')
  setattr(dt$preg_weight, 'units', 'g')

  #Poniższe przedziały są traktowane jako zamknięte z prawej strony
  if (!flag_only_fix_attributes) {
    weight_labels<-c(
      TO_LOW_TO_BE_REAL=10,
      ILBW=750,
      ELBW=1000,
      VLBW=1500,
      LBW=2500,
      Eutroficzna=4000,
      Hipertroficzna=max(dt$preg_weight))

    dt[,preg_weight_gr:=cut(dt$preg_weight, breaks=weight_labels, labels=names(weight_labels[-1]), ordered_result = TRUE)]
    #  dt[,preg_weight_gr:=cut(dt$preg_weight, breaks=weight_labels)]
  }
  setattr(dt$preg_weight_gr, 'label', 'Masa ciała noworodka w podziale na kategorie')
  setattr(dt$preg_weight_gr, 'f.o.b', 2) #zmienna porządkowa


  # if (!flag_only_fix_attributes) {
  dt[,sex:=factor(dt$sex, labels=c('♂', '♀', 'Płeć nieokreślona'))]

  setattr(dt$sex, 'label', 'Płeć noworodka')


  if (!flag_only_fix_attributes) {
    dt[,preg_weeks:=as.integer(dt$preg_weeks)]
  }
  setattr(dt$preg_weeks, 'label', 'Wiek płodowy')
  setattr(dt$preg_weeks, 'units', 'tyg.')


  if (!flag_only_fix_attributes) {
    weeks_gr<-c(
      '.'=(min(dt$preg_weeks)-1),
      'Wcześniak'=37-1,
      'Prawidłowy czas porodu'=41-1,
      'Ciąża przenoszona'=max(dt$preg_weeks)
    )
    dt[,preg_weeks_gr:=cut(dt$preg_weeks, breaks=weeks_gr, labels=names(weeks_gr[-1]), ordered_result = TRUE)]
  }
  setattr(dt$preg_weeks_gr, 'label', 'Wiek płodowy w podziale na kategorie')
  setattr(dt$preg_weeks_gr, 'f.o.b', 2) #zmienna porządkowa

  if (!flag_only_fix_attributes) {
    # < 28 hbd ekstremalnie przedwczesny
    # 28- 31 hbd bardzo przedwczesny
    # 32- 33 hbd umiarkowanie przedwczesny
    # 34- 36 hbd późny przedwczesny
    # 37- 38 hbd wczesny o czasie
    # 39-41 hbd o czasie
    # >41 hbd po czasie
    weeks_gr<-c(
      '.'=(min(dt$preg_weeks)-1),
      'Ekstremalnie przedwczesny'=28-0.001,
      'Bardzo przedwczesny'=32-0.001,
      'Umiarkowanie przedwczesny'=34-0.001,
      'Późny przedwczesny'=37-0.001,
      'Wczesny o czasie'=39-0.001,
      'O czasie'=41-0.001,
      'Po czasie'=max(dt$preg_weeks)
    )
    dt[,preg_weeks_gr2:=cut(dt$preg_weeks, breaks=weeks_gr, labels=names(weeks_gr[-1]), ordered_result = TRUE)]
  }
  setattr(dt$preg_weeks_gr2, 'label', 'Wiek płodowy w podziale na kategorie (2)')
  setattr(dt$preg_weeks_gr2, 'f.o.b', 2) #zmienna porządkowa




  if (!flag_only_fix_attributes) {
    dt[,m_birth:=
         as.Date(ifelse(is.na(dt$m_birth),
                        NA,
                        paste0(as.numeric(substring(dt$m_birth, 1,3))+1911,'.',substring(dt$m_birth,4,6),'.15')),
                 format='%Y.%m.%d')]
    dt[,m_b_month:=
         factor(month(dt$m_birth),labels=c('Sty','Lut','Mar','Kwi','Maj','Cze','Lip','Sie','Wrz','Paz','Lis','Gru'), levels=1:12)]
    dt[,m_age:=as.numeric(birth - m_birth)/365]
    #Usuwamy wiek matek, które mają mniej niż 10 lat lub więcej niż 60 lat. Razem: 30 szt.
    bad_records <- which(dt$m_age > 60 | dt$m_age < 10)

    dt[bad_records, m_age:=NA]
    dt[bad_records, m_birth:=NA]

    rm(bad_records)
    dt[,m_birth_year:=as.numeric(difftime(m_birth,as.Date('0-1-1')))/365.24]
  }
  setattr(dt$m_b_month, 'label', 'Miesiąc urodzenia matki')
  setattr(dt$m_birth, 'label', 'Moment urodzenia matki')
  setattr(dt$m_birth_year, 'label', 'Rok i miesiąc urodzenia matki')
  setattr(dt$m_age, 'label', 'Wiek matki w chwili porodu')
  setattr(dt$m_age, 'units', 'lata')


  if (!flag_only_fix_attributes) {
    age_gr<-c(
      '.'=(min(dt$m_age)-1),
      '14 lat i mniej'=15-0.001,
      '15-19 lat'=20-0.001,
      '20-24 lat'=25-0.001,
      '25-29 lat'=30-0.001,
      '30-34 lat'=35-0.001,
      '35-39 lat'=40-0.001,
      '40-44 lat'=45-0.001,
      '45-49 lat'=50-0.001,
      '50-54 lat'=55-0.001,
      '55 lat i więcej'=max(dt$m_age)
    )
    dt[,m_age_gr:=cut(dt$m_age, breaks=weeks_gr, labels=names(weeks_gr[-1]), ordered_result = TRUE)]
  }
  setattr(dt$m_age_gr, 'label', 'Przedział wieku matki')
  setattr(dt$m_age_gr, 'f.o.b', 2) #zmienna porządkowa



  if (!flag_only_fix_attributes) {
    dt[,m_birth_place:=encode_region(dt$m_reg_place)]
    dt[,m_reg_place:=NULL]
    dt[,m_preg_place:=encode_region(dt$m_preg_place)]
    dt[,
       m_preg_place_size:=do.call(dplyr::recode,
                                  c(list(.x=dt$m_preg_place),
                                    setNames(as.character(SlownikNazwRegionow$rozmiar),
                                             SlownikNazwRegionow$label)
                                  )
       )
       ]
  }
  setattr(dt$m_birth_place, 'label', 'Miejsce zameldowania matki')
  setattr(dt$m_preg_place, 'label', 'Miejsce zamieszkania matki')
  setattr(dt$m_preg_place_size, 'label', 'Kategoria administracyjna miejsca zamieszkania matki podczas ciąży')

  if (!flag_only_fix_attributes) {
    dt[,f_birth_place:=encode_region(dt$f_reg_place)]
    dt[,f_reg_place:=NULL]

    dt[,f_birth:=
         as.Date(ifelse(is.na(dt$f_birth),
                        NA,
                        paste0(as.numeric(substring(dt$f_birth, 1,3))+1911,'.',substring(dt$f_birth,4,6),'.15')),
                 format='%Y.%m.%d')]

    dt[,f_age:=as.numeric(birth - f_birth)/365]
    #Usuwamy wiek ojców, którzy mają mniej niż 10
    bad_records <- which(dt$f_age < 10)

    dt[bad_records, f_age:=NA]
    dt[bad_records, f_birth:=NA]

    rm(bad_records)
    dt[,f_birth_year:=as.numeric(difftime(f_birth,as.Date('0-1-1')))/365.24]
  }

  setattr(dt$f_birth_year, 'label', 'Rok urodzenia ojca')
  setattr(dt$f_birth, 'label', 'Rok i miesiąc urodzenia ojca')
  setattr(dt$f_birth_place, 'label', 'Region urodzenia ojca')
  setattr(dt$f_age, 'units', 'lata')
  setattr(dt$f_age, 'label', 'Wiek ojca w chwili porodu')

  if (!flag_only_fix_attributes) {
    dt[,p_age_diff_abs:=abs(f_age - m_age)]
    dt[,p_age_diff:=f_age - m_age]
  }
  setattr(dt$p_age_diff_abs, 'label', 'O ile lat ojciec starszy od matki, wartość bezwzględna')
  setattr(dt$p_age_diff_abs, 'units', 'lata')
  setattr(dt$p_age_diff, 'label', 'Różnica wieku rodziców ')
  setattr(dt$p_age_diff, 'units', 'lata')



  if (!flag_only_fix_attributes) {
    dt[,pregtype:=as.integer(dt$pregtype)]
    dt[pregtype==1,pregtype_gr:=as.integer(1)]
    dt[pregtype>1,pregtype_gr:=as.integer(2)]
    dt[,pregtype_gr:=factor(dt$pregtype_gr, levels=c(1,2), labels=c('Ciąża jednopłodowa', 'Ciąża wielopłodowa'))]
  }
  setattr(dt$pregtype, 'label', 'Liczba płodów')
  setattr(dt$pregtype_gr, 'label', 'Wielopłodowość')

  if (!flag_only_fix_attributes) {
    dt[pregtype==1,pregtype_gr2:=as.integer(1)]
    dt[pregtype==2,pregtype_gr2:=as.integer(2)]
    dt[pregtype>2,pregtype_gr2:=as.integer(3)]
    dt[,pregtype_gr2:=factor(dt$pregtype_gr2, levels=c(1,2,3), labels=c('Pojedyncze', 'Dwojacze', 'Trojacze lub większe'))]
  }


  setattr(dt$pregtype_gr2, 'label', 'Liczba płodów')


  if (!flag_only_fix_attributes) {
    dt[,b_order:=as.ordered(dt$b_order)]
  }
  setattr(dt$b_order, 'label', 'Kolejność urodzenia w ciąży mnogiej')
  setattr(dt$b_order, 'f.o.b', 2)

  if (!flag_only_fix_attributes) {
    dt[,pregn_num:=suppressWarnings(as.integer(dt$pregn_num))]
    dt[pregn_num==36,pregn_num:=NA]
  }
  setattr(dt$pregn_num, 'label', 'Liczba ciąż matki, zakończonych urodzeniem żywego dziecka')

  if (!flag_only_fix_attributes) {
    #Poniższe przedziały są traktowane jako zamknięte z prawej strony
    pregn_num_labels<-c(
      'brak'=0,
      '1'=1,
      '2'=2,
      '3'=3,
      '4 i więcej'=max(dt$pregn_num, na.rm=TRUE))
    dt[,pregn_num_gr:=cut(dt$pregn_num, breaks=pregn_num_labels, labels=names(pregn_num_labels[-1]), ordered_result = TRUE)]
    #  dt[,preg_weight_gr:=cut(dt$preg_weight, breaks=weight_labels)]
  }
  setattr(dt$pregn_num_gr, 'label', 'Liczba ciąż matki, zakonczonych urodzeniem żywego dziecka, uproszczona')
  setattr(dt$pregn_num_gr, 'f.o.b', 2) #zmienna porządkowa


  if (!flag_only_fix_attributes) {
    dt[,b_facility:=factor(dt$b_facility,
                           labels=c(
                             'Szpital',
                             'Klinika',
                             'Klinika położnicza',
                             'Miejsce zamieszkania',
                             'Inne',
                             'Publiczny Ośrodek Zdrowia'))]
  }
  setattr(dt$b_facility, 'label', 'Typ placówki przyjmującej poród')

  if (!flag_only_fix_attributes) {
    dt[,b_receiv_id:=factor(dt$b_receiv_id, labels=c('Lekarz', 'Położnik', 'Nie lekarz i nie położnik'))]
  }
  setattr(dt$b_receiv_id, 'label', 'Osoba odbierająca poród')

  if (!flag_only_fix_attributes) {
    var<-as.integer(dt$m_orig_nat)
    var[is.na(var)]<-'00' #Tajwan to NA
    kraje<-c('00', 'Tajwan',   'Tajwan',                     'Tajwan',    'Tajwan',
             '01', 'ChRL',     'ChRL',                       'ChRL',      'ChRL',
             '02', 'Wietnam',  'Wietnam',                    'Wietnam',   'Kontynentalne',
             '03', 'Indonezja','Protopolinezyjskie',         'Indonezja', 'Protopolinezyjskie',
             '04', 'Tajlandia','Kontynentalne',              'Tajlandia', 'Kontynentalne',
             '05', 'Filipiny', 'Protopolinezyjskie',         'Filipiny',  'Protopolinezyjskie',
             '06', 'Malezja',  'Kontynentalne',              'Malezja',   'Kontynentalne',
             '07', 'Birma (Mjanma)','Kontynentalne',         'Mjanma',    'Kontynentalne',
             '08', 'Kambodża', 'Kontynentalne',              'Kambodża',  'Kontynentalne',
             '09', 'Stany Zjednoczone', 'Inne',              'Inne',      'NA',
             '10', 'Japonia',  'Inne',                       'Inne',      'NA',
             '11', 'Korea',    'Inne',                       'Inne',      'NA',
             '12', 'Singapur', 'Kontynentalne',              'Inne',      'Kontynentalne',
             '13', 'Kanada',   'Inne',                       'Inne',      'NA',
             '14', 'Indie',    'Inne',                       'Inne',      'NA',
             '15', 'Republika Południowej Afryki', 'Inne',   'Inne',      'NA',
             '16', 'Brazylia', 'Inne',                       'Inne',      'NA',
             '17', 'Brytania', 'Inne',                       'Inne',      'NA',
             '18', 'Niemcy',   'Inne',                       'Inne',      'NA',
             '19', 'Francja',  'Inne',                       'Inne',      'NA',
             '20', 'Hongkong', 'Inne',                       'ChRL',      'NA',
             '21', 'Makao',    'Inne',                       'ChRL',      'NA',
             '22', 'Mongolia', 'Inne',                       'Inne',      'NA',
             '99', 'inna',     'Inne',                       'Inne',      'NA'
    )
    dim(kraje)<-c(5,length(kraje)/5)
    kraje<-t(kraje)
    labels<-kraje[,2]
    levels<-as.integer(kraje[,1])
    var[var==42]<-NA
    var[var==99]<-NA
    dt[,m_orig_nat:=factor(as.integer(var), levels=levels, labels=labels)]
    dt[birth < '2004-01-04',m_orig_nat:=NA]
    rm(var)

    rekoduj<-function(var, levels_from, levels_to) {
      var<-car::recode(var, recodes = paste0(paste0("'", levels_from, "'='", levels_to, "'"), collapse=";"))
      var[as.character(var)=='NA']<-NA
      ord<-table(factor(var))
      var<-factor(readr::parse_factor(var, levels=names(ord)[order(ord, decreasing = TRUE)]))
      setattr(var,'problems',NULL)
    }

    var<-rekoduj(dt$m_orig_nat, levels_from=kraje[,2], levels_to=kraje[,3])
    dt[,m_haplotyp:=var]

    var<-rekoduj(dt$m_orig_nat, levels_from=kraje[,2], levels_to=kraje[,4])
    dt[,m_orig_nat_gr:=var]

    var<-rekoduj(dt$m_orig_nat, levels_from=kraje[,2], levels_to=kraje[,5])
    dt[,m_orig_2:=var]

    main_cat<-c('Tajwan','ChRL','Wietnam','Indonezja', 'Kambodża', 'Filipiny')
    var<-dt$m_orig_nat_gr
    var[var %in% setdiff(levels(var), main_cat)]<-NA
    dt[,m_orig_nat_largest:=factor(var)]
    rm(var)

    levels<-as.character(head(data.frame(table(dt$m_orig_nat)) %>% arrange(-Freq),6)$Var1)
    suppressWarnings(var<-factor(readr::parse_factor(as.character(dt$m_orig_nat), levels=levels)))
    setattr(var,'problems',NULL)
    dt[,m_orig_1:=var]

    rm(var)
  }
  setattr(dt$m_orig_nat, 'label', 'Początkowe obywatelstwo matki')
  setattr(dt$m_orig_nat_gr, 'label', 'Region pochodzenia matki')
  setattr(dt$m_orig_nat_largest, 'label', 'Kraj pochodzenia matki')
  setattr(dt$m_haplotyp, 'label', 'Uproszczony podział miejsca pochodzenia matki')
  setattr(dt$m_orig_1, 'label', 'Kraj pochodzenia matki')
  setattr(dt$m_orig_2, 'label', 'Uproszczony podział miejsca pochodzenia matki')





  if (!flag_only_fix_attributes) {
    dt[,b_reg:=encode_region(dt$b_reg)]
  }
  setattr(dt$b_reg, 'label', 'Miejsce urodzenia ')


  if (!flag_only_fix_attributes) {
    dt[,b_type:=factor(dt$b_type,
                       labels=c(
                         'Siłami natury',
                         'Siłami natury wspomagany kleszczowy',
                         'Siłami natury wspomagany próżnociąg',
                         'Siłami natury z historią cesarskiego',
                         'Pierwszy cesarski',
                         'Cesarski z historią cesarskiego'
                       )
    )]
  }

  setattr(dt$b_type, 'label', 'Rodzaj porodu')
  if (!flag_only_fix_attributes) {
    dt[,b_type_rec:=car::recode(dt$b_type, "
                                'Siłami natury wspomagany kleszczowy'='Siłami natury';
                                'Siłami natury wspomagany próżnociąg'='Siłami natury';
                                'Siłami natury z historią cesarskiego'='VBAC';
                                'Pierwszy cesarski'='Cesarski';
                                'Cesarski z historią cesarskiego'='Cesarski'
                                ")]
  }
  setattr(dt$b_type_rec, 'label', 'Skrótowy rodzaj porodu')



  # talasamia, cukrzyca normalna, cukrzyca ciazaowa, nadcisnienie, nadcisnienie indukowane ciaza,
  # zatrucie ciążowe





  if (!flag_only_fix_attributes) {
    apgar1<-dt$apgar1
    apgar1[apgar1=='A'] <- 10
    #  apgar1[apgar1==''] <- 0
    apgar1[apgar1=='X'] <- NA

    apgar_labels<-c(
      '0-3 punktów'=3,
      '4-6 punkty'=6,
      '7-10 punktów'=10)

    dt[,apgar1_gr:=cut(suppressWarnings(as.integer(dt$apgar1)),
                       breaks=c(-1,apgar_labels), labels=names(apgar_labels), ordered_result = TRUE)]
    dt[,apgar1:=NULL]
    dt[,apgar1:=apgar1]
    #    dt[,apgar1:=suppressWarnings(as.integer(apgar1))]
    dt[birth < '2004-01-01', apgar1:=NA]
    dt[birth < '2004-01-01', apgar1_gr:=NA]
    rm(apgar1)

    apgar5<-dt$apgar5
    apgar5[apgar5=='A'] <- 10
    apgar5[apgar5=='X'] <- NA
    #  apgar5[apgar5==''] <- 0
    dt[,apgar5_gr:=cut(suppressWarnings(as.integer(dt$apgar5)),
                       breaks=c(-1,apgar_labels), labels=names(apgar_labels), ordered_result = TRUE)]
    dt[,apgar5:=NULL]
    dt[,apgar5:=apgar5]

    dt[birth < '2004-01-01', apgar5:=NA]
    dt[birth < '2004-01-01', apgar5_gr:=NA]
    rm(apgar5)
  }
  setattr(dt$apgar1, 'label', 'Liczba punktów Apgar w 1 minucie życia')
  setattr(dt$apgar5, 'label', 'Liczba punktów Apgar w 5 minucie życia')
  setattr(dt$apgar1_gr, 'label', 'Ocena Apgar w 1. minucie życia')
  setattr(dt$apgar5_gr, 'label', 'Ocena Apgar w 5. minucie życia')

  # NarodowoscUPR=c('Tajwan',  'ChRL',  'Wietnam',  'APW B4a1a',  'APW nie-B4a1a',  'APW B4a1a',  'APW nie-B4a1a',  'APW nie-B4a1a',  'APW nie-B4a1a',  'Inne',  'Inne',  'Inne' ,  'APW nie-B4a1a',  'Inne',
  #                 'Inne',  'Inne',  'Inne',  'Inne',  'Inne',  'Inne',
  #                 'Inne',  'Inne',  'Inne',  'Inne')
  # NarodowoscUPR2=c('1.Tajwan',  '2.ChRL',  '3.Wietnam',  '4.APW B4a1a',  '5.APW nie-B4a1a',  '4.APW B4a1a',  '5.APW nie-B4a1a',  '5.APW nie-B4a1a',  '5.APW nie-B4a1a',  '6.Inne',  '6.Inne',  '6.Inne' ,  '5.APW nie-B4a1a',  '6.Inne',
  #                  '6.Inne',  '6.Inne',  '6.Inne',  '6.Inne',  '6.Inne',  '6.Inne',
  #                  '6.Inne',  '6.Inne',  '6.Inne',  '6.Inne')
  #
  # Haplotypy=c('Tajwan',  'ChRL',  'Wietnam',  'APW B4a1a',  'APW nie-B4a1a','Inne')
  # HaplotypyN=c('Tajwan',  'ChRL',  'Wietnam',  'Pozostale APW B4a1a',  'Pozostale APW nie-B4a1a','Inne')
  #
  #




  if (!flag_only_fix_attributes) {
    dt[,.empty:=NULL]; dt[,.empty2:=NULL]; dt[,.empty3:=NULL]; dt[,.empty4:=NULL]; dt[,.empty5:=NULL]
    dt[,.empty6:=NULL]; dt[,.empty7:=NULL];
    dt[,.remove1:=NULL]; dt[,.remove2:=NULL]; dt[,.remove3:=NULL]; dt[,.remove4:=NULL]
    dt[,.remove5:=NULL]; dt[,.remove6:=NULL]; dt[,.remove7:=NULL]; dt[,.remove8:=NULL]
    dt[,.remove9:=NULL]; dt[,.remove10:=NULL]; dt[,.remove11:=NULL]

    dt[,m_reg_place2:=NULL]
    dt[,m_preg_place2:=NULL]
  }

  recode_wady<-function(dt, flag_only_fix_attributes, kodowanieECpath, zamiany_zmiennych_path)
  {
    #  dt<-readRDS('db_fixed.rds')
    library(xlsx)
    if(!file.exists(kodowanieECpath)) {
      browser()
    }
    if(!file.exists(zamiany_zmiennych_path)) {
      browser()
    }
    keys_dt<-xlsx::read.xlsx2(kodowanieECpath, sheetIndex = 1)
    keys_dt <- (keys_dt %>% filter(keys_dt$label != 'BŁĄD' & keys_dt$label != 'NA'))

    if (!flag_only_fix_attributes) {

      nazwy<-colnames(dt)[stringi::stri_detect_fixed(colnames(dt),'cong_dis_')]
      cong_dis_2001_dt <- as.data.table(dt %>% filter(birth >= as.Date('2001-01-01') & birth < as.Date('2002-01-01') ) %>% select_( .dots = c('id', nazwy)))
      cong_dis_2002a_dt <- as.data.table(dt %>% filter(birth >= as.Date('2002-01-01') & birth < as.Date('2002-07-01') ) %>% select_( .dots = c('id',nazwy)))
      cong_dis_2002b_dt <- as.data.table(dt %>% filter(birth >= as.Date('2002-07-01') & birth < as.Date('2003-01-01') ) %>% select_( .dots = c('id', nazwy)))
      cong_dis_2003_dt <- as.data.table(dt %>% filter(birth >= as.Date('2003-01-01') & birth < as.Date('2010-01-01') ) %>% select_( .dots = c('id', nazwy)))

      fdt_2001<-launch_conv_to_wide(dt = cong_dis_2001_dt,
                                    varnames = nazwy,
                                    valid_levels = as.integer(as.character(keys_dt$key_2001)),
                                    valid_varnames = paste0('cong_dis_',as.character(keys_dt$key_2003)),
                                    levels_to_ignore = c('NA'),
                                    flag_remove_original_fields = TRUE)

      fdt_2002a<-launch_conv_to_wide(dt = cong_dis_2002a_dt,
                                     varnames = nazwy,
                                     valid_levels = as.integer(as.character(keys_dt$key_2002a)),
                                     valid_varnames = paste0('cong_dis_',as.character(keys_dt$key_2003)),
                                     levels_to_ignore = c('NA'),
                                     flag_remove_original_fields = TRUE)

      fdt_2002b<-launch_conv_to_wide(dt = cong_dis_2002b_dt,
                                     varnames = nazwy,
                                     valid_levels = as.integer(as.character(keys_dt$key_2002b)),
                                     valid_varnames = paste0('cong_dis_',as.character(keys_dt$key_2003)),
                                     levels_to_ignore = c('NA'),
                                     flag_remove_original_fields = TRUE)

      fdt_2003<-launch_conv_to_wide(dt = cong_dis_2003_dt,
                                    varnames = nazwy,
                                    valid_levels = as.integer(as.character(keys_dt$key_2003)),
                                    valid_varnames = paste0('cong_dis_',as.character(keys_dt$key_2003)),
                                    flag_remove_original_fields = TRUE)
      dt_2001<-conv_to_wide_from_odp(parallel_odp = fdt_2001, dt = cong_dis_2001_dt)
      rm(cong_dis_2001_dt)
      dt_2002a<-conv_to_wide_from_odp(parallel_odp = fdt_2002a, dt = cong_dis_2002a_dt)
      rm(cong_dis_2002a_dt)
      dt_2002b<-conv_to_wide_from_odp(parallel_odp = fdt_2002b, dt = cong_dis_2002b_dt)
      rm(cong_dis_2002b_dt)
      dt_2003<-conv_to_wide_from_odp(parallel_odp = fdt_2003, dt = cong_dis_2003_dt)
      rm(cong_dis_2003_dt)


      dt_all<-rbind(dt_2001, dt_2002a, dt_2002b, dt_2003, fill=TRUE)
      dt_all_bak <- data.table::copy(dt_all)
      rm(dt_2001); rm(dt_2002a); rm(dt_2002b); rm(dt_2003)

      duplikaty<-readODS::read_ods(zamiany_zmiennych_path)

      data.table::setorder(dt_all,id)
      data.table::setorder(dt, id)
      dt[,(nazwy):=NULL]
      #dt[,(dt %>% select(starts_with('cong_dis_')) %>% colnames):=NULL]
      #dt_all<-copy(dt_all_bak)
      for(i in seq(nrow(duplikaty)))
      {
        #    cat(paste0(i,'\n'))
        zmienna_src <- paste0('cong_dis_', duplikaty[i,1])
        zmienna_dest <- paste0('cong_dis_', duplikaty[i,2])
        var_src <- dt_all[[zmienna_src]]
        var_dest <- dt_all[[zmienna_dest]]
        sum_vec <- (var_src == 1 & var_dest == 1)
        suma <- sum(ifelse(is.na(var_dest) & is.na(var_src),0 , sum_vec), na.rm=TRUE)
        if (suma>0)
        {
          warning(paste0("Istnieje ", suma, " rekordów, które mają zaznaczone jednocześnie wadę ", zmienna_src, " i ", zmienna_dest))
        }
        dt_all[,(zmienna_dest):= ifelse(
          is.na(var_src),
          ifelse(is.na(var_dest),
                 as.integer(0),
                 var_dest),
          ifelse(is.na(var_dest),
                 var_src,
                 var_src + var_dest))]
        dt_all[,(zmienna_src):=NULL]
      }
      for(i in (seq_along(dt_all)[-1])){
        varname <- colnames(dt_all)[[i]]
        dt[,(varname):=ifelse(is.na(dt_all[[varname]]),as.integer(1), dt_all[[varname]] + as.integer(1)) ]
        setattr(dt[[varname]], 'levels', c('Nie stwierdzono', 'Stwierdzono'))
        setattr(dt[[varname]], 'class', 'factor')

        kod <- stringi::stri_match(varname, regex='\\d{4}')[[1]]
        pos <- match(kod, keys_dt$key_2003)
        newlabel <- as.character(keys_dt$label[[pos]])
        newlabel <- paste0("Czy u dziecka stwierdzono ", newlabel)
        setattr(dt[[varname]], 'label', newlabel)
        #        dt_all[,(varname):=NULL]
      }


      rm(dt_all)
    }
    return(dt)

  }

  dt <- recode_wady(dt, flag_only_fix_attributes = flag_only_fix_attributes, kodowanieECpath = kodowanieECpath,
                    zamiany_zmiennych_path=zamiany_zmiennych_path)


  if (!flag_only_fix_attributes) {
    #We need to convert to data.frame because of https://github.com/hadley/dtplyr/issues/51
    a<- dt %>% select(starts_with('cong_dis_')) %>% data.frame %>% mutate_all(funs(as.integer(.)-1))  %>% mutate(sum=(sign(rowSums(.)))) %>% select(sum)
    dt[,cong_dis_any:=factor(a$sum, levels=c(0,1), labels=c('Brak wad', 'Co najmniej jedna wada')) ]
  }
  setattr(dt$cong_dis_any, 'label', "Czy stwierdzono jakąkolwiek wadę płodu")

  if (!flag_only_fix_attributes) {
    dt<-conv_to_wide_from_odp(parallel_odp = p_risk_f_, dt = dt)

    a<- dt %>% select(starts_with('m_risk_f')) %>% data.frame %>% mutate_all(funs(as.integer(.)-1)) %>% mutate(sum=(sign(rowSums(.)))) %>% select(sum)
    dt[,m_risk_f_any:=factor(a$sum, levels=c(0,1), labels=c('Brak czynników ryzyka', 'Co najmniej jeden czynnik ryzyka')) ]
    rm(a)
  }
  setattr(dt$m_risk_f_any, 'label', "Obecność przynajmniej jednego czynnika ryzyka u matki")


  if (!flag_only_fix_attributes) {
    dt<-conv_to_wide_from_odp(parallel_odp = m_proc_, dt = dt)

    dt<-conv_to_wide_from_odp(parallel_odp = b_cmpl_, dt = dt)
  }

  setattr(dt, 'label', 'Noworodki w Republice Chińskiej 2001-2009')


  if (!flag_only_fix_attributes) {
    dt<-filter_out_bad_records(dt)
  }

  return(list(dt=dt, SlownikNazwRegionow=SlownikNazwRegionow))

  }

conv_to_wide<-function(dt, varnames, valid_levels=NULL, valid_varnames=NULL, valid_labels=NULL, labels=NULL, levels_to_ignore=NULL,  flag_remove_original_fields=FALSE)
{
  #Select relevant columns
  dm<-data.matrix(dt[,(varnames),with=FALSE])
  elements<-sort(unique(as.numeric(dm)))
  if(is.null(valid_levels))
  {
    valid_levels<-elements
  }

  common_elements<-intersect(elements, valid_levels)

  if(!is.null(levels_to_ignore))
  {
    common_elements<-setdiff(common_elements, levels_to_ignore)
  }

  if(is.null(valid_varnames))
  {
    valid_varnames<-paste0('level_',
                           paste0(sample(letters, size=4, replace=TRUE),collapse=''),
                           '_',
                           common_elements)
  } else {
    valid_varnames <- valid_varnames[valid_levels %in% common_elements]
    if(length(valid_varnames)!=length(common_elements))
    {
      stop(paste0("valid_varnames must have the same length as valid_levels, i.e. size of ", length(valid_levels)))
    }
  }
  elements <- common_elements

  odp<-integer(nrow(dm) * length(elements))
  dim(odp)<-c(nrow(dm), length(elements))

  transp_f<-function(i, elements)
  {
    odp[i, ]<<-as.integer(elements %in% dm[i,])
  }


  plyr::a_ply(seq(nrow(dm)),1,transp_f,elements=elements)

  if(is.null(labels))
  {
    for(i in seq_along(elements))
    {
      varname<-valid_varnames[[i]]
      dt[,(varname):=odp[,i] ]
    }
  } else {
    for(i in seq_along(elements))
    {
      varname<-valid_varnames[[i]]
      dt[,(varname):=factor(odp[,i]+1, labels = labels) ]
    }
  }


  if(!is.null(valid_labels))
  {
    if(length(valid_labels)!=length(valid_levels))
    {
      stop(paste0("valid_labels must have the same length as valid_levels, i.e. size of ", length(valid_levels)))
    }
    for(i in seq_along(valid_levels))
    {
      setattr(dt[[valid_varnames[[i]] ]], 'label', valid_labels[[i]])
    }
  }

  if(flag_remove_original_fields)
  {
    for(varname in varnames)
    {
      dt[,(varname):=NULL]
    }
  }
  return(dt)
}

filter_out_bad_records<-function(dt) {
  #Dobre rekordy muszą spełniać następujące cechy:


  cond1 <-data.table(rownr=which(dt$preg_weight<=10),
                     label="Noworodek nie może mieć mniej, niż 10g.")

  cond2 <- data.table(rownr=which(dt$preg_weeks<20),
                      label="Nie można urodzić w mniej, niż 20 tygodniu.")

  cond3 <- data.table(rownr=which(dt$sex == 'Płeć nieokreślona'),
                      label="Płeć niekreślona")

  cond4 <- data.table(rownr=which(dt$m_preg_place_size == 'Obca' | dt$m_preg_place_size == 'Nieznana'),
                      label="Zagraniczne lub nieznane miejsce urodzenia matki")

  #  cond5 <- data.table(rownr=which(dt$m_age > 60), label="Wiek matki większy od 60 lat")

  #  cond6 <- data.table(rownr=which(dt$m_age < 10), label="Wiek matki mniejszy od 10 lat")

  ktore_usuwamy<-unique(rbind(cond1, cond2, cond3,cond4)[,rownr])

  return(dt[!(ktore_usuwamy),])
}

launch_conv_to_wide<-function(dt, varnames, valid_levels=NULL, valid_varnames=NULL, valid_labels=NULL, labels=NULL, levels_to_ignore=NULL,  flag_remove_original_fields=FALSE){
  prepare_conv_to_wide<-function(dt, varnames, valid_levels=NULL, valid_varnames=NULL, valid_labels=NULL, labels=NULL, levels_to_ignore=NULL,  flag_remove_original_fields=FALSE)
  {

    #Select relevant columns
    dm<-data.matrix(dt[,(varnames),with=FALSE])
    elements<-sort(unique(as.numeric(dm)))
    if(is.null(valid_levels))
    {
      valid_levels<-elements
    }

    common_elements<-intersect(elements, valid_levels)

    if(!is.null(levels_to_ignore))
    {
      common_elements<-setdiff(common_elements, levels_to_ignore)
    }

    if(is.null(valid_varnames))
    {
      valid_varnames<-paste0('level_',
                             paste0(sample(letters, size=4, replace=TRUE),collapse=''),
                             '_',
                             common_elements)
    } else {
      valid_varnames <- valid_varnames[valid_levels %in% common_elements]
      if(length(valid_varnames)!=length(common_elements))
      {
        browser()
        stop(paste0("valid_varnames must have the same length as valid_levels, i.e. size of ", length(valid_levels)))
      }
    }
    elements <- common_elements

    odp<-integer(nrow(dm) * length(elements))
    dim(odp)<-c(nrow(dm), length(elements))

    transp_f<-function(i, elements)
    {
      odp[i, ]<<-as.integer(elements %in% dm[i,])
    }


    plyr::a_ply(seq(nrow(dm)),1,transp_f,elements=elements)

    return(list(odp=odp, elements=elements, varnames=varnames,
                valid_levels=valid_levels, valid_varnames=valid_varnames,
                valid_labels=valid_labels, labels=labels,
                levels_to_ignore=levels_to_ignore,
                flag_remove_original_fields=flag_remove_original_fields))
  }
  return(parallel::mcparallel(prepare_conv_to_wide(dt=dt, varnames = varnames,
                                                   valid_levels = valid_levels,
                                                   valid_varnames = valid_varnames,
                                                   valid_labels = valid_labels,
                                                   labels = labels,
                                                   levels_to_ignore = levels_to_ignore,
                                                   flag_remove_original_fields = flag_remove_original_fields)))
}



conv_to_wide_from_odp<-function(parallel_odp, dt){
  odp<-parallel::mccollect(parallel_odp)[[1]]


  if(is.null(odp$labels))
  {
    for(i in seq_along(odp$elements))
    {
      varname<-odp$valid_varnames[[i]]
      dt[,(varname):=odp$odp[,i] ]
    }
  } else {
    for(i in seq_along(odp$elements))
    {
      varname<-odp$valid_varnames[[i]]
      dt[,(varname):=factor(odp$odp[,i]+1, labels = odp$labels) ]
    }
  }


  if(!is.null(odp$valid_labels))
  {
    if(length(odp$valid_labels)!=length(odp$valid_levels))
    {
      stop(paste0("valid_labels must have the same length as valid_levels, i.e. size of ", length(odp$valid_levels)))
    }
    for(i in seq_along(odp$valid_levels))
    {
      setattr(dt[[odp$valid_varnames[[i]] ]], 'label', odp$valid_labels[[i]])
    }
  }

  if(odp$flag_remove_original_fields)
  {
    for(varname in odp$varnames)
    {
      dt[,(varname):=NULL]
    }
  }
  return(dt)
}



