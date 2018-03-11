gen_stat_opisowe<-function(DT) {
  options(OutDec=',')

  #browser()
  #TODO: Dodaj kolumnę "jednostka"
  #TODO: Jeśli jest jednostka, to ma być podana w nawiasach kwadratowych w nazwie label_txt
  #TODO: Poporaw tabelę porządkowych

  #Produkujemy zbiór o następujących kolumnach:
  #nazwa wewn
  #label
  #typ
  #liczba_NA
  #mean +- se (dla ordered i ilościowych)
  #sd (jw)
  #median (jw)
  #quant_diff (jw)
  #moda
  #ile sztuk dla mody
  #entropia (dla nominalnych i porządkowych)
  fn<-function(varname) {
    var<-DT[[varname]]
    fob<-danesurowe::GetFOB(var, flag_recalculate_uniques=TRUE, flag_update_dt=FALSE)
    tab_raw<-table(var)
    tab<-sort(tab_raw, decreasing = TRUE)
    levname<-names(tab)[[1]]
    if(fob!=0){
      entr<-entropy::entropy.empirical(y = tab, unit = 'log2')
    } else {
      entr<-NA
    }
    if('factor' %in% class(var)) {
      level<-match(levname, attr(var, 'levels'))
    } else {
      level<-levname
    }
    if(fob==3) {
      lab_positive<-names(tab_raw)[[2]]
    } else {
      lab_positive<-NA
    }
    if(fob!=1) {
      if(fob!=3) {
        minv<-min(var, na.rm=TRUE)
        maxv<-max(var, na.rm=TRUE)
      } else {
        minv<-min(as.ordered(var), na.rm=TRUE)
        maxv<-max(as.ordered(var), na.rm=TRUE)
      }
      if(fob!=0) {
        all_levels<-danesurowe::GetLevels(var, flag_recalculate = FALSE)
        if(as.numeric(minv) %in% all_levels) {
          minv<-names(all_levels)[as.numeric(minv) == all_levels]
        }
        if(as.numeric(maxv) %in% all_levels) {
          maxv<-names(all_levels)[as.numeric(maxv) == all_levels]
        }
      }
      if(is.numeric(minv)) {
        library(danesurowe)
        minv<-danesurowe::format_values(minv)
        maxv<-danesurowe::format_values(maxv)
      }
    } else {
      minv<-NA
      maxv<-NA
    }
    if(fob==0) {
      mytab<-NA
    } else {
      mytab<-tab_raw
    }
    return(list(level=level, count=tab[[1]], entropy=entr, fob=fob, lab_positive=lab_positive, counts=mytab, min_txt=minv, max_txt=maxv))

  }

  j<-parallel::mcparallel(parallel::mclapply(colnames(DT), fn, mc.cores = 4))

  descrs <- data.table(name=colnames(DT))
  descrs[,label:=map_chr(colnames(DT), ~ danesurowe::GetVarLabel(varname = ., dt =  DT))]
  descrs$units<-danesurowe::GetUnits(DT)

  descrs[,type_txt:=map_chr(colnames(DT), function(x) danesurowe::nice_class_names_1(DT[[x]], language = 'PL'))]
  descrs[,type:=map_chr(colnames(DT), function(x) danesurowe::nice_class_names_1(DT[[x]], language = ''))]

  descrs[,na_count:=map_int(colnames(DT), ~ sum(is.na(DT[[.]])))]
  descrs[,count:=nrow(DT) - na_count]



  modes<-parallel::mccollect(j)[[1]]

  descrs[,mode_count:=modes %>%map_int('count')]
  descrs[,mode:=modes %>%map_chr('level')]
  descrs[,entropy:=modes %>%map_dbl('entropy')]
  descrs[,fob:=modes %>%map_dbl('fob')]
  descrs[,lab_positive:=modes %>%map_chr('lab_positive')]
  descrs[,min:=modes %>%map_chr('min_txt')]
  descrs[,max:=modes %>%map_chr('max_txt')]

  descrs[,mean:=map2_dbl(colnames(DT), fob, function(varname, fob) {
    var<-DT[[varname]]
    #    fob<-danesurowe::GetFOB(var)
    if(fob %in% c(0,2)) {
      return(mean(as.numeric(var), na.rm=TRUE))
    } else if (fob == 3) {
      var<-as.numeric(var)
      return(mean(var, na.rm=TRUE) - min(var, na.rm=TRUE))
    } else {
      return(NA)
    }
  })]
  descrs[,sd:=map2_dbl(colnames(DT), fob, function(varname, fob) {
    var<-DT[[varname]]
    #    fob<-danesurowe::GetFOB(var)
    if(fob %in% c(0,2,3)) {
      return(sd(as.numeric(var), na.rm=TRUE))
    } else {
      return(NA)
    }
  })]
  descrs[,se:=sd/sqrt(count)]
  descrs[,median:=map2_dbl(colnames(DT), fob, function(varname, fob) {
    var<-DT[[varname]]
    #    fob<-danesurowe::GetFOB(var)
    if(fob %in% c(0,2)) {
      return(median(as.numeric(var), na.rm=TRUE))
    } else {
      return(NA)
    }
  })]
  descrs[,quant_diff:=map2_dbl(colnames(DT), fob, function(varname, fob) {
    var<-DT[[varname]]
    #    fob<-danesurowe::GetFOB(var)
    if(fob %in% c(0,2)) {
      qs <- quantile(as.numeric(var), probs = c(0.75, 0.25), na.rm=TRUE)
      return((qs[[1]] - qs[[2]])/2)
    } else {
      return(NA)
    }
  })]

  descrs[,label_txt:=danesurowe::format_var_name(name,longcolname = label,flag_main_is_short_name = FALSE, flag_quote_shortname = '`', unit=units)]
  descrs[,na_count_txt:=danesurowe::report_integer(na_count)]
  descrs[,na_proc_txt:=danesurowe::report_proc(na_count/nrow(DT), flag_use_small_mark = TRUE, flag_include_proc_sign=FALSE)]
  descrs[,count_txt:=danesurowe::report_integer(nrow(DT)-na_count)]
  descrs[,is_date:=map_lgl(descrs$name, ~length(intersect(c('POSIXct', 'Date'), class(DT[[.]])))>0)]

  descrs[,mean_txt:=ifelse(is_date, paste0(as.character(as.Date(mean, origin='1970-01-01')), "\uA0±\uA0",
                                           danesurowe::report_single_value(se), " dni"),
                           ifelse(type=='L',
                                  paste0(danesurowe::report_value_with_error(value = mean*100, ci = se*100, n.significant = 3),
                                         '% ', lab_positive),
                                  danesurowe::report_value_with_error(value = mean, ci = se, n.significant = 3)))]
  descrs[,sd_txt:=danesurowe::report_value_with_error(value = sd, ci = sd/(2*sqrt(count)), n.significant = 3, flag_insert_error=FALSE)]
  descrs[,quant_diff_txt:=danesurowe::report_single_value(value = quant_diff)]
  descrs[,mode_count_txt:=danesurowe::report_integer(mode_count)]
  descrs[,mode_proc_txt:=danesurowe::report_proc(mode_count/(nrow(DT)-na_count), flag_include_proc_sign=FALSE)]
  descrs[,mode_txt:=map2_chr(name, mode,
                             function(varname, mode) {
                               var<-DT[[varname]]
                               ans<-danesurowe::ValueToLabel(var, mode)
                               if(length(ans)==0){
                                 typ <- danesurowe::class2vartype(var)
                                 if(typ=='D') {
                                   return(as.character(as.Date(as.numeric(mode), origin='1970-01-01')))
                                 } else {
                                   return(as.character(mode))
                                 }
                               } else {
                                 return(ans)
                               }
                             }    )]
  descrs[,median_txt:=map2_chr(name, median,
                               function(varname, mode) {
                                 var<-DT[[varname]]
                                 ans<-danesurowe::ValueToLabel(var, mode)
                                 if(length(ans)==0){
                                   typ <- danesurowe::class2vartype(var)
                                   if(typ=='D') {
                                     return(as.character(as.Date(as.numeric(mode), origin='1970-01-01')))
                                   } else {
                                     if('integer' %in% class(var)) {
                                       return(as.character(danesurowe::report_integer(as.numeric(mode))))
                                     } else {
                                       return(as.character(danesurowe::report_single_value_1(as.numeric(mode))))
                                     }
                                   }
                                 } else {
                                   return(ans)
                                 }
                               }    )]

  descrs[,entropy_txt:=danesurowe::report_single_value(entropy)]

  return(descrs)
}

report_stat_opisowe<-function(descrs, dt) {
  types<-sort(table(descrs$type))

  set_common<-c('label_txt', 'na_count_txt', 'na_proc_txt')
  set_L <- c('mean_txt', 'entropy_txt')
  types_L <- 'L'
  set_F <- c('mode_txt', 'mode_count_txt', 'mode_proc_txt', 'entropy_txt')
  types_F <- c('F', 'T')
  set_N <- c('mean_txt', 'sd_txt', 'median_txt', 'min', 'max')
  types_N <-c('I', 'N', 'D')
  set_O <- c('median_txt', 'mode_txt', 'mode_proc_txt', 'quant_diff_txt', 'min', 'max')
  types_O <- 'O'

  dic_names_PL<-c('label_txt'="Zmienna",
                  'na_count_txt'="Liczba braków",
                  'na_proc_txt'="Udział braków (%)",
                  'mean_txt'="Średnia",
                  'entropy_txt'="Entropia rozkładu (binarna)",
                  'mode_txt'="Wartość najczęstsza",
                  'mode_count_txt'="Liczba obserwacji",
                  'mode_proc_txt'='Udział wartości najczęstszej (%)',
                  'sd_txt'= 'Odch. stand.',
                  'median_txt'='Mediana',
                  'quant_diff_txt'='Rozrzut kwantylowy',
                  'min'='min',
                  'max'='max'
  )
  dic_names<-dic_names_PL
  my_set<-character(0)
  incl<-list()

  if(length(intersect(names(types), types_L)>0)) {
    incl$L<-danesurowe::liczebnik(liczba = types['L'], mianownik = 'zmiennej binarnej', 'zmiennych binarnych', 'zmiennych binarnych')
    my_set<-c(my_set, set_L)
  }

  if(length(intersect(names(types), types_F)>0)) {
    incl$F<-danesurowe::liczebnik(liczba = sum(c(types['F'], types['T']),na.rm=TRUE),
                                  mianownik = 'zmiennej nominalna', 'zmiennych nominalnych', 'zmiennych nominalnych')
    my_set<-c(my_set, set_F)
  }

  if(length(intersect(names(types), types_N)>0)) {
    incl$N<-danesurowe::liczebnik(liczba = sum(c(types['I'], types['N'], types['D']),na.rm=TRUE),
                                  mianownik = 'zmiennej przedziałowa', 'zmiennych przedziałowych', 'zmiennych przedziałowych')
    my_set<-c(my_set, set_N)
  }

  if(length(intersect(names(types), types_O)>0)) {
    incl$O<-danesurowe::liczebnik(liczba = types['O'],
                                  mianownik = 'zmiennej porządkowa', 'zmiennych porządkowych', 'zmiennych porządkowych')
    my_set<-c(my_set, set_O)
  }

  if(length(incl)>1){
    my_set<-c('type_txt', my_set)
  }
  my_set<-unique(c(set_common, my_set))


  df<-descrs %>% select_(.dots=my_set) %>% as.data.table()

  for(i in seq_along(my_set)) {
    setattr(df[[i]], 'label', dic_names[colnames(df)[[i]]])
  }

  opis<-NULL
  if( 'entropy_txt' %in% my_set) {
    opis<-c(opis, list(paste0('Entropia oznacza ilość informacji wyrażoną w bitach, jaką przekazuje jedna obserwacja danej zmiennej. ',
                              'Im wyższa wartość, tym bardziej różnorodne wartości zmienna przyjmuje. ',
                              'W obliczeniach entropii każdy unikalny poziom zmiennej jest traktowany ex aequo, dlatego entropia nadaje się tylko dla zmiennych nominalnych. ')))
  }
  if('mean_txt' %in% my_set) {
    opis<-c(opis, list(paste0('Średnia arytmetyczna jest zestawiona razem z jej błędem standardowym, zaokrąglonym do 3 miejsc znaczących. ')))
  }
  if('sd_txt' %in% my_set) {
    opis<-c(opis, list(paste0('Odchylenie standardowe jest zaokrąglone do 3 miejsc znaczących względem jej błędu standardowego (wyrażonego formułą $\\frac{sd}{\\sqrt{N}}$). ')))
  }
  if('D' %in% names(types)) {
    opis<-c(opis, list(paste0('Jednostką przedziałów między momentami czasu są dni. ')))
  }

  if(('mean_txt' %in% my_set) && ('L' %in% names(types)) ) {
    opis<-c(opis, list(paste0('Średnia policzona dla zmiennych binarnych kodowanych jako "porażka"/"sukces", oznacza średni udział "sukcesów"',
                              ' w próbie. ')))
  }
  if('mode_proc_txt' %in% my_set) {
    opis<-c(opis, list(paste0('Udział wartości najczęstszej jest oznaczony w grupie tylko pełnych obserwacji. ')))
  }

  cap_txt <- paste0("Statystyki opisowe policzone dla ", danesurowe::format_item_list(as_tibble(as.character(incl))),
                    ' policzone na zbiorze ', danesurowe::GetDBName(dt), '. ', paste0(as.character(flatten(opis)), collapse = ''))
  return(list(table=df, caption=cap_txt))
}


yuxia_raport_stat_opisowych<-function(rap, level=1, flag_gen_header=FALSE) {
  dt<-read_all()
  descrs<-gen_stat_opisowe(dt)
  #map_chr(colnames(dt), function(x) {cat(paste0("\nvar: ", x, " ")); danesurowe::nice_class_names(dt[[x]], language = 'PL')})
  saveRDS(descrs, 'descrs.rds')
  descrs<-readRDS('descrs.rds')

  descrs<-descrs%>%filter(! name %in% c('b_facility', 'b_receiv_id', 'b_reg', 'm_birth_place',
                                        'm_orig_nat', 'm_preg_place', 'id' , 'xlsid') )

  exclusion_list<-c('^cong_dis','^m_risk_f','^m_proc', '^b_cmpl')
  var_filters<-map(exclusion_list, ~stringr::str_detect(descrs$name, stringr::regex(.)))

  other_vars<-descrs$name[!data.matrix(do.call(rbind, var_filters) %>% as_tibble %>% dplyr::summarise_all(sum))]
  names(var_filters)<-exclusion_list

  #descrs%>%filter(label=='Miejsce ')

  if(flag_gen_header) {
    rap$add.paragraph(pander::pandoc.header.return("Statystyki opisowe", level=level))
  }
  #  rap<-prepare_report('Statystyki opisowe')

  rap$add.paragraph(pander::pandoc.header.return("Zmienne nominalne", level=level+1))
  ans<-report_stat_opisowe(descrs %>% filter(type %in% c('L', 'F') & name %in% other_vars), dt=dt[,..other_vars])
  rap$add.paragraph(add_simple_table(ans$table, caption = ans$caption))

  rap$add.paragraph(pander::pandoc.header.return("Zmienne porządkowe", level=level+1))
  ans<-report_stat_opisowe(descrs %>% filter(type %in% c('O') & name %in% other_vars),dt=dt[,..other_vars])
  rap$add.paragraph(relationshipMatrix::add_simple_table(ans$table, caption = ans$caption))

  rap$add.paragraph(pander::pandoc.header.return("Zmienne przedziałowe", level=level+1))
  ans<-report_stat_opisowe(descrs %>% filter(type %in% c('I', 'N', 'D') & name %in% other_vars),dt=dt[,..other_vars])
  rap$add.paragraph(relationshipMatrix::add_simple_table(ans$table, caption = ans$caption))


  rap$add.paragraph(pander::pandoc.header.return("Występowanie wad płodu", level=level+1))
  #  vars<-(descrs %>% filter(!stringr::str_detect(descrs$name, stringr::regex("^cong_dis"))))$name
  #  congdis<-as.character(na.omit(stringr::str_extract(vars, 'cong_dis_.*')))
  vars<-descrs$name[var_filters$`^cong_dis`]

  groups<-split(vars, ggplot2::cut_number(seq_along(vars), length(vars)/20))


  for(gr in groups){
    ans<-report_stat_opisowe(descrs %>% filter(name %in% gr), dt=dt)
    rap$add.paragraph(relationshipMatrix::add_simple_table(ans$table, caption = ans$caption))
  }


  rap$add.paragraph(pander::pandoc.header.return("Występowanie czynników ryzyka u matki", level=level+1))
  #  selvars<-as.character(na.omit(stringr::str_extract(vars, 'm_risk_f.*')))
  #  vars<-setdiff(vars, selvars)
  vars<-descrs$name[var_filters$`^m_risk_f`]
  ans<-report_stat_opisowe(descrs %>% filter(name %in% vars), dt=dt)
  rap$add.paragraph(relationshipMatrix::add_simple_table(ans$table, caption = ans$caption))


  rap$add.paragraph(pander::pandoc.header.return("Występowanie powikłań okołoporodowych", level=level+1))
  #  selvars<-as.character(na.omit(stringr::str_extract(vars, 'b_cmpl_.*')))
  #  vars<-setdiff(vars, selvars)
  vars<-descrs$name[var_filters$`^b_cmpl`]
  ans<-report_stat_opisowe(descrs %>% filter(name %in% vars), dt=dt)
  rap$add.paragraph(relationshipMatrix::add_simple_table(ans$table, caption = ans$caption))

  rap$add.paragraph(pander::pandoc.header.return("Zastosowane procedury medyczne podczas porodu", level=level+1))
  #  selvars<-as.character(na.omit(stringr::str_extract(vars, 'm_proc_.*')))
  #  vars<-setdiff(vars, selvars)
  vars<-descrs$name[var_filters$`^m_proc`]
  ans<-report_stat_opisowe(descrs %>% filter(name %in% vars), dt=dt)
  rap$add.paragraph(relationshipMatrix::add_simple_table(ans$table, caption = ans$caption))

  return(rap)
}

myfreqs<-function(dt) {
  rap<-prepare_report()
  vars<-names(dt)
  struct_df<-danesurowe::create_df_from_df_structure(dt)
  struct_df$class_letter<-map_chr(dt, danesurowe::class2vartype)
  struct_df<-struct_df %>% filter(class_letter %in% c('F'))
  varnames<-struct_df$colname
  for(varname in varnames){
    msg<-capture.output(print(summarytools::freq(x = dt[[varname]], style='rmarkdown')))
    rap$add.paragraph(msg)
  }
  save_report(rap, filename='freqs')
}
