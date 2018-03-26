library(data.table)
dt<-readRDS('db.rds')
#debugonce(danesurowe::create_df_from_df_structure)
template<-system.file('D-rat.dotx', package = 'yuxiaAnaliza')
dt_structure<-danesurowe::create_df_from_df_structure(dt, flag_include_vartype = TRUE)
aggrt<-yuxiaAnaliza::allAggregates()
macierze_path<-system.file('macierze_analiz.xlsx', package='yuxiaAnaliza')
m<-relationshipMatrix::read_matrix(macierze_path, data.table(dt_structure), aggregate_types = aggrt)
cl<-yuxiaCharts::classify_analyses(m)
tododf<-cl$tododf
#subset_df<-data.table(dplyr::filter(tododf, dispatcher %in% c('boxplot_wyliczany', 'crosstab', 'boxplot', 'ts_nominal', 'ts_trend')))
subset_df<-data.table(dplyr::filter(tododf, dispatcher %in% c('crosstab', 'ts_nominal', 'ts_trend')))
#subset_df<-data.table(dplyr::filter(tododf, dispatcher %in% c('boxplot')))
#subset_df<-data.table(dplyr::filter(tododf, dispatcher %in% c('boxplot_wyliczany')))
#subset_df<-data.table(dplyr::filter(tododf, dispatcher %in% c('crosstab')))
#subset_df<-data.table(dplyr::filter(tododf, dispatcher %in% c('ts_nominal')))
#subset_df<-data.table(dplyr::filter(tododf, dispatcher %in% c('ts_trend')))

dvs<-unique(unlist(subset_df$prefix2))
doc_dir<-'/home/Adama-docs/Adam/MyDocs/Statystyka/Aktywne analizy/Yu Xia/yuxia-local/raportyAR/raporty/Podstawowe/Sawdust'
chart_dir<-'/home/Adama-docs/Adam/MyDocs/Statystyka/Aktywne analizy/Yu Xia/yuxia-local/raportyAR/wykresy/Podstawowe'

dv_dfs<-purrr::map(dvs, function(dv) data.table(subset_df[purrr::map_lgl(subset_df$prefix2, ~ dv %in% unlist(.)),]))

purrr::map_int(dv_dfs, ~nrow(.))

dv_df10_groups<-split(dv_dfs[[10]], cut(seq_len(nrow(dv_dfs[[10]])), 3, FALSE))
dvs<-c(dvs[1:9], paste0(rep(dvs[[10]], 3)," ", 1:3))

list_tododf<-c(dv_dfs[1:9], dv_df10_groups)
list_tododf<-dv_dfs
i<-10
for(i in seq(3, length(list_tododf))) {
  dv_df<-list_tododf[[i]]
  doc<-relationshipMatrix::render_matrix(cellsdf=dv_df, author="Adam", title=dvs[[i]],
                                         stats_dispatchers=cl$dispatchers,
                                         report_dispatchers=list(),
                                         chart_foldername=pathcat::path.cat(chart_dir, paste0('ch_', i)),
                                         report_functions=list(), header_depth_offset=3, flag_add_chapter_for_each_cell = FALSE,
                                         aggregates=aggrt, filters=yuxiaAnaliza::get_filters(), df_task=dt)
  doc<-filter_doc(doc, exclude_tags =c('mozaic_plot', 'crosstab_test', 'crosstab'))
  doc$set_property('chart_debug', TRUE)
  doc$pre_render()
  saveRDS(doc, file=pathcat::path.cat(doc_dir, paste0('ch_', i, '.rds')), compress='xz')
  pandoc<-pander::Pandoc$new()
  doc$render(pandoc)

  save_report(pandoc, template = template,  filename = pathcat::path.cat(doc_dir, paste0('ch_', i)))
}
#a1<-tododf %>% filter( dv.vartype=='F' & iv.vartype=='F' & ((iv.f.o.b==2 & dv.f.o.b==1) | (dv.f.o.b==2 & iv.f.o.b==1 )))
doc<-relationshipMatrix::render_matrix(cellsdf=as.data.table(dv_df[24,]), author="Adam", title="Time series vs nominal",
                                       stats_dispatchers=cl$dispatchers,
                                       report_dispatchers=list(),
                                       report_functions=list(), header_depth_offset=4, flag_add_chapter_for_each_cell = FALSE,
                                       aggregates=aggrt, filters=yuxiaAnaliza::get_filters(), df_task=dt)
doc<-relationshipMatrix::render_matrix(cellsdf=tododf[716,], author="Adam", title="analiza",
                                       stats_dispatchers=cl$dispatchers,
                                       report_dispatchers=list(),
                                       report_functions=list(), flag_add_chapter_for_each_cell = FALSE,
                                       aggregates=aggrt, filters=yuxiaAnaliza::get_filters(), df_task=dt)
doc<-relationshipMatrix::render_matrix(cellsdf=subset_df[c(46, 47, 49,  96, 97,98,99,100),], author="Adam", title="analiza",
                                       stats_dispatchers=cl$dispatchers,
                                       report_dispatchers=list(),
                                       report_functions=list(), flag_add_chapter_for_each_cell = FALSE,
                                       aggregates=aggrt, filters=yuxiaAnaliza::get_filters(), df_task=dt)
doc$set_property('chart_debug', TRUE)
doc$set_property('chart_postprocess', FALSE)
doc$pre_render()
saveRDS(doc, file='doc_crosstab.rds', compress='xz')
pandoc<-pander::Pandoc$new()
doc<-readRDS('doc2.rds')
rm(dt)
rm(cl)
gc()
#  debugonce(doc$render)
doc$render(pandoc)
unlink('/tmp/cos.docx')
unlink('/tmp/cos.md')
save_report(pandoc, filename = '/tmp/cos')
