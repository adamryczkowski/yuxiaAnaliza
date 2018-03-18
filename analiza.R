library(data.table)
dt<-readRDS('db.rds')
#debugonce(danesurowe::create_df_from_df_structure)
dt_structure<-danesurowe::create_df_from_df_structure(dt, flag_include_vartype = TRUE)
aggrt<-yuxiaAnaliza::allAggregates()
macierze_path<-system.file('macierze_analiz.xlsx', package='yuxiaAnaliza')
m<-relationshipMatrix::read_matrix(macierze_path, data.table(dt_structure), aggregate_types = aggrt)
cl<-yuxiaCharts::classify_analyses(m)
tododf<-cl$tododf
subset_df<-data.table(dplyr::filter(tododf, dispatcher %in% c('boxplot_wyliczany', 'crosstab', 'boxplots', 'ts_nominal', 'ts_trend')))
subset_df<-data.table(dplyr::filter(tododf, dispatcher %in% c('boxplot')))
subset_df<-data.table(dplyr::filter(tododf, dispatcher %in% c('boxplot_wyliczany')))
subset_df<-data.table(dplyr::filter(tododf, dispatcher %in% c('crosstab')))
subset_df<-data.table(dplyr::filter(tododf, dispatcher %in% c('ts_nominal')))
subset_df<-data.table(dplyr::filter(tododf, dispatcher %in% c('ts_trend')))

dvs<-unique(unlist(subset_df$prefix2))
doc_dir<-'/home/Adama-docs/Adam/MyDocs/Statystyka/Aktywne analizy/Yu Xia/yuxia-local/raportyAR/raporty/Podstawowe'
chart_dir<-'/home/Adama-docs/Adam/MyDocs/Statystyka/Aktywne analizy/Yu Xia/yuxia-local/raportyAR/wykresy/Podstawowe'
dv<-dvs[[1]]

for(dv in dvs) {
  dv_df<-data.table(subset_df[purrr::map_lgl(subset_df$prefix2, ~ dv %in% unlist(.)),])
  a<-which(dv_df$cellnr==570)
  doc<-relationshipMatrix::render_matrix(cellsdf=dv_df[seq(20, 20),], author="Adam", title=dv,
                                         stats_dispatchers=cl$dispatchers,
                                         report_dispatchers=list(),
                                         chart_foldername=pathcat::path.cat(chart_dir, paste0('ch_', which(dv %in% dvs))),
                                         report_functions=list(), header_depth_offset=4, flag_add_chapter_for_each_cell = FALSE,
                                         aggregates=aggrt, filters=yuxiaAnaliza::get_filters(), df_task=dt)
  doc$set_property('chart_debug', TRUE)
  doc$pre_render()
  saveRDS(doc, file=pathcat::path.cat(doc_dir, paste0('ch_', which(dv %in% dvs), '.rds')), compress='xz')
  pandoc<-pander::Pandoc$new()
  doc$render(pandoc)
  save_report(pandoc,  filename = pathcat::path.cat(doc_dir, paste0('ch_', which(dv %in% dvs))))
}

doc<-relationshipMatrix::render_matrix(cellsdf=subset_df, author="Adam", title="Time series vs nominal",
                                       stats_dispatchers=cl$dispatchers,
                                       report_dispatchers=list(),
                                       report_functions=list(), header_depth_offset=4, flag_add_chapter_for_each_cell = FALSE,
                                       aggregates=aggrt, filters=yuxiaAnaliza::get_filters(), df_task=dt)
doc<-relationshipMatrix::render_matrix(cellsdf=subset_df[c(1),], author="Adam", title="analiza",
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
