library(data.table)
dt<-readRDS('db.rds')
#debugonce(danesurowe::create_df_from_df_structure)
dt_structure<-danesurowe::create_df_from_df_structure(dt, flag_include_vartype = TRUE)
aggrt<-yuxiaAnaliza::allAggregates()
macierze_path<-system.file('macierze_analiz.xlsx', package='yuxiaAnaliza')
m<-relationshipMatrix::read_matrix(macierze_path, data.table(dt_structure), aggregate_types = aggrt)
cl<-yuxiaCharts::classify_analyses(m)
tododf<-cl$tododf
subset_df<-data.table(dplyr::filter(tododf, dispatcher %in% c('boxplot_wyliczany', 'crosstab', 'boxplot')))
subset_df<-data.table(dplyr::filter(tododf, dispatcher %in% c('boxplot')))
subset_df<-data.table(dplyr::filter(tododf, dispatcher %in% c('boxplot_wyliczany')))
subset_df<-data.table(dplyr::filter(tododf, dispatcher %in% c('crosstab')))
subset_df<-data.table(dplyr::filter(tododf, dispatcher %in% c('ts_nominal')))

<<<<<<< HEAD
doc<-relationshipMatrix::render_matrix(cellsdf=subset_df, author="Adam", title="Nominal trend",
=======
doc<-relationshipMatrix::render_matrix(cellsdf=subset_df, author="Adam", title="Time series vs nominal",
>>>>>>> 01f27a8e9687268c79e9053b6a25292a7aa1d090
                                       stats_dispatchers=cl$dispatchers,
                                       report_dispatchers=list(),
                                       report_functions=list(), header_depth_offset=4, flag_add_chapter_for_each_cell = FALSE,
                                       aggregates=aggrt, filters=yuxiaAnaliza::get_filters(), df_task=dt)
doc<-relationshipMatrix::render_matrix(cellsdf=subset_df[c(46),], author="Adam", title="analiza",
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
<<<<<<< HEAD
saveRDS(doc, file='doc_crosstab.rds', compress='xz')
=======
saveRDS(doc, file='doc_boxploty.rds', compress='xz')
>>>>>>> 01f27a8e9687268c79e9053b6a25292a7aa1d090
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
