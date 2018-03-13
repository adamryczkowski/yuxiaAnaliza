library(data.table)
dt<-readRDS('db.rds')
dt_structure<-danesurowe::create_df_from_df_structure(dt, flag_include_vartype = TRUE)
aggrt<-yuxiaAnaliza::allAggregates()
macierze_path<-system.file('macierze_analiz.xlsx', package='yuxiaAnaliza')
m<-relationshipMatrix::read_matrix(macierze_path, data.table(dt_structure), aggregate_types = aggrt)
cl<-yuxiaCharts::classify_analyses(m)
tododf<-cl$tododf
subset_df<-data.table(dplyr::filter(tododf, dispatcher %in% c('boxplot_wyliczany', 'crosstab', 'boxplot')))
subset_df<-data.table(dplyr::filter(tododf, dispatcher %in% c('boxplot')))
subset_df<-data.table(dplyr::filter(tododf, dispatcher %in% c('boxplot_wyliczany')))
doc<-relationshipMatrix::render_matrix(cellsdf=subset_df, author="Adam", title="Boxploty wyliczane",
                                       stats_dispatchers=cl$dispatchers,
                                       report_dispatchers=list(),
                                       report_functions=list(),
                                       aggregates=aggrt, filters=yuxiaAnaliza::get_filters(), df_task=dt)
doc<-relationshipMatrix::render_matrix(cellsdf=subset_df[50:50,], author="Adam", title="analiza",
                                       stats_dispatchers=cl$dispatchers,
                                       report_dispatchers=list(),
                                       report_functions=list(),
                                       aggregates=aggrt, filters=yuxiaAnaliza::get_filters(), df_task=dt)
doc$set_property('chart_debug', TRUE)
doc$set_property('chart_postprocess', FALSE)
doc$pre_render()
saveRDS(doc, file='doc_boxploty_wyliczane.rds', compress='xz')
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
