library(data.table)
dt<-readRDS('db.rds')
dt_structure<-danesurowe::create_df_from_df_structure(dt, flag_include_vartype = TRUE)
aggrt<-allAggregates()
macierze_path<-system.file('macierze_analiz.xlsx', package='yuxiaAnaliza')
m<-relationshipMatrix::read_matrix(macierze_path, data.table(dt_structure), aggregate_types = aggrt)
cl<-yuxiaCharts::classify_analyses(m)
tododf<-cl$tododf
subset_df<-data.table(dplyr::filter(tododf, dispatcher %in% c('boxplot_wyliczany', 'crosstab', 'boxplot')))
doc<-relationshipMatrix::render_matrix(cellsdf=subset_df, author="Adam", title="analiza",
                                       stats_dispatchers=cl$dispatchers,
                                       report_dispatchers=list(),
                                       report_functions=list(),
                                       aggregates=aggrt, filters=get_filters(), df_task=dt)
