# sources table output -----------------------------
output$sourcesTable <- DT::renderDataTable({
  sourcesTable <- TCMN_sources
  return(sourcesTable)
})

# indicators table output -----------------------------
output$indicatorsTable <- DT::renderDataTable({
  indicatorsTable <- TCMN_indic
  return(indicatorsTable)
}) 

# countries table output -----------------------------
output$countriesTable <- DT::renderDataTable({
  countriesTable <- countries
  return(countriesTable)
}) 

