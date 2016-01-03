# sources table output -----------------------------
output$sourcesTable <- DT::renderDataTable({
  sourcesTable <- TCMN_sources
  return(sourcesTable)
}) # disable all the table fancy options  

# indicators table output -----------------------------
output$indicatorsTable <- DT::renderDataTable({
  indicatorsTable <- TCMN_indic
  return(indicatorsTable)
}) # disable all the table fancy options  

# sources table output -----------------------------
output$countriesTable <- DT::renderDataTable({
  countriesTable <- countries
  return(countriesTable)
}) # disable all the table fancy options  

