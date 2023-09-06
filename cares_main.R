library(httr)
library(plyr)
library(dplyr)
library(tidyr)
library(xlsx)

# Set Download directory: Alterar aqui
download_dir <- '~/Documents/cares/'

source(file = 'misc_functions.R')
api.dhis.base.url <- "https://mail.ccsaude.org.mz:5459/"

dhis2.username   <- "ccs_datim"
dhis2.password   <-'Kxw59wApRccGEfW_'

program.id                      <- 'x8spmDYgDso' # Monitoria de Pacientes com covid 19         
org.unit                        <- 'ebcn8hWYrg3' # CIDADE DE MAPUTO


unidadesSanitarias <- getOrganizationUnits(api.dhis.base.url,org.unit)
dataElements <- getDataElements(api.dhis.base.url)
dataElements$name <- as.character(dataElements$name)
dataElements$shortName <- as.character(dataElements$shortName)
dataElements$id <- as.character(dataElements$id)


# Program stages
programStages <- getProgramStages(api.dhis.base.url,program.id)
programStages$name <- as.character(programStages$name)
programStages$id <- as.character(programStages$id)


program.stage.id.rastreio.recrut         <- 'LhQHoaXYgut'   #  Rastreio e recrutamento de pacientes para os cuidados domiciliários
program.stage.id.monitorizacao           <- 'WWBRzJmmSeI'   #  Monitorização dos Cuidados-Domiciliários do Paciente 
program.stage.id.resultados              <- 'AVVCk7Wb7rL'   # Resultados de Pacientes com COVID-19 Necessitando de Cuidados Especializados  AVVCk7Wb7rL
program.stage.id.condicoes.pos.covid     <- 'FRYErSJFtnY'   #  Condições pós-COVID FRYErSJFtnY


 
# Get program enrollment: pi.dhis.base.url ,org.unit,program.id
cares_enrollments <- getEnrollments(api.dhis.base.url ,org.unit, program.id )

# get TrackedInstances: api.dhis.base.url,program.id,org.unit
trackedInstances <- getTrackedInstances(api.dhis.base.url,program.id,org.unit  )

# GET ALL EVENTS FROM PROGRAM STAGE ( Rastreio e recrutamento de pacientes para os cuidados domiciliários) 
# paramentros api.dhis.base.url,org.unit,program.id,program.stage.id
events_rastreio_recrutamento <- getTrackerEvents(api.dhis.base.url,org.unit,program.id,program.stage.id.rastreio.recrut)

if (nrow(events_rastreio_recrutamento) > 0 ){
  
events_rastreio_recrutamento$dataElement <- as.character(events_rastreio_recrutamento$dataElement)
events_rastreio_recrutamento$programStage <- as.character(events_rastreio_recrutamento$programStage)
events_rastreio_recrutamento$dataElement <- sapply(events_rastreio_recrutamento$dataElement, findDataElementByID)
events_rastreio_recrutamento$programStage <- sapply(events_rastreio_recrutamento$programStage, findProgramStageByID)
events_rastreio_recrutamento$trackedInstance <- NULL
events_rastreio_recrutamento$trackedEntity <- NULL
events_rastreio_recrutamento$providedElsewhere <- NULL
events_rastreio_recrutamento$created <- NULL
events_rastreio_recrutamento$dueDate <- NULL
events_rastreio_recrutamento$orgUnitName <- NULL
events_rastreio_recrutamento$trackedEntityIntance <- NULL
events_rastreio_recrutamento$eventDate <- NULL
events_rastreio_recrutamento$href <- NULL
events_rastreio_recrutamento$enrollmentStatus <- NULL
events_rastreio_recrutamento$dataElementName <- NULL
events_rastreio_recrutamento$deleted <- NULL
events_rastreio_recrutamento$programStage <- NULL
events_rastreio_recrutamento$program <- NULL
events_rastreio_recrutamento$attributeCategoryOptions <- NULL
events_rastreio_recrutamento$attributeOptionCombo <- NULL
events_rastreio_recrutamento$updatedAt <- NULL
events_rastreio_recrutamento$createdAt <- substring(events_rastreio_recrutamento$createdAt, first = 0 , last = 10)
events_rastreio_recrutamento$status <- NULL
events_rastreio_recrutamento$orgUnit <- NULL

events_rastreio_recrutamento <- left_join(events_rastreio_recrutamento,cares_enrollments, by="enrollment") %>% select(!ends_with("edAt.y")) %>%
  rename( createdAt=createdAt.x)

events_rastreio_recrutamento <-  left_join(events_rastreio_recrutamento,trackedInstances[,c(1,3,4)],by="trackedEntity") 


events_rastreio_recrutamento$enrollment <- NULL
events_rastreio_recrutamento$deleted <- NULL
events_rastreio_recrutamento$orgUnit <- NULL
events_rastreio_recrutamento$enrolledAt <- NULL
events_rastreio_recrutamento$org_unit <- NULL
events_rastreio_recrutamento$orrgUnitName <- NULL


events_rastreio_recrutamento   <- spread(data = events_rastreio_recrutamento,key =dataElement,value =  value)
}

# GET ALL EVENTS FROM PROGRAM STAGE :(Monitorização dos Cuidados-Domiciliários do Paciente )

events_monitorizacao_cuidados <- getTrackerEvents(api.dhis.base.url,org.unit,program.id,program.stage.id.monitorizacao)

if(nrow(events_monitorizacao_cuidados) > 0 ) { 
events_monitorizacao_cuidados$dataElement <- as.character(events_monitorizacao_cuidados$dataElement)
events_monitorizacao_cuidados$programStage <- as.character(events_monitorizacao_cuidados$programStage)
events_monitorizacao_cuidados$dataElement <- sapply(events_monitorizacao_cuidados$dataElement, findDataElementByID)
events_monitorizacao_cuidados$programStage <- sapply(events_monitorizacao_cuidados$programStage, findProgramStageByID)
events_monitorizacao_cuidados$trackedInstance <- NULL
events_monitorizacao_cuidados$trackedEntity <- NULL
events_monitorizacao_cuidados$providedElsewhere <- NULL
events_monitorizacao_cuidados$created <- NULL
events_monitorizacao_cuidados$dueDate <- NULL
events_monitorizacao_cuidados$orgUnitName <- NULL
events_monitorizacao_cuidados$trackedEntityIntance <- NULL
events_monitorizacao_cuidados$eventDate <- NULL
events_monitorizacao_cuidados$href <- NULL
events_monitorizacao_cuidados$enrollmentStatus <- NULL
events_monitorizacao_cuidados$dataElementName <- NULL
events_monitorizacao_cuidados$deleted <- NULL
events_monitorizacao_cuidados$programStage <- NULL
events_monitorizacao_cuidados$program <- NULL
events_monitorizacao_cuidados$attributeCategoryOptions <- NULL
events_monitorizacao_cuidados$attributeOptionCombo <- NULL
events_monitorizacao_cuidados$updatedAt <- NULL
events_monitorizacao_cuidados$createdAt <- substring(events_monitorizacao_cuidados$createdAt, first = 0 , last = 10)
events_monitorizacao_cuidados$status <- NULL
events_monitorizacao_cuidados$orgUnit <- NULL

events_monitorizacao_cuidados <- left_join(events_monitorizacao_cuidados,cares_enrollments, by="enrollment") %>% select(!ends_with("edAt.y")) %>%
  rename( createdAt=createdAt.x)

events_monitorizacao_cuidados <-  left_join(events_monitorizacao_cuidados,trackedInstances[,c(1,3,4)],by="trackedEntity") 


events_monitorizacao_cuidados$enrollment <- NULL
events_monitorizacao_cuidados$deleted <- NULL
events_monitorizacao_cuidados$orgUnit <- NULL
events_monitorizacao_cuidados$enrolledAt <- NULL
events_monitorizacao_cuidados$org_unit <- NULL
events_monitorizacao_cuidados$orrgUnitName <- NULL

events_monitorizacao_cuidados   <- spread(data = events_monitorizacao_cuidados,key =dataElement,value =  value)

}

# GET ALL EVENTS FROM PROGRAM STAGE :(Resultados de Pacientes com COVID-19 Necessitando de Cuidados Especializados  AVVCk7Wb7rL )

events_resultados<- getTrackerEvents(api.dhis.base.url,org.unit,program.id,program.stage.id.resultados)

if(nrow(events_resultados) > 0) {
  

events_resultados$dataElement <- as.character(events_resultados$dataElement)
events_resultados$programStage <- as.character(events_resultados$programStage)
events_resultados$dataElement <- sapply(events_resultados$dataElement, findDataElementByID)
events_resultados$programStage <- sapply(events_resultados$programStage, findProgramStageByID)
events_resultados$trackedInstance <- NULL
events_resultados$trackedEntity <-NULL
events_resultados$providedElsewhere <- NULL
events_resultados$created <- NULL
events_resultados$dueDate <- NULL
events_resultados$orgUnitName <- NULL
events_resultados$trackedEntityIntance <- NULL
events_resultados$eventDate <- NULL
events_resultados$href <- NULL
events_resultados$enrollmentStatus <- NULL
events_resultados$dataElementName <- NULL
events_resultados$deleted <- NULL
events_resultados$programStage <- NULL
events_resultados$program <- NULL
events_resultados$attributeCategoryOptions <- NULL
events_resultados$attributeOptionCombo <- NULL
events_resultados$updatedAt <- NULL
events_resultados$createdAt <- substring(events_resultados$createdAt, first = 0 , last = 10)
events_resultados$status <- NULL
events_resultados$orgUnit <- NULL

events_resultados <- left_join(events_resultados,cares_enrollments, by="enrollment") %>% select(!ends_with("edAt.y")) %>%
  rename( createdAt=createdAt.x)


events_resultados <-  left_join(events_resultados,trackedInstances[,c(1,3,4)],by="trackedEntity") 


events_resultados$enrollment <- NULL
events_resultados$deleted <- NULL
events_resultados$orgUnit <- NULL
events_resultados$enrolledAt <- NULL
events_resultados$org_unit <- NULL
events_resultados$orrgUnitName <- NULL
events_resultados   <- spread(data = events_resultados,key =dataElement,value =  value)

}


# GET ALL EVENTS FROM PROGRAM STAGE :(Monitorização dos Cuidados-Domiciliários do Paciente )
events_condicoes_pos_covid<- getTrackerEvents(api.dhis.base.url,org.unit,program.id,program.stage.id.condicoes.pos.covid)

if(nrow(events_condicoes_pos_covid) > 0) {
events_condicoes_pos_covid$dataElement <- as.character(events_condicoes_pos_covid$dataElement)
events_condicoes_pos_covid$programStage <- as.character(events_condicoes_pos_covid$programStage)
events_condicoes_pos_covid$dataElement <- sapply(events_condicoes_pos_covid$dataElement, findDataElementByID)
events_condicoes_pos_covid$programStage <- sapply(events_condicoes_pos_covid$programStage, findProgramStageByID)
events_condicoes_pos_covid$trackedInstance <- NULL
events_condicoes_pos_covid$trackedEntity <- NULL
events_condicoes_pos_covid$providedElsewhere <- NULL
events_condicoes_pos_covid$created <- NULL
events_condicoes_pos_covid$dueDate <- NULL
events_condicoes_pos_covid$orgUnitName <- NULL
events_condicoes_pos_covid$trackedEntityIntance <- NULL
events_condicoes_pos_covid$eventDate <- NULL
events_condicoes_pos_covid$href <- NULL
events_condicoes_pos_covid$enrollmentStatus <- NULL
events_condicoes_pos_covid$dataElementName <- NULL
events_condicoes_pos_covid$deleted <- NULL
events_condicoes_pos_covid$programStage <- NULL
events_condicoes_pos_covid$program <- NULL
events_condicoes_pos_covid$attributeCategoryOptions <- NULL
events_condicoes_pos_covid$attributeOptionCombo <- NULL
events_condicoes_pos_covid$updatedAt <- NULL
events_condicoes_pos_covid$createdAt <- substring(events_condicoes_pos_covid$createdAt, first = 0 , last = 10)
events_condicoes_pos_covid$status <- NULL
events_condicoes_pos_covid$orgUnit <- NULL

events_condicoes_pos_covid <- left_join(events_condicoes_pos_covid,cares_enrollments, by="enrollment") %>% select(!ends_with("edAt.y")) %>%
  rename( createdAt=createdAt.x)


events_condicoes_pos_covid <-  left_join(events_condicoes_pos_covid,trackedInstances[,c(1,3,4)],by="trackedEntity") 


events_condicoes_pos_covid$enrollment <- NULL
events_condicoes_pos_covid$deleted <- NULL
events_condicoes_pos_covid$orgUnit <- NULL
events_condicoes_pos_covid$enrolledAt <- NULL
events_condicoes_pos_covid$org_unit <- NULL
events_condicoes_pos_covid$orrgUnitName <- NULL

events_condicoes_pos_covid   <- spread(data = events_condicoes_pos_covid,key =dataElement,value =  value)

}


# Create a new Excel workbook
excel_file <- createWorkbook()

# Add data1 to the first sheet with the name "Sheet1"
sheet1 <- createSheet(excel_file, sheetName = "pacientes")
addDataFrame(trackedInstances, sheet1)

# Add data2 to the second sheet with the name "Sheet2"
sheet2 <- createSheet(excel_file, sheetName = "eventos_rastreio")
addDataFrame(events_rastreio_recrutamento, sheet2)


sheet3 <- createSheet(excel_file, sheetName = "eventos_monitorizacao")
addDataFrame(events_monitorizacao_cuidados, sheet3)

sheet4 <- createSheet(excel_file, sheetName = "eventos_condicoes_pos_covid")
addDataFrame(events_condicoes_pos_covid, sheet4)


# Save the workbook to a file
saveWorkbook(excel_file, file = paste0(download_dir,"cares.xlsx") )

# Excluir linhas duplicadas
#s   <- s[!duplicated(events_monitorizacao_cuidados), ]
#s           <- tidyr::spread(data = s,key =dataElement,value =  value)
