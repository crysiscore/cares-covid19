
#' getProgramStages ->  Busca os estagios de um programa
#' @param  base.url base.url 
#' @param  program.id program.id
#' @examples
#' getProgramStages("YiMCSzx23b")
getProgramStages <- function(base.url, program.id) {
  ## location pode ser distrito , provincia
  url <-
    paste0(
      base.url,
      paste0(
        "api/programs/",
        program.id,
        "/programStages?fields=id,name"
      )
    )
  r <- content(GET(url, authenticate(dhis2.username, dhis2.password)), as = "parsed")
  do.call(rbind.data.frame, r$programStages)
}

getDataElements <- function(base.url) {
  url <-
    paste0(base.url,
           "api/dataElements?fields=id,name,shortName&paging=false")
  r <- content(GET(url, authenticate(dhis2.username, dhis2.password)), as = "parsed")
  do.call(rbind.data.frame, r$dataElements)
}

getEnrollments <- function(base.url,org.unit, program.id ) {
  
  url <-    paste0( base.url,paste0("api/tracker/enrollments?orgUnit=", org.unit, "&program=",program.id, "&ouMode=DESCENDANTS", "&skipPaging=TRUE"  )  )
  
  
  r <- content(GET(url, authenticate(dhis2.username, dhis2.password), timeout(60000)), as = "parsed")
  
  # criar um df vazio para armazenar os Enrrolments
  df_te <- data.frame(matrix(ncol = 8, nrow =  length(r$instances) ))
  x <- c("enrollment", "trackedEntity", "status" , "createdAt", "deleted", "orgUnit" , "orgUnitName", "enrolledAt")
  colnames(df_te) <- x
  
  
  if(typeof(r)=="list" && length(r$instances)>0) {
    
    
    
    # Iterar a lista para extrair os attributes
    for (i in 1:length(r$instances)) {
      
      
      enrollment <- r$instances[[i]]
      enrollment_id <- enrollment$enrollment
      tracked_entity_id <- enrollment$trackedEntity
      status <- enrollment$status
      createdAt <- enrollment$createdAt
      enrolledAt <- enrollment$enrolledAt
      deleted <- enrollment$deleted
      org_unit <- enrollment$orgUnit
      orgUnitName <- enrollment$orgUnitName
      
      
      # prencher o df df_te
      df_te$enrollment[i] <-  enrollment_id 
      df_te$trackedEntity[i] <- tracked_entity_id
      df_te$status[i] <- status
      df_te$createdAt[i] <-  createdAt
      df_te$deleted[i] <- deleted
      df_te$orgUnit[i] <- org_unit
      df_te$orgUnitName[i] <- orgUnitName
      df_te$enrolledAt[i] <- enrolledAt
      
    }
    
  }
  
  
  df_te
  
}

getTrackedInstances <- function(base.url, program.id,org.unit) {
  ## location pode ser distrito , provincia
  url <-
    paste0(
      base.url,
      paste0(
        "api/tracker/trackedEntities?orgUnit=",
        org.unit,
        "&program=",
        program.id,
        "&ouMode=DESCENDANTS",
        "&skipPaging=TRUE"
      )
    )
  r <- content(GET(url, authenticate(dhis2.username, dhis2.password),timeout(60000)), as = "parsed")
  

  
  
  if(typeof(r)=="list" && length(r$instances)>0) {
    
    # criar um df vazio para armazenar os TE
    df_te <- data.frame(matrix(ncol = 13, nrow =  length(r$instances) ))
    
    x <- c("trackedEntity","org_unit", "id_paciente", "nome_paciente", "data_nascimento", "sexo", "telefone" , "assinatura_activ" ,"vacina_alguma_vez", "raca","primeira_dose", "seg_dose","endereco")
    
    colnames(df_te) <- x
    
    
    # Iterar a lista para extrair os attributes
    for (i in 1:length(r$instances)) {
      
      
      tracked_entity <- r$instances[[i]]
      tracked_entity_id <- tracked_entity$trackedEntity
      org_unit <- tracked_entity$orgUnit
      id_paciente <- ""
      nome_paciente <- ""
      data_nascimento <- ""
      sexo <- ""
      telefone <- ""
      assinatura_activ <- ""
      vacina_alguma_vez <- ""
      raca <- ""
      primeira_dose <- ""
      seg_dose <- ""
      endereco <- ""

      
      list_atributos <- tracked_entity$attributes
      
      for (v in 1:length(list_atributos)) {
        
        atributo <- list_atributos[[v]]
        displayName <- atributo$displayName
        value <- atributo$value
        attribute_id <- atributo$attribute
        
        if(attribute_id=="tDOAHRHvnHP"){
          data_nascimento <- value
          
        } else  if(attribute_id=="qMsgcUH3acl"){
          sexo <- value
          
        } else if(attribute_id=="RVdj48fCgn8"){
          telefone <- value
          
        } else if(attribute_id=="BrqoPlsW2bF"){
          nome_paciente<- value
          
        } else if(attribute_id=="NRQhZLrSfd9"){
          id_paciente<- value
          
        } else if(attribute_id=="b9YVSnJqrAh"){
          endereco <- value
          
        }else if(attribute_id=="LOzAI726l8T"){
          assinatura_activ <- value
          
        }else if(attribute_id=="VG7Y5wHQFQ1"){
          vacina_alguma_vez <- value
          
        }else if(attribute_id=="x9OHhsnFp3w"){
          raca <- value
          
        }else if(attribute_id=="gy06bMam6c6"){
          primeira_dose <- value
          
        }else if(attribute_id=="jNjhZ9EtXSg"){
          seg_dose <- value
          
        }
      }
      
   
      df_te$trackedEntity[i] <- tracked_entity_id
      df_te$org_unit[i]  <- org_unit
      
      df_te$id_paciente[i] <-  id_paciente 
      df_te$nome_paciente[i] <- nome_paciente
      df_te$sexo[i] <- sexo
      df_te$data_nascimento[i] <-  data_nascimento
      df_te$telefone[i] <- telefone
      df_te$endereco[i] <- endereco
      df_te$assinatura_activ[i] <- assinatura_activ
      df_te$vacina_alguma_vez[i] <- vacina_alguma_vez
      df_te$raca[i] <- raca
      df_te$primeira_dose[i] <- primeira_dose
      df_te$seg_dose[i] <- seg_dose
      
    }
    
    return( df_te)
  } else {
    return(data.frame())
  }
  
}



getOrganizationUnits <- function(base.url, location_id) {
  ## location pode ser distrito , provincia
  url <-
    paste0(
      base.url,
      paste0(
        "api/organisationUnits/",
        location_id,
        "?includeDescendants=true&level=3&fields=id,name,shortName&paging=false"
      )
    )
  r <- content(GET(url, authenticate(dhis2.username, dhis2.password)), as = "parsed")
  do.call(rbind.data.frame, r$organisationUnits)
}

getTrackerEvents <- function(base.url,org.unit,program.id, program.stage.id){
  url <-
    paste0(base.url,
           paste0(
             "api/tracker/events.json?orgUnit=",
             org.unit,
             '&program=',
             program.id,
             '&programStage=',
             program.stage.id,
             "&ouMode=DESCENDANTS&skipPaging=true"
           )
    )
  df_event_values  <- data.frame()
  # Get the data
  r2 <- content(GET(url, authenticate(dhis2.username, dhis2.password),timeout(600000)),as = "parsed")
  
  if(typeof(r2)=="list" && length(r2$instances)>0) {
    
    vec_size_datavalues <- c()
    for(event in r2$instances){
      
      size <- length( event$dataValues)
      vec_size_datavalues <-  c(vec_size_datavalues,size)
    }
    
    # primeiro evento da lista com maior  nr de colunas
    index = which.max(vec_size_datavalues)
    #event_metadata_col_names <- names(r2$instances[[index]])
    #event_values_col_names   <- names(r2$instances[[index]]$dataValues[[1]])
    # Quantidade de variaveis de cada evento
    #length(r2$instances[[index]]$dataValues)
    #df_events_col_names <- c(event_metadata_names, event_values_col_names)
    
    # inicializar o df 
    df_event_values <- do.call(rbind.data.frame,r2$instances[[1]]$dataValues)
    df_event_values <- df_event_values[1,]
    df_event_values$storedBy <- ""
    df_event_values$programStage <- ""
    df_event_values$status <- ""
    df_event_values$created <- ""
    #df_event_values$notes <- ""
    df_event_values$dueDate <- ""
    df_event_values$orgUnit <- ""
    df_event_values$orgUnitName <- ""
    df_event_values$program <- ""
    df_event_values$trackedEntityIntance <- ""
    df_event_values$eventDate <- ""
    df_event_values$deleted <- ""
    df_event_values$href <- ""
    df_event_values$enrollment <- ""
    df_event_values$attributeCategoryOptions <- ""
    df_event_values$attributeOptionCombo <- ""
    df_event_values$event <- ""
    df_event_values$enrollmentStatus <- ""
    df_event_values <- df_event_values[0,]
    
    #  Junta todos  dataValues  de todos  eventos
    
    for (index in 1:length(r2$instances)) {
      
      if(length(r2$instances[[index]]$dataValues)>0) {
        
        temp <-  do.call(rbind.data.frame,r2$instances[[index]]$dataValues)
        
        
        temp$storedBy <-r2$instances[[index]]$storedBy
        temp$programStage <-r2$instances[[index]]$programStage
        temp$status <- r2$instances[[index]]$status
        temp$created <- r2$instances[[index]]$created
        
        # Existem eventos sem notas
        #if(length(r2$instances[[index]]$notes)>0){
        #
        #  temp$notes <- r2$instances[[index]]$notes
        #}
        
        temp$dueDate <- r2$instances[[index]]$dueDate
        temp$orgUnit <- r2$instances[[index]]$orgUnit
        temp$orrgUnitName <- r2$instances[[index]]$orgUnitName
        temp$program <- r2$instances[[index]]$program
        temp$trackedEntityIntance <- r2$instances[[index]]$trackedEntityInstance
        temp$eventDate <- r2$instances[[index]]$eventDate
        temp$deleted <- r2$instances[[index]]$deleted
        temp$href <- r2$instances[[index]]$href
        temp$enrollment <- r2$instances[[index]]$enrollment
        temp$attributeCategoryOptions <- r2$instances[[index]]$attributeCategoryOptions
        temp$attributeOptionCombo <- r2$instances[[index]]$attributeOptionCombo
        temp$event <- r2$instances[[index]]$event
        temp$enrollmentStatus <- r2$instances[[index]]$enrollmentStatus
        
        
        df_event_values <- rbind.fill(df_event_values, temp)
      }
    }
    
  }
  
  df_event_values
  
  
}

findDataElementByID <- function(id){
  
  dataElement <- dataElements[which(dataElements$id==id),]
  as.character(dataElement$name)
}


findTrackedInstanceByID <- function(id){
  
  trackedInstance <- trackedInstance[which(trackedInstances$Instance==id),]
  as.character(trackedInstance$name)
}


findProgramStageByID <- function(id){
  
  stage <- programStages[which(programStages$id==id),]
  as.character(stage$name)
}


getStageNameByID <- function(stage.id, df.stages){
  
  stage_name <- df.stages$name[which(df.stages$id==stage.id)]
  stage_name
}
