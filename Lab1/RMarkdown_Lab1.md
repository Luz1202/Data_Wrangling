Lab1: Solucion
================
Luz Arevalo
2022-08-03

## Problema 1

PASO 1: Obtuve la lista de archivos que deseo unificar

``` r
todos <- list.files(path=getwd(),pattern = '.xls')
```

PASO 2: Creé un data.frame padre con las variables definidas

``` r
full <- data.frame(COD_VIAJE=double(),
                   CLIENTE=character(),
                   UBICACION=double(),
                   CANTIDAD=double(),
                   PILOTO=character(),
                   Q=double(),
                   CREDITO=double(),
                   UNIDAD=character(),
                   Fecha=character())
```

PASO 3: Formé un loop que ejecutar diversas tareas en cada archivo.

``` r
for (i in 1:length(todos)){
  
    #Extraemos la fecha del nombre del archivo
    fecha <- tools:: file_path_sans_ext(basename(paste(getwd(),
                                                       todos[i],
                                                       sep = '//')))
  
    #Leemos el archivo
    temporal <- separate(readxl::read_excel(todos[i]),
                         CLIENTE,
                         into=c('CLIENTE',NULL),
                         sep = '/')
  
    #Agregamos la columna 'fecha' que repita la fecha obtenida
    temporal$Fecha <-
      rep(fecha,sum(plyr::count(temporal,names(temporal)[1])['freq']))
  
    #Creamos un nuevo data.frame con la información de cada archivo
    df <- data.frame(COD_VIAJE=temporal$COD_VIAJE,
                        CLIENTE=temporal$CLIENTE,
                        UBICACION=temporal$UBICACION,
                        CANTIDAD=temporal$CANTIDAD,
                        PILOTO=temporal$PILOTO,
                        Q=temporal$Q,
                        CREDITO=temporal$CREDITO,
                        UNIDAD=temporal$UNIDAD,
                        Fecha=temporal$Fecha)
  
    #Unimos el data.frame nuevo al padre
    full<-full_join(full,df)
}
str(full)
```

    ## 'data.frame':    2180 obs. of  9 variables:
    ##  $ COD_VIAJE: num  1e+07 1e+07 1e+07 1e+07 1e+07 ...
    ##  $ CLIENTE  : chr  "EL PINCHE OBELISCO " "TAQUERIA EL CHINITO |||Faltante" "TIENDA LA BENDICION " "TAQUERIA EL CHINITO" ...
    ##  $ UBICACION: num  76002 76002 76002 76002 76001 ...
    ##  $ CANTIDAD : num  1200 1433 1857 339 1644 ...
    ##  $ PILOTO   : chr  "Fernando Mariano Berrio" "Hector Aragones Frutos" "Pedro Alvarez Parejo" "Angel Valdez Alegria" ...
    ##  $ Q        : num  300 358.2 464.2 84.8 411 ...
    ##  $ CREDITO  : num  30 90 60 30 30 30 90 60 30 90 ...
    ##  $ UNIDAD   : chr  "Camion Grande" "Camion Grande" "Camion Grande" "Panel" ...
    ##  $ Fecha    : chr  "01-2018" "01-2018" "01-2018" "01-2018" ...

PASO 4: Por último, exporté el data.frame padre en un archivo excel

``` r
write.xlsx(full,"Problema_1.xlsx")
```

La cantidad de observaciones en el archivo final es:

``` r
sum(unlist(as.data.frame(laply(full$COD_VIAJE,count))$freq))
```

    ## [1] 2180

## Problema 2

PASO 1: Creé una lista con 5 vectores extraídos del data.frame del
problema anterior

``` r
mi_lista <- list('CLientes_2018'=unlist(full$CLIENTE),
                 'Pilotos_2018'=unlist(full$PILOTO),
                 'Pilotos de camión grande'=unlist(full[full$UNIDAD == 'Camion Grande','PILOTO']),
                 'Clientes con créditos >=90'=unlist(full[full$CREDITO >= 90,'CLIENTE']),
                 'Pilotos de camión grande'=unlist(full[full$UNIDAD == 'Camion Pequeño','PILOTO'])
                 )
```

PASO 2: Formé una función que devuelva la cantidad de elementos, la moda
y la frecuencia de la moda de un vector

``` r
moda <- function(x){
  freqs <- table(x)
  return(c("Observaciones:"=length(x),
           'Moda:'=names(which.max(freqs)),
           'Frecuencia moda:'=max(freqs))
  )
}
```

PASO 3: Por último, apliqué la función ‘moda’ a ‘mi_lista’ y obtenemos
una lista con vectores

``` r
resultado<-lapply(mi_lista,moda)
names(resultado) <- c('Clientes_2018',
                      'Pilotos_2018',
                      'Pilotos de camión grande',
                      'Clientes con créditos >= 90',
                      'Pilotos de camión pequeño')
resultado
```

    ## $Clientes_2018
    ##        Observaciones:                 Moda:      Frecuencia moda: 
    ##                "2180" "TAQUERIA EL CHINITO"                 "139" 
    ## 
    ## $Pilotos_2018
    ##            Observaciones:                     Moda:          Frecuencia moda: 
    ##                    "2180" "Fernando Mariano Berrio"                     "267" 
    ## 
    ## $`Pilotos de camión grande`
    ##            Observaciones:                     Moda:          Frecuencia moda: 
    ##                    "1211" "Fernando Mariano Berrio"                     "157" 
    ## 
    ## $`Clientes con créditos >= 90`
    ##   Observaciones:            Moda: Frecuencia moda: 
    ##            "661"   "SPORTA, S.A."             "49" 
    ## 
    ## $`Pilotos de camión pequeño`
    ##     Observaciones:              Moda:   Frecuencia moda: 
    ##              "605" "Felipe Villatoro"               "78"

## Problema 3

PASO ÚNICO: Leí el archivo que descargué del portal SAT

``` r
PV <- read_delim('PV 01-19.txt',
                 delim = '|',
                 skip_empty_rows = TRUE,
                 show_col_types = FALSE)

str(PV,give.attr=FALSE,give.length=FALSE)
```

    ## spec_tbl_df [2,435,294 × 11] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ ANIO_ALZA          : num  2007 2007 2007 2007 2007 ...
    ##  $ MES                : chr  "05" "05" "05" "05" ...
    ##  $ NOMBRE_DEPARTAMENTO: chr  "HUEHUETENANGO" "EL PROGRESO" "SAN MARCOS" "ESCUINTLA" ...
    ##  $ NOMBRE_MUNICIPIO   : chr  "HUEHUETENANGO" "EL JICARO" "OCOS" "SAN JOS\xc9" ...
    ##  $ MODELO_VEHICULO    : chr  "2007" "2007" "2007" "2006" ...
    ##  $ LINEA_VEHICULO     : chr  "SPORT125" "BT-50 DBL CAB 4X2 TURBO" "JL125" "JL125T-15" ...
    ##  $ TIPO_VEHICULO      : chr  "MOTO" "PICK UP" "MOTO" "MOTO" ...
    ##  $ USO_VEHICULO       : chr  "MOTOCICLETA" "PARTICULAR" "MOTOCICLETA" "MOTOCICLETA" ...
    ##  $ MARCA_VEHICULO     : chr  "ASIA HERO" "MAZDA" "KINLON" "JIALING" ...
    ##  $ CANTIDAD           : num  1 1 1 1 1 1 1 4 11 15 ...
    ##  $ ...11              : logi  NA NA NA NA NA NA ...
