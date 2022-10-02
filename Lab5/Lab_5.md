Laboratorio_5
================
Luz Arevalo
2022-10-02

## Parte 1: Predecir un eclipse solar

``` r
#Variables
last_eclipse <- ymd_hms("2017 august 21 18:26:40")
saros <- 223
synodic <- ddays(29)+dhours(12)+dminutes(44)+dseconds(3)

#Estimación
next_eclipse <- (synodic*saros)+last_eclipse

# Próximo eclipse
next_eclipse
```

    ## [1] "2035-09-02 02:09:49 UTC"

## Parte 2: Agrupaciones y operaciones con fechas

Limpieza de datos

``` r
data <- read_xlsx("data.xlsx")%>%
  rename_with(~tolower(gsub(" ","_",.x)))%>%
  rename_with(~tolower(gsub("ó","o",.x)))

transformar <- function(df,col){
  library(dplyr)
  library(lubridate)
  
  a <- df%>%
    mutate(temporal = dmy(unlist(data[,col],use.names = FALSE)))%>%
    filter(!is.na(temporal))%>%
    select(-one_of(col))
  
  names(a)[length(a)] <- col
  
  b <- df%>%
    dplyr::mutate(temporal = as.numeric(unlist(data[,col],use.names = FALSE)))%>%
    filter(!is.na(temporal))%>%
    mutate(temporal = as.Date(temporal,origin="1900-01-01"))%>%
    select(-one_of(col))
  
  names(b)[length(b)] <- col
  
  return(rbind(a,b))
}

data <- transformar(data,"fecha_creacion")
data <- transformar(data,"fecha_final")
```

#### Mes en el que hubo más llamadas de determinado código

``` r
Codigo <- data %>%
  select(cod)%>%
  group_by(cod)%>%summarise(n())%>%select(cod)%>%unlist(use.names = FALSE)

Q_1 <- data.frame()

for (i in Codigo){
  
  extraer <- data %>%
    select(fecha_creacion,cod,caller_id)%>%
    filter(cod == i)%>%
    mutate(Mes = month(fecha_creacion,label = TRUE,FALSE))%>%
    group_by(Mes,cod)%>%
    summarise(Llamadas = n())%>%
    arrange(desc(Llamadas))
  
  Q_1 <- rbind(Q_1,extraer[1,])%>%arrange(desc(Llamadas))
}

Q_1
```

    ## # A tibble: 7 × 3
    ## # Groups:   Mes [5]
    ##   Mes     cod                          Llamadas
    ##   <ord>   <chr>                           <int>
    ## 1 October Consultas                       10790
    ## 2 March   Cancelaciones                    4092
    ## 3 October Empresarial                      3136
    ## 4 May     Actualización de Información     1691
    ## 5 July    0                                1463
    ## 6 January Otros/Varios                     1129
    ## 7 January Cobros                            688

#### Día de la semana más ocupado

``` r
Q_2 <- data %>%
  select(fecha_creacion,caller_id)%>%
  mutate(Dia = weekdays(fecha_creacion))%>%
  group_by(Dia)%>%
  summarise(Llamadas = n())%>%
  arrange(desc(Llamadas))

Q_2%>%
  head(n=1)
```

    ## # A tibble: 1 × 2
    ##   Dia     Llamadas
    ##   <chr>      <int>
    ## 1 Tuesday    39043

#### Mes más ocupado

``` r
Q_3 <- data %>%
  select(fecha_creacion,caller_id)%>%
  mutate(Mes = month(fecha_creacion,TRUE,FALSE))%>%
  group_by(Mes)%>%
  summarise(Llamadas = n())%>%
  arrange(desc(Llamadas))

Q_3%>%
  head(n=1)
```

    ## # A tibble: 1 × 2
    ##   Mes   Llamadas
    ##   <ord>    <int>
    ## 1 March    22708

#### Comportamiento de las llamadas

``` r
Q_4 <- data%>%
  select(fecha_creacion,caller_id)%>%
  group_by(fecha_creacion)%>%
  summarise(Llamadas = n())

Q_4%>%
  select(fecha_creacion,Llamadas)%>%
  plot("fecha_creacion","Llamadas",type="line")
```

![](Lab_5_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
Q_4%>%
  filter(Llamadas>1000)%>%
  mutate(Dia = paste(day(fecha_creacion),month(fecha_creacion,TRUE,FALSE)))%>%
  select(Dia,Llamadas)
```

    ## # A tibble: 24 × 2
    ##    Dia         Llamadas
    ##    <chr>          <int>
    ##  1 13 January      1437
    ##  2 14 January      1467
    ##  3 13 February     1421
    ##  4 14 February     1469
    ##  5 13 March        1461
    ##  6 14 March        1501
    ##  7 13 April        1429
    ##  8 14 April        1382
    ##  9 13 May          1450
    ## 10 14 May          1492
    ## # … with 14 more rows
    ## # ℹ Use `print(n = ...)` to see more rows

Hay alta concentración de llamadas los días 13 y 14 de cada mes, es
decir, que el número de llamadas muestran dependencia a dos días antes
de la quincena de cada mes.

#### Duración promedio de una llamada

``` r
data%>%
  select(fecha_final,hora_final,fecha_creacion,hora_creacion)%>%
  summarise(Duracion = mean(difftime(hora_final,hora_creacion,tz="UTC",units = "mins")))
```

    ## # A tibble: 1 × 1
    ##   Duracion     
    ##   <drtn>       
    ## 1 14.88962 mins

#### Distribución de la duración de llamadas

``` r
data%>%
  select(fecha_final,hora_final,fecha_creacion,hora_creacion)%>%
  summarise(Duracion = difftime(hora_final,hora_creacion,tz="UTC",units = "mins"))%>%
  group_by(Duracion)%>%
  summarise(Freq = n())
```

    ## # A tibble: 31 × 2
    ##    Duracion  Freq
    ##    <drtn>   <int>
    ##  1 0 mins    9706
    ##  2 1 mins    8741
    ##  3 2 mins    8508
    ##  4 3 mins    8445
    ##  5 4 mins    8513
    ##  6 5 mins    8413
    ##  7 6 mins    8501
    ##  8 7 mins    8420
    ##  9 8 mins    8576
    ## 10 9 mins    8504
    ## # … with 21 more rows
    ## # ℹ Use `print(n = ...)` to see more rows

## Parte 3: Signo Zodiacal

``` r
#### Data frame ####
zod_f <- data.frame(From= mdy(c("Jan 20 2000","Feb 19 2000","Mar 21 2000",
                              "Apr 20 2000","May 21 2000","Jun 21 2000",
                              "Jul 23 2000","Aug 23 2000", "Sep 23 2000",
                              "Oct 23 2000","Nov 22 2000","Dec 22 2000")),
                       To = mdy(c("Feb 18 2000","Mar 20 2000","Apr 19 2000",
                              "May 20 2000","Jun 20 2000", "Jul 22 2000",
                              "Aug 22 2000", "Sep 22 2000","Oct 22 2000", 
                              "Nov 21 2000","Dec 21 2000", "Jan 19 2000")), 
                       Sig = c("Acuario","Piscis","Aries","Tauro",
                               "Geminis","Cancer","Leo","Virgo",
                               "Libra","Escorpio","Sagitario","Capricornio"))

#### Function ####

zod <- function(x){
  
  if(!is.na(dmy(x))){
    if(months(month(dmy(x)))+days(day(dmy(x))) >= months(12)+days(22) |
       months(month(dmy(x)))+days(day(dmy(x))) <= months(1)+days(19)){
      
      library(dplyr)
      library(stringr)
      a <- zod_f %>%
      filter(months(month(zod_f$From))+days(day(zod_f$From)) >=
               months(12)+days(22) |
               months(month(zod_f$To))+days(day(zod_f$To)) <=
               months(1)+days(19))
      
    cat("Fecha de nacimiento:",
        day(dmy(x)),toString(month(dmy(x),label = TRUE)),year(dmy(x)),
        "\nSu signo sodiacal es:",a[,3],
        "\n\nSi la fecha no es correcta asegúrese de haberla ingresado de la forma: d/m/y")
      
    } else{
    library(dplyr)
    a <- zod_f %>%
      filter(months(month(zod_f$From))+days(day(zod_f$From)) <=
               months(month(dmy(x)))+days(day(dmy(x))) &
               months(month(zod_f$To))+days(day(zod_f$To)) >=
               months(month(dmy(x)))+days(day(dmy(x))))
    
    cat("Fecha de nacimiento:",
        day(dmy(x)),toString(month(dmy(x),label = TRUE)),year(dmy(x)),
        "\nSu signo zodiacal:",a[,3],
        "\n\nSi la fecha no es correcta asegúrese de haberla ingresado de la forma: d/m/y")
    
  }} else {
    print("Ingrese su fecha de nacimiento de la forma: d/m/y")
  }
}
#### Uso ####
zod("24 12 2002")
```

    ## Fecha de nacimiento: 24 Dec 2002 
    ## Su signo sodiacal es: Capricornio 
    ## 
    ## Si la fecha no es correcta asegúrese de haberla ingresado de la forma: d/m/y

## Parte 4: Flights

#### Columnas nuevas

``` r
library(nycflights13)
vuelos <- flights

#### Función para convertirlo a horas ####
n_col <- function(df,col.objetivo,col.name){
  v1 <- df%>%
    mutate(temp = nchar(as.character(unlist(df[,col.objetivo]))))%>%
    filter(!is.na(temp))
  
  v2 <- v1%>%
    filter(temp==4)%>%
    mutate(temporal = paste(substring(unlist(v1[v1[,"temp"]==4,col.objetivo]),1,2),
                            substring(unlist(v1[v1[,"temp"]==4,col.objetivo]),3,4),sep = ":"))
  
  v3 <- v1%>%
    filter(temp==3)%>%
    mutate(temporal = paste(substring(unlist(v1[v1[,"temp"]==3,col.objetivo]),1,1),
                            substring(unlist(v1[v1[,"temp"]==3,col.objetivo]),2,3),sep = ":"))
  
  v4 <- v1%>%
    filter(temp==2)%>%
    mutate(temporal = paste("00",substring(unlist(v1[v1[,"temp"]==2,col.objetivo]),1,2),sep = ":"))
  
  v5 <- v1%>%
    filter(temp==1)%>%
    mutate(temporal = paste("00:0",substring(unlist(v1[v1[,"temp"]==1,col.objetivo]),1,1),sep = ""))
  
  v1 <- rbind(v2,v3,v4,v5)
  v1[,"temporal"] <- hm(v1$temporal)
  names(v1)[length(v1)] <- col.name
  return(v1%>%
           select(-temp))
}

vuelos <- n_col(vuelos,"dep_time","dep_time_2")
vuelos <- n_col(vuelos,"sched_dep_time","sched_dep_time_2")
vuelos <- n_col(vuelos,"arr_time","arr_time_2")
vuelos <- n_col(vuelos,"sched_arr_time","sched_arr_time_2")

vuelos[,20:23]
```

    ## # A tibble: 328,063 × 4
    ##    dep_time_2 sched_dep_time_2 arr_time_2 sched_arr_time_2
    ##    <Period>   <Period>         <Period>   <Period>        
    ##  1 10H 3M 0S  10H 10M 0S       12H 55M 0S 13H 20M 0S      
    ##  2 10H 5M 0S  10H 0M 0S        12H 39M 0S 12H 34M 0S      
    ##  3 10H 7M 0S  10H 10M 0S       11H 47M 0S 11H 40M 0S      
    ##  4 10H 9M 0S  10H 15M 0S       12H 19M 0S 12H 29M 0S      
    ##  5 10H 10M 0S 10H 15M 0S       12H 4M 0S  12H 10M 0S      
    ##  6 10H 10M 0S 10H 15M 0S       12H 25M 0S 12H 14M 0S      
    ##  7 10H 11M 0S 10H 1M 0S        11H 33M 0S 11H 28M 0S      
    ##  8 10H 11M 0S 10H 15M 0S       12H 46M 0S 13H 7M 0S       
    ##  9 10H 21M 0S 10H 23M 0S       12H 54M 0S 12H 52M 0S      
    ## 10 10H 24M 0S 10H 29M 0S       11H 40M 0S 11H 50M 0S      
    ## # … with 328,053 more rows
    ## # ℹ Use `print(n = ...)` to see more rows

#### Atraso total

``` r
vuelos <- vuelos%>%
  mutate(delay_total = minutes(dep_delay+arr_delay))

vuelos[,"delay_total"]
```

    ## # A tibble: 328,063 × 1
    ##    delay_total
    ##    <Period>   
    ##  1 -32M 0S    
    ##  2 10M 0S     
    ##  3 4M 0S      
    ##  4 -16M 0S    
    ##  5 -11M 0S    
    ##  6 6M 0S      
    ##  7 15M 0S     
    ##  8 -25M 0S    
    ##  9 0S         
    ## 10 -15M 0S    
    ## # … with 328,053 more rows
    ## # ℹ Use `print(n = ...)` to see more rows
