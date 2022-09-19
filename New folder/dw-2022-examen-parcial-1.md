dw-2022-parcial-1
================
Luz Arévalo
9/19/2022

# Examen parcial

Indicaciones generales:

-   Usted tiene el período de la clase para resolver el examen parcial.

-   La entrega del parcial, al igual que las tareas, es por medio de su
    cuenta de github, pegando el link en el portal de MiU.

-   Pueden hacer uso del material del curso e internet (stackoverflow,
    etc.). Sin embargo, si encontramos algún indicio de copia, se
    anulará el exámen para los estudiantes involucrados. Por lo tanto,
    aconsejamos no compartir las agregaciones que generen.

## Sección I: Preguntas teóricas.

-   Existen 10 preguntas directas en este Rmarkdown, de las cuales usted
    deberá responder 5. Las 5 a responder estarán determinadas por un
    muestreo aleatorio basado en su número de carné.

-   Ingrese su número de carné en `set.seed()` y corra el chunk de R
    para determinar cuáles preguntas debe responder.

``` r
#set.seed("20200392") 
v<- 1:10
preguntas <-sort(sample(v, size = 5, replace = FALSE ))

paste0("Mis preguntas a resolver son: ",paste0(preguntas,collapse = ", "))
```

    ## [1] "Mis preguntas a resolver son: 1, 3, 4, 8, 10"

#### \[1\] “Mis preguntas a resolver son: 1, 3, 4, 8, 10”

### Listado de preguntas teóricas

1.  Para las siguientes sentencias de `base R`, liste su contraparte de
    `dplyr`:

    -   `str()`
    -   `df[,c("a","b")]`
    -   `names(df)[4] <- "new_name"` donde la posición 4 corresponde a
        la variable `old_name`
    -   `df[df$variable == "valor",]`

2.  ¿Por qué en R utilizamos funciones de la familia apply
    (lapply,vapply) en lugar de utilizar ciclos?

3.  ¿Cuál es la diferencia entre utilizar `==` y `=` en R?

4.  Si en un dataframe, a una variable de tipo `factor` le agrego un
    nuevo elemento que *no se encuentra en los niveles existentes*,
    ¿cuál sería el resultado esperado y por qué?

    -   El nuevo elemento
    -   `NA`

5.  Si quiero obtener como resultado las filas de la tabla A que no se
    encuentran en la tabla B, ¿cómo debería de completar la siguiente
    sentencia de SQL?

    -   SELECT \* FROM A \_\_\_\_\_\_\_ B ON A.KEY = B.KEY WHERE
        \_\_\_\_\_\_\_\_\_\_ = \_\_\_\_\_\_\_\_\_\_

Extra: ¿Cuántos posibles exámenes de 5 preguntas se pueden realizar
utilizando como banco las diez acá presentadas? (responder con código de
R.)

## Sección II Preguntas prácticas.

-   Conteste las siguientes preguntas utilizando sus conocimientos de R.
    Adjunte el código que utilizó para llegar a sus conclusiones en un
    chunk del markdown.

A. De los clientes que están en más de un país,¿cuál cree que es el más
rentable y por qué?

B. Estrategia de negocio ha decidido que ya no operará en aquellos
territorios cuyas pérdidas sean “considerables”. Bajo su criterio,
¿cuáles son estos territorios y por qué ya no debemos operar ahí?

# I. Preguntas teóricas

### Respuesta 1

    * `glimpse()`
    * `select(df,a,b)`
    * `rename(df, new_name = old_name)`
    * `filter(df,varible == "valor")`

### Respuesta 3

La familia apply permite aplicar una función a cada elemento de una
lista, usualmente con un menor tiempo de ejecución que un bucle, y,
además, su sintaxys es más sencilla.

### Respuesta 4

La diferencia es que `==` se trata de un operador relacional de tipo
boleano, se utiliza para comparar dos objetos. Por otro lado `=` es un
operador que permite asignar parámetros en un argumento/función.

### Respuesta 8

El resultado sería `El nuevo elemento`, debido a que se trata de una
variable categorica. Al momento de agregar un elemento que no existe lo
coercirá al tipo factor.

### Respuesta 10

-   `SELECT * FROM A LEFT JOIN B ON A.KEY = B.KEY WHERE Columna.A = 'NA'`

# I. Preguntas prácticas

## A

``` r
parcial_anonimo <- readRDS("parcial_anonimo.rds")
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
parcial_anonimo %>%
  select(Cliente,Pais,Venta) %>%
  group_by(Cliente) %>%
  dplyr::summarise(paises = n_distinct(Pais),
                   venta = sum(Venta))%>%
  filter(paises > 1)%>%
  arrange(desc(venta))
```

    ## # A tibble: 7 × 3
    ##   Cliente  paises  venta
    ##   <chr>     <int>  <dbl>
    ## 1 a17a7558      2 19818.
    ## 2 ff122c3f      2 15359.
    ## 3 c53868a0      2 13813.
    ## 4 044118d4      2  9436.
    ## 5 f676043b      2  3635.
    ## 6 f2aab44e      2   400.
    ## 7 bf1e94e9      2     0

``` r
# El cliente `a17a7558` se encuentra en 2 países y se considera el más rentable, porque sus ventas son las más altas en los últimos 2 años.
```

## B

``` r
###resuelva acá
```
