Missing Data and Feature Engineering
================
Luz Arevalo - 20200392

## Parte 1

#### 1. Missing values report

``` r
for (i in names(missing_values)) {
  print(paste("Missing values in ",i,":",missing_values[i]))
}
```

    ## [1] "Missing values in  Age : 25"
    ## [1] "Missing values in  Sex : 51"
    ## [1] "Missing values in  SibSp : 3"
    ## [1] "Missing values in  Embarked : 12"
    ## [1] "Missing values in  Parch : 12"
    ## [1] "Missing values in  Survived : 0"
    ## [1] "Missing values in  Pclass : 0"
    ## [1] "Missing values in  PassengerId : 0"
    ## [1] "Missing values in  Name : 0"
    ## [1] "Missing values in  Ticket : 0"
    ## [1] "Missing values in  Fare : 8"
    ## [1] "Missing values in  Cabin : 0"

#### Métodos

Los métodos utilizados en las columnas con valores perdidos serían los
siguientes:

Para la columna Sexo utilizaría imputación sectorizada por la moda, la
sectorizacion sería por medio de la variable de supervivencia, porque de
esta manera los valores de sexo serán reemplazados con valores que
contienen aproximaciones a los valores reales.

Para la columna de Edad utilizaría un standar deviation approach para
hacer “Cap” de los outliers y una imputación general por la media,
porque este tipo de valores no es posible determinarlo con precisión por
medio de una regresión lineal. Con este método se respeta la
distribución de los datos.

Para las columnas SibSp y Parch utilizaría una imputación general por la
moda, porque los valores faltantes de esta variable son menos del 10% de
los datos, por lo que no afectaría significativamente las probabilidades
de cada valor. Para la columna Fare utilizaría una imputación por
regresión, donde las variables explicativas serían variables dummy Class
y Embarked, porque se esperaría obtener una explicación bastante
aproximada de los datos reales.

Finalmente, para Embarked utilizaría una imputación general por la moda,
porque los valores faltantes de esta variable también son menos del 10%
de los datos, por lo que no afectaría significativamente las
probabilidades de cada valor.

No se consideran los métodos de Deletion (listwise), porque las
observaciones disponibles son pocas y cada una podría agregar valor. Las
columnas de Name y PassangerId no pueden ser determinadas, por lo que se
utilizarían métodos de pairwaise si hiciera falta alguno. Survived y
Pclass se haría imputación por regresión. Ticket y Cabin se haría con
imputación sectorizada (con la clase del pasajero y puerto de embarque)
por la moda, porque son valores categóricos.

#### Casos completos

``` r
cc <- df[complete.cases(df),]
cat("Número de casos completos:",nrow(cc))
```

    ## Número de casos completos: 141

## Parte_2
