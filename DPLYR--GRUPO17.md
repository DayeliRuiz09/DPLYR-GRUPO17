DPLYR
================
Dayeli Ruiz, Angely Rojas, Nolberto Comeca
18/1/2022

``` r
library(tidyverse)
library("nycflights13")
```

## 9.1 DPLYER - FILTER

### 1a. Encuentra todos los vuelos que:Tuvieron un retraso de llegada de dos o más horas

``` r
d<-data("flights")
filter(flights,arr_delay>=120)
```

    ## # A tibble: 10,200 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      811            630       101     1047            830
    ##  2  2013     1     1      848           1835       853     1001           1950
    ##  3  2013     1     1      957            733       144     1056            853
    ##  4  2013     1     1     1114            900       134     1447           1222
    ##  5  2013     1     1     1505           1310       115     1638           1431
    ##  6  2013     1     1     1525           1340       105     1831           1626
    ##  7  2013     1     1     1549           1445        64     1912           1656
    ##  8  2013     1     1     1558           1359       119     1718           1515
    ##  9  2013     1     1     1732           1630        62     2028           1825
    ## 10  2013     1     1     1803           1620       103     2008           1750
    ## # ... with 10,190 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

### 1b. Volaron a Houston (IAH oHOU)

``` r
destino<-c("IAH", "HOU")
filter(flights, (dest %in% destino))
```

    ## # A tibble: 9,313 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      517            515         2      830            819
    ##  2  2013     1     1      533            529         4      850            830
    ##  3  2013     1     1      623            627        -4      933            932
    ##  4  2013     1     1      728            732        -4     1041           1038
    ##  5  2013     1     1      739            739         0     1104           1038
    ##  6  2013     1     1      908            908         0     1228           1219
    ##  7  2013     1     1     1028           1026         2     1350           1339
    ##  8  2013     1     1     1044           1045        -1     1352           1351
    ##  9  2013     1     1     1114            900       134     1447           1222
    ## 10  2013     1     1     1205           1200         5     1503           1505
    ## # ... with 9,303 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

### 1c. Fueron operados por United, American o Delta

``` r
operarios <- c("UA", "AA","DL")
filter(flights, (carrier %in% operarios))
```

    ## # A tibble: 139,504 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      517            515         2      830            819
    ##  2  2013     1     1      533            529         4      850            830
    ##  3  2013     1     1      542            540         2      923            850
    ##  4  2013     1     1      554            600        -6      812            837
    ##  5  2013     1     1      554            558        -4      740            728
    ##  6  2013     1     1      558            600        -2      753            745
    ##  7  2013     1     1      558            600        -2      924            917
    ##  8  2013     1     1      558            600        -2      923            937
    ##  9  2013     1     1      559            600        -1      941            910
    ## 10  2013     1     1      559            600        -1      854            902
    ## # ... with 139,494 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

### 1d. Partieron en invierno del hemisferio sur (julio, agosto y septiembre)

``` r
meses <- c(7,8,9)
filter(flights, (month %in% meses))
```

    ## # A tibble: 86,326 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     7     1        1           2029       212      236           2359
    ##  2  2013     7     1        2           2359         3      344            344
    ##  3  2013     7     1       29           2245       104      151              1
    ##  4  2013     7     1       43           2130       193      322             14
    ##  5  2013     7     1       44           2150       174      300            100
    ##  6  2013     7     1       46           2051       235      304           2358
    ##  7  2013     7     1       48           2001       287      308           2305
    ##  8  2013     7     1       58           2155       183      335             43
    ##  9  2013     7     1      100           2146       194      327             30
    ## 10  2013     7     1      100           2245       135      337            135
    ## # ... with 86,316 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

### 1e.Llegaron más de dos horas tarde, pero no salieron tarde

``` r
filter(flights,  arr_delay > 120 & dep_delay <= 0)
```

    ## # A tibble: 29 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1    27     1419           1420        -1     1754           1550
    ##  2  2013    10     7     1350           1350         0     1736           1526
    ##  3  2013    10     7     1357           1359        -2     1858           1654
    ##  4  2013    10    16      657            700        -3     1258           1056
    ##  5  2013    11     1      658            700        -2     1329           1015
    ##  6  2013     3    18     1844           1847        -3       39           2219
    ##  7  2013     4    17     1635           1640        -5     2049           1845
    ##  8  2013     4    18      558            600        -2     1149            850
    ##  9  2013     4    18      655            700        -5     1213            950
    ## 10  2013     5    22     1827           1830        -3     2217           2010
    ## # ... with 19 more rows, and 11 more variables: arr_delay <dbl>, carrier <chr>,
    ## #   flight <int>, tailnum <chr>, origin <chr>, dest <chr>, air_time <dbl>,
    ## #   distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

### 1f. Se retrasaron por lo menos una hora, pero repusieron más de 30 minutos en vuelo

``` r
retrasos<- select(flights,ends_with("delay"))
repusieron<- mutate(retrasos, tiemporecuperado= flights$dep_delay-flights$arr_delay)
filter(retrasos,dep_delay>=60 & arr_delay < 30)
```

    ## # A tibble: 206 x 2
    ##    dep_delay arr_delay
    ##        <dbl>     <dbl>
    ##  1        65        28
    ##  2        65         1
    ##  3        60        24
    ##  4        79        28
    ##  5        73        23
    ##  6        60        26
    ##  7        66        22
    ##  8        68        17
    ##  9        61        21
    ## 10        61        28
    ## # ... with 196 more rows

### 1g. Partieron entre la medianoche y las 6 a.m.

``` r
filter(flights,dep_time >= 0 & dep_time <= 600)
```

    ## # A tibble: 9,344 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      517            515         2      830            819
    ##  2  2013     1     1      533            529         4      850            830
    ##  3  2013     1     1      542            540         2      923            850
    ##  4  2013     1     1      544            545        -1     1004           1022
    ##  5  2013     1     1      554            600        -6      812            837
    ##  6  2013     1     1      554            558        -4      740            728
    ##  7  2013     1     1      555            600        -5      913            854
    ##  8  2013     1     1      557            600        -3      709            723
    ##  9  2013     1     1      557            600        -3      838            846
    ## 10  2013     1     1      558            600        -2      753            745
    ## # ... with 9,334 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

### 2.Otra función de dplyr que es útil para usar filtros es between(). ¿Qué hace? ¿Puedes usarla para simplificar el código necesario para responder a los desafíos anteriores?

``` r
filter(flights, between(month, 7, 9))
```

    ## # A tibble: 86,326 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     7     1        1           2029       212      236           2359
    ##  2  2013     7     1        2           2359         3      344            344
    ##  3  2013     7     1       29           2245       104      151              1
    ##  4  2013     7     1       43           2130       193      322             14
    ##  5  2013     7     1       44           2150       174      300            100
    ##  6  2013     7     1       46           2051       235      304           2358
    ##  7  2013     7     1       48           2001       287      308           2305
    ##  8  2013     7     1       58           2155       183      335             43
    ##  9  2013     7     1      100           2146       194      327             30
    ## 10  2013     7     1      100           2245       135      337            135
    ## # ... with 86,316 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

### 3.¿Cuántos vuelos tienen datos faltantes en horario_salida? ¿Qué otras variables tienen valores faltantes? ¿Qué representan estas filas?

``` r
a<-filter(flights, is.na(dep_time))
count(a)
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1  8255

``` r
# Son 8255 vuelos, y dep_delay, arr_tiem_arr_delay tambien tienen valores faltantes
# Las filas con datos faltantes representan los vuelos que fueron cancelados
```

### 4.¿Por qué NA^0 no es faltante? ¿Por qué NA \| TRUE no es faltante? ¿Por qué FALSE & NA no es faltante? ¿Puedes descubrir la regla general? (¡NA \* 0 es un contraejemplo complicado!)

``` r
x <- c(NA) 
is.na(x)
```

    ## [1] TRUE

### La función `is.na()` determina si falta un valor y devuelve un valor lógico `TRUE` en los casos en que es NA (Not Available).

``` r
x^0
```

    ## [1] 1

### Como que el NA podría tomar cualquier valor, se puede deducir que cualquier número (aunque sea muy grande) a la potencia cero es igual a 1.

``` r
x | TRUE
```

    ## [1] TRUE

### Es igual a TRUE pues el NA se entiende como un valor lógico (`TRUE` or `FALSE`) y por lógica proposicional `TRUE` \| `TRUE` y `FALSE` \| `TRUE` es siempre igual a `TRUE`.

``` r
x & FALSE
```

    ## [1] FALSE

### Es igual a `TRUE` porque el NA se entiende como un valor lógico (`TRUE` or `FALSE`) y por lógica proposicional `TRUE`&`FALSE` y `FALSE`&`FALSE` es siempre `FALSE`.

### El contraejemplo a la regla general:

``` r
x * 0
```

    ## [1] NA

``` r
Inf*0
```

    ## [1] NaN

``` r
### En este contraejemplo puede ser útil pensar que el `NA` puede tomar cualquier valor, incluso podría ser un número muy grande; el cual al multiplicarse por cero 
#nos da una indeterminación definida por`NaN` (Not a Number).Si el `NA` fuese un valor pequeño, entonces `NA*0` sería igual a cero. Luego, es mejor pensar en `x*0` como un `NA` porque no sabemos en cual de los dos casos anteriores estamos.
```

## 9.2 DPLYER - ARRANGE

### 1. ¿Cómo podrías usar arrange() para ordenar todos los valores faltantes al comienzo? (Sugerencia: usa is.na())

``` r
#Usamos la variable "air_time" porque es la que contiene mayor número de NA´s.
d<-data("flights")
flights %>%
  arrange(desc(is.na(air_time)))
```

    ## # A tibble: 336,776 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1     1525           1530        -5     1934           1805
    ##  2  2013     1     1     1528           1459        29     2002           1647
    ##  3  2013     1     1     1740           1745        -5     2158           2020
    ##  4  2013     1     1     1807           1738        29     2251           2103
    ##  5  2013     1     1     1939           1840        59       29           2151
    ##  6  2013     1     1     1952           1930        22     2358           2207
    ##  7  2013     1     1     2016           1930        46       NA           2220
    ##  8  2013     1     1       NA           1630        NA       NA           1815
    ##  9  2013     1     1       NA           1935        NA       NA           2240
    ## 10  2013     1     1       NA           1500        NA       NA           1825
    ## # ... with 336,766 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

### 2. Ordena vuelos para encontrar los vuelos más retrasados. Encuentra los vuelos que salieron más temprano

``` r
# Vuelos que salieron con más retraso
flights %>%
  arrange(desc(dep_delay))
```

    ## # A tibble: 336,776 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     9      641            900      1301     1242           1530
    ##  2  2013     6    15     1432           1935      1137     1607           2120
    ##  3  2013     1    10     1121           1635      1126     1239           1810
    ##  4  2013     9    20     1139           1845      1014     1457           2210
    ##  5  2013     7    22      845           1600      1005     1044           1815
    ##  6  2013     4    10     1100           1900       960     1342           2211
    ##  7  2013     3    17     2321            810       911      135           1020
    ##  8  2013     6    27      959           1900       899     1236           2226
    ##  9  2013     7    22     2257            759       898      121           1026
    ## 10  2013    12     5      756           1700       896     1058           2020
    ## # ... with 336,766 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

``` r
# Vuelos que salieron más temprano
flights %>%
  arrange(dep_delay)
```

    ## # A tibble: 336,776 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013    12     7     2040           2123       -43       40           2352
    ##  2  2013     2     3     2022           2055       -33     2240           2338
    ##  3  2013    11    10     1408           1440       -32     1549           1559
    ##  4  2013     1    11     1900           1930       -30     2233           2243
    ##  5  2013     1    29     1703           1730       -27     1947           1957
    ##  6  2013     8     9      729            755       -26     1002            955
    ##  7  2013    10    23     1907           1932       -25     2143           2143
    ##  8  2013     3    30     2030           2055       -25     2213           2250
    ##  9  2013     3     2     1431           1455       -24     1601           1631
    ## 10  2013     5     5      934            958       -24     1225           1309
    ## # ... with 336,766 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

### 3. Ordena vuelos para encontrar los vuelos más rápidos (que viajaron a mayor velocidad).

``` r
# Si se considera que la velocidad es igual a `distance`/`air_time` (distancia recorrida por minuto).

# Los datos ordenados de forma descendente nos dará como resultado los vuelos más rápidos primero.
flights %>%
  arrange(desc(distance/air_time))
```

    ## # A tibble: 336,776 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     5    25     1709           1700         9     1923           1937
    ##  2  2013     7     2     1558           1513        45     1745           1719
    ##  3  2013     5    13     2040           2025        15     2225           2226
    ##  4  2013     3    23     1914           1910         4     2045           2043
    ##  5  2013     1    12     1559           1600        -1     1849           1917
    ##  6  2013    11    17      650            655        -5     1059           1150
    ##  7  2013     2    21     2355           2358        -3      412            438
    ##  8  2013    11    17      759            800        -1     1212           1255
    ##  9  2013    11    16     2003           1925        38       17             36
    ## 10  2013    11    16     2349           2359       -10      402            440
    ## # ... with 336,766 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

### 4. ¿Cuáles vuelos viajaron más lejos? ¿Cuál viajó más cerca?

``` r
# Vuelos que viajaron más lejos:
flights %>%
  arrange(desc(distance))
```

    ## # A tibble: 336,776 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      857            900        -3     1516           1530
    ##  2  2013     1     2      909            900         9     1525           1530
    ##  3  2013     1     3      914            900        14     1504           1530
    ##  4  2013     1     4      900            900         0     1516           1530
    ##  5  2013     1     5      858            900        -2     1519           1530
    ##  6  2013     1     6     1019            900        79     1558           1530
    ##  7  2013     1     7     1042            900       102     1620           1530
    ##  8  2013     1     8      901            900         1     1504           1530
    ##  9  2013     1     9      641            900      1301     1242           1530
    ## 10  2013     1    10      859            900        -1     1449           1530
    ## # ... with 336,766 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

``` r
# Vuelos que viajaron más cerca:
flights %>%
  arrange(distance)
```

    ## # A tibble: 336,776 x 19
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     7    27       NA            106        NA       NA            245
    ##  2  2013     1     3     2127           2129        -2     2222           2224
    ##  3  2013     1     4     1240           1200        40     1333           1306
    ##  4  2013     1     4     1829           1615       134     1937           1721
    ##  5  2013     1     4     2128           2129        -1     2218           2224
    ##  6  2013     1     5     1155           1200        -5     1241           1306
    ##  7  2013     1     6     2125           2129        -4     2224           2224
    ##  8  2013     1     7     2124           2129        -5     2212           2224
    ##  9  2013     1     8     2127           2130        -3     2304           2225
    ## 10  2013     1     9     2126           2129        -3     2217           2224
    ## # ... with 336,766 more rows, and 11 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>

## DPYLR - SELECT

### 1. Haz una lluvia de ideas sobre tantas maneras como sea posible para seleccionar dep_time, dep_delay, arr_time, and arr_delay de flights.

``` r
# Primera forma
flights %>%
  select(dep_time, dep_delay, arr_time, arr_delay)
```

    ## # A tibble: 336,776 x 4
    ##    dep_time dep_delay arr_time arr_delay
    ##       <int>     <dbl>    <int>     <dbl>
    ##  1      517         2      830        11
    ##  2      533         4      850        20
    ##  3      542         2      923        33
    ##  4      544        -1     1004       -18
    ##  5      554        -6      812       -25
    ##  6      554        -4      740        12
    ##  7      555        -5      913        19
    ##  8      557        -3      709       -14
    ##  9      557        -3      838        -8
    ## 10      558        -2      753         8
    ## # ... with 336,766 more rows

``` r
# Segunda forma
flights %>%
  select(starts_with("arr"), starts_with("dep"))
```

    ## # A tibble: 336,776 x 4
    ##    arr_time arr_delay dep_time dep_delay
    ##       <int>     <dbl>    <int>     <dbl>
    ##  1      830        11      517         2
    ##  2      850        20      533         4
    ##  3      923        33      542         2
    ##  4     1004       -18      544        -1
    ##  5      812       -25      554        -6
    ##  6      740        12      554        -4
    ##  7      913        19      555        -5
    ##  8      709       -14      557        -3
    ##  9      838        -8      557        -3
    ## 10      753         8      558        -2
    ## # ... with 336,766 more rows

### 2. ¿Qué sucede si incluyes el nombre de una variable varias veces en una llamada a select()?

``` r
# Como se puede apreciar, apesar de que usamos mas de una vez la variable dep_time al ultilizar la accion select, esta solo la considero una sola vez
flights %>%
  select(dep_time, dep_time, dep_time)
```

    ## # A tibble: 336,776 x 1
    ##    dep_time
    ##       <int>
    ##  1      517
    ##  2      533
    ##  3      542
    ##  4      544
    ##  5      554
    ##  6      554
    ##  7      555
    ##  8      557
    ##  9      557
    ## 10      558
    ## # ... with 336,766 more rows

### 3. ¿Qué hace la función any_of()? ¡¿Por qué podría ser útil en conjunto con este vector?

``` r
mm=c("year","month","day")
```

``` r
# Con la función `one_of()` podemos indicar las variables que queremos seleccionar con el nombre del vector que las contiene.

# La función `one_of()` selecciona todas las variables que están en el vector `mm`:
flights %>%
  select(one_of(mm))
```

    ## # A tibble: 336,776 x 3
    ##     year month   day
    ##    <int> <int> <int>
    ##  1  2013     1     1
    ##  2  2013     1     1
    ##  3  2013     1     1
    ##  4  2013     1     1
    ##  5  2013     1     1
    ##  6  2013     1     1
    ##  7  2013     1     1
    ##  8  2013     1     1
    ##  9  2013     1     1
    ## 10  2013     1     1
    ## # ... with 336,766 more rows

## 9.4 DPLYR - MUTATE

### 1. Las variables horario_salida y salida_programada tienen un formato conveniente para leer, pero es difícil realizar cualquier cálculo con ellas porque no son realmente números continuos. Transfórmalas haciaun formato más conveniente como número de minutos desde la medianoche.

``` r
659%/%100 # número de horas desde la media noche
```

    ## [1] 6

``` r
659%/%100 *60 #cantidad de minutos en esas 6 horas
```

    ## [1] 360

``` r
659 %% 100  # sumar los 59 minutos
```

    ## [1] 59

``` r
# minutos transcurridos desde las 00:00 hasta las 6:59 AM 
659 %/% 100 * 60 + 659 %% 100
```

    ## [1] 419

``` r
(659 %/% 100 * 60 + 659 %% 100) %% 1440
```

    ## [1] 419

``` r
# número de minutos desde la medianoche es 1440 

mutate(flights, salidaprog_min = (sched_dep_time %/% 100 * 60 + sched_dep_time %% 100) %% 1440)
```

    ## # A tibble: 336,776 x 20
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      517            515         2      830            819
    ##  2  2013     1     1      533            529         4      850            830
    ##  3  2013     1     1      542            540         2      923            850
    ##  4  2013     1     1      544            545        -1     1004           1022
    ##  5  2013     1     1      554            600        -6      812            837
    ##  6  2013     1     1      554            558        -4      740            728
    ##  7  2013     1     1      555            600        -5      913            854
    ##  8  2013     1     1      557            600        -3      709            723
    ##  9  2013     1     1      557            600        -3      838            846
    ## 10  2013     1     1      558            600        -2      753            745
    ## # ... with 336,766 more rows, and 12 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>,
    ## #   salidaprog_min <dbl>

``` r
mutate(flights, horsalida_min = (dep_time %/% 100 * 60 + dep_time%% 100) %% 1440)
```

    ## # A tibble: 336,776 x 20
    ##     year month   day dep_time sched_dep_time dep_delay arr_time sched_arr_time
    ##    <int> <int> <int>    <int>          <int>     <dbl>    <int>          <int>
    ##  1  2013     1     1      517            515         2      830            819
    ##  2  2013     1     1      533            529         4      850            830
    ##  3  2013     1     1      542            540         2      923            850
    ##  4  2013     1     1      544            545        -1     1004           1022
    ##  5  2013     1     1      554            600        -6      812            837
    ##  6  2013     1     1      554            558        -4      740            728
    ##  7  2013     1     1      555            600        -5      913            854
    ##  8  2013     1     1      557            600        -3      709            723
    ##  9  2013     1     1      557            600        -3      838            846
    ## 10  2013     1     1      558            600        -2      753            745
    ## # ... with 336,766 more rows, and 12 more variables: arr_delay <dbl>,
    ## #   carrier <chr>, flight <int>, tailnum <chr>, origin <chr>, dest <chr>,
    ## #   air_time <dbl>, distance <dbl>, hour <dbl>, minute <dbl>, time_hour <dttm>,
    ## #   horsalida_min <dbl>

### 2.Compara tiempo_vuelo con horario_llegada - horario_salida. ¿Qué esperas ver? ¿Qué ves? ¿Qué necesitas hacer para arreglarlo?

``` r
# `air_time` = `arr_time` - `dep_time`
```

### 3.Compara horario_salida, salida_programada, y atraso_salida. ¿Cómo esperarías que esos tres números estén relacionados?

### 4. Encuentra los 10 vuelos más retrasados utilizando una función de ordenamiento. ¿Cómo quieres manejar los empates? Lee atentamente la documentación de min_rank().

### 5. ¿Qué devuelve 1:3 + 1:10? ¿Por qué?

``` r
1:3+1:10 # los numeros del 1:10 se suman entre filas del 1:3, cada grupo de 3 numeros
```

    ## Warning in 1:3 + 1:10: longitud de objeto mayor no es múltiplo de la longitud de
    ## uno menor

    ##  [1]  2  4  6  5  7  9  8 10 12 11

### 6.¿Qué funciones trigonométricas proporciona R

``` r
# R nos proporciona el sen, cos, tan y cot
```

## 9.5 DPLYR - GROUP BY & SUMMARIZE

### 5. ¿Qué compañía tiene los peores retrasos?

``` r
arrange(flights, arr_delay) %>%
group_by(arr_delay, carrier, dest) %>%
summarise()
```

    ## `summarise()` has grouped output by 'arr_delay', 'carrier'. You can override using the `.groups` argument.

    ## # A tibble: 39,730 x 3
    ## # Groups:   arr_delay, carrier [4,683]
    ##    arr_delay carrier dest 
    ##        <dbl> <chr>   <chr>
    ##  1       -86 VX      SFO  
    ##  2       -79 VX      SFO  
    ##  3       -75 AA      SEA  
    ##  4       -75 UA      LAX  
    ##  5       -74 AS      SEA  
    ##  6       -73 UA      SFO  
    ##  7       -71 B6      LAX  
    ##  8       -71 DL      PDX  
    ##  9       -71 UA      SFO  
    ## 10       -70 B6      LGB  
    ## # ... with 39,720 more rows

### 6. ¿Qué hace el argumento sort a count()

``` r
# La función count() nos permite saber cuantas observaciones hay en una variable especifica. Al agregar el argumento sort = TRUE devuelve una tabla descendiente con el número de observaciones.
```

## 9.6 DPLYR - TRANSFORMACIONES AGRUPADAS

### 1. Remítete a las listas de funciones útiles de mutación y filtrado. Describe cómo cambia cada operación cuando las combinas con la agrupación.

``` r
# mutate() Nos permite crear una columna de datos a partir de un grupo 
# filter() Nos permite filtrar datos por filas de distintos grupos de datos
```

### 2. ¿Qué avión (codigo_cola) tiene el peor registro de tiempo?

``` r
no_vuelo<-flights %>% filter(!is.na(dep_delay),!is.na(arr_delay))
xy<-no_vuelo %>%count(tailnum,wt=air_time)
arrange(xy,n)
```

    ## # A tibble: 4,037 x 2
    ##    tailnum     n
    ##    <chr>   <dbl>
    ##  1 N505SW     43
    ##  2 N746SK     50
    ##  3 N824AS     53
    ##  4 N881AS     57
    ##  5 N766SK     60
    ##  6 N701SK     64
    ##  7 N760SK     64
    ##  8 N795SK     66
    ##  9 N726SK     67
    ## 10 N772SK     67
    ## # ... with 4,027 more rows

### 3. ¿A qué hora del día deberías volar si quieres evitar lo más posible los retrasos?

``` r
vuelos<-select(flights,hour, minute, dep_delay, arr_delay)
no_cancel<-vuelos %>% filter(!is.na(dep_delay),!is.na(arr_delay))
no_retraso<-no_cancel%>%filter((arr_delay<0) & (dep_delay<0))%>%select(hour,minute,dep_delay,arr_delay)
arrange(no_retraso,hour,minute)
```

    ## # A tibble: 144,346 x 4
    ##     hour minute dep_delay arr_delay
    ##    <dbl>  <dbl>     <dbl>     <dbl>
    ##  1     5      0        -4       -19
    ##  2     5      0        -2       -10
    ##  3     5      0        -6       -11
    ##  4     5      0        -6       -23
    ##  5     5      0        -3        -1
    ##  6     5      0       -10       -14
    ##  7     5      0        -7        -5
    ##  8     5      0        -7        -8
    ##  9     5      0        -7        -3
    ## 10     5      0        -7       -12
    ## # ... with 144,336 more rows

### 4. Para cada destino, calcula los minutos totales de demora. Para cada vuelo, calcula la proporción de la demora total para su destino.

``` r
tiem_demo<- group_by(flights, dest)
summarise(tiem_demo, minutdemora = sum(arr_delay, na.rm = TRUE))
```

    ## # A tibble: 105 x 2
    ##    dest  minutdemora
    ##    <chr>       <dbl>
    ##  1 ABQ          1113
    ##  2 ACK          1281
    ##  3 ALB          6018
    ##  4 ANC           -20
    ##  5 ATL        190260
    ##  6 AUS         14514
    ##  7 AVL          2089
    ##  8 BDL          2904
    ##  9 BGR          2874
    ## 10 BHM          4540
    ## # ... with 95 more rows

``` r
prop<- group_by(flights, dest)
summarise(prop,minutdemora = mean(arr_delay, na.rm = TRUE))
```

    ## # A tibble: 105 x 2
    ##    dest  minutdemora
    ##    <chr>       <dbl>
    ##  1 ABQ          4.38
    ##  2 ACK          4.85
    ##  3 ALB         14.4 
    ##  4 ANC         -2.5 
    ##  5 ATL         11.3 
    ##  6 AUS          6.02
    ##  7 AVL          8.00
    ##  8 BDL          7.05
    ##  9 BGR          8.03
    ## 10 BHM         16.9 
    ## # ... with 95 more rows

### 7. Encuentra todos los destinos que son volados por al menos dos operadores. Usa esta información para clasificar a las aerolíneas.

``` r
v<-flights%>%select(carrier,dest)%>%count(dest,carrier)%>%group_by(dest)%>%filter(rank(desc(carrier))>2)
unique(v$dest)
```

    ##  [1] "ATL" "AUS" "BNA" "BOS" "BTV" "BUF" "BWI" "CHS" "CLE" "CLT" "CMH" "CVG"
    ## [13] "DCA" "DEN" "DFW" "DTW" "FLL" "IAD" "IND" "JAX" "LAS" "LAX" "MCI" "MCO"
    ## [25] "MEM" "MIA" "MKE" "MSP" "MSY" "OMA" "ORD" "ORF" "PBI" "PDX" "PHL" "PHX"
    ## [37] "PIT" "PWM" "RDU" "ROC" "RSW" "SAN" "SAT" "SDF" "SEA" "SFO" "SJU" "SRQ"
    ## [49] "STL" "STT" "SYR" "TPA"
