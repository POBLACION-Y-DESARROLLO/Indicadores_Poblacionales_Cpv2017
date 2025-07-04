DEFINE Poblacio.EDADCICL
    AS RECODE Poblacio.EDAD
    (0 - 5=1)
    (6 - 11=2)
    (12 - 17=3)
    (18 - 29=4)
    (30 - 44=5)
    (45 - 59=6)
    (60 - HIGHEST =7)
    TYPE INTEGER
    RANGE 1 - 7
    VARLABEL "P: Población según ciclo de vida"
    VALUELABELS
    1 "Primera infancia (0 - 5 años)"
    2 "Niñez (6 - 11 años)"
    3 "Adolescencia (12 - 17 años)"
    4 "Jóvenes (18 - 29 años)"
    5 "Adultos/as jóvenes (30 - 44 años)"
    6 "Adultos/as (45 - 59 años)"
    7 "Adultos/as mayores (60 y más años)"

TABLE ttito
    AS CROSSTABS
    OF Distrito.CCDI
     BY Poblacio.EDADCICL
    WEIGHT Poblacio.FACTORPOND
    AREABREAK Distrito
     DECIMALS 2