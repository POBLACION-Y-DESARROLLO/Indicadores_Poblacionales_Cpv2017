RUNDEF Job
    SELECTION ALL

DEFINE Poblacio.EDADGRA
    AS RECODE Poblacio.EDAD
    (0 - 14=1)
    (15 - 59=2)
    (60 - 64=3)
    (65 - 115=4)
    TYPE INTEGER
    RANGE 1 - 4
    VARLABEL "P: Edad por Grandes Grupos"
    VALUELABELS
    1 "De 00 a 14 años"
    2 "De 15 a 59 años"
    3 "De 60 a 64 años"
    4 "De 65 a más años"

DEFINE Distrito.EDADDENOM
    AS COUNT Poblacio
    WEIGHT Poblacio.FACTORPOND
    FOR ( Poblacio.EDADGRA=2 OR Poblacio.EDADGRA=3 ) AND ( 1=1 )
    TYPE INTEGER

DEFINE Distrito.EDADNUMER
    AS COUNT Poblacio
    WEIGHT Poblacio.FACTORPOND
    FOR ( Poblacio.EDADGRA=1 OR Poblacio.EDADGRA=4 ) AND ( 1=1 )
    TYPE INTEGER

DEFINE Distrito.INDEPR
    AS ( Distrito.EDADNUMER / Distrito.EDADDENOM ) * 100
    FOR Distrito.EDADDENOM > 0
    TYPE REAL
    VARLABEL "Relación de dependencia"
     DECIMALS 2

TABLE AVar0
     AS AREALIST    
     OF Distrito, Distrito.NCCDI 80.0,Distrito.EDADDENOM,  Distrito.EDADNUMER, Distrito.INDEPR INDEPR
     DECIMALS 2
