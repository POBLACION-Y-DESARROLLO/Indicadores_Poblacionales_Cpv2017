/* CALCULAR LA RAZON DE DEPENDENCIA BASADA EN GRUPOS DE EDAD

Descripción del programa:
El programa calcula el indicador de dependencia por distrito. La 
razón de dependencia de un área dado se define como la razón de 
dependientes (niños y adultos mayores) por los independientes
(adultos). Se utiliza la variable PERSON.GRP3 (seccion07). El 
programa muestra la generación de una lista a nivel de COMUNA.         */

// Variable que crea tres grandes grupos de edad
DEFINE PERSONA.GRUPO AS RECODE PERSONA.C5P41
    (0 - 14=1)
    (15 - 64=2)
    (65 - HIGHEST =3)
    RANGE 1 - 3
    VARLABEL "Tres grandes grupos de edad"
    VALUELABELS 1 "Jóvenes" 2 "Adultos" 3 "Adultos mayores"
    
// cuenta la cantidad de Jóvenes y Adultos Mayores por distrito (mediante el filtro
// GRUPO <> 2 (aquellos que no sean PET))
DEFINE DISTRITO.DEPENDIENTE AS COUNT PERSONA
  WEIGHT  PERSONA.FACTORPOND
  FOR PERSONA.GRUPO <> 2
  VARLABEL "Población Dependiente"

// cuenta la cantidad de Adultos (PET) por distrito (mediante el filtro
// GRUPO = 2 
DEFINE DISTRITO.INDEPENDIENTE AS COUNT PERSONA
  WEIGHT  PERSONA.FACTORPOND
  FOR PERSONA.GRUPO = 2
  VARLABEL "Población Indepediente"
  
//crea la variable que divide la población dependiente por la independiente
DEFINE DISTRITO.RAZON_DEPEND AS 100 * ( DISTRITO.DEPENDIENTE/ DISTRITO.INDEPENDIENTE)
  TYPE REAL
  FOR DISTRITO.INDEPENDIENTE > 0
  VARLABEL "Razón de dependencia"
  DECIMALS 2
  
//Finalmente solicitamos un Arealist a Nivel de Comuna con las variables creadas
AREALIST DISTRITO, DISTRITO.NCCDI, DISTRITO.DEPENDIENTE, DISTRITO.INDEPENDIENTE, DISTRITO.RAZON_DEPEND
  TITLE "Razón de dependencia en Nueva Miranda"
  DECIMALS 2
  OUTPUTFILE XLS "Z:\04. INFOPOBLACION_V2\#TEMAS\Poblacion_Dinamica\Insumos\Razon_Depedencia.xls" UTF8 OVERWRITE
 
 
/*
AUTOR:  Denis Rodríguez
CARGO:  Analista de datos
UNIDAD ORGÁNICA: Dirección de Población  
INSTITUCIÓN: Ministerio de la Mujer y Poblaciones Vulnerbales - MIMP
*/