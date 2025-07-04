/* CALCULAR LA RELACIÓN DE APOYO POTENCIAL

Descripción del programa:
El programa calcula el indicador de relación de apoyo potencial por distrito. La 
cual se define como la razón de población en edad de trabajr (15-65) sobre la población
depediente de 65 a más años. Se utiliza la variable PERSON.C5P41. El 
programa muestra la generación de una lista a nivel de distrito. */

// Variable que crea tres grandes grupos de edad
DEFINE PERSONA.GRUPO AS RECODE PERSONA.C5P41
    (0 - 14=1)
    (15 - 64=2)
    (65 - HIGHEST =3)
    RANGE 1 - 3
    VARLABEL "Tres grandes grupos de edad"
    VALUELABELS 1 "Jóvenes" 2 "Adultos" 3 "Adultos mayores"
    
// cuenta la cantidad de Adultos Mayores  de 65 a más por distrito (mediante el filtro
// GRUPO = 3
DEFINE DISTRITO.DEPENDIENTE_65 AS COUNT PERSONA
  WEIGHT  PERSONA.FACTORPOND
  FOR PERSONA.GRUPO = 3
  VARLABEL "Población Dependiente de 65 a más"

// cuenta la cantidad de población en edad de trabjar PET por distrito (mediante el filtro
// GRUPO = 2 
DEFINE DISTRITO.INDEPENDIENTE AS COUNT PERSONA
  WEIGHT  PERSONA.FACTORPOND
  FOR PERSONA.GRUPO = 2
  VARLABEL "Población Indepediente"
  
//crea la variable que divide la población dependiente por la independiente
DEFINE DISTRITO.RELACION_APOYO AS DISTRITO.INDEPENDIENTE/DISTRITO.DEPENDIENTE_65
  TYPE REAL
  FOR DISTRITO.DEPENDIENTE_65 > 0
  VARLABEL "Relación de apoyo potencial"
  DECIMALS 2
  
//Finalmente solicitamos un Arealist a Nivel de Comuna con las variables creadas
AREALIST DISTRITO, DISTRITO.NCCDI, DISTRITO.INDEPENDIENTE, DISTRITO.DEPENDIENTE_65, DISTRITO.RELACION_APOYO
  TITLE "Razón de dependencia en Nueva Miranda"
  DECIMALS 2
/*  OUTPUTFILE XLS "Z:\04. INFOPOBLACION_V2\#TEMAS\Poblacion_Dinamica\Insumos\Razon_Depedencia.xls" UTF8 OVERWRITE
 
 
/*
AUTOR:  Denis Rodríguez
CARGO:  Analista de datos
UNIDAD ORGÁNICA: Dirección de Población  
INSTITUCIÓN: Ministerio de la Mujer y Poblaciones Vulnerbales - MIMP
*/