
/*POBLACIÓN TOTAL CON ACCESO A DESAGÜE

Descripción del programa:
Este programa crea una variable a nivel de persona para contar
el número de personas con acceso a agua por red publica. Usando la variable 
servicio higiénico que tiene la vivienda VIVIENDA.C2P106 <= 4, ponderando a población total.

Solo se considera viviendas con ocupantes presentes VIVIENDA.C2P2 =1 
*/

// Variable que cuenta el total de personas por distrito
DEFINE DISTRITO.T_PERSONAS AS COUNT PERSONA
   WEIGHT Poblacio.FACTORPOND
   FOR VIVIENDA.C2P2 =1
   VARLABEL "Total personas"

// Variable que cuenta el total de personas con desagüe por distrito
DEFINE DISTRITO.T_DESAGUE AS COUNT PERSONA
  WEIGHT Poblacio.FACTORPOND     
  FOR VIVIENDA.C2P10 <= 3
  VARLABEL "Total personas con desagüe"
  
// Variable que considera el porcentaje de personas con acceso a desagüe por distrito
DEFINE DISTRITO.POR_DESAGUE
  AS 100 * (DISTRITO.T_DESAGUE / DISTRITO.T_PERSONAS)
  TYPE REAL
  VARLABEL "Porcentaje de personas con acceso a desagüe"
  DECIMALS 1
  
// Tabla de verificación de resultados
  AREALIST DISTRITO, DISTRITO.CCDI,
  DISTRITO.T_DESAGUE, DISTRITO.T_PERSONAS, DISTRITO.POR_DESAGUE
  DECIMALS 1
  OUTPUTFILE XLS "Z:\04. INFOPOBLACION_V2\#TEMAS\Presencia de Servicios\Insumos\Poblacion_Desague.xls" UTF8 OVERWRITE
  
/*
AUTOR:  Denis Rodríguez
CARGO:  Analista de datos
UNIDAD ORGÁNICA: Dirección de Población  
INSTITUCIÓN: Ministerio de la Mujer y Poblaciones Vulnerbales - MIMP
*/