
/*POBLACIÓN TOTAL CON ACCESO A AGUA POR RED PÚBLICA

Descripción del programa:
Este programa crea una variable a nivel de persona para contar
el número de personas con acceso a agua por red publica. Usando la variable 
abastecimento de agua por red publica VIVIENDA.C2P6 <= 3, ponderando a población total.

Solo se considera viviendas con ocupantes presentes VIVIENDA.C2P2 =1 
*/

// Variable que cuenta el total de personas por distrito
DEFINE DISTRITO.T_PERSONAS AS COUNT PERSONA
   WEIGHT Poblacio.FACTORPOND
   FOR VIVIENDA.C2P2 =1
   VARLABEL "Total Personas"

// Variable que cuenta el total de personas con acceso agua por distrito
DEFINE DISTRITO.T_AGUA AS COUNT PERSONA
  WEIGHT Poblacio.FACTORPOND     
  FOR VIVIENDA.C2P6  <=3
  VARLABEL "Total personas con agua"
  
// Variable que considera el porcentaje de personas con acceso a agua por distrito
DEFINE DISTRITO.POR_AGUA
  AS 100 * (DISTRITO.T_AGUA / DISTRITO.T_PERSONAS)
  TYPE REAL
  VARLABEL "Porcentaje de personas con acceso agua"
  DECIMALS 1
  
// Tabla de verificación de resultados
  AREALIST DISTRITO, DISTRITO.CCDI,
  DISTRITO.T_AGUA, DISTRITO.T_PERSONAS, DISTRITO.POR_AGUA
  DECIMALS 1
  OUTPUTFILE XLS "Z:\04. INFOPOBLACION_V2\#TEMAS\Presencia de Servicios\Insumos\Poblacion_Agua.xls" UTF8 OVERWRITE
  
/*
AUTOR:  Denis Rodríguez
CARGO:  Analista de datos
UNIDAD ORGÁNICA: Dirección de Población  
INSTITUCIÓN: Ministerio de la Mujer y Poblaciones Vulnerbales - MIMP
*/