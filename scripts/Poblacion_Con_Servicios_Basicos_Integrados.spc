
/*POBLACIÓN TOTAL CON ACCESO SERVICIOS BÁSICOS INTEGRADOS

Descripción del programa:
Este programa crea una variable a nivel de persona para contar
el número de personas con acceso a agua, desagüe y electricidad. Usando las variable 
abastecimiento por red pública (VIVIENDA.C2P6 <= 3) Y
Servicio higíenico que tiene la vivienda (VIVIENDA.C2P10 <=3 ) Y
alumbrado electrico por red pública (VIVIENDA.C2P11 = 1), ponderando a población total.

Solo se considera viviendas con ocupantes presentes VIVIENDA.C2P2 =1 
*/

// Variable que cuenta el total de personas por distrito
DEFINE DISTRITO.T_PERSONAS AS COUNT PERSONA
   WEIGHT Poblacio.FACTORPOND
   FOR VIVIENDA.C2P2 =1
   VARLABEL "Total personas"

// Variable que cuenta el total de personas con servicios básicos integrados
DEFINE DISTRITO.T_SSBB_INTEGRADOS AS COUNT PERSONA
  WEIGHT Poblacio.FACTORPOND     
  FOR (VIVIENDA.C2P6 <= 3 AND VIVIENDA.C2P10 <=3 AND VIVIENDA.C2P11 = 1)
  VARLABEL "Total personas con servicios básicos integrados"
  
// Variable que considera el porcentaje de personas con acceso a electricidad por distrito
DEFINE DISTRITO.POR_SSBB_INTEGRADOS
  AS 100 * (DISTRITO.T_SSBB_INTEGRADOS / DISTRITO.T_PERSONAS)
  TYPE REAL
  VARLABEL "Porcentaje de personas con servicios básicos integrados"
  DECIMALS 1
  
// Tabla de verificación de resultados
  AREALIST DISTRITO, DISTRITO.CCDI,
  DISTRITO.T_SSBB_INTEGRADOS, DISTRITO.T_PERSONAS, DISTRITO.POR_SSBB_INTEGRADOS
  DECIMALS 1
  OUTPUTFILE XLS "Z:\04. INFOPOBLACION_V2\#TEMAS\Presencia de Servicios\Insumos\Poblacion_Ssbb_Integrados.xls" UTF8 OVERWRITE
  
/*
AUTOR:  Denis Rodríguez
CARGO:  Analista de datos
UNIDAD ORGÁNICA: Dirección de Población  
INSTITUCIÓN: Ministerio de la Mujer y Poblaciones Vulnerbales - MIMP
*/