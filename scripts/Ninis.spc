/* NINIS: JÓVENES QUE NO ESTUDIAN NI TRABAJAN  

 Se usa las variables:
 - DENOMINADOR: PERSONA.C5P41 >= 15 Y PERSONA.C5P41 =< 29
 - PERSONA.C5P16 = 2, la semana pasada trabajo por algún pago de dinero o en especie.
 - PERSONA.C5P18 = 2, la semana pasada estuvo buscando trabajo activamente.
 - PERSONA.C5P17 > 5, la semana pasada: ¿Qué hacía?, no se considera 
   estuvo al cuidado del hogar y no trabajó (6) y no trabajó (7).  
 - PERSONA.FACTORPOND, se pondera la población.
*/


// Población total de 15 a 29 años
DEFINE DISTRITO.JOVEN AS COUNT PERSONA
FOR PERSONA.C5P41 >= 15 AND PERSONA.C5P41 <= 29
WEIGHT PERSONA.FACTORPOND
VARLABEL "Población total de 15 a 29 años"

// No estudia y no trabaja
DEFINE DISTRITO.NINI_15_29 AS COUNT PERSONA
FOR 
  PERSONA.C5P41 >= 15 AND PERSONA.C5P41 <= 29 AND
  PERSONA.C5P16 <> 1 AND
  (PERSONA.C5P16 <> 2 OR PERSONA.C5P18 <> 1) AND
  PERSONA.C5P14 <> 1
WEIGHT PERSONA.FACTORPOND
VARLABEL "Jóvenes de 15 a 29 años que no estudian ni trabajan"

// Porcentaje de personas en la PEA respecto a la población en edad de trabajar
DEFINE DISTRITO.PER_NINI
  AS 100 * (DISTRITO.NINI_15_29/DISTRITO.JOVEN)
  TYPE REAL
  VARLABEL "Porcentaje de NINIS"
  DECIMALS 1

// Tabla de verificación de resultados
AREALIST DISTRITO, DISTRITO.CCDI,
  DISTRITO.JOVEN, 
  DISTRITO.NINI_15_29, 
  DISTRITO.PER_NINI
 
 /*OUTPUTFILE XLS "Z:\04. INFOPOBLACION_V2\#TEMAS\Presencia de Servicios\Insumos\Poblacion_Economicamente_Activa.xls" UTF8 OVERWRITE
  /*
AUTOR:  Denis Rodríguez
CARGO:  Analista de datos
UNIDAD ORGÁNICA: Dirección de Población  
INSTITUCIÓN: Ministerio de la Mujer y Poblaciones Vulnerbales - MIMP
*/