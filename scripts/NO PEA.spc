/* PEA: POBLACIÓN ECONOMICAMENTE NO ACTIVA  SEGÚN DISTRITO

 Se usa las variables:
 - PERSONA.C5P41 >= 14, población de 14 a más años en el perú se considera la edad
 legal para empezar a trabajar.
 - PERSONA.C5P16 = 1, la semana pasada trabajo por algún pago de dinero o en especie.
 - PERSONA.C5P18 = 1, la semana pasada estuvo buscando trabajo activamente.
 - PERSONA.C5P17< = 5, la semana pasada: ¿Qué hacía?, no se considera 
   estuvo al cuidado del hogar y no trabajó (6) y no trabajó (7).  
 - PERSONA.FACTORPOND, se pondera la población.
*/

 //* DENOMINADOR: POBLACIÓN EN EDAD DE TRABAJAR
// Población total de 14 a más años
DEFINE DISTRITO.PET AS COUNT PERSONA
FOR PERSONA.C5P41 >= 14
//WEIGHT PERSONA.FACTORPOND
VARLABEL "Población total de 14 a más años"


 
 // DENOMINADOR: NO PEA 

// La semana pasada no trabajó ni estuvo buscando trabajo activamente
DEFINE DISTRITO.NO_PEA COUNT PERSONA
FOR (PERSONA.C5P41 >= 14 AND PERSONA.C5P16 = 2 AND PERSONA.C5P18 = 2)
//WEIGHT PERSONA.FACTORPOND
VARLABEL "No trabajó ni buscando trabajo activamente"
   
// Porcentaje de personas en la PEA respecto a la población en edad de trabajar
DEFINE DISTRITO.PER_NO_PEA
  AS 100 * (DISTRITO.NO_PEA / DISTRITO.PET)
  TYPE REAL
  VARLABEL "Porcentaje de personas  no PEA"
  DECIMALS 1

// Tabla de verificación de resultados
AREALIST DISTRITO, DISTRITO.CCDI,
  DISTRITO.NO_PEA, 
  DISTRITO.PET, 
  DISTRITO.PER_NO_PEA
 
OUTPUTFILE XLS "Z:\04. INFOPOBLACION_V2\#TEMAS\Aglomeracion\Insumos\Poblacion_No_PEA.xls" UTF8 OVERWRITE
 
  /*
AUTOR:  Denis Rodríguez
CARGO:  Analista de datos
UNIDAD ORGÁNICA: Dirección de Población  
INSTITUCIÓN: Ministerio de la Mujer y Poblaciones Vulnerbales - MIMP
*/