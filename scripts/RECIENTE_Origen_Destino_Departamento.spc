/* MIGRACIÓN RECIENTE: MATRIZ ORIGEN DESTINO SEGUN DEPARTAMENTO 
Se usa la variable PERSONA.C5P5BCOD departamento donde  vive permanentemente
que se considera como la residecia habitual la cual va ser el destino. Por otro
lado la variable PERSONA.C5P6BCOD departamento donde vivia hace 5 años se concidera
como departamento de nacimiento el cual sera el origen en la matriz.
Nota: Se excluyen los casos no válidos, residentes hace 5 años en el extranjero, menores de 5 años.
*/

RUNDEF 
SELECTION ALL

///VARIABLES A NIVEL DEPARTAMENTAL RECIENTE

DEFINE PERSONA.DEPARTAMENTO_HABITUAL
AS PERSONA.C5P5BCOD
TYPE INTEGER
LIKE PERSONA.C5P5BCOD
VARLABEL "Departamento de residencia habitual"   

DEFINE PERSONA.DEPARTAMENTO_RESIDENCIA_RECIENTE
AS PERSONA.C5P6BCOD
TYPE INTEGER
LIKE PERSONA.C5P6BCOD
FOR PERSONA.C5P6BCOD<26 AND PERSONA.C5P5BCOD<26
VARLABEL "Departamento de residencia reciente" 


//MATRIZ BÁSICA DE MIGRACIÓN RECIENTE
TABLE MB 
AS CROSSTABS
OF PERSONA.DEPARTAMENTO_HABITUAL                                    
BY PERSONA.DEPARTAMENTO_RESIDENCIA_RECIENTE
FOR PERSONA.DEPARTAMENTO_HABITUAL>0 AND PERSONA.DEPARTAMENTO_RESIDENCIA_RECIENTE>0
WEIGHT PERSONA.FACTORPOND
/*OUTPUTFILE XLS "Z:\04. INFOPOBLACION_V2\#TEMAS\Presencia de Servicios\Insumos\MRECIENTE_Origen_Destino_Departamento.xls" UTF8 OVERWRITE
  
/*
AUTOR:  Denis Rodríguez
CARGO:  Analista de datos
UNIDAD ORGÁNICA: Dirección de Población  
INSTITUCIÓN: Ministerio de la Mujer y Poblaciones Vulnerbales - MIMP
*/