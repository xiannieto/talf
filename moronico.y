 
%{

  #include <stdio.h>
  extern FILE *yyin;
  extern int yylex();
  
  int yyerror(char *s);

  #define YYDEBUG 1

%}

%token ABSTRACTO AND ASOCIATIVA BOOLEANO CABECERA CADENA CASO CARACTER CARGA CLASE CONJUNTO CONSTANTE CUERPO CTC_BOOLEANA CTC_CADENA CTC_CARACTER CTC_ENTERA CTC_REAL CONSTRUCTOR CUANDO CUATRO_PTOS DESCENDENTE DESPD DESPI DESTRUCTOR DE DEVOLVER DOS_PTOS EJECUTA ELEMENTO EN ENTERO ENTONCES EQ ESPECIFICO EXCEPTO FICHERO FINAL FINALMENTE FLECHA_DOBLE FUNCION GEQ GENERICO HASTA IDENTIFICADOR INTERFAZ LANZAR LEQ LISTA MIENTRAS MODIFICABLE NEQ OTRO OR PAQUETE PARA PATH POTENCIA PRIVADO PROBAR PROCEDIMIENTO PROGRAMA PUBLICO REAL REGISTRO REPITE SEA SALIR SEMIPUBLICO SI SINO TIPO VARIABLE

%right OR
%right AND
%nonassoc '!'
%left '<' '>' LEQ GEQ EQ NEQ
%left '@' 
%left '^'
%left '&'
%left DESPI DESPD
%left '+' '-'
%left '*' '/' '%'
%right POTENCIA
%nonassoc MENOS_UNITARIO


%%

/********************************/
/* programas, paquetes y cargas */
/********************************/


programa : definicion_programa		{printf("programa -> definicion_programa \n");}
	   | definicion_paquete		{printf("programa -> definicion_paquete \n");}
;

definicion_programa : PROGRAMA nombre ';' bloque_programa	{printf("definicion_programa -> PROGRAMA nombre ';' bloque programa \n");}
;

/*[IDENTIFICADOR "::"]**/
nombre : IDENTIFICADOR CUATRO_PTOS nombre	{printf("nombre -> IDENTIFICADOR CUATRO_PTOS nombre \n");}
       | IDENTIFICADOR				{printf("nombre -> IDENTIFICADOR \n");}
       ;

bloque_programa : declaracion_cargas_op		{printf("bloque_programa -> declaracion_cargas_op \n");}
		    declaracion_tipos_op	{printf("bloque_programa -> declaracion_tipos_op \n");}
		    declaracion_constantes_op	{printf("bloque_programa -> declaracion_constantes_op \n");}
		    declaracion_variables_op	{printf("bloque_programa -> declaracion_variables_op \n");}
		    bloque_instrucciones	{printf("bloque_programa -> bloque_instrucciones \n");}
;

/*[ declaracion_cargas ]?*/
declaracion_cargas_op : declaracion_cargas	{printf("declaracion_cargas_op -> declaracion_cargas \n");}
					| 	{printf("declaracion_cargas_op ->  \n");}
					;

/*[ declaracion_tipos ]?*/
declaracion_tipos_op : declaracion_tipos	{printf("declaracion_tipos_op -> declaracion_tipos \n");}
					| 	{printf("declaracion_tipos_op ->  \n");}
					;

/*[ declaracion_constantes ]?*/
declaracion_constantes_op : declaracion_constantes	{printf("declaracion_constantes_op -> declaracion_constantes \n");}
					| 		{printf("declaracion_constantes_op ->  \n");}
					;

/*[ declaracion_variables ]?*/
declaracion_variables_op : declaracion_variables	{printf("declaracion_variables_op -> declaracion_variables \n");}
					| 		{printf("declaracion_variables_op ->  \n");}
					;

bloque_instrucciones : '{' instruccion_mas '}'	{printf("bloque_instrucciones -> '{' instruccion_mas '} \n");}
                    ;

/*[instruccion]+*/
instruccion_mas : instruccion_mas instruccion 	{printf(" instruccion_mas -> instruccion_mas instruccion \n");}
              | instruccion			{printf(" instruccion_mas -> instruccion \n");}
              ;

definicion_paquete : PAQUETE nombre ';' seccion_cabecera seccion_cuerpo	{printf(" definicion_paquete -> PAQUETE nombre ';' seccion_cabecera seccion_cuerpo \n");}
;

seccion_cabecera : CABECERA			{printf("seccion_cabecera -> CABECERA \n");}
		     declaracion_cargas_op 	{printf("seccion_cabecera -> declaracion_cargas_op \n");}
		     declaracion_tipos_op	{printf("seccion_cabecera -> declaracion_tipos_op \n");}
		     declaracion_constantes_op	{printf("seccion_cabecera -> declaracion_constantes_op \n");}
		     declaracion_variables_op	{printf("seccion_cabecera -> declaracion_variables_op \n");}
		     declaracion_interfaces_op	{printf("seccion_cabecera -> declaracion_interfaces_op \n");}
;

/*[ declaracion_interfaces ]?*/
declaracion_interfaces_op : declaracion_interfaces	{printf("declaracion_interfaces_op -> declaracion_interfaces \n");}
					| 		{printf("declaracion_interfaces_op -> \n");}
					;

seccion_cuerpo : CUERPO				{printf(" seccion_cuerpo -> CUERPO \n");}
		   declaracion_tipos_op		{printf(" seccion_cuerpo -> declaracion_tipos_op \n");}
		   declaracion_constantes_op	{printf(" seccion_cuerpo -> declaracion_constantes_op \n");}
		   declaracion_variables_op	{printf(" seccion_cuerpo -> declaracion_variables_op \n");}
		   declaracion_subprograma_mas	{printf(" seccion_cuerpo -> declaracion_subprograma_mas \n");}
;

/*[declaracion_subprograma]+*/
declaracion_subprograma_mas : declaracion_subprograma_mas declaracion_subprograma	{printf("declaracion_subprograma_mas -> declaracion_subprograma_mas declaracion_subprograma \n");}
              | declaracion_subprograma		{printf("declaracion_subprograma_mas -> declaracion_subprograma \n");}
              ;

declaracion_cargas : CARGA declaracion_carga_mas ';'	{printf("declaracion_cargas -> CARGA declaracion_cargas_mas ';' \n");}
;

/*( declaracion_carga )+*/
declaracion_carga_mas : declaracion_carga_mas ',' declaracion_carga	{printf("declaracion_carga_mas -> declaracion_carga_mas ',' declaracion_carga \n");}
                        | declaracion_carga	{printf("declaracion_carga_mas -> declaracion_carga \n");}
                        ;

/*[ EN PATH ]? [ '(' ( nombre )+ ')' ]?*/
declaracion_carga : nombre EN PATH '(' nombre_mas ')' 	{printf("declaracion_carga -> nombre EN PATH '(' nombre_mas ') \n");}
                  | nombre EN PATH			{printf("declaracion_carga -> nombre EN PATH \n");}
                  | nombre '(' nombre_mas ')'		{printf("declaracion_carga -> nombre '(' nombre_mas ')' \n");}
                  | nombre				{printf("declaracion_carga -> nombre \n");}
                  ;

nombre_mas : nombre_mas ',' nombre		{printf("nombre_mas -> nombre_mas ',' nombre \n");}
                 | nombre			{printf("nombre_mas -> nombre \n");}
                 ;


/************************/
/* tipos (incl. clases) */
/************************/


declaracion_tipos : TIPO declaracion_tipo_mas		{printf("declaracion_tipos -> TIPO declaracion_tipo_mas \n");}
;

/*[declaracion_tipo_mas]+*/
declaracion_tipo_mas : declaracion_tipo_mas declaracion_tipo	{printf(" declaracion_tipo_mas -> declaracion_tipo_mas declaracion_tipo \n");}
              | declaracion_tipo				{printf("declaracion_tipo_mas -> declaracion_tipo \n");}
              ;	

declaracion_tipo : nombre '=' tipo_no_estructurado_o_nombre_tipo ';'	{printf("declaracion_tipo -> nombre '=' tipo_no_estructurado_o_nombre_tipo ';' \n");}
		   | nombre '=' tipo_estructurado			{printf("declaracion_tipo -> nombre '=' tipo_estructurado \n");}
;

tipo_no_estructurado_o_nombre_tipo : nombre		{printf("tipo_no_estructurado_o_nombre_tipo -> nombre \n");}
				     | tipo_escalar	{printf("tipo_no_estructurado_o_nombre_tipo -> tipo_escalar \n");}
				     | tipo_fichero	{printf("tipo_no_estructurado_o_nombre_tipo -> tipo_fichero \n");}
				     | tipo_enumerado	{printf("tipo_no_estructurado_o_nombre_tipo -> tipo_enumerado \n");}
				     | tipo_lista	{printf("tipo_no_estructurado_o_nombre_tipo -> tipo_lista \n");}
				     | tipo_lista_asociativa	{printf("tipo_no_estructurado_o_nombre_tipo -> tipo_lista_asociativa \n");}
				     | tipo_conjunto	{printf("tipo_no_estructurado_o_nombre_tipo -> tipo_conjunto \n");}
;

tipo_estructurado : tipo_registro 	{printf("tipo_estructurado -> tipo_registro \n");}
		| declaracion_clase	{printf("tipo_estructurado -> declaracion_clase \n");}
;

tipo_escalar : ENTERO 	{printf("tipo_escalar -> ENTERO \n");}
	| REAL 		{printf("tipo_escalar -> REAL \n");}
	| BOOLEANO 	{printf("tipo_escalar -> BOOLEANO \n");}
	| CARACTER 	{printf("tipo_escalar -> CARACTER \n");}
	| CADENA	{printf("tipo_escalar -> CADENA \n");}
;

tipo_fichero : FICHERO	{printf("tipo_fichero -> FICHERO \n");}
;

tipo_enumerado : '(' expresion_constante_mas ')'	{printf("tipo_enumerado -> '(' expresion_constante_mas ') \n");}
;

/* ( expresion_constante )+ */
expresion_constante_mas : expresion_constante_mas ',' expresion_constante	{printf(" expresion_constante_mas -> expresion_constante_mas ',' expresion_constante \n");}
                             | expresion_constante		{printf("expresion_constante_mas -> expresion_constante  \n");}
                             ;

tipo_lista : LISTA '[' rango_mas ']' DE tipo_no_estructurado_o_nombre_tipo	{printf(" tipo_lista -> LISTA '[' rango_mas ']' DE tipo_no_estructurado_o_nombre_tipo \n");}	
			| LISTA DE tipo_no_estructurado_o_nombre_tipo		{printf("tipo_lista -> LISTA DE tipo_no_estructurado_o_nombre_tipo \n");}
			;

/* ( rango )+ */
rango_mas : rango_mas ',' rango		{printf(" rango_mas -> rango_mas ',' rango \n");}
        	| rango			{printf(" rango_mas -> rango \n");}
            ;

rango : expresion DOS_PTOS expresion DOS_PTOS expresion	{printf(" rango -> expresion DOS_PTOS expresion DOS_PTOS expresion \n");}
	| expresion DOS_PTOS expresion			{printf(" rango -> expresion DOS_PTOS expresion \n");}
	;

tipo_lista_asociativa : LISTA ASOCIATIVA DE tipo_no_estructurado_o_nombre_tipo	{printf("tipo_lista_asociativa -> LISTA ASOCIATIVA DE tipo_no_estructurado_o_nombre_tipo \n");}
;

tipo_conjunto : CONJUNTO DE tipo_no_estructurado_o_nombre_tipo	{printf(" tipo_conjunto -> CONJUNTO DE tipo_no_estructurado_o_nombre_tipo \n");}
;

tipo_registro : REGISTRO '{' declaracion_campo_mas '}'	{printf(" tipo_registro -> REGISTRO '{' declaracion_campo_mas '}' \n");}
;

/*[ declaracion_campo ]+*/
declaracion_campo_mas : declaracion_campo_mas declaracion_campo	{printf(" declaracion_campo_mas -> declaracion_campo_mas declaracion_campo \n");}
					| declaracion_campo	{printf(" declaracion_campo_mas -> declaracion_campo  \n");}
					;

declaracion_campo : nombre_mas ':' tipo_no_estructurado_o_nombre_tipo ';'	{printf(" declaracion_campo -> nombre_mas ':' tipo_no_estructurado_o_nombre_tipo ';' \n");}
;

declaracion_clase : CLASE final_op nombre_op '{' declaraciones_publicas declaraciones_semi_op declaraciones_privadas_op '}' {printf(" declaracion_clase -> CLASE final_op nombre_op '{' declaraciones_publicas declaraciones_semi_op declaraciones_privadas_op '}' \n");}
;

/*[ FINAL ]?*/
final_op : FINAL	{printf("final_op -> FINAL \n");}
		| 	{printf("final_op ->  \n");}
		;

/*[ '(' ( nombre )+ ')' ]?*/
nombre_op : '(' nombre_mas ')'	{printf(" nombre_op -> '(' nombre_mas ')' \n");}
		| 		{printf(" nombre_op ->  \n");}
		;

/*[ declaraciones_semi ]?*/
declaraciones_semi_op : declaraciones_semi	{printf(" declaraciones_semi_op -> declaraciones_semi \n");}
					|	{printf(" declaraciones_semi_op ->  \n");}
					;

/*[ declaraciones_privadas ]?*/
declaraciones_privadas_op : declaraciones_privadas	{printf(" declaraciones_privadas_op -> declaraciones_privadas \n");}
						| 	{printf(" declaraciones_privadas_op ->  \n");}
						;

declaraciones_publicas : PUBLICO  declaracion_componente_mas	{printf(" declaraciones_publicas -> PUBLICO  declaracion_componente_mas \n");}
					| declaracion_componente_mas	{printf(" declaraciones_publicas -> declaracion_componente_mas \n");}
;

/*[ declaracion_componente ]+*/
declaracion_componente_mas : declaracion_componente_mas declaracion_componente	{printf(" declaracion_componente_mas -> declaracion_componente_mas declaracion_componente \n");}
						| declaracion_componente	{printf(" declaracion_componente_mas -> declaracion_componente \n");}
						;

declaraciones_semi : SEMIPUBLICO declaracion_componente_mas		{printf(" declaraciones_semi -> SEMIPUBLICO declaracion_componente_mas \n");}
;

declaraciones_privadas : PRIVADO declaracion_componente_mas		{printf(" declaraciones_privadas -> PRIVADO declaracion_componente_mas \n");}
;

declaracion_componente : declaracion_tipo_anidado			{printf(" declaracion_componente -> declaracion_tipo_anidado \n");}
			| declaracion_constante_anidada			{printf(" declaracion_componente -> declaracion_constante_anidada \n");}
			| declaracion_atributos				{printf(" declaracion_componente -> declaracion_atributos \n");}
			| cabecera_subprograma ';' modificadores_op	{printf(" declaracion_componente -> cabecera_subprograma \n");}
			;

/*[ modificadores ';' ]?*/
modificadores_op : modificadores ';'			{printf(" modificadores_op -> modificadores ';' \n");}
				| 			{printf(" modificadores_op ->  \n");}
				;

declaracion_tipo_anidado : TIPO declaracion_tipo	{printf(" declaracion_tipo_anidado -> TIPO declaracion_tipo \n");}
;

declaracion_constante_anidada : CONSTANTE declaracion_constante		{printf(" declaracion_constante_anidada -> CONSTANTE declaracion_constante \n");}
;

declaracion_atributos : nombre_mas ':' tipo_no_estructurado_o_nombre_tipo ';'		{printf(" declaracion_atributos -> nombre_mas ':' tipo_no_estructurado_o_nombre_tipo ';' \n");}
;

modificadores :  modificadores ',' modificador	{printf(" modificadores ->  modificadores ',' modificador \n");}
			| modificador		{printf(" modificadores -> modificador \n");}
;

modificador : GENERICO 		{printf(" modificador -> GENERICO \n");}
	| ABSTRACTO 		{printf(" modificador -> ABSTRACTO \n");}
	| ESPECIFICO 		{printf(" modificador -> ESPECIFICO \n");}
	| FINAL			{printf(" modificador -> FINAL \n");}
;


/*************************************/
/* constantes, variables, interfaces */
/*************************************/


declaracion_constantes : CONSTANTE declaracion_constante_mas	{printf(" declaracion_constantes -> CONSTANTE declaracion_constante_mas \n");}
;

/*[ declaracion_constante ]+*/
declaracion_constante_mas : declaracion_constante_mas declaracion_constante	{printf("declaracion_constante_mas -> declaracion_constante_mas declaracion_constante \n");}
					| declaracion_constante			{printf("declaracion_constante_mas -> declaracion_constante \n");}
					;

declaracion_constante : nombre ':' tipo_no_estructurado_o_nombre_tipo '=' valor_constante ';'		{printf(" declaracion_constante -> nombre ':' tipo_no_estructurado_o_nombre_tipo '=' valor_constante ';' \n");}
;

valor_constante : expresion				{printf(" valor_constante -> expresion \n");}
		  | '[' valor_constante_mas ']'		{printf(" valor_constante -> '[' valor_constante_mas ']' \n");}
		  | '[' clave_valor_mas ']'		{printf(" valor_constante -> '[' clave_valor_mas ']' \n");}
		  | '[' campo_valor_mas ']'		{printf(" valor_constante -> '[' campo_valor_mas '] \n");}
;

/*( valor_constante )+*/
valor_constante_mas : valor_constante_mas ',' valor_constante	{printf(" valor_constante_mas -> valor_constante_mas ',' valor_constante \n");}
					| valor_constante	{printf(" valor_constante_mas -> valor_constante \n");}
					;

/*( clave_valor )+*/
clave_valor_mas : clave_valor_mas ',' clave_valor		{printf(" clave_valor_mas -> clave_valor_mas ',' clave_valor \n");}
					| clave_valor		{printf(" clave_valor_mas -> clave_valor \n");}
					;

/*( campo_valor )+*/
campo_valor_mas : campo_valor_mas ',' campo_valor		{printf(" campo_valor_mas -> campo_valor_mas ',' campo_valor \n");}
					| campo_valor		{printf(" campo_valor_mas -> campo_valor \n");}
					;

clave_valor : CTC_CADENA FLECHA_DOBLE valor_constante		{printf(" clave_valor -> CTC_CADENA FLECHA_DOBLE valor_constante \n");}
;

campo_valor : nombre FLECHA_DOBLE valor_constante		{printf(" campo_valor -> nombre FLECHA_DOBLE valor_constante \n");}
;

declaracion_variables : VARIABLE declaracion_variable_mas	{printf(" declaracion_variables -> VARIABLE declaracion_variable_mas \n");}
;

/*[ declaracion_variable ]+*/
declaracion_variable_mas : declaracion_variable_mas declaracion_variable	{printf(" declaracion_variable_mas -> declaracion_variable_mas declaracion_variable \n");}
						| declaracion_variable		{printf(" declaracion_variable_mas -> declaracion_variable \n");}
						;

declaracion_variable : nombre_mas ':' tipo_no_estructurado_o_nombre_tipo valor_constante_op ';'	{printf(" declaracion_variable -> nombre_mas ':' tipo_no_estructurado_o_nombre_tipo valor_constante_op ';' \n");}
;

/*[ '=' valor_constante ]?*/
valor_constante_op : '=' valor_constante	{printf(" valor_constante_op -> '=' valor_constante \n");}
				|		{printf(" valor_constante_op -> \n");}
				;

declaracion_interfaces : INTERFAZ cabecera_subprograma_mas	{printf(" declaracion_interfaces -> INTERFAZ cabecera_subprograma_mas \n");}
;

/*[ cabecera_subprograma ';' ]+*/
cabecera_subprograma_mas :  cabecera_subprograma_mas cabecera_subprograma ';'	{printf(" cabecera_subprograma_mas -> cabecera_subprograma_mas cabecera_subprograma ';'  \n");}
						| cabecera_subprograma ';'	{printf(" cabecera_subprograma_mas -> cabecera_subprograma ';' \n");}
						;


/****************/
/* subprogramas */
/****************/


declaracion_subprograma : cabecera_subprograma bloque_subprograma	{printf(" declaracion_subprograma -> cabecera_subprograma bloque_subprograma \n");}
;

cabecera_subprograma : cabecera_funcion			{printf(" cabecera_subprograma -> cabecera_funcion \n");}
			| cabecera_procedimiento	{printf(" cabecera_subprograma -> cabecera_procedimiento \n");}
			| cabecera_constructor		{printf(" cabecera_subprograma -> cabecera_constructor \n");}
			| cabecera_destructor		{printf(" cabecera_subprograma -> cabecera_destructor \n");}
;

cabecera_funcion : FUNCION nombre declaracion_parametros_op FLECHA_DOBLE tipo_no_estructurado_o_nombre_tipo	{printf(" cabecera_funcion -> FUNCION nombre declaracion_parametros_op FLECHA_DOBLE tipo_no_estructurado_o_nombre_tipo \n");}
;

/*[ declaracion_parametros ]?*/
declaracion_parametros_op : declaracion_parametros	{printf(" declaracion_parametros_op -> declaracion_parametros \n");}
						|	{printf(" declaracion_parametros_op ->  \n");}
						;

cabecera_procedimiento : PROCEDIMIENTO nombre declaracion_parametros_op		{printf(" cabecera_procedimiento -> PROCEDIMIENTO nombre declaracion_parametros_op \n");}
;

cabecera_constructor : CONSTRUCTOR nombre declaracion_parametros_op		{printf(" cabecera_constructor -> CONSTRUCTOR nombre declaracion_parametros_op \n");}
;

cabecera_destructor : DESTRUCTOR nombre		{printf(" cabecera_destructor -> DESTRUCTOR nombre \n");}
;

declaracion_parametros : '(' lista_parametros_formales ')'	{printf(" declaracion_parametros -> '(' lista_parametros_formales ')' \n");}
;

lista_parametros_formales : parametros_formales					{printf(" lista_parametros_formales -> parametros_formales \n");}
			| lista_parametros_formales ';' parametros_formales	{printf(" lista_parametros_formales -> lista_parametros_formales ';' parametros_formales \n");}
;

parametros_formales : nombre_mas ':' tipo_no_estructurado_o_nombre_tipo MODIFICABLE 		{printf(" parametros_formales -> nombre_mas ':' tipo_no_estructurado_o_nombre_tipo MODIFICABLE \n");}
					| nombre_mas ':' tipo_no_estructurado_o_nombre_tipo	{printf(" parametros_formales -> tipo_no_estructurado_o_nombre_tipo  \n");}
;

bloque_subprograma : declaracion_tipos_op declaracion_constantes_op declaracion_variables_op bloque_instrucciones	{printf(" bloque_subprograma -> declaracion_tipos_op declaracion_constantes_op declaracion_variables_op bloque_instrucciones \n");}
;


/*****************/
/* instrucciones */
/*****************/


instruccion : ';'			{printf(" instruccion -> ';' \n");}
	| instruccion_asignacion	{printf(" instruccion -> instruccion_asignacion \n");}
	| instruccion_salir		{printf(" instruccion -> instruccion_salir \n");}
	| instruccion_devolver		{printf(" instruccion -> instruccion_devolver \n");}
	| instruccion_llamada		{printf(" instruccion -> instruccion_llamada \n");}
	| instruccion_si		{printf(" instruccion -> instruccion_si \n");}
	| instruccion_casos		{printf(" instruccion -> instruccion_casos \n");}
	| instruccion_bucle		{printf(" instruccion -> instruccion_bucle \n");}
	| instruccion_probar_excepto	{printf(" instruccion -> instruccion_probar_excepto \n");}
	| instruccion_lanzar		{printf(" instruccion -> instruccion_lanzar \n");}
;

instruccion_asignacion : objeto '=' expresion ';'	{printf(" instruccion_asignacion -> objeto '=' expresion ';' \n");}
;

instruccion_salir : SALIR SI expresion ';'	{printf(" instruccion_salir -> SALIR SI expresion ';' \n");}
				| SALIR ';'	{printf(" instruccion_salir -> SALIR ';' \n");}
;

instruccion_devolver : DEVOLVER expresion ';'	{printf(" instruccion_devolver -> DEVOLVER expresion ';' \n");}
			| DEVOLVER ';'		{printf(" instruccion_devolver -> DEVOLVER ';' \n");}
;

instruccion_llamada : llamada_subprograma ';'	{printf(" instruccion_llamada -> llamada_subprograma ';' \n");}
;

llamada_subprograma : nombre '(' expresion_por ')'	{printf(" llamada_subprograma -> nombre '(' expresion_por ')' \n");}
                    | nombre '(' ')'	{printf(" llamada_subprograma -> nombre '(' ')' \n");}
;

/*( expresion )* */
expresion_por : expresion_por ',' expresion	{printf(" expresion_por -> expresion_por ',' expresion \n");}
			| expresion			{printf(" expresion_por ->  \n");}
			;

instruccion_si : SI expresion ENTONCES bloque_instrucciones SINO bloque_instrucciones	{printf(" instruccion_si -> SI expresion ENTONCES bloque_instrucciones SINO bloque_instrucciones \n");}
				| SI expresion ENTONCES bloque_instrucciones		{printf(" instruccion_si -> SI expresion ENTONCES bloque_instrucciones \n");}
;

instruccion_casos : EN CASO expresion SEA caso_mas ';'		{printf(" instruccion_casos -> EN CASO expresion SEA caso_mas ';'  \n");}
;

/*[ caso ]+*/
caso_mas : caso_mas caso	{printf(" caso_mas -> caso_mas caso \n");}
		| caso		{printf(" caso_mas ->  caso \n");}
		;

caso : entradas FLECHA_DOBLE bloque_instrucciones	{printf(" caso -> entradas FLECHA_DOBLE bloque_instrucciones \n");}
;

entradas : entrada '|' entradas		{printf(" entradas -> entrada '|' entradas \n");}
	| entrada			{printf(" entradas -> entrada \n");}
;
	
entrada : expresion	{printf(" entrada -> expresion \n");}
	| rango 	{printf(" entrada -> rango \n");}
	| OTRO		{printf(" entrada -> OTRO \n");}
;

instruccion_bucle : clausula_iteracion bloque_instrucciones	{printf(" instruccion_bucle -> clausula_iteracion bloque_instrucciones \n");}
;

clausula_iteracion : PARA nombre EN objeto				{printf(" clausula_iteracion -> PARA nombre EN objeto \n");}
		| REPITE ELEMENTO nombre EN rango descendente_op	{printf(" clausula_iteracion -> REPITE ELEMENTO nombre EN rango descendente_op \n");}
		| MIENTRAS expresion					{printf(" clausula_iteracion -> MIENTRAS expresion \n");}
		| REPITE HASTA expresion				{printf(" clausula_iteracion -> REPITE HASTA expresion \n");}
;

/*[ DESCENDENTE ]?*/
descendente_op : DESCENDENTE	{printf(" descendente_op -> DESCENDENTE \n");}
		|		{printf(" descendente_op ->  \n");}
;

instruccion_probar_excepto : PROBAR bloque_instrucciones EXCEPTO clausula_excepcion_mas finalmente_op	{printf(" instruccion_probar_excepto -> PROBAR bloque_instrucciones EXCEPTO clausula_excepcion_mas finalmente_op \n");}
;

/*[ clausula_excepcion ]+*/
clausula_excepcion_mas : clausula_excepcion_mas clausula_excepcion	{printf(" clausula_excepcion_mas -> clausula_excepcion_mas clausula_excepcion \n");}
		| clausula_excepcion					{printf(" clausula_excepcion_mas -> clausula_excepcion \n");}
;

/*[ FINALMENTE bloque_instrucciones ]?*/
finalmente_op : FINALMENTE bloque_instrucciones		{printf(" finalmente_op -> FINALMENTE bloque_instrucciones \n");}
		|					{printf(" finalmente_op ->  \n");}
;

clausula_excepcion : CUANDO nombre EJECUTA bloque_instrucciones		{printf(" clausula_excepcion -> CUANDO nombre EJECUTA bloque_instrucciones \n");}
;

instruccion_lanzar : LANZAR nombre ';'		{printf(" instruccion_lanzar -> LANZAR nombre ';' \n");}
;


/***************/
/* expresiones */
/***************/


objeto : nombre				{printf(" objeto -> nombre \n");}
	| objeto '[' expresion_mas ']'	{printf(" objeto -> '[' expresion_mas ']' \n");}
	| objeto '.' IDENTIFICADOR	{printf(" objeto -> objeto '.' IDENTIFICADOR \n");}
;

/*( expresion )+*/
expresion_mas : expresion_mas ',' expresion	{printf(" expresion_mas -> expresion_mas ',' expresion \n");}
	| expresion				{printf(" expresion_mas -> expresion \n");}
;

expresion_constante : CTC_ENTERA	{printf("expresion_constante ->  ENTERO \n");} 	
		| CTC_REAL 		{printf("expresion_constante -> REAL \n");}
		| CTC_CADENA 		{printf("expresion_constante -> CADENA \n");}
		| CTC_CARACTER 		{printf("expresion_constante -> CARACTER \n");}
		| CTC_BOOLEANA		{printf("expresion_constante -> BOOLEANA \n");}
;

expresion_primaria : expresion_constante 	{printf("expresion_primaria ->  expresion_constante \n");}
		| objeto 			{printf("expresion_primaria ->  objeto \n");}
		| llamada_subprograma 		{printf("expresion_primaria ->  llamada_subprograma \n");}
		| '(' expresion ')'		{printf("expresion_primaria ->  (' expresion ')' \n");}
;

expresion: '-' expresion %prec MENOS_UNITARIO	{printf("expresion ->  '-' expresion \n");}
  | expresion POTENCIA expresion		{printf("expresion ->  expresion POTENCIA expresion \n");}
  | expresion '*' expresion			{printf("expresion ->  expresion '*' expresion \n");}
  | expresion '/' expresion			{printf("expresion ->  expresion '/' expresion \n");}
  | expresion '%' expresion			{printf("expresion ->  expresion PORCENTAJE expresion \n");}
  | expresion '+' expresion			{printf("expresion ->  expresion '+' expresion \n");}
  | expresion '-' expresion			{printf("expresion ->  expresion '-' expresion \n");}
  | expresion DESPI expresion			{printf("expresion ->  expresion DESPI expresion \n");}
  | expresion DESPD expresion			{printf("expresion ->  expresion DESPD expresion \n");}
  | expresion '&' expresion			{printf("expresion ->  expresion '&' expresion \n");}
  | expresion '^' expresion			{printf("expresion ->  expresion '^' expresion \n");}
  | expresion '@' expresion			{printf("expresion ->  expresion '@' expresion \n");}
  | expresion '<' expresion			{printf("expresion ->  expresion '<' expresion \n");}
  | expresion '>' expresion			{printf("expresion ->  expresion '>' expresion \n");}
  | expresion LEQ expresion			{printf("expresion ->  expresion LEQ expresion \n");}
  | expresion GEQ expresion			{printf("expresion ->  expresion GEQ expresion \n");}
  | expresion EQ expresion			{printf("expresion ->  expresion EQ expresion \n");}
  | expresion NEQ expresion			{printf("expresion ->  expresion NEQ expresion \n");}
  | '!' expresion				{printf("expresion ->  '!' expresion \n");}
  | expresion AND expresion			{printf("expresion ->  expresion AND expresion \n");}
  | expresion OR expresion			{printf("expresion ->  expresion OR expresion \n");}
  | expresion_primaria				{printf("expresion ->  expresion_primaria \n");}
  ;
  
%%

int yyerror(char *s) {
  fflush(stdout);
  printf("***************** %s\n",s);
  }

int yywrap() {
  return(1);
  }

int main(int argc, char *argv[]) {

  yydebug = 0;

  if (argc < 2) {
    printf("Uso: ./moronico NombreArchivo\n");
    }
  else {
    yyin = fopen(argv[1],"r");
    yyparse();
    }
  }
