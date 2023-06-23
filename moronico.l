%option yylineno
%{
#include <stdio.h>
#include "moronico.tab.h"
%}

/*Palabras Reservadas*/
PR1 (?i:abstracto)
PR2 (?i:asociativa)
PR3 (?i:booleano)
PR4 (?i:cabecera)
PR5 (?i:cadena)
PR6 (?i:caso)
PR7 (?i:caracter)
PR8 (?i:clase)
PR9 (?i:conjunto)
PR10 (?i:constante)
PR11 (?i:cuerpo)
PR12 (?i:constructor)
PR13 (?i:cuando)
PR14 (?i:descendente)
PR15 (?i:destructor)
PR16 (?i:de)
PR17 (?i:devolver)
PR18 (?i:carga)
PR19 (?i:elemento)
PR20 (?i:ejecuta)
PR21 (?i:en)
PR22 (?i:entero)
PR23 (?i:entonces)
PR24 (?i:especifico)
PR25 (?i:excepto)
PR26 (?i:fichero)
PR27 (?i:final)
PR28 (?i:finalmente)
PR29 (?i:funcion)
PR30 (?i:generico)
PR31 (?i:hasta)
PR32 (?i:interfaz)
PR33 (?i:lanzar)
PR34 (?i:lista)
PR35 (?i:mientras)
PR36 (?i:modificable)
PR37 (?i:otro)
PR38 (?i:paquete)
PR39 (?i:para)
PR40 (?i:privado)
PR41 (?i:probar)
PR42 (?i:procedimiento)
PR43 (?i:programa)
PR44 (?i:publico)
PR45 (?i:real)
PR46 (?i:registro)
PR47 (?i:repite)
PR48 (?i:salir)
PR49 (?i:sea)
PR50 (?i:semipublico)
PR51 (?i:si)
PR52 (?i:sino)
PR53 (?i:tipo)
PR54 (?i:variable)

/*Indentificadores*/
IDENTIFICADOR  [A-Za-z_]+[0-9A-Za-z_]*

/*Constantes*/
DECIMAL [0-9]+
OCTAL \\[0-7]+
HEXADECIMAL \\[Xx][0-9a-fA-F]+
CTC_ENTERA {DECIMAL}|{OCTAL}|{HEXADECIMAL}

TIPO1 {DECIMAL}?'{DECIMAL}
EXPONENTE (\^([+-]?){DECIMAL})?
MANTISA {DECIMAL}|{TIPO1}
TIPO2 {MANTISA}{EXPONENTE}
CTC_REAL  {TIPO1}|{TIPO2}

CARACTER [^\n'\"\?\\] 
CARACTER_ESCAPADO \\['\"\?\\abfnrtv]
CARACTER_DECIMAL \\[0-9]{1,2}|\\[1][0-9][0-9]|\\[2]([0-4][0-9]|[5][0-5])
CARACTER_OCTAL \\[0-3]?[0-7]{1,2}
CARACTER_HEXADECIMAL \\[x][0-9A-Fa-f]{1,2}
PRECARACTER {CARACTER}|{CARACTER_ESCAPADO}|{CARACTER_DECIMAL}|{CARACTER_OCTAL}|{CARACTER_HEXADECIMAL}
CTC_CARACTER '{PRECARACTER}'

PRE_PATH ["/""./""../"][^\\n\"]+
PATH \"{PRE_PATH}\"

ESCAPE_Y_LINEA \\\n
EXCEPCION [\\n]
PRECADENA {CARACTER}|{ESCAPE_Y_LINEA}|{EXCEPCION}|{CARACTER_ESCAPADO}|{CARACTER_DECIMAL}|{CARACTER_OCTAL}|{CARACTER_HEXADECIMAL}
CTC_CADENA \"{PRECADENA}*\"

/*Delimitadores*/
DELIMITADOR "{"|"}"|"("|")"|":"|";"|","|"|"
FLECHA_DOBLE "=>"
DOS_PTOS ".."
CUATRO_PTOS "::"

/*Operadores*/
OPERADOR "="|"+"|"-"|"*"|"%"|"."|"["|"]"|"@"|"~"|"&"|"<"|">"|"!"|"/"
POTENCIA "**"
LEQ "=<"
GEQ ">="
EQ ":="
NEQ "!="
DESPI "<-"
DESPD "->"
AND \/\\
OR \\\/

ESPACIO  [ \t]+

%x COMENTARIO
%x COM_MULTI

%%

\n ; 

{PR1}    printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(ABSTRACTO);
{PR2}    printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(ASOCIATIVA);
{PR3}    printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(BOOLEANO);
{PR4}    printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(CABECERA);
{PR5}    printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(CADENA);
{PR6}    printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(CASO);
{PR7}    printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(CARACTER);
{PR8}    printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(CLASE);
{PR9}    printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(CONJUNTO);
{PR10}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(CONSTANTE);
{PR11}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(CUERPO);
{PR12}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(CONSTRUCTOR);
{PR13}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(CUANDO);
{PR14}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(DESCENDENTE);
{PR15}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(DESTRUCTOR);
{PR16}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(DE);
{PR17}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(DEVOLVER);
{PR18}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(CARGA);
{PR19}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(ELEMENTO); 
{PR20}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(EJECUTA);
{PR21}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(EN);
{PR22}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(ENTERO);
{PR23}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(ENTONCES);
{PR24}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(ESPECIFICO);
{PR25}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(EXCEPTO);
{PR26}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(FICHERO);
{PR27}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(FINAL);
{PR28}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(FINALMENTE);
{PR29}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(FUNCION);
{PR30}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(GENERICO);
{PR31}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(HASTA);
{PR32}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(INTERFAZ);
{PR33}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(LANZAR);
{PR34}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(LISTA);
{PR35}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(MIENTRAS);
{PR36}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(MODIFICABLE);
{PR37}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(OTRO);
{PR38}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(PAQUETE);
{PR39}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(PARA);
{PR40}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(PRIVADO);
{PR41}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(PROBAR);
{PR42}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(PROCEDIMIENTO);
{PR43}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(PROGRAMA);
{PR44}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(PUBLICO);
{PR45}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(REAL);
{PR46}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(REGISTRO);
{PR47}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(REPITE);
{PR48}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(SALIR);
{PR49}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(SEA);
{PR50}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(SEMIPUBLICO);
{PR51}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(SI);
{PR52}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(SINO);
{PR53}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(TIPO);
{PR54}   printf("Linea %d - Palabra reservada: %s\n", yylineno, yytext);return(VARIABLE);

{IDENTIFICADOR}		printf("Linea %d - Identificador: %s\n",yylineno,yytext);return(IDENTIFICADOR);

{CTC_ENTERA}		  printf("Linea %d - Constante Entera: %s\n",yylineno,yytext);return(CTC_ENTERA);
{CTC_REAL}		  printf("Linea %d - Constante Real: %s\n",yylineno,yytext);return(CTC_REAL);
{CTC_CARACTER}	  	  printf("Linea %d - Caracter: %s\n",yylineno,yytext);return(CTC_CARACTER);
{CTC_CADENA}		  printf("Linea %d - Cadena: %s\n",yylineno,yytext);return(CTC_CADENA);
{PATH}            	  printf("Linea %d - Path: %s\n",yylineno,yytext);return(PATH);

{DELIMITADOR}		  printf("Linea %d - Delimitador: %s\n",yylineno,yytext);return(yytext[0]);
{FLECHA_DOBLE}		  printf("Linea %d - FLECHA_DOBLE: %s\n",yylineno,yytext);return(FLECHA_DOBLE);
{DOS_PTOS}		  printf("Linea %d - DOS_PTOS: %s\n",yylineno,yytext);return(DOS_PTOS);
{CUATRO_PTOS}		  printf("Linea %d - CUATRO_PTOS: %s\n",yylineno,yytext);return(CUATRO_PTOS);

{OPERADOR}		  printf("Linea %d - Operador: %s\n",yylineno,yytext);return(yytext[0]);
{POTENCIA}		  printf("Linea %d - Operador POTENCIA: %s\n",yylineno,yytext);return(POTENCIA);
{LEQ} 		  printf("Linea %d - Operador LEQ: %s\n",yylineno,yytext);return(LEQ);
{GEQ} 		  printf("Linea %d - Operador GEQ: %s\n",yylineno,yytext);return(GEQ);
{EQ} 		  printf("Linea %d - Operador EQ: %s\n",yylineno,yytext);return(EQ);
{NEQ} 		  printf("Linea %d - Operador NEQ: %s\n",yylineno,yytext);return(NEQ);
{DESPI} 		  printf("Linea %d - Operador DESPI: %s\n",yylineno,yytext);return(DESPI);
{DESPD} 		  printf("Linea %d - Operador DESPD: %s\n",yylineno,yytext);return(DESPD);
{AND} 		  printf("Linea %d - Operador AND: %s\n",yylineno,yytext);return(AND);
{OR} 		  printf("Linea %d - Operador OR: %s\n",yylineno,yytext);return(OR);

{ESPACIO} ;

"//" BEGIN(COMENTARIO);  printf("Linea %d - Comentario: ",yylineno);
<COMENTARIO>[^\n]        printf("%s",yytext);
<COMENTARIO>\n           printf("\n");BEGIN (0);

"/*" BEGIN(COM_MULTI);  printf("Linea %d - Comentario Multiple: ",yylineno);
<COM_MULTI>[^\n]        printf("%s",yytext);
<COM_MULTI>\n           printf("\n");
<COM_MULTI>"*/"    printf("%s\n",yytext);BEGIN (0);

%%
