%{
  (* autor: Maciej Pacut *)

  open Model;;
%}

%token <float> FLOAT
%token <int> INT
%token V
%token F
%token EOE
%token NEWLINE
%start main
%type <Model.model> main
%%
main:
  | EOE  { { vertices=[]; tris=[] } }
  | vertex NEWLINE main { {$3 with vertices=($1 :: $3.vertices)} };
  | tri NEWLINE main { {$3 with tris=($1 :: $3.tris)} };

vertex:
  V FLOAT FLOAT FLOAT { ($2,$3,$4) : vertex3d };
tri:
  F INT INT INT       { ($2,$3,$4) : indexed_tri };

%%
