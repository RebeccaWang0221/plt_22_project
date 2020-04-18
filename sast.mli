open Ast

type sexpr = typ * sx
and sx =
    SIntLit of int
  | SStrLit of string
  | SBoolLit of bool 
  | SFloatLit of float 
  | SCharLit of char 
  | SLstLit of string
  | SId of string 
  | SBinop of sexpr * op * sexpr 
  | SUnop of string * un
  | SCall of string * sexpr list
  | SPrint of sexpr
  | SAccess of string * sexpr
  | SSlice of string * sexpr * sexpr

type sstmt = 
    SBlock of sstmt list
  | SExpr of sexpr
  | SBind of typ * string
  | SFuncDef of sfunc_def
  | SIf of sexpr * sstmt list * sstmt list
  | SElif of sexpr * sstmt list
  | SElse of sstmt list
  | SWhile of sexpr * sstmt list
  | SFor of sstmt * sexpr * sstmt list
  | SDo of sstmt list * sexpr
  | SReturn of sexpr 
  | SAssign of sexpr * sexpr
  | SDecAssign of sstmt * sexpr
  | SStruct of string * sstmt list
  | SCont 
  | SBreak
  | SPass

type sfunc_def {
	srtyp: typ;
	sfname: string;
	sformals: (typ * string) list;
	slocals: (typ * string) list;
	sbody: sstmt list;
}

type sprogram = sstmt list




