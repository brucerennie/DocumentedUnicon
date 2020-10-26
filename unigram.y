/*
#
# verified to revision 5707 - use svn blame unigram.y &>unigram.blame to get new revisions
# have added the regexp changes not included in the main repository
#
*/
/*
 * NOTE: these %token declarations are generated
 *  automatically by mktoktab from tokens.txt and
 *  op.txt.
 */

/* primitive tokens */

%token  IDENT
%token  INTLIT
%token  REALLIT
%token  STRINGLIT
%token  CSETLIT
%token  EOFX

/* reserved words */

%token  BREAK       /* break     */
%token  BY          /* by        */
%token  CASE        /* case      */
%token  CLASS       /* class     */
%token  CREATE      /* create    */
%token  CRITICAL    /* critical  */
%token  DEFAULT     /* default   */
%token  DO          /* do        */
%token  ELSE        /* else      */
%token  END         /* end       */
%token  EVERY       /* every     */
%token  FAIL        /* fail      */
%token  GLOBAL      /* global    */
%token  IF          /* if        */
%token  IMPORT      /* import    */
%token  iconINITIAL /* initial   */
%token  INITIALLY   /* initially */
%token  INVOCABLE   /* invocable */
%token  LINK        /* link      */
%token  LOCAL       /* local     */
%token  METHOD      /* method    */
%token  NEXT        /* next      */
%token  NOT         /* not       */
%token  OF          /* of        */
%token  PACKAGE     /* package   */
%token  PROCEDURE   /* procedure */
%token  RECORD      /* record    */
%token  REPEAT      /* repeat    */
%token  RETURN      /* return    */
%token  STATIC      /* static    */
%token  SUSPEND     /* suspend   */
%token  THEN        /* then      */
%token  THREAD      /* thread    */
%token  TO          /* to        */
%token  UNTIL       /* until     */
%token  WHILE       /* while     */

/* operators */

%token  BANG        /* !         */
%token  MOD         /* %         */
%token  AUGMOD      /* %:=       */
%token  AND         /* &         */
%token  AUGAND      /* &:=       */
%token  STAR        /* *         */
%token  AUGSTAR     /* *:=       */
%token  INTER       /* **        */
%token  AUGINTER    /* **:=      */
%token  PLUS        /* +         */
%token  AUGPLUS     /* +:=       */
%token  UNION       /* ++        */
%token  AUGUNION    /* ++:=      */
%token  MINUS       /* -         */
%token  AUGMINUS    /* -:=       */
%token  DIFF        /* --        */
%token  AUGDIFF     /* --:=      */
%token  DOT         /* .         */
%token  SLASH       /* /         */
%token  AUGSLASH    /* /:=       */
%token  ASSIGN      /* :=        */
%token  SWAP        /* :=:       */
%token  NMLT        /* <         */
%token  AUGNMLT     /* <:=       */
%token  REVASSIGN   /* <-        */
%token  REVSWAP     /* <->       */
%token  SLT         /* <<        */
%token  AUGSLT      /* <<:=      */
%token  SLE         /* <<=       */
%token  AUGSLE      /* <<=:=     */
%token  NMLE        /* <=        */
%token  AUGNMLE     /* <=:=      */
%token  NMEQ        /* =         */
%token  AUGNMEQ     /* =:=       */
%token  SEQ         /* ==        */
%token  AUGSEQ      /* ==:=      */
%token  EQUIV       /* ===       */
%token  AUGEQUIV    /* ===:=     */
%token  NMGT        /* >         */
%token  AUGNMGT     /* >:=       */
%token  NMGE        /* >=        */
%token  AUGNMGE     /* >=:=      */
%token  SGT         /* >>        */
%token  AUGSGT      /* >>:=      */
%token  SGE         /* >>=       */
%token  AUGSGE      /* >>=:=     */
%token  QMARK       /* ?         */
%token  AUGQMARK    /* ?:=       */
%token  AT          /* @         */
%token  AUGAT       /* @:=       */
%token  BACKSLASH   /* \         */
%token  CARET       /* ^         */
%token  AUGCARET    /* ^:=       */
%token  BAR         /* |         */
%token  CONCAT      /* ||        */
%token  AUGCONCAT   /* ||:=      */
%token  LCONCAT     /* |||       */
%token  AUGLCONCAT  /* |||:=     */
%token  TILDE       /* ~         */
%token  NMNE        /* ~=        */
%token  AUGNMNE     /* ~=:=      */
%token  SNE         /* ~==       */
%token  AUGSNE      /* ~==:=     */
%token  NEQUIV      /* ~===      */
%token  AUGNEQUIV   /* ~===:=    */
%token  LPAREN      /* (         */
%token  RPAREN      /* )         */
%token  PCOLON      /* +:        */
%token  COMMA       /* ,         */
%token  MCOLON      /* -:        */
%token  COLON       /* :         */
%token  COLONCOLON  /* ::        */
%token  SEMICOL     /* ;         */
%token  LBRACK      /* [         */
%token  RBRACK      /* ]         */
%token  LBRACE      /* {         */
%token  RBRACE      /* }         */

%token  DOLLAR      /* $         */
%token  ABSTRACT    /* abstract  */
%token  PMATCH      /*??         */
%token  PAND        /*&&         */
%token  POR        /* .|         */
%token  PUNEVAL      /* ` */
%token  PASSNONMATCH  /* -> */
%token  PIMDASSN      /* $$ */
%token  PSETCUR       /* .$ */

%token  SND          /* @>       */
%token  SNDBK        /* @>>      */
%token  RCV          /* @<       */
%token  RCVBK        /* @<<      */

%token  REGEX
%token  REGEXSTART
%token  REGEXEND
%token  REGEXSTAR
%token  REGEXPLUS
%token  REGEXBAR
%token  REGEXQMARK
%token  REGEXCARET
%token  REGEXHYPHEN
%token  REGEXLBRACE
%token  REGEXRBRACE
%token  REGEXLBRACK
%token  REGEXRBRACK
%token  REGEXDOT
%token  REGEXLPAREN
%token  REGEXRPAREN
%token  REGEXCHAR

%{
#PD:
#: This procedure is used to verify that the value of x2 is one of the actual
#: Unicon keywords
#:
procedure Keyword(x1, x2)
    static  keywords
    initial {
        keywords := set(["allocated",
                         "clock",
                         "collections",
                         "column",
                         "current",
                         "date",
                         "now",
                         "dateline",
                         "digits",
                         "e",
                         "error",
                         "errornumber",
                         "errortext",
                         "errorvalue",
                         "errout",
                         "fail",
                         "eventcode",
                         "eventsource",
                         "eventvalue",
                         "features",
                         "file",
                         "host",
                         "input",
                         "lcase",
                         "letters",
                         "level",
                         "line",
                         "main",
                         "null",
                         "output",
                         "phi",
                         "pi",
                         "pick",
                         "pos",
                         "progname",
                         "random",
                         "regions",
                         "source",
                         "storage",
                         "subject",
                         "time",
                         "trace",
                         "dump",
                         "ucase",
                         "version",
                         "errno",
                         "window",
                         "col",
                         "row",
                         "x",
                         "y",
                         "interval",
                         "control",
                         "shift",
                         "meta",
                         "lpress",
                         "mpress",
                         "rpress",
                         "lrelease",
                         "mrelease",
                         "rrelease",
                         "ldrag",
                         "mdrag",
                         "rdrag",
                         "resize",
                         "ascii",
                         "cset"])
    }

    # verify that x2 is a valid keyword
    if not member(keywords, x2.s) then {
        yyerror("&" || x2.s || " is not a valid keyword!")
    }

    return node("keyword", x1, x2)
end

#PD:
#: This procedure adds any field names from classes to the set used by the iconc
#: compiler. Nothing is required for the interpreter. The relevant field being
#: checked is x3
#:
procedure Field(x1, x2, x3)
    initial {
        set_of_all_fields := set()
        dummyrecno := 1
    }
    #
    # if the source will be processed by iconc, we need to add any identifier
    # that is referencing a field in a record to the set.
    #
    if \iconc then {
        if type(x3) == "token" then {
            insert(set_of_all_fields, x3.s)
#           write(&errout, "field ", image(x3.s))
        }
    }
    #
    # we just return a node of type field as our result
    #
    return node("field", x1, x2, x3)
end

#PD:
#: Using a depth first search, find the first node of type "token" and return
#: a copy of this token.
#;
procedure Clone1stToken(n)
    #
    # there only two cases we need to look at, tokens or treenodes, anything
    # else will fail and cause the search to check the next child in sequence
    #
    case type(n) of {
        #
        # we have found a token, return a copy of the token record
        #
        "token": {
            return copy(n)
        }
        #
        # check each of the children for a successful find of token, failure
        # causes the next child in sequence to be searched, search finishes when
        # a token has been found
        #
        "treenode": {
            return Clone1stToken(!n.children)
        }
    }
end

#PD:
#: This procedure is called at the completion of the syntax parsing process.
#:
procedure Progend(x1)
    local   pe,                         #LV: temprorary to hold each parsing error
                                        #:   that has been found in the program, if any
            cl,                         #LV: temporary to hold each class found in this
                                        #:   run
            super,                      #LV: temporary to hold each of the super class
                                        #:   associated with each of the classes found or
                                        #:   for each class that has been imported
            temp,                       #LV: temporary to hold each of the identifiers
                                        #:   found in unevaluated expression in patterns
            i,                          #LV: used as a counter while moving through the
                                        #:   identifiers found in unevaluated pattern
                                        #:   expressions for the purpose of printing
                                        #:   intervening ","
            arandomfield                #LV: a temporary to hold a random field from

    if *\parsingErrors > 0 then {
        every pe := !parsingErrors do {
            write(&errout, pe.errorMessage)
        }
        istop(*\parsingErrors || " error" ||
              (if *\parsingErrors > 1 then "s" else ""))
    }

    if /x1 then {
        istop("error: empty file")
    }

    package_level_syms := set()
    package_level_class_syms := set()
    set_package_level_syms(x1)
    scopecheck_superclass_decs(x1)

    outline := 1
    outcol := 1
    #
    # export specifications for each class
    #
    native := set()
    every cl := classes.foreach_t() do {
        cl.WriteSpec()
        insert(native, cl)
    }
    #
    # import class specifications, transitively
    #
    repeat {
        added := 0
        every super := ((classes.foreach_t()).foreachsuper() | !imports) do {
            if /classes.lookup(super) then {
                added := 1
                readspec(super)
                cl := classes.lookup(super)
                if /cl then {
                    halt("can't inherit class '", super, "'")
                }
                iwrite("  inherits ", super, " from ", cl.linkfile)
                writelink(cl.dir, cl.linkfile)
            }
        }
        if added = 0 then {
            break
        }
    }
    #
    # Compute the transitive closure of the superclass graph. Then
    # resolve inheritance for each class, and use it to apply scoping rules.
    #
    every (classes.foreach_t()).transitive_closure()
    every (classes.foreach_t()).resolve()

    scopecheck_bodies(x1)

    if \thePackage then {
        every thePackage.insertsym(!package_level_syms)
    }

    if \iconc then  {
        iconc_prep_parse_tree(&null, x1)
    }

    #
    # generate output
    #
#   iwrite("Generating code:")
    yyprint(x1)
    write(yyout)

$ifndef NoPatternIntegration
    # generate invocable declarations for identifiers in unevaluated exprs
    if (*\list_of_invocables) > 0 then {
        writes(yyout, "invocable ")
        every temp := list_of_invocables[i := 1 to *list_of_invocables] do {
            writes(yyout, image(temp))
            if i < *list_of_invocables then {
                writes(yyout, ",")
            }
        }
        write(yyout)
    }
$endif                                  # NoPatternIntegration

    if \iconc &
            (type(set_of_all_fields) == "set") &
            (*set_of_all_fields > 0) then {
        arandomfield := !set_of_all_fields
        writes(yyout, "record __dummyrecord", dummyrecno, "(", arandomfield)
        delete(set_of_all_fields, arandomfield)
        every writes(yyout, ",", !set_of_all_fields)
        write(yyout, ")")
        dummyrecno +:= 1
        set_of_all_fields := set()
    }
end
%}

%%

/*
 * igram.y -- iYacc grammar for Icon
 *
 * This file is the iYacc input for building Icon-based Icon tools.
 */

program : decls EOFX { Progend($1) } ;

decls   : { $$ := &null } ;
        | decls decl {
             if /parsingErrors | *parsingErrors = 0 then iwrites(&errout,".")
             $$ := node("decls", $1, $2)
             } ;

decl    : record
        | proc
        | global
        | link
        | package
        | import
        | invocable
        | cl
        ;

initiallysection: { $$ := &null }
        | INITIALLY SEMICOL locals initial procbody {
           $$ := Method( , , , , , $1, "initially", &null, "method", "(", ")")
           $$.locals := $3
           $$.initl := $4
           $$.procbody := $5
        }
        | INITIALLY LPAREN arglist RPAREN SEMICOL locals initial procbody {
           $$ := Method( , , , , , $1, "initially", $3, "method", "(", ")")
           $$.locals := $6
           $$.initl := $7
           $$.procbody := $8
        }
        ;

optsemi : { $$ := &null } ;
        | SEMICOL;

cl: classhead optsemi clocals methods optsemi initiallysection END {
    $$ := class_from_parts($1, $3, $4, $6)
   } ;

classhead : CLASS IDENT supers LPAREN carglist RPAREN {
   $$ := Class()
   $$.tag := $1
   $$.unmangled_name := $2.s
   $$.name := package_mangled_symbol($2.s)
   if proc($$.name, 0) then
      warning("Warning: class "|| $$.name ||" overrides the built-in function")
   else if \ (foobar := classes.lookup($$.name)) then {
      yyerror("redeclaration of class " || $$.name)
      }
   else
      classes.insert($$, $$.name)

   $$.supers_node := $3
   $$.fields := $5
   $$.lptoken := $4
   $$.rptoken := $6
   } ;

supers: { $$ := &null } ;
   | COLON IDENT supers { $$ := node("supers", $1, $2, $3) }
   | COLON packageref supers { $$ := node("supers", $1, $2, $3) }
   ;

packageref : IDENT COLONCOLON IDENT { $$ := node("packageref", $1,$2,$3) }
   | COLONCOLON IDENT { $$ := node("packageref", $1,$2) }
   ;

methods: { $$ := &null } ;
   | meth methods { $$ := node("methods", $1,$2) }
   | global methods { $$ := node("methods", $1,$2) }
   | record methods { $$ := node("methods", $1,$2) }
   ;

invocable : INVOCABLE invoclist { $$ := node("invocable", $1, $2) } ;

invoclist : invocop;
          | invoclist COMMA invocop { $$ := node("invoclist", $1,$2,$3) } ;

invocop  : IDENT ;
         | STRINGLIT ;
         | STRINGLIT COLON INTLIT {$$ := node("invocop3", $1,$2,$3) } ;

package  : PACKAGE lnkfile {
   if \thePackage then {
      if not (thePackage.name == $2.s) then {
         yyerror(yyfilename || " cannot be in both package "|| thePackage.name ||
            " and package " || $2.s)
         $$ := &null
         }
      else { # this branch allowed for -C / iconc
         thePackage.insertfname(yyfilename)
         thePackage.add_imported()
         }
      }
   else {
      $$ := node("package", $1,$2);
      thePackage := Package($2.s)
      thePackage.insertfname(yyfilename)
      thePackage.add_imported()
      }
   } ;

import: IMPORT implist {
   $$ := node("import", $1,$2," ")
   import_class($2)
   } ;

link    : LINK lnklist { $$ := node("link", $1,$2," ") } ;

lnklist : lnkfile ;
        | lnklist COMMA lnkfile { $$ := node("lnklist", $1,$2,$3) } ;

implist : lnkfile ;
        | implist COMMA lnkfile { $$ := node("implist", $1,$2,$3) } ;

lnkfile : IDENT ;
        | STRINGLIT ;

global  : GLOBAL idlist { $$ := node("global", $1,$2) } ;

record  : RECORD IDENT LPAREN fldlist RPAREN {
                $$ := declaration($2,$4,$1,$3,$5)
                if \iconc then
                   ca_add_proc(yyfilename, $2.s)
                } ;

fldlist : { $$ := &null } ;
        | idlist ;

proc    : prochead SEMICOL locals initial procbody END {
                $3 := AppendListCompTemps($3, $5)
                $$ := node("proc", $1,";",$3,$4,$5,$6)
                } ;

meth    : methhead SEMICOL locals initial procbody END {
                $$ := $1
                $$.locals := $3
                $$.initl := $4
                $$.procbody := $5
                }
        | ABSTRACT methhead {
                $$ := $2
                $$.abstract_flag := 1
                } ;

prochead: PROCEDURE IDENT LPAREN arglist RPAREN {
                $$ := declaration($2, $4, $1, $3, $5)
                if \iconc then
                   ca_add_proc(yyfilename, $2.s)
                } ;

methhead: METHOD IDENT LPAREN arglist RPAREN {
                $$ := Method( , , , , , $1, $2.s, $4, $1.s, $3, $5)
                } ;


arglist : { $$ := argList( , , &null) } ;
        | parmlist { $$ := argList( , , $1) } ;
        | parmlist LBRACK RBRACK { $$ := argList("[]" , , $1) } ;

carglist: { $$ := argList( , , &null) } ;
        | cparmlist { $$ := argList( , , $1) } ;
        | cparmlist LBRACK RBRACK { $$ := argList("[]" , , $1) } ;


idlist  : IDENT ;
        | idlist COMMA IDENT { $$ := node("idlist", $1,$2,$3) } ;

varlist : IDENT ;
        | IDENT ASSIGN expr1 { $$ := node("varlist2", $1, $2, $3)}
        | varlist COMMA IDENT { $$ := node("varlist3", $1, $2, $3)}
        | varlist COMMA IDENT ASSIGN expr1 { $$ := node("varlist4",$1,$2,$3,$4,$5)};

stalist : IDENT ;
        | IDENT ASSIGN expr1 { $$ := node("stalist2", $1, $2, $3)}
        | stalist COMMA IDENT { $$ := node("stalist3", $1, $2, $3)}
        | stalist COMMA IDENT ASSIGN expr1 { $$ := node("stalist4",$1,$2,$3,$4,$5)};

parmlist: arg ;
        | parmlist COMMA arg { $$ := node("parmlist", $1,$2,$3) } ;

cparmlist: carg ;
        | cparmlist COMMA carg { $$ := node("parmlist", $1,$2,$3) } ;

arg     : IDENT ;
        | IDENT COLON IDENT { $$ := node("arg2", $1, $2, $3) } ;
        | IDENT COLON literal { $$ := node("arg3", $1, $2, $3) } ;
        | IDENT COLON IDENT COLON literal { $$ := node("arg4", $1,$2,$3,$4,$5)};
        | IDENT COLON AND IDENT { $$ := node("arg5", $1, $2, Keyword($3, $4)) };
        | IDENT COLON IDENT COLON AND IDENT { $$ := node("arg6", $1, $2, $3, $4, Keyword($5, $6)) } ;
        | IDENT COLON LBRACK RBRACK { $$ := node("arg7", $1, $2, "[]") } ;
        | IDENT COLON IDENT COLON LBRACK RBRACK { $$ := node("arg8", $1, $2, $3, $4, "[]") } ;


carg    : priv arg { $$ := $2 };

priv: { $$ := &null;} ;
        | PLUS  ;
        | MINUS ;

clocals : { $$ := &null;} ;
        | clocals LOCAL varlist optsemi { $$ := node("locals2", $1,$2,$3,";") } ;

locals  : { $$ := &null;} ;
        | locals LOCAL varlist SEMICOL { $$ := node("locals2", $1,$2,$3,";") } ;
        | locals STATIC stalist SEMICOL { $$ := node("locals3", $1,$2,$3,";") } ;

initial : { $$ := &null } ;
        | iconINITIAL expr SEMICOL {
           $$ := node("initial", $1, $2,";")
              } ;

procbody: { $$ := &null } ;
        | nexpr SEMICOL procbody { $$ := node("procbody", $1,";",$3) } ;

nexpr   : { $$ := &null } ;
        | expr ;

expr    : expr1a ;
        | expr AND expr1a       { $$ := node("and", $1,$2,$3) } ;

expr1a  : expr1 ;
        | expr1a QMARK expr1    { $$ := node("binques", $1,$2,$3);} ;

expr1   : expr2a ;
        | expr2a SWAP expr1      { $$ := node("swap", $1,$2,$3);} ;
        | expr2a ASSIGN expr1    {
                $$ := parenthesize_assign(node("assign",$1,$2,$3));
                } ;
        | expr2a REVSWAP expr1   { $$ := node("revswap", $1,$2,$3);} ;
        | expr2a REVASSIGN expr1 { $$ := node("revasgn", $1,$2,$3);} ;
        | expr2a AUGCONCAT expr1 { $$ := node("augcat", $1,$2,$3);} ;
        | expr2a AUGLCONCAT expr1 { $$ := node("auglcat", $1,$2,$3);} ;
        | expr2a AUGDIFF expr1   { $$ := node("Bdiffa", $1,$2,$3);} ;
        | expr2a AUGUNION expr1  { $$ := node("Buniona", $1,$2,$3);} ;
        | expr2a AUGPLUS expr1   { $$ := node("Bplusa", $1,$2,$3);} ;
        | expr2a AUGMINUS expr1  { $$ := node("Bminusa", $1,$2,$3);} ;
        | expr2a AUGSTAR expr1   { $$ := node("Bstara", $1,$2,$3);} ;
        | expr2a AUGINTER expr1  { $$ := node("Bintera", $1,$2,$3);} ;
        | expr2a AUGSLASH expr1  { $$ := node("Bslasha", $1,$2,$3);} ;
        | expr2a AUGMOD expr1    { $$ := node("Bmoda", $1,$2,$3);} ;
        | expr2a AUGCARET expr1  { $$ := node("Bcareta", $1,$2,$3);} ;
        | expr2a AUGNMEQ expr1   { $$ := node("Baugeq", $1,$2,$3);} ;
        | expr2a AUGEQUIV expr1  { $$ := node("Baugeqv", $1,$2,$3);} ;
        | expr2a AUGNMGE expr1   { $$ := node("Baugge", $1,$2,$3);} ;
        | expr2a AUGNMGT expr1   { $$ := node("Bauggt", $1,$2,$3);} ;
        | expr2a AUGNMLE expr1   { $$ := node("Baugle", $1,$2,$3);} ;
        | expr2a AUGNMLT expr1   { $$ := node("Bauglt", $1,$2,$3);} ;
        | expr2a AUGNMNE expr1   { $$ := node("Baugne", $1,$2,$3);} ;
        | expr2a AUGNEQUIV expr1 { $$ := node("Baugneqv", $1,$2,$3);} ;
        | expr2a AUGSEQ expr1    { $$ := node("Baugseq", $1,$2,$3);} ;
        | expr2a AUGSGE expr1    { $$ := node("Baugsge", $1,$2,$3);} ;
        | expr2a AUGSGT expr1    { $$ := node("Baugsgt", $1,$2,$3);} ;
        | expr2a AUGSLE expr1    { $$ := node("Baugsle", $1,$2,$3);} ;
        | expr2a AUGSLT expr1    { $$ := node("Baugslt", $1,$2,$3);} ;
        | expr2a AUGSNE expr1    { $$ := node("Baugsne", $1,$2,$3);} ;
        | expr2a AUGQMARK expr1  { $$ := node("Baugques", $1,$2,$3);} ;
        | expr2a AUGAND expr1    { $$ := node("Baugamper", $1,$2,$3);} ;
        | expr2a AUGAT expr1     { $$ := node("Baugact", $1,$2,$3);} ;

expr2a  : expr2;
        | expr2a PMATCH expr2   { $$ := node("BPmatch", $1,$2,$3);} ;

expr2   : expr3 ;
        | expr2 TO expr3 { $$ := node("to", $1,$2,$3);} ;
        | expr2 TO expr3 BY expr3 { $$ := node("toby", $1,$2,$3,$4,$5);} ;
        | expr2 POR expr3 { $$ := node("BPor", $1,$2,$3); };

expr3   : expr4 ;
        | expr4 PAND expr3 { $$ := node("BPand", $1,$2,$3); };
        | expr4 BAR expr3  { $$ := node(BAR, $1,$2,$3);} ;

expr4   : expr5;
        | expr4 SEQ expr5 { $$ := node("Bseq", $1,$2,$3);} ;
        | expr4 SGE expr5 { $$ := node("Bsge", $1,$2,$3);} ;
        | expr4 SGT expr5 { $$ := node("Bsgt", $1,$2,$3);} ;
        | expr4 SLE expr5 { $$ := node("Bsle", $1,$2,$3);} ;
        | expr4 SLT expr5 { $$ := node("Bslt", $1,$2,$3);} ;
        | expr4 SNE expr5 { $$ := node("Bsne", $1,$2,$3);} ;
        | expr4 NMEQ expr5 { $$ := node("Beq", $1,$2,$3);} ;
        | expr4 NMGE expr5 { $$ := node("Bge", $1,$2,$3);} ;
        | expr4 NMGT expr5 { $$ := node("Bgt", $1,$2,$3);} ;
        | expr4 NMLE expr5 { $$ := node("Ble", $1,$2,$3);} ;
        | expr4 NMLT expr5 { $$ := node("Blt", $1,$2,$3);} ;
        | expr4 NMNE expr5 { $$ := node("Bne", $1,$2,$3);} ;
        | expr4 EQUIV expr5 { $$ := node("Beqv", $1,$2,$3);} ;
        | expr4 NEQUIV expr5 { $$ := node("Bneqv", $1,$2,$3);} ;

expr5   : expr6 ;
        | expr5 CONCAT expr6 { $$ := node("Bcat", $1,$2,$3);} ;
        | expr5 LCONCAT expr6 { $$ := node("Blcat", $1,$2,$3);} ;

expr6   : expr7 ;
        | expr6 PIMDASSN expr7 { $$ := node("BPiam", $1,$2,$3);} ;
        | expr6 PASSNONMATCH expr7 { $$ := node("BPaom", $1,$2,$3);} ;
        | expr6 PLUS expr7 { $$ := node("Bplus", $1,$2,$3);} ;
        | expr6 DIFF expr7 { $$ := node("Bdiff", $1,$2,$3);} ;
        | expr6 UNION expr7 { $$ := node("Bunion", $1,$2,$3);} ;
        | expr6 MINUS expr7 { $$ := node("Bminus", $1,$2,$3);} ;

expr7   : expr8 ;
        | expr7 STAR expr8 { $$ := node("Bstar", $1,$2,$3);} ;
        | expr7 INTER expr8 { $$ := node("Binter", $1,$2,$3);} ;
        | expr7 SLASH expr8 { $$ := node("Bslash", $1,$2,$3);} ;
        | expr7 MOD expr8 { $$ := node("Bmod", $1,$2,$3);} ;

expr8   : expr9 ;
        | postfixthreadop ;
        | expr9 CARET expr8 { $$ := node("Bcaret", $1,$2,$3);} ;

postfixthreadop:
          expr9 SND { $$ := node("Bsnd", $1,$2,&null);} ;
        | expr9 SNDBK { $$ := node("Bsndbk", $1,$2,&null);} ;
        | expr9 RCV { $$ := node("Brcv", $1,$2,&null);} ;
        | expr9 RCVBK { $$ := node("Brcvbk", $1,$2,&null);} ;

expr9   : expr10 ;
        | expr9 BACKSLASH expr10 { $$ := node("limit", $1,$2,$3);} ;
        | expr9 AT expr10 { $$ := node("at", $1,$2,$3);} ;
        | expr9 SND expr10 { $$ := node("Bsnd", $1,$2,$3);} ;
        | expr9 SNDBK expr10 { $$ := node("Bsndbk", $1,$2,$3);} ;
        | expr9 RCV expr10 { $$ := node("Brcv", $1,$2,$3);} ;
        | expr9 RCVBK expr10 { $$ := node("Brcvbk", $1,$2,$3);} ;
        | expr9 BANG expr10 { $$ := node("apply", $1,$2,$3);};

expr10  : expr11 ;
        | AT expr10 { $$ := node("uat", $1,$2);} ;
        | SND expr10 { $$ := node("Bsnd", &null,$1,$2);} ;
        | SNDBK expr10 { $$ := node("Bsndbk", &null,$1,$2);} ;
        | RCV expr10 { $$ := node("Brcv", &null,$1,$2);} ;
        | RCVBK expr10 { $$ := node("Brcvbk", &null,$1,$2);} ;
        | NOT expr10 { $$ := node("unot", $1,$2);} ;
        | BAR expr10 { $$ := node("ubar", $1,$2);} ;
        | CONCAT expr10 { $$ := node("uconcat", $1,$2);} ;
        | LCONCAT expr10 { $$ := node("ulconcat", $1,$2);} ;
        | DOT expr10 { $$ := node("udot", $1,$2);} ;
        | BANG expr10 { $$ := node("ubang", $1,$2);} ;
        | DIFF expr10 { $$ := node("udiff", $1,$2);} ;
        | PLUS expr10 { $$ := node("uplus", $1,$2);} ;
        | STAR expr10 { $$ := node("ustar", $1,$2);} ;
        | SLASH expr10 { $$ := node("uslash", $1,$2);} ;
        | CARET expr10 { $$ := node("ucaret", $1,$2);} ;
        | INTER expr10 { $$ := node("uinter", $1,$2);} ;
        | TILDE expr10 { $$ := node("utilde", $1,$2);} ;
        | MINUS expr10 { $$ := node("uminus", $1,$2);} ;
        | NMEQ expr10 { $$ := node("unumeq", $1,$2);} ;
        | NMNE expr10 { $$ := node("unumne", $1,$2);} ;
        | SEQ expr10 { $$ := node("ulexeq", $1,$2);} ;
        | SNE expr10 { $$ := node("ulexne", $1,$2);} ;
        | EQUIV expr10 { $$ := node("uequiv", $1,$2);} ;
        | UNION expr10 { $$ := node("uunion", $1,$2);} ;
        | QMARK expr10 { $$ := node("uqmark", $1,$2);} ;
        | NEQUIV expr10 { $$ := node("unotequiv", $1,$2);} ;
        | BACKSLASH expr10 { $$ := node("ubackslash", $1,$2);} ;
        | PSETCUR expr10 { $$ := node("upsetcur", $1,$2);} ;

expr11  : literal ;
        | NMLT { yylex2 := yylex2Regex; } regex REGEXEND { $$ := node("regex", $3); }
        | section ;
        | return ;
        | if ;
        | case ;
        | while ;
        | until ;
        | every ;
        | repeat ;
        | SND { $$ := node("Bsnd", &null,$1,&null);} ;
        | SNDBK { $$ := node("Bsndbk", &null,$1,&null);} ;
        | RCV { $$ := node("Brcv", &null,$1,&null);} ;
        | RCVBK { $$ := node("Brcvbk", &null,$1,&null);} ;
        | PUNEVAL { $$ := node("BPuneval", $1);} ;
        | CREATE expr { $$ := node("create", $1,$2);} ;
        | THREAD expr {
              fakeThreadIdent := Clone1stToken($1)
              fakeThreadIdent.tok := IDENT
              fakeCreate := Clone1stToken($1)
              fakeCreate.tok := CREATE
              fakeCreate.s := "create"
              fakeThreadIdent.s := "spawn"
              fakeLParen := Clone1stToken($1)
              fakeLParen.tok := LPAREN
              fakeLParen.s := "("
              fakeRParen := Clone1stToken($1)
              fakeRParen.tok := RPAREN
              fakeRParen.s := ")"

              $$ := SimpleInvocation(fakeThreadIdent,fakeLParen,
                                     node("create", fakeCreate, $2),
                                     fakeRParen);
              } ;
        | CRITICAL expr2a COLON expr { $$ := node("critical", $1,$2,$3,$4);} ;
        | IDENT ;
        | NEXT { $$ := node("Next", $1);} ;
        | BREAK nexpr { $$ := node("Break", $1,$2);} ;
        | LPAREN exprlist RPAREN { $$ := node("Paren", $1,$2,$3);} ;
        | LBRACE compound RBRACE { $$ := node("Brace", $1,$2,$3);} ;
        | LBRACK caselist RBRACK { $$ := tablelit($1,$2,$3);} ;
        | LBRACK exprlist RBRACK { $$ := node("Brack", $1,$2,$3);} ;
        | LBRACK COLON expr COLON RBRACK { $$ := ListComp($3);} ;
        | expr11 LBRACK exprlist RBRACK { $$ := node("Subscript", $1,$2,$3,$4);} ;
        | expr11 LBRACE RBRACE { $$ := node("Pdco0", $1,$2,$3);} ;
        | expr11 LBRACE pdcolist RBRACE { $$ := node("Pdco1", $1,$2,$3,$4);} ;
        | expr11 LPAREN exprlist RPAREN {
                 $$ := SimpleInvocation($1,$2,$3,$4);
      } ;
        | expr11 DOLLAR INITIALLY LPAREN exprlist RPAREN {
           $$ := InvocationNode($1,$2,$3,$4,$5,$6)
           } ;
        | expr11 DOLLAR IDENT LPAREN exprlist RPAREN {
           $$ := InvocationNode($1,$2,$3,$4,$5,$6)
           } ;
        | expr11 DOLLAR IDENT DOT INITIALLY LPAREN exprlist RPAREN {
           $$ := InvocationNode($1,$2,$3,$4,$5,$6,$7,$8)
           } ;
        | expr11 DOLLAR IDENT DOT IDENT LPAREN exprlist RPAREN {
           $$ := InvocationNode($1,$2,$3,$4,$5,$6,$7,$8)
           } ;
        | expr11 DOT IDENT {
                 $$ := FieldRef($1,$2,$3);
      } ;
        | packageref;
        | expr11 DOT INITIALLY { $$ := Field($1,$2,$3) } ;
        | AND FAIL { $$ := node("keyword",$1,$2) } ;
        | AND IDENT { $$ := Keyword($1,$2) } ;

while   : WHILE expr {
               $$ := node("While0", $1,$2);
               } ;
        | WHILE expr DO expr {
            # warn if a while loop should be an every.
            # should generalize; compute a semantic attribute and
            # warn if a while loop control expression is a generator.
            # but for now, only complain about the most obvious case
            if type($2) == "treenode" & $2.label === "assign" &
               *$2.children = 3 & type($2.children[3]) == "treenode" &
               $2.children[3].label == "to" & *($2.children[3].children)=3 &
                     (type($2.children[3].children[1]) ===
                      type($2.children[3].children[3]) === "token") &
                     ($2.children[3].children[1].tok =
                      $2.children[3].children[3].tok = INTLIT) &
                     $2.children[3].children[1].s<=$2.children[3].children[3].s
            then {
                warning("infinite loop; use \"every\" to loop on generator results",
                        $1.line, $1.filename, $1.s
                        )
               }
            $$ := node("While1", $1,$2,$3,$4);
            } ;

until   : UNTIL expr { $$ := node("until", $1,$2);} ;
        | UNTIL expr DO expr { $$ := node("until1", $1,$2,$3,$4);} ;

every   : EVERY expr { $$ := node("every", $1,$2);} ;
        | EVERY expr DO expr { $$ := node("every1", $1,$2,$3,$4);} ;

repeat  : REPEAT expr { $$ := node("repeat", $1,$2);} ;

return  : FAIL ;
        | RETURN nexpr { $$ := node("return", $1, $2);} ;
        | SUSPEND nexpr { $$ := node("Suspend0", $1,$2);} ;
        | SUSPEND expr DO expr { $$ := node("Suspend1", $1,$2,$3,$4);};

if      : IF expr THEN expr { $$ := node("If0", $1,$2,$3,$4);} ;
        | IF expr THEN expr ELSE expr { $$ := node("If1", $1,$2,$3,$4,$5,$6);} ;

case    : CASE expr OF LBRACE caselist RBRACE { $$ := node("Case", $1,$2,$3,$4,$5,$6);} ;

caselist: cclause ;
        | caselist SEMICOL cclause { $$ := node("Caselist", $1,";",$3);} ;

cclause : DEFAULT COLON expr { $$ := node("cclause0", $1,$2,$3);} ;
        | expr COLON expr { $$ := node("cclause1", $1,$2,$3);} ;

exprlist: nexpr ;
        | exprlist COMMA nexpr {
           if type($1)=="treenode" & ($1.label=="elst1") then {
              $$ := $1; put($$.children, $2, $3)
              }
           else
              $$ := node("elst1", $1,$2,$3)
           } ;

pdcolist: nexpr { $$ := node("pdcolist0", $1) } ;
        | pdcolist COMMA nexpr { $$ := node("pdcolist1", $1,$2,$3); } ;

literal : INTLIT ;
        | REALLIT ;
        | STRINGLIT ;
        | CSETLIT ;

/*
 * The grammar has been changed here to correctly allow an empty regex
 */
regex: neregex { $$ := regexp($1) }
          |  { $$ := regexp(node("emptyregex")) }
          ;

/* nonempty regexp */
neregex: neregex2a
          | neregex2a REGEXBAR neregex { $$ := node("regexbar", $1, $2, $3) }
          ;

neregex2a: neregex2
          | neregex2 neregex2a { $$ := node("regexconcat", $1, $2) }
          ;

neregex2: neregex3 ;
          | neregex2 REGEXSTAR { $$ := node("kleene", $1, $2) }
          | neregex2 REGEXPLUS { $$ := node("oneormore", $1, $2) }
          | neregex2 REGEXQMARK { $$ := node("optional", $1, $2) }
          | neregex2 REGEXLBRACE {regexintlit := 1} INTLIT REGEXRBRACE {
              # Within the regex lexer, integer literals are always positive,
              # any time a - is placed before a number, it will come back as a
              # REGEXHYPHEN and will give rise to a parse error
              #
              if $4.s = 0 then {
                  yyerror("regex occurrences may not be zero yet")
                  $$ := node("error")
                  }
              else if $4.s = 1 then $$ := $1
              else { # normal case, positive number of repeats of $1
                  $$ := $1
                  every i := 2 to $4.s do {
                      $$ := node("regexconcat", $$, $1)
                      }
                  }
              }
          ;

neregex3:  REGEXCHAR
          | REGEXDOT
          | REGEXHYPHEN
          | REGEXLPAREN regex REGEXRPAREN { $$ := node("Paren",$1,$2,$3); }
          | REGEXLBRACK brackchars REGEXRBRACK {
                  $$ := node("acset", $1, $2, $3)
                  }
          | REGEXLBRACK REGEXCARET brackchars REGEXRBRACK {
                    $$ := node("notany", $1, $2, $3, $4)
                }
          ;

brackchars: brackchars2
          | brackchars REGEXHYPHEN brackchars2 {
                    $$ := node("brackchars", $1, $2, $3) }
          | brackchars brackchars2 {
              if type($1) == "treenode" then {
                 c1 := csetify($1)
                 }
              if type($2) == "treenode" then c2 := csetify($2)

              $$ := copy($1)
              while type($$) == "treenode" do {
                  $$ := copy($$.children[1])
                  $$.s := c1
                  }
              if type($$) ~== "token" then stop("regex type ", image($$))

              if type($2) == "treenode" then $$.s ||:= c2
              else $$.s ||:= $2.s
              }
          ;
/*
 * To allow for a - or a ], escape these these characters and they will come
 * back as a REGECHAR
 */
brackchars2: REGEXCHAR
            | REGEXSTAR
            | REGEXPLUS
            | REGEXBAR
            | REGEXQMARK
            | REGEXLBRACE
            | REGEXRBRACE
            | REGEXLBRACK
            | REGEXDOT
            | REGEXLPAREN
            | REGEXRPAREN
          ;

section : expr11 LBRACK expr sectop expr RBRACK { $$ := node("section", $1,$2,$3,$4,$5,$6);} ;

sectop  : COLON ;
        | PCOLON ;
        | MCOLON ;

compound: nexpr ;
        | nexpr SEMICOL compound { $$ := node("compound", $1,";",$3);} ;

program : error decls EOFX ;
proc    : prochead error procbody END { $$ := node("error", $1,$3,$4); } ;
expr    : error { $$ := node("error"); } ;

%%


#PD:
#: this procedure parenthesizes the rhs side expression of an assignment when
#: the rhs is an invoke operation. An invoke operation affects field references,
#: parameter calls and thread invocations. This additional parentheesizing is only
#: needed when passing code to iconc.
procedure parenthesize_assign(nd)
    local   rhs                         #LV: temporary to hold the rhs of an expression

    #
    # process only if using iconc and the node has 3 children and the rhs expression
    # is a treenode with a value of invoke
    #
    if \iconc &
            *nd.children = 3 &
            type(rhs := nd.children[3]) == "treenode" &
            rhs.label == "invoke" then {
        nd.children[3] := node("Paren", "(", rhs, ")")
    }
    return nd

end

#PD:
#: this procedure is used to create the code for iconc for correctly finding the
#: relevant field within a record by using a temporary to hold lhs of the field reference
#:
procedure FieldRef(lhs, dot, rhs)
    #
    # do additional process of the field reference only if using iconc & the lhs
    # is a treenode with a value of invoke
    #
    if \iconc &
            type(lhs) == "treenode" &
            lhs.lable == "invoke" then {
        tmpcount +:= 1
        return node("Paren",
                    "(",
                    node("assign", "__" || tmpcount, " := ", lhs, ")", "& "),
                    node("invoke", Field("__" || tmpcount, ".", rhs))
                   )
    #
    # otherwise just return a field reference
    } else {
        return Field(lhs, dot, rhs)
    }
end

#PD:
#:
#:
procedure InvocationNode(args[])
    local   n1,                         #LV: temporary variable to hold a new node
                                        #:   for use in creating required
                                        #:   parsetree elements
            lparen                      #LV: holds a parenthesis token or a "("
                                        #:
    #
    # we will need a new temporary variable, so increase the global temporary
    # variable count
    #
    tmpcount +:= 1
    #
    # if what we have is a identifier token in the parse tree, the invocation will be its
    # value and is hence not an expression that has to be evaluated first
    #
    if type(args[1]) == "token" & (args[1].tok = IDENT) then {
        n1 := args[1]
        #
        # create a new token that will have the same line number, column number and
        # filename as the original token, but change the type and string to that
        # of a parenthesis so that any error reporting in the code will have the
        # correct position noted.
        #
        lparen := copy(args[1])
        lparen.tok := LPAREN
        lparen.s := "("
    #
    # otherwise, we need to create a new parse tree element which is an assignment
    # of the expression to a new temporary variable. This variable will be used to
    # invoke the expression result. We surround this assignment with parentheses
    #
    } else {
        n1 := node("Paren", "(", node("assign", "__" || tmpcount, ":=", args[1]), ")")
        #
        # since this is an expression as it is a treenode, we need to find the
        # first actual token and use it for any error reporting in the code
        #
        if lparen := Clone1stToken(args[1]) then {
            lparen.tok := LPAREN
            lparen.s := "("
        #
        # if there is no applicable token found, we just set the value to the "("
        # string
        #
        } else {
            lparen := "("
        }
    }
    #
    # if the size of args is 6, then we have a direct method call for the class
    # in question, this includes the initially method as well
    #
    if *args = 6 then {
        return node("Paren",
                    lparen,
                    node("invoke",
                         #
                         # iconc doesn't use the __m field reference
                         # in determining the relevant method to call
                         #
                         # However, for the interpreter, we need to
                         # find the relevant method which is contained
                         # in the method record referenced by the __m field
                         # in the object record
                         #
                         (if /iconc then {
                              Field(Field(n1, ".", "__m"), "." , args[3])
                          } else {
                              Field(n1, ".", args[3])
                          }
                         ),
                         #
                         # the actual parenthesis token found
                         #
                         args[4],
                         node("exprlist",
                              #
                              # if we found an identifier token as the first argument
                              # then we use it, otherwise use the new temporary created
                              #
                              if n1 === args[1] then {
                                  args[1]
                              } else {
                                  "__" || tmpcount
                              },
                              #
                              # if the fifth argument is &null then there are
                              # no parameters for this method call, otherwise use the
                              # supplied parameters for this method call
                              #
                              if /args[5] then {
                                  &null
                              } else {
                                  ","
                              },
                              args[5]),
                         args[6]),
                    ")")
    #
    # it will be an superclass method invocation
    #
    } else {
        #
        # the interpreter uses the __m field in the object record to access the
        # methods in the superclass
        #
        if /iconc then {
            return node("Paren",
                        lparen,
                        node("invoke",
                             Field(Field(Field(n1, ".", "__m"),
                                         ".",
                                         args[3]),
                                   ".",
                                   args[5]),
                             args[6],
                             node("exprlist",
                                  if n1 === args[1] then {
                                      args[1]
                                  } else {
                                      "__" || tmpcount
                                  },
                                  if /args[7] then {
                                      &null
                                  } else {
                                      ","
                                  },
                                  args[7]),
                             args[8]),
                        ")")
        #
        # the iconc compiler doesn't use the object record to access the
        # superclass methods
        #
        } else {
            return SuperMethodInvok ! args
        }
    }
end

#PD:
#:
#:
procedure SimpleInvocation(expr11, lparen, args, rparen)
    local   n1                          #LV: temporary to hold a newly created node
                                        #:   for use in the iconc generated code

    #
    # if the invocation is to be processed by iconc and the label of expr11 is a
    # field and the first of the children of expr11 is a treenode then additional
    # processing is required
    #
    if \iconc &
            type(expr11) == "treenode" &
            expr11.label == "field"  &
            type(expr11.children[1]) == "treenode"then {
        tmpcount +:= 1;
        n1 := node("Paren",
                   "(",
                   node("assign",
                        "__" || tmpcount,
                        ":=",
                        expr11.children[1]),
                   ")")
        return node("exprlist",
                    "(",
                    n1,
                    "&",
                    node("invoke",
                         Field("__" || tmpcount, ".", expr11.children[3]),
                         lparen,
                         args,
                         rparen),
                    ")")
    #
    # otherwise justreturn an invoke node
    #
    } else {
        return node("invoke", expr11, lparen, args, rparen)
    }
end

#PD:
#: This procedure is used for iconc processing to handle the invocation of
#: superclass methods, as iconc doesn't use the __m field in the object records
#:
procedure SuperMethodInvok(args[])
    local   n1,                         #LV: temporary variable to hold a new node
                                        #:   for use in creating required
                                        #:   parsetree elements
            lparen                      #LV: holds a left parenthesis token or a "("
                                        #:   character

    #
    # we will need a new temporary variable, so increase the temporary count
    #
    tmpcount +:= 1
    #
    # if what we have is a identifier token in the parse tree, the invocation will be its
    # value and is hence not an expression that has to be evaluated first
    #
    if (type(args[1]) == "token") & (args[1].tok = IDENT) then {
        n1 := args[1]
        #
        # create a new token that will have the same line number, column number and
        # filename as the original token, but change the type and string to that
        # of a parenthesis so that any error reporting in the code will have the
        # correct position noted.
        #
        lparen := copy(args[1])
        lparen.tok := LPAREN
        lparen.s := "("
    #
    # otherwise, we need to create a new parse tree element which is an assignment
    # of the expression to a new temporary variable. This variable will be used to
    # invoke the expression result. We surround this assignment with parentheses
    #
    } else {
        n1 := node("Paren", "(", node("assign", "__" || tmpcount, ":=", args[1]), ")")
        #
        # since this is an expression as it is a treenode, we need to find the
        # first actual token and use it for any error reporting in the code
        #
        if lparen := Clone1stToken(args[1]) then {
            lparen.tok := LPAREN
            lparen.s := "("
        #
        # if there is no applicable token found, we just set the value to the "("
        # string
        #
        } else {
         lparen := "("
        }
    }

    #
    # since this is a superclass reference, we need to incorporate the class
    # name into the method identifier
    #
    args[3].s := mangle_class_sym(args[3].s)
    #
    # finalise the reference to the oprec variable for this class
    #
    args[3].s ||:= "__oprec"
    #
    # Now that we have adjusted the name of the method, just return the parse tree
    # relevant for the invocation of this superclass method
    #
    return node("exprlist",
                "(",
                n1,
                ")",
                " & ",
                node("Paren",
                     lparen,
                     node("invoke",
                          Field(args[3], ".", args[5]),
                          args[6],
                          node("exprlist",
                               if n1 === args[1] then {
                                   args[1]
                               } else {
                                   "__" || tmpcount
                               },
                               if /args[7] then {
                                   &null
                               } else {
                                   ","
                               },
                               args[7]),
                          args[8]),
                     ")"))
end

#PD:
#:
#:
procedure isloco(node, s)
    case type(node) of {
        "treenode": {
            if *node.children > 0 then {
                return isloco(!node.children, s)
            }
        }
        "token" : {
            if (node.tok = IDENT) & (node.s == s) then {
                return
            }
        }
    }
end

#PD:
#:
#:
procedure buildtab_from_cclause(n, args)
    local   comma                       #LV: temporary to hold a new node for a ,

    if type(n) ~== "treenode" then {
        stop("bad table literal")
    }
    comma := copy(n.children[2])
    comma.tok := COMMA
    comma.s := ","
    case n.label of {
        "cclause0": {
            if *args.children > 0 then {
                push(args.children, comma)
            }
            push(args.children, n.children[3])
        }
        "cclause1": {
            if *args.children > 0 then {
                push(args.children, comma)
            }
            push(args.children, n.children[3])
            push(args.children, comma)
            push(args.children, n.children[1])
        }
    }
end

#PD:
#: build a parse tree equivalent to
#: { __tmp := []; every put(__tmp, expr); if __tmp>0 then __tmp}
#: The enclosing procedure/method also needs to declare this temp var;
#: see AppendListCompTemps
#:
procedure ListComp(expr)
    local   tmp                         #LV:

    tmpcount +:= 1
    tmp := "__" || tmpcount
    return node("ListComp",
                "{",
                string(tmp),
                " := []; every put(" || tmp || ", ",
                expr,
                "); if *" || tmp || " > 0 then {" || tmp || "}}")
end

#PD:
#: AppendListCompTemps(localdecls, procbody) - at the procedure/method
#: outermost level, we may need to add some declarations based on what's
#: in the procbody. If there are changes to the declared lcls section,
#: returns changed locals.
#:
procedure AppendListCompTemps(lcls, body)
    local   i,                          #LV:
            vl                          #LV:

    # if there is, in the procbody, a list of varnames to declare
    if *\(ltmps := ListCompTemps(body)) > 0 then {
        # make a varlist containing ltmps
        if *ltmps > 1 then {
            vl := token(IDENT, ltmps[1], 0, 0, "lambda.icn")
            every i := 2 to *ltmps do {
                vl := node("varlist3",
                           vl,
                           ",",
                           token(IDENT, ltmps[i], 0, 0, "lambda.icn"))
            }
        } else {
            # the varlist will just be an IDENT
            vl := token(IDENT, ltmps[1], 0, 0, "lambda.icn")
        }
        if (/lcls) |
                (type(lcls) == "treenode" &
                 lcls.label == ("locals2" | "locals3")) then {
            return node("locals2", lcls, "local", vl, ";")
        } else {
            write(&errout, "don't know what to do with ", image(lcls))
        }
    }
end

#PD:
#: ListCompTemps(n) - lower level temp. var extraction from proc body.
#: returns list of strings containing temp. variables from list comprehension
#: L
procedure ListCompTemps(n)
    local   LCT                         #LV:

    if type(n) == "treenode" then {
        if n.label=="ListComp" then {
            LCT := [n.children[2]]
            LCT |||:= ListCompTemps(n.children[4])
            return LCT
        } else if LCT := ListCompTemps(n.children[k := 1 to *(n.children)]) then {
            every kk := k+1 to *(n.children) do {
                LCT |||:= ListCompTemps(n.children[kk])
            }
            return LCT
        }
    }
end

#PD:
#:
#:
procedure tablelit(lb, cl, rb)
    local   tabid,                      #LV:
            lp,                         #LV:
            rp,                         #LV:
            args                        #LV:

    args := node("elst1")
    #write("I am a tablelit, cl is ", image(cl.label))
    while type(cl)== "treenode" & cl.label == "Caselist" do {
        buildtab_from_cclause(cl.children[3], args)
        cl := cl.children[1]
    }
    buildtab_from_cclause(cl, args)
    tabid := copy(lb)
    tabid.tok := IDENT
    tabid.s := "table"
    lp := copy(lb)
    lp.tok := LPAREN
    lp.s := "("
    rp := copy(rb)
    rp.tok := RPAREN
    rp.s := ")"
    return node("invoke", tabid, lp, args, rp)
end

