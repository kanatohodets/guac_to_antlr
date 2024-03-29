grammar Perl;

program : statementSeq ;

statementSeq : statement
             | statement Semicolon
             | statement Semicolon statementSeq
             | blockStatement
             | blockStatement statementSeq ;

// Statements that end with a block and do not need a semicolon terminator.
blockStatement : loopStatement
               | packageStatement
               | subStatement
               | condition
               | blockNonEmpty ;

statement : blockLevelExpression statementModifier
          | blockLevelExpression
          | ellipsisStatement
          | useStatement
          | noStatement
          | requireStatement
          | packageDeclaration
          | subDeclaration ;

loopStatement : forStatement
              | whileStatement
              | untilStatement ;

forStatement : forStatementOp LParen statement Semicolon statement Semicolon statement RParen block continueExpr
             | forStatementOp LParen statement Semicolon statement Semicolon statement RParen block
             | forStatementOp OpKeywordMy varScalar LParen expression RParen block continueExpr
             | forStatementOp OpKeywordMy varScalar LParen expression RParen block
             | forStatementOp varScalar LParen expression RParen block continueExpr
             | forStatementOp varScalar LParen expression RParen block
             | forStatementOp LParen Semicolon Semicolon RParen block continueExpr
             | forStatementOp LParen Semicolon Semicolon RParen block
             | forStatementOp LParen expression RParen block continueExpr
             | forStatementOp LParen expression RParen block ;

continueExpr : OpKeywordContinue block ;

forStatementOp : OpKeywordFor
               | OpKeywordForeach ;

whileStatement : ConditionWhile LParen expression RParen block OpKeywordContinue block
               | ConditionWhile LParen expression RParen block
               | ConditionWhile LParen RParen block OpKeywordContinue block
               | ConditionWhile LParen RParen block ;

untilStatement : ConditionUntil LParen expression RParen block OpKeywordContinue block
                 | ConditionUntil LParen expression RParen block ;

statementModifier : conditionIfPostfixExpr
                  | conditionUnlessPostfixExpr
                  | conditionWhilePostfixExpr
                  | conditionUntilPostfixExpr
                  | conditionForPostfixExpr
                  | conditionForeachPostfixExpr ;

ellipsisStatement : Ellipsis ;

useStatement : OpKeywordUse classIdent versionExpr expression
               | OpKeywordUse classIdent versionExpr
               | OpKeywordUse classIdent expression
               | OpKeywordUse versionExpr
               | OpKeywordUse classIdent ;

noStatement : OpKeywordNo classIdent versionExpr expression
              | OpKeywordNo classIdent versionExpr
              | OpKeywordNo classIdent expression
              | OpKeywordNo versionExpr
              | OpKeywordNo classIdent ;

requireStatement : OpKeywordRequire versionExpr
                 | OpKeywordRequire classIdent
                 | OpKeywordRequire expression ;

packageStatement : OpKeywordPackage classIdent versionExpr block
                   | OpKeywordPackage classIdent block ;
packageDeclaration : OpKeywordPackage classIdent versionExpr
                   | OpKeywordPackage classIdent ;

subStatement : phaseStatement block
             | OpKeywordSub phaseStatement block 
             | OpKeywordSub subNameExpr subDefinition ;

subDeclaration : OpKeywordSub subNameExpr ;

subDefinition : subAttrsDefinitionSeq subSigsDefinition block
              | subAttrsDefinitionSeq block
              | subSigsDefinition block
              | block ;

subAttrsDefinitionSeq : subAttrsDefinition subAttrsDefinitionSeq
                      | subAttrsDefinition ;

subAttrsDefinition : Colon IdentComp SubAttrArgs
                   | Colon IdentComp ;

subSigsDefinition : parenExpr ;

phaseStatement : PhaseName ;

condition : conditionIfExpr conditionElsifExpr conditionElseExpr
          | conditionIfExpr conditionElseExpr
          | conditionIfExpr conditionElsifExpr
          | conditionIfExpr
          | conditionUnlessExpr ;

conditionUnlessExpr         : ConditionUnless  LParen expression RParen block ;
conditionIfExpr             : ConditionIf      LParen expression RParen block ;
conditionElsifExpr          : ConditionElsif   LParen expression RParen block conditionElsifExpr 
                            | ConditionElsif   LParen expression RParen block ;
conditionElseExpr           : ConditionElse    block ;
conditionIfPostfixExpr      : ConditionIf      expression ;
conditionUnlessPostfixExpr  : ConditionUnless  expression ;
conditionWhilePostfixExpr   : ConditionWhile   expression ;
conditionUntilPostfixExpr   : ConditionUntil   expression ;
conditionForPostfixExpr     : ConditionFor     expression ;
conditionForeachPostfixExpr : ConditionForeach expression ;

label : IdentComp Colon ;

/*
# this is based on the order of ops in `perldoc perlop`
# U can be LHS of shift and up
# 0 can be LHS of assignment and up
# L can be LHS of comma and up
# R can be LHS of anything
*/
exprValueU    : value ;
exprValue0    : value | opUnaryKeywordExpr ;
exprValueL    : value | opAssignKeywordExpr | opUnaryKeywordExpr ;
exprValueR    : value | opListKeywordExpr | opAssignKeywordExpr | opUnaryKeywordExpr ;
exprArrowU    : exprArrowU  OpArrow   arrowRHS        | exprValueU   ; // action => ::first
exprArrow0    : exprArrowU  OpArrow   arrowRHS        | exprValue0   ; // action => ::first
exprArrowL    : exprArrowU  OpArrow   arrowRHS        | exprValueL   ; // action => ::first
exprArrowR    : exprArrowU  OpArrow   arrowRHS        | exprValueR   ; // action => ::first
exprIncU      : OpInc exprArrowU | exprArrowR OpInc   | exprArrowU   ; // action => ::first
exprInc0      : OpInc exprArrow0 | exprArrowR OpInc   | exprArrow0   ; // action => ::first
exprIncL      : OpInc exprArrowL | exprArrowR OpInc   | exprArrowL   ; // action => ::first
exprIncR      : OpInc exprArrowR | exprArrowL OpInc   | exprArrowR   ; // action => ::first
exprPowerU    : exprIncU    OpPower   exprUnaryU      | exprIncU     ; // action => ::first
exprPower0    : exprIncU    OpPower   exprUnary0      | exprInc0     ; // action => ::first
exprPowerL    : exprIncU    OpPower   exprUnaryL      | exprIncL     ; // action => ::first
exprPowerR    : exprIncU    OpPower   exprUnaryR      | exprIncR     ; // action => ::first
exprUnaryU    : OpUnary     exprUnaryU                | exprPowerU   ; // action => ::first
exprUnary0    : OpUnary     exprUnary0                | exprPower0   ; // action => ::first
exprUnaryL    : OpUnary     exprUnaryL                | exprPowerL   ; // action => ::first
exprUnaryR    : OpUnary     exprUnaryR                | exprPowerR   ; // action => ::first
exprRegexU    : exprRegexU  OpRegex   exprUnaryU      | exprUnaryU   ; // action => ::first
exprRegex0    : exprRegexU  OpRegex   exprUnary0      | exprUnary0   ; // action => ::first
exprRegexL    : exprRegexU  OpRegex   exprUnaryL      | exprUnaryL   ; // action => ::first
exprRegexR    : exprRegexU  OpRegex   exprUnaryR      | exprUnaryR   ; // action => ::first
exprMulU      : exprMulU    OpMulti   exprRegexU      | exprRegexU   ; // action => ::first
exprMul0      : exprMulU    OpMulti   exprRegex0      | exprRegex0   ; // action => ::first
exprMulL      : exprMulU    OpMulti   exprRegexL      | exprRegexL   ; // action => ::first
exprMulR      : exprMulU    OpMulti   exprRegexR      | exprRegexR   ; // action => ::first
exprAddU      : exprAddU    OpAdd     exprMulU        | exprMulU     ; // action => ::first
exprAdd0      : exprAddU    OpAdd     exprMul0        | exprMul0     ; // action => ::first
exprAddL      : exprAddU    OpAdd     exprMulL        | exprMulL     ; // action => ::first
exprAddR      : exprAddU    OpAdd     exprMulR        | exprMulR     ; // action => ::first
exprShiftU    : exprShiftU  OpShift   exprAddU        | exprAddU     ; // action => ::first
exprShift0    : exprShiftU  OpShift   exprAdd0        | exprAdd0     ; // action => ::first
exprShiftL    : exprShiftU  OpShift   exprAddL        | exprAddL     ; // action => ::first
exprShiftR    : exprShiftU  OpShift   exprAddR        | exprAddR     ; // action => ::first
exprNeq0      : exprShift0  OpInequal exprShift0      | exprShift0   ; // action => ::first
exprNeqL      : exprShift0  OpInequal exprShiftL      | exprShiftL   ; // action => ::first
exprNeqR      : exprShift0  OpInequal exprShiftR      | exprShiftR   ; // action => ::first
exprEq0       : exprNeq0    OpEqual   exprNeq0        | exprNeq0     ; // action => ::first
exprEqL       : exprNeq0    OpEqual   exprNeqL        | exprNeqL     ; // action => ::first
exprEqR       : exprNeq0    OpEqual   exprNeqR        | exprNeqR     ; // action => ::first
exprBinAnd0   : exprBinAnd0 OpBinAnd  exprEq0         | exprEq0      ; // action => ::first
exprBinAndL   : exprBinAnd0 OpBinAnd  exprEqL         | exprEqL      ; // action => ::first
exprBinAndR   : exprBinAnd0 OpBinAnd  exprEqR         | exprEqR      ; // action => ::first
exprBinOr0    : exprBinOr0  OpBinOr   exprBinAnd0     | exprBinAnd0  ; // action => ::first
exprBinOrL    : exprBinOr0  OpBinOr   exprBinAndL     | exprBinAndL  ; // action => ::first
exprBinOrR    : exprBinOr0  OpBinOr   exprBinAndR     | exprBinAndR  ; // action => ::first
exprLogAnd0   : exprLogAnd0 OpLogAnd  exprBinOr0      | exprBinOr0   ; // action => ::first
exprLogAndL   : exprLogAnd0 OpLogAnd  exprBinOrL      | exprBinOrL   ; // action => ::first
exprLogAndR   : exprLogAnd0 OpLogAnd  exprBinOrR      | exprBinOrR   ; // action => ::first
exprLogOr0    : exprLogOr0  OpLogOr   exprLogAnd0     | exprLogAnd0  ; // action => ::first
exprLogOrL    : exprLogOr0  OpLogOr   exprLogAndL     | exprLogAndL  ; // action => ::first
exprLogOrR    : exprLogOr0  OpLogOr   exprLogAndR     | exprLogAndR  ; // action => ::first
exprRange0    : exprLogOr0  OpRange   exprLogOr0      | exprLogOr0   ; // action => ::first
exprRangeL    : exprLogOr0  OpRange   exprLogOrL      | exprLogOrL   ; // action => ::first
exprRangeR    : exprLogOr0  OpRange   exprLogOrR      | exprLogOrR   ; // action => ::first
exprCond0     : exprRange0  OpTriThen exprRange0 OpTriElse exprCond0 | exprRange0 ; // action => ::first
exprCondL     : exprRange0  OpTriThen exprRangeL OpTriElse exprCondL | exprRangeL ; // action => ::first
exprCondR     : exprRange0  OpTriThen exprRangeR OpTriElse exprCondR | exprRangeR ; // action => ::first
exprAssignL   : exprCond0   OpAssign  exprAssignL     | opAssignKeywordExpr
                                                        | exprCondL     ; // action => ::firstk
exprAssignR   : exprCond0   OpAssign  exprAssignR     | exprCondR     ; // action => ::first
exprComma     : exprAssignL OpComma exprComma | exprAssignL OpComma | exprAssignR ; // action => ::first
exprNameNot   : OpNameNot   exprNameNot               | exprComma     ; // action => ::first
exprNameAnd   : exprNameAnd OpNameAnd exprNameNot     | exprNameNot   ; // action => ::first
exprNameOr    : exprNameOr  OpNameOr  exprNameAnd     | exprNameAnd   ; // action => ::first
expression    :                                         exprNameOr    ; // action => ::first

/*
# These will never be evaluated as a hashref (LiteralHash)
# because hashrefs are not allowed to be top-level
# Being combined combined with '+' or 'return' means they aren't top-level,
# but follow top-level tokens ('+' or 'return')
*/
nonBraceExprValueU    : nonBraceValue ;
nonBraceExprValue0    : nonBraceValue | opUnaryKeywordExpr ;
nonBraceExprValueL    : nonBraceValue | opAssignKeywordExpr | opUnaryKeywordExpr ;
nonBraceExprValueR    : nonBraceValue | opListKeywordExpr | opAssignKeywordExpr | opUnaryKeywordExpr ;
nonBraceExprArrowU    : nonBraceExprArrowU  OpArrow   arrowRHS        | nonBraceExprValueU   ; // action => ::first
nonBraceExprArrow0    : nonBraceExprArrowU  OpArrow   arrowRHS        | nonBraceExprValue0   ; // action => ::first
nonBraceExprArrowL    : nonBraceExprArrowU  OpArrow   arrowRHS        | nonBraceExprValueL   ; // action => ::first
nonBraceExprArrowR    : nonBraceExprArrowU  OpArrow   arrowRHS        | nonBraceExprValueR   ; // action => ::first
nonBraceExprIncU      : OpInc exprArrowU | nonBraceExprArrowR OpInc   | nonBraceExprArrowU   ; // action => ::first
nonBraceExprInc0      : OpInc exprArrow0 | nonBraceExprArrowR OpInc   | nonBraceExprArrow0   ; // action => ::first
nonBraceExprIncL      : OpInc exprArrowL | nonBraceExprArrowR OpInc   | nonBraceExprArrowL   ; // action => ::first
nonBraceExprIncR      : OpInc exprArrowR | nonBraceExprArrowL OpInc   | nonBraceExprArrowR   ; // action => ::first
nonBraceExprPowerU    : nonBraceExprIncU    OpPower   exprUnaryU      | nonBraceExprIncU     ; // action => ::first
nonBraceExprPower0    : nonBraceExprIncU    OpPower   exprUnary0      | nonBraceExprInc0     ; // action => ::first
nonBraceExprPowerL    : nonBraceExprIncU    OpPower   exprUnaryL      | nonBraceExprIncL     ; // action => ::first
nonBraceExprPowerR    : nonBraceExprIncU    OpPower   exprUnaryR      | nonBraceExprIncR     ; // action => ::first
nonBraceExprUnaryU    : OpUnary     exprUnaryU                | nonBraceExprPowerU   ; // action => ::first
nonBraceExprUnary0    : OpUnary     exprUnary0                | nonBraceExprPower0   ; // action => ::first
nonBraceExprUnaryL    : OpUnary     exprUnaryL                | nonBraceExprPowerL   ; // action => ::first
nonBraceExprUnaryR    : OpUnary     exprUnaryR                | nonBraceExprPowerR   ; // action => ::first
nonBraceExprRegexU    : nonBraceExprRegexU  OpRegex   exprUnaryU      | nonBraceExprUnaryU   ; // action => ::first
nonBraceExprRegex0    : nonBraceExprRegexU  OpRegex   exprUnary0      | nonBraceExprUnary0   ; // action => ::first
nonBraceExprRegexL    : nonBraceExprRegexU  OpRegex   exprUnaryL      | nonBraceExprUnaryL   ; // action => ::first
nonBraceExprRegexR    : nonBraceExprRegexU  OpRegex   exprUnaryR      | nonBraceExprUnaryR   ; // action => ::first
nonBraceExprMulU      : nonBraceExprMulU    OpMulti   exprRegexU      | nonBraceExprRegexU   ; // action => ::first
nonBraceExprMul0      : nonBraceExprMulU    OpMulti   exprRegex0      | nonBraceExprRegex0   ; // action => ::first
nonBraceExprMulL      : nonBraceExprMulU    OpMulti   exprRegexL      | nonBraceExprRegexL   ; // action => ::first
nonBraceExprMulR      : nonBraceExprMulU    OpMulti   exprRegexR      | nonBraceExprRegexR   ; // action => ::first
nonBraceExprAddU      : nonBraceExprAddU    OpAdd     exprMulU        | nonBraceExprMulU     ; // action => ::first
nonBraceExprAdd0      : nonBraceExprAddU    OpAdd     exprMul0        | nonBraceExprMul0     ; // action => ::first
nonBraceExprAddL      : nonBraceExprAddU    OpAdd     exprMulL        | nonBraceExprMulL     ; // action => ::first
nonBraceExprAddR      : nonBraceExprAddU    OpAdd     exprMulR        | nonBraceExprMulR     ; // action => ::first
nonBraceExprShiftU    : nonBraceExprShiftU  OpShift   exprAddU        | nonBraceExprAddU     ; // action => ::first
nonBraceExprShift0    : nonBraceExprShiftU  OpShift   exprAdd0        | nonBraceExprAdd0     ; // action => ::first
nonBraceExprShiftL    : nonBraceExprShiftU  OpShift   exprAddL        | nonBraceExprAddL     ; // action => ::first
nonBraceExprShiftR    : nonBraceExprShiftU  OpShift   exprAddR        | nonBraceExprAddR     ; // action => ::first
nonBraceExprNeq0      : nonBraceExprShift0  OpInequal exprShift0      | nonBraceExprShift0   ; // action => ::first
nonBraceExprNeqL      : nonBraceExprShift0  OpInequal exprShiftL      | nonBraceExprShiftL   ; // action => ::first
nonBraceExprNeqR      : nonBraceExprShift0  OpInequal exprShiftR      | nonBraceExprShiftR   ; // action => ::first
nonBraceExprEq0       : nonBraceExprNeq0    OpEqual   exprNeq0        | nonBraceExprNeq0     ; // action => ::first
nonBraceExprEqL       : nonBraceExprNeq0    OpEqual   exprNeqL        | nonBraceExprNeqL     ; // action => ::first
nonBraceExprEqR       : nonBraceExprNeq0    OpEqual   exprNeqR        | nonBraceExprNeqR     ; // action => ::first
nonBraceExprBinAnd0   : nonBraceExprBinAnd0 OpBinAnd  exprEq0         | nonBraceExprEq0      ; // action => ::first
nonBraceExprBinAndL   : nonBraceExprBinAnd0 OpBinAnd  exprEqL         | nonBraceExprEqL      ; // action => ::first
nonBraceExprBinAndR   : nonBraceExprBinAnd0 OpBinAnd  exprEqR         | nonBraceExprEqR      ; // action => ::first
nonBraceExprBinOr0    : nonBraceExprBinOr0  OpBinOr   exprBinAnd0     | nonBraceExprBinAnd0  ; // action => ::first
nonBraceExprBinOrL    : nonBraceExprBinOr0  OpBinOr   exprBinAndL     | nonBraceExprBinAndL  ; // action => ::first
nonBraceExprBinOrR    : nonBraceExprBinOr0  OpBinOr   exprBinAndR     | nonBraceExprBinAndR  ; // action => ::first
nonBraceExprLogAnd0   : nonBraceExprLogAnd0 OpLogAnd  exprBinOr0      | nonBraceExprBinOr0   ; // action => ::first
nonBraceExprLogAndL   : nonBraceExprLogAnd0 OpLogAnd  exprBinOrL      | nonBraceExprBinOrL   ; // action => ::first
nonBraceExprLogAndR   : nonBraceExprLogAnd0 OpLogAnd  exprBinOrR      | nonBraceExprBinOrR   ; // action => ::first
nonBraceExprLogOr0    : nonBraceExprLogOr0  OpLogOr   exprLogAnd0     | nonBraceExprLogAnd0  ; // action => ::first
nonBraceExprLogOrL    : nonBraceExprLogOr0  OpLogOr   exprLogAndL     | nonBraceExprLogAndL  ; // action => ::first
nonBraceExprLogOrR    : nonBraceExprLogOr0  OpLogOr   exprLogAndR     | nonBraceExprLogAndR  ; // action => ::first
nonBraceExprRange0    : nonBraceExprLogOr0  OpRange   exprLogOr0      | nonBraceExprLogOr0   ; // action => ::first
nonBraceExprRangeL    : nonBraceExprLogOr0  OpRange   exprLogOrL      | nonBraceExprLogOrL   ; // action => ::first
nonBraceExprRangeR    : nonBraceExprLogOr0  OpRange   exprLogOrR      | nonBraceExprLogOrR   ; // action => ::first
nonBraceExprCond0     : nonBraceExprRange0  OpTriThen exprRange0 OpTriElse exprCond0 | nonBraceExprRange0 ; // action => ::first
nonBraceExprCondL     : nonBraceExprRange0  OpTriThen exprRangeL OpTriElse exprCondL | nonBraceExprRangeL ; // action => ::first
nonBraceExprCondR     : nonBraceExprRange0  OpTriThen exprRangeR OpTriElse exprCondR | nonBraceExprRangeR ; // action => ::first
nonBraceExprAssignL   : nonBraceExprCond0   OpAssign  exprAssignL     | opAssignKeywordExpr
                                                                      | nonBraceExprCondL ; //   action => ::first
nonBraceExprAssignR   : nonBraceExprCond0   OpAssign  exprAssignR     | nonBraceExprCondR ; //    action => ::first




nonBraceExprComma     : nonBraceExprAssignL OpComma exprComma    | nonBraceExprAssignR ; // action => ::first

// Comma is only allowed if it follows a keyword operator, to avoid block/hash disambiguation in perl.
blockLevelExprNameNot : OpNameNot exprNameNot | nonBraceExprAssignR ; // action => ::first
blockLevelExprNameAnd : blockLevelExprNameAnd OpNameAnd exprNameNot | blockLevelExprNameNot ; // action => ::first
blockLevelExprNameOr  : blockLevelExprNameOr OpNameOr exprNameAnd | blockLevelExprNameAnd ; // action => ::first
blockLevelExpression  : blockLevelExprNameOr ; // action => ::first

value : literal | nonLiteral | qLikeValue ;

// Arguments of operators according to the operator precedence
opUnaryKeywordArg         : exprShiftR ;
opUnaryKeywordArgNonBrace : nonBraceExprShiftR ;
opAssignKeywordArg        : exprAssignR ;
opListKeywordArg          : exprComma ;
opListKeywordArgNonBrace  : nonBraceExprComma ;

// Same as Value above, but with a NonBraceLiteral
nonBraceValue : nonBraceLiteral | nonLiteral | qLikeValue ;

nonLiteral : variable
           | derefVariable
           | modifier variable
           | modifier parenExpr
           | underscoreValues
           | subCall
           | packageArrow
           | parenExpr elemSeq0
           | opNullaryKeywordExpr
           | diamondExpr ;

diamondExpr : diamond
            | doubleDiamond ;

// This is written this way because of whitespace rules
diamond : '<' varScalar '>'
        | '<' BuiltinFilehandle '>'
        | '<>' ;

// This is written this way because of whitespace rules
doubleDiamond : '<<' varScalar '>>'
              | '<<' BuiltinFilehandle '>>'
              | '<<>>' ;

parenExpr : LParen expression RParen
          | LParen RParen ; // support ()

modifier  : OpKeywordMy | OpKeywordOur | OpKeywordLocal | OpKeywordState ;

elemSeq0 : element* ;
elemSeq1 : element+ ;
element  : arrayElem | hashElem ;

// UnderscoreData and UnderscoreEnd are not values
underscoreValues : UnderscorePackage
                 | UnderscoreFile
                 | UnderscoreLine
                 | UnderscoreSub ;

/*
# Silence these until they are supported
#UnderscoreTokens ::= UnderscoreValues
#                   | UnderscoreData
#                   | UnderscoreEnd
*/

variable : globalVarExpr
         | varScalar
         | varArray
         | varHash
         | varCode
         | varGlob
         | varArrayTop ;

globalVarExpr : '$#'
              | SigilScalar GlobalVariables elemSeq0
              | SigilArray  GlobalVariables elemSeq0
              | SigilHash   GlobalVariables elemSeq0
              | SigilGlob   GlobalVariables elemSeq0 ;

varScalar   : SigilScalar varIdentExpr elemSeq0 ;
varArray    : SigilArray varIdentExpr elemSeq0 ;
varHash     : SigilHash varIdentExpr elemSeq0 ;
varCode     : SigilCode varIdentExpr ;
varGlob     : SigilGlob varIdentExpr ;
varArrayTop : SigilArrayTop varIdentExpr ;

subCall : subNameCallExpr callArgs
        | varCode callArgs ;

packageArrow  : subNameExpr OpArrow packageArrowRHS ;

packageArrowRHS : arrowMethodCall
                | arrowIndirectCall ;

// Used for function calls (Non-QLikeValue string)
subNameCallExpr : SubNameNonQLike
                | subNameCallExpr PackageSep SubNameNonQLike ;

// Used for defining subs (no limits)
subNameExpr : SubNameNonQLike
            | subNameExpr PackageSep SubName ;

SubName : LeadingSubLetter CoreSubLetters ;
LeadingSubLetter : [a-zA-Z_] ;
fragment CoreSubLetters : [a-zA-Z0-9_]* ;
/*
// SubNameNonQLike is for function calls
// They are not allowed to be:
// q / qq / qw / qr / qx
// s / m / y / tr
*/
SubNameNonQLike :
                  NonQLikeLetters                  // [non-qlike]
                | NonQLikeLetters AllSubLetters    // [non-qlike][*]
                | 'q' NonQRWXLetters               // q[non-qrwx]
                | 'q' NonQRWXLetters AllSubLetters // q[non-qrwx][*]
                | 'qq' AllSubLetters               // qq[*]
                | 'qr' AllSubLetters               // qr[*]
                | 'qw' AllSubLetters               // qw[*]
                | 'qx' AllSubLetters               // qx[*]
                | 't'                              // t
                | 't' NonRLetter                   // t[non-r]
                | 't' NonRLetter AllSubLetters     // t[non-r][*]
                | 'tr' AllSubLetters               // tr[*]
                | 's' AllSubLetters                // s[*]
                | 'm' AllSubLetters                // m[*]
                | 'y' AllSubLetters ;              // y[*]

/*
// Variables are defined using a different ident
// Namespaced variables ($x::y) might have a different ident
*/
varIdentExpr : VarIdent
             | varIdentExpr PackageSep VarIdent ;

VarIdent : NonGlobalVarLetters
         | NonGlobalVarLetters AllVarLetters
         | '_' AllVarLetters ;

NonGlobalVarLetters : [a-zA-Z]+ ;
AllVarLetters : [a-zA-Z0-9_]+;

GlobalVariables : '!'
                | '"'
                | '%'
                | '&'
                | [']
                | '('
                | ')'
                | '*'
                | '+'
                | ','
                | '-'
                | '.'
                | '/'
                | ':'
                | ';'
                | '<'
                | '='
                | '>'
                | '?'
                | '@'
                | '['
                | '\\'
                | ']'
                | '^'
                | '_'
                | '`'
                | '|'
                | '~'
                | '$'
                | '^A'
                | '^C'
                | '{^CHILD_ERROR_NATIVE}'
                | '^D'
                | '^E'
                | '{^ENCODING}'
                | '^F'
                | '{^GLOBAL_PHASE}'
                | '^H'
                | '^I'
                | '^L'
                | '{^LAST_FH}'
                | '^M'
                | '{^MATCH}'
                | '^N'
                | '^O'
                | '{^OPEN}'
                | '^P'
                | '{^POSTMATCH}'
                | '{^PREMATCH}'
                | '^R'
                | '{^RE_COMPILE_RECURSION_LIMIT}'
                | '{^RE_DEBUG_FLAGS}'
                | '{^RE_TRIE_MAXBUF}'
                | '^S'
                | '{^SAFE_LOCALES}'
                | '^T'
                | '{^TAINT}'
                | '{^UNICODE}'
                | '{^UTF8CACHE}'
                | '{^UTF8LOCALE}'
                | '^V'
                | '^W'
                | '{^WARNING_BITS}'
                | '{^WIN32_SLOPPY_STAT}'
                | '^X'
                | '{^CAPTURE}'
                | '{^CAPTURE_ALL}'
                | Digits ;

/*
// This uses the same definition as subroutine names
// In the future, we might want to split those
// but they are basically the same 
*/
classIdent : subNameExpr ;

callArgs : parenExpr ;

// Depending on context, "{}" may be interpreted as an empty block or a hash literal.
block   : blockEmpty                    // action => ::first
        | blockNonEmpty ;               // action => ::first

blockEmpty    : LBrace RBrace ;          // name => Block
blockNonEmpty : LBrace statementSeq RBrace ; // name => Block

arrayElem : LBracket expression RBracket ;

hashElem : LBrace expression RBrace ;

nonBraceLiteral : litNumber
                | litArray
                | litString
                | interpolString
                | litHashEmpty ;

literal         : nonBraceLiteral
                | litHashNonEmpty ;

litArray        : LBracket expression RBracket
                | LBracket RBracket ;

// Depending on context, "{}" may be interpreted as an empty block or a hash literal.
litHashEmpty    : LBrace RBrace ;             // name => LitHash
litHashNonEmpty : LBrace expression RBrace ;  // name => LitHash

litString  : SingleQuote NonSingleOrEscapedQuote_Many SingleQuote
           | SingleQuote SingleQuote ;

interpolString : DoubleQuote NonDoubleOrEscapedQuote_Many DoubleQuote
               | DoubleQuote DoubleQuote ;

arrowRHS   : arrowDerefCall
           | arrowDerefVariable
           | arrowMethodCall
           | arrowIndirectCall
           | elemSeq1 ;

arrowDerefCall     : callArgs ;
arrowDerefVariable : derefVariableArgsAll
                   | derefVariableSlice ;
arrowMethodCall    : subNameExpr callArgs
                   | subNameExpr ;
arrowIndirectCall  : SigilScalar varIdentExpr callArgs ;

derefVariableArgsAll : '$*' | '@*' | '%*' | '&*' | '**' | '$#*' ;

derefVariableSlice : '@[' expression ']'
                   | '@{' expression '}'
                   | '%[' expression ']'
                   | '%{' expression '}' ;

derefVariable : SigilScalar   blockNonEmpty
              | SigilArray    blockNonEmpty elemSeq0
              | SigilHash     blockNonEmpty elemSeq0
              | SigilGlob     blockNonEmpty
              | SigilArrayTop blockNonEmpty ;

opNullaryKeywordExpr :
      opKeywordBreakExpr
    | opKeywordForkExpr
    | opKeywordGetloginExpr
    | opKeywordGetppidExpr
    | opKeywordGetpwentExpr
    | opKeywordGetgrentExpr
    | opKeywordGethostentExpr
    | opKeywordGetnetentExpr
    | opKeywordGetprotoentExpr
    | opKeywordGetserventExpr
    | opKeywordSetpwentExpr
    | opKeywordSetgrentExpr
    | opKeywordEndpwentExpr
    | opKeywordEndgrentExpr
    | opKeywordEndhostentExpr
    | opKeywordEndnetentExpr
    | opKeywordEndprotoentExpr
    | opKeywordEndserventExpr
    | opKeywordEvalExpr
    | opKeywordSubExpr
    | opKeywordTimeExpr
    | opKeywordTimesExpr
    | opKeywordWaitExpr
    | opKeywordWantarrayExpr ;

/*
# Unary keyword operators:
#       abs $a, $b => ((abs $a), $b)
#       abs $a = $b => ((abs $a) = $b)
# List keyword operators:
#       push $a, $b => (push ($a, $b))
#       push $a = $b => (push ($a = $b))
# Assign keyword operators:
#       goto $a, $b => ((goto $a), $b)
#       goto $a = $b => (goto ($a = $b))
*/
opUnaryKeywordExpr :
      opKeywordAbsExpr
    | opKeywordAlarmExpr
    | opKeywordCallerExpr
    | opKeywordChdirExpr
    | opKeywordChompExpr
    | opKeywordChopExpr
    | opKeywordChrExpr
    | opKeywordChrootExpr
    | opKeywordCloseExpr
    | opKeywordClosedirExpr
    | opKeywordCosExpr
    | opKeywordDbmcloseExpr
    | opKeywordDefinedExpr
    | opKeywordDeleteExpr
    | opKeywordDoExpr
    | opKeywordEachExpr
    | opKeywordEofExpr
    | opKeywordEvalbytesExpr
    | opKeywordExistsExpr
    | opKeywordExitExpr
    | opKeywordExpExpr
    | opKeywordFcExpr
    | opKeywordFilenoExpr
    | opKeywordGetcExpr
    | opKeywordGetpeernameExpr
    | opKeywordGetpgrpExpr
    | opKeywordGetpwnamExpr
    | opKeywordGetgrnamExpr
    | opKeywordGethostbynameExpr
    | opKeywordGetnetbynameExpr
    | opKeywordGetprotobynameExpr
    | opKeywordGetpwuidExpr
    | opKeywordGetgrgidExpr
    | opKeywordGetprotobynumberExpr
    | opKeywordSethostentExpr
    | opKeywordSetnetentExpr
    | opKeywordSetprotoentExpr
    | opKeywordSetserventExpr
    | opKeywordGetsocknameExpr
    | opKeywordGmtimeExpr
    | opKeywordHexExpr
    | opKeywordIntExpr
    | opKeywordKeysExpr
    | opKeywordLcExpr
    | opKeywordLcfirstExpr
    | opKeywordLengthExpr
    | opKeywordLocaltimeExpr
    | opKeywordLockExpr
    | opKeywordLogExpr
    | opKeywordLstatExpr
    | opKeywordOctExpr
    | opKeywordOrdExpr
    | opKeywordPopExpr
    | opKeywordPosExpr
    | opKeywordPrototypeExpr
    | opKeywordQuotemetaExpr
    | opKeywordRandExpr
    | opKeywordReaddirExpr
    | opKeywordReadlineExpr
    | opKeywordReadlinkExpr
    | opKeywordReadpipeExpr
    | opKeywordRefExpr
    | opKeywordResetExpr
    | opKeywordRewinddirExpr
    | opKeywordRmdirExpr
    | opKeywordScalarExpr
    | opKeywordShiftExpr
    | opKeywordSinExpr
    | opKeywordSleepExpr
    | opKeywordSqrtExpr
    | opKeywordSrandExpr
    | opKeywordStatExpr
    | opKeywordStudyExpr
    | opKeywordTellExpr
    | opKeywordTelldirExpr
    | opKeywordTiedExpr
    | opKeywordUcExpr
    | opKeywordUcfirstExpr
    | opKeywordUmaskExpr
    | opKeywordUndefExpr
    | opKeywordUnlinkExpr
    | opKeywordUntieExpr
    | opKeywordUtimeExpr
    | opKeywordValuesExpr
    | opFileExpr ;

opListKeywordExpr :
      opKeywordAcceptExpr
    | opKeywordAtan2Expr
    | opKeywordBindExpr
    | opKeywordBinmodeExpr
    | opKeywordBlessExpr
    | opKeywordChmodExpr
    | opKeywordChownExpr
    | opKeywordConnectExpr
    | opKeywordCryptExpr
    | opKeywordDbmopenExpr
    | opKeywordDieExpr
    | opKeywordFcntlExpr
    | opKeywordFlockExpr
    | opKeywordGetpriorityExpr
    | opKeywordGetservbynameExpr
    | opKeywordGethostbyaddrExpr
    | opKeywordGetnetbyaddrExpr
    | opKeywordGetservbyportExpr
    | opKeywordExecExpr
    | opKeywordGetsockoptExpr
    | opKeywordGlobExpr
    | opKeywordGrepExpr
    | opKeywordIndexExpr
    | opKeywordIoctlExpr
    | opKeywordJoinExpr
    | opKeywordKillExpr
    | opKeywordLinkExpr
    | opKeywordListenExpr
    | opKeywordMapExpr
    | opKeywordMkdirExpr
    | opKeywordMsgctlExpr
    | opKeywordMsggetExpr
    | opKeywordMsgrcvExpr
    | opKeywordMsgsndExpr
    | opKeywordOpenExpr
    | opKeywordOpendirExpr
    | opKeywordPackExpr
    | opKeywordPipeExpr
    | opKeywordPrintExpr
    | opKeywordPrintfExpr
    | opKeywordPushExpr
    | opKeywordReadExpr
    | opKeywordRecvExpr
    | opKeywordRenameExpr
    | opKeywordReturnExpr
    | opKeywordReverseExpr
    | opKeywordRindexExpr
    | opKeywordSayExpr
    | opKeywordSeekExpr
    | opKeywordSeekdirExpr
    | opKeywordSelectExpr
    | opKeywordSemctlExpr
    | opKeywordSemgetExpr
    | opKeywordSemopExpr
    | opKeywordSendExpr
    | opKeywordSetpgrpExpr
    | opKeywordSetpriorityExpr
    | opKeywordSetsockoptExpr
    | opKeywordShmctlExpr
    | opKeywordShmgetExpr
    | opKeywordShmreadExpr
    | opKeywordShmwriteExpr
    | opKeywordShutdownExpr
    | opKeywordSocketExpr
    | opKeywordSocketpairExpr
    | opKeywordSortExpr
    | opKeywordSpliceExpr
    | opKeywordSplitExpr
    | opKeywordSprintfExpr
    | opKeywordSubstrExpr
    | opKeywordSymlinkExpr
    | opKeywordSyscallExpr
    | opKeywordSysopenExpr
    | opKeywordSysreadExpr
    | opKeywordSysseekExpr
    | opKeywordSyswriteExpr
    | opKeywordSystemExpr
    | opKeywordTieExpr
    | opKeywordTruncateExpr
    | opKeywordUnpackExpr
    | opKeywordUnshiftExpr
    | opKeywordVecExpr
    | opKeywordWaitpidExpr
    | opKeywordWarnExpr
    | opKeywordWriteExpr ;

opAssignKeywordExpr :
      opKeywordDumpExpr
    | opKeywordGotoExpr
    | opKeywordLastExpr
    | opKeywordNextExpr
    | opKeywordRedoExpr ;

opKeywordAbsExpr                : OpKeywordAbs opUnaryKeywordArg
                                | OpKeywordAbs ;

opKeywordAcceptExpr             : OpKeywordAccept opListKeywordArg ;

opKeywordAlarmExpr              : OpKeywordAlarm opUnaryKeywordArg
                                | OpKeywordAlarm ;

opKeywordAtan2Expr              : OpKeywordAtan2 opListKeywordArg ;

opKeywordBindExpr               : OpKeywordBind opListKeywordArg ;

opKeywordBinmodeExpr            : OpKeywordBinmode opListKeywordArg ;

opKeywordBlessExpr              : OpKeywordBless opListKeywordArg ;

opKeywordBreakExpr              : OpKeywordBreak label
                                | OpKeywordBreak ;

opKeywordCallerExpr             : OpKeywordCaller opUnaryKeywordArg
                                | OpKeywordCaller ;

opKeywordChdirExpr              : OpKeywordChdir opUnaryKeywordArg
                                | OpKeywordChdir ;

opKeywordChmodExpr              : OpKeywordChmod opListKeywordArg ;

opKeywordChompExpr              : OpKeywordChomp opUnaryKeywordArg
                                | OpKeywordChomp ;

opKeywordChopExpr               : OpKeywordChop opUnaryKeywordArg
                                | OpKeywordChop ;

opKeywordChownExpr              : OpKeywordChown opListKeywordArg ;

opKeywordChrExpr                : OpKeywordChr opUnaryKeywordArg
                                | OpKeywordChr ;

opKeywordChrootExpr             : OpKeywordChroot opUnaryKeywordArg
                                | OpKeywordChroot ;

opKeywordCloseExpr              : OpKeywordClose opUnaryKeywordArg
                                | OpKeywordClose ;

opKeywordClosedirExpr           : OpKeywordClosedir opUnaryKeywordArg ;

opKeywordConnectExpr            : OpKeywordConnect opListKeywordArg ;

opKeywordCosExpr                : OpKeywordCos opUnaryKeywordArg ;

opKeywordCryptExpr              : OpKeywordCrypt opListKeywordArg ;

opKeywordDbmcloseExpr           : OpKeywordDbmclose opUnaryKeywordArg ;

opKeywordDbmopenExpr            : OpKeywordDbmopen opListKeywordArg ;

opKeywordDefinedExpr            : OpKeywordDefined opUnaryKeywordArg
                                | OpKeywordDefined ;

opKeywordDeleteExpr             : OpKeywordDelete opUnaryKeywordArg ;

opKeywordDieExpr                : OpKeywordDie opListKeywordArg ;

opKeywordDoExpr                 : OpKeywordDo blockNonEmpty 
                                | OpKeywordDo opUnaryKeywordArgNonBrace ;

opKeywordDumpExpr               : OpKeywordDump opAssignKeywordArg
                                | OpKeywordDump label
                                | OpKeywordDump ;

opKeywordEachExpr               : OpKeywordEach opUnaryKeywordArg ;

opKeywordEofExpr                : OpKeywordEof opUnaryKeywordArg
                                | OpKeywordEof ;

opKeywordEvalExpr               : OpKeywordEval blockNonEmpty ;

opKeywordEvalbytesExpr          : OpKeywordEvalbytes opUnaryKeywordArg
                                | OpKeywordEvalbytes ;

opKeywordExistsExpr             : OpKeywordExists opUnaryKeywordArg ;

opKeywordExitExpr               : OpKeywordExit opUnaryKeywordArg
                                | OpKeywordExit ;

opKeywordExpExpr                : OpKeywordExp opUnaryKeywordArg
                                | OpKeywordExp ;

opKeywordFcExpr                 : OpKeywordFc opUnaryKeywordArg
                                | OpKeywordFc ;

opKeywordFcntlExpr              : OpKeywordFcntl opListKeywordArg ;

opKeywordFilenoExpr             : OpKeywordFileno opUnaryKeywordArg ;

opKeywordFlockExpr              : OpKeywordFlock opListKeywordArg ;

opKeywordForkExpr               : OpKeywordFork LParen RParen
                                | OpKeywordFork ;

opKeywordGetcExpr               : OpKeywordGetc opUnaryKeywordArg
                                | OpKeywordGetc ;

opKeywordGetloginExpr           : OpKeywordGetlogin LParen RParen
                                | OpKeywordGetlogin ;

opKeywordGetpeernameExpr        : OpKeywordGetpeername opUnaryKeywordArg ;

opKeywordGetpgrpExpr            : OpKeywordGetpgrp opUnaryKeywordArg ;

opKeywordGetppidExpr            : OpKeywordGetppid LParen RParen
                                | OpKeywordGetppid ;

opKeywordGetpriorityExpr        : OpKeywordGetpriority opListKeywordArg ;

opKeywordGetpwnamExpr           : OpKeywordGetpwnam opUnaryKeywordArg ;

opKeywordGetgrnamExpr           : OpKeywordGetgrnam opUnaryKeywordArg ;

opKeywordGethostbynameExpr      : OpKeywordGethostbyname opUnaryKeywordArg ;

opKeywordGetnetbynameExpr       : OpKeywordGetnetbyname opUnaryKeywordArg ;

opKeywordGetprotobynameExpr     : OpKeywordGetprotobyname opUnaryKeywordArg ;

opKeywordGetpwuidExpr           : OpKeywordGetpwuid opUnaryKeywordArg ;

opKeywordGetgrgidExpr           : OpKeywordGetgrgid opUnaryKeywordArg ;

opKeywordGetservbynameExpr      : OpKeywordGetservbyname opListKeywordArg ;

opKeywordGethostbyaddrExpr      : OpKeywordGethostbyaddr opListKeywordArg ;

opKeywordGetnetbyaddrExpr       : OpKeywordGetnetbyaddr opListKeywordArg ;

opKeywordGetprotobynumberExpr   : OpKeywordGetprotobynumber opUnaryKeywordArg ;

opKeywordGetservbyportExpr      : OpKeywordGetservbyport opListKeywordArg ;

opKeywordGetpwentExpr           : OpKeywordGetpwent LParen RParen
                                | OpKeywordGetpwent ;

opKeywordGetgrentExpr           : OpKeywordGetgrent LParen RParen
                                | OpKeywordGetgrent ;

opKeywordGethostentExpr         : OpKeywordGethostent LParen RParen
                                | OpKeywordGethostent ;

opKeywordGetnetentExpr          : OpKeywordGetnetent LParen RParen
                                | OpKeywordGetnetent ;

opKeywordGetprotoentExpr        : OpKeywordGetprotoent LParen RParen
                                | OpKeywordGetprotoent ;

opKeywordGetserventExpr         : OpKeywordGetservent LParen RParen
                                | OpKeywordGetservent ;

opKeywordSetpwentExpr           : OpKeywordSetpwent LParen RParen
                                | OpKeywordSetpwent ;

opKeywordSetgrentExpr           : OpKeywordSetgrent LParen RParen
                                | OpKeywordSetgrent ;

opKeywordSethostentExpr         : OpKeywordSethostent opUnaryKeywordArg ;

opKeywordSetnetentExpr          : OpKeywordSetnetent opUnaryKeywordArg ;

opKeywordSetprotoentExpr        : OpKeywordSetprotoent opUnaryKeywordArg ;

opKeywordSetserventExpr         : OpKeywordSetservent opUnaryKeywordArg ;

opKeywordEndpwentExpr           : OpKeywordEndpwent LParen RParen
                                | OpKeywordEndpwent ;

opKeywordEndgrentExpr           : OpKeywordEndgrent LParen RParen
                                | OpKeywordEndgrent ;

opKeywordEndhostentExpr         : OpKeywordEndhostent LParen RParen
                                | OpKeywordEndhostent ;

opKeywordEndnetentExpr          : OpKeywordEndnetent LParen RParen
                                | OpKeywordEndnetent ;

opKeywordEndprotoentExpr        : OpKeywordEndprotoent LParen RParen
                                | OpKeywordEndprotoent ;

opKeywordEndserventExpr         : OpKeywordEndservent LParen RParen
                                | OpKeywordEndservent ;

opKeywordExecExpr               : OpKeywordExec blockNonEmpty opListKeywordArg
                                | OpKeywordExec opListKeywordArgNonBrace ;

opKeywordGetsocknameExpr        : OpKeywordGetsockname opUnaryKeywordArg ;

opKeywordGetsockoptExpr         : OpKeywordGetsockopt opListKeywordArg ;

opKeywordGlobExpr               : OpKeywordGlob opListKeywordArg
                                | OpKeywordGlob ;

opKeywordGmtimeExpr             : OpKeywordGmtime opUnaryKeywordArg
                                | OpKeywordGmtime ;

// &NAME is an expression too
opKeywordGotoExpr               : OpKeywordGoto opAssignKeywordArg
                                | OpKeywordGoto label ;

opKeywordGrepExpr               : OpKeywordGrep blockNonEmpty opListKeywordArg
                                | OpKeywordGrep opListKeywordArgNonBrace ;

opKeywordHexExpr                : OpKeywordHex opUnaryKeywordArg
                                | OpKeywordHex ;

opKeywordIndexExpr              : OpKeywordIndex opListKeywordArg ;

opKeywordIntExpr                : OpKeywordInt opUnaryKeywordArg
                                | OpKeywordInt ;

opKeywordIoctlExpr              : OpKeywordIoctl opListKeywordArg ;

opKeywordJoinExpr               : OpKeywordJoin opListKeywordArg ;

opKeywordKeysExpr               : OpKeywordKeys opUnaryKeywordArg ;

opKeywordKillExpr               : OpKeywordKill opListKeywordArg
                                | OpKeywordKill expression ;

opKeywordLastExpr               : OpKeywordLast opAssignKeywordArg
                                | OpKeywordLast label
                                | OpKeywordLast ;

opKeywordLcExpr                 : OpKeywordLc opUnaryKeywordArg
                                | OpKeywordLc ;

opKeywordLcfirstExpr            : OpKeywordLcfirst opUnaryKeywordArg
                                | OpKeywordLcfirst ;

opKeywordLengthExpr             : OpKeywordLength opUnaryKeywordArg
                                | OpKeywordLength ;

opKeywordLinkExpr               : OpKeywordLink opListKeywordArg ;

opKeywordListenExpr             : OpKeywordListen opListKeywordArg ;

opKeywordLocaltimeExpr          : OpKeywordLocaltime opUnaryKeywordArg
                                | OpKeywordLocaltime ;

opKeywordLockExpr               : OpKeywordLock opUnaryKeywordArg ;

opKeywordLogExpr                : OpKeywordLog opUnaryKeywordArg
                                | OpKeywordLog ;

opKeywordLstatExpr              : OpKeywordLstat opUnaryKeywordArg
                                | OpKeywordLstat ;

opKeywordMapExpr                : OpKeywordMap blockNonEmpty opListKeywordArg
                                | OpKeywordMap opListKeywordArgNonBrace ;

opKeywordMkdirExpr              : OpKeywordMkdir opListKeywordArg
                                | OpKeywordMkdir ;

opKeywordMsgctlExpr             : OpKeywordMsgctl opListKeywordArg ;

opKeywordMsggetExpr             : OpKeywordMsgget opListKeywordArg ;

opKeywordMsgrcvExpr             : OpKeywordMsgrcv opListKeywordArg ;

opKeywordMsgsndExpr             : OpKeywordMsgsnd opListKeywordArg ;

opKeywordNextExpr               : OpKeywordNext opAssignKeywordArg
                                | OpKeywordNext label
                                | OpKeywordNext ;

opKeywordOctExpr                : OpKeywordOct opUnaryKeywordArg
                                | OpKeywordOct ;

opKeywordOpenExpr               : OpKeywordOpen opListKeywordArg ;

opKeywordOpendirExpr            : OpKeywordOpendir opListKeywordArg ;

opKeywordOrdExpr                : OpKeywordOrd opUnaryKeywordArg
                                | OpKeywordOrd ;

opKeywordPackExpr               : OpKeywordPack opListKeywordArg ;

opKeywordPipeExpr               : OpKeywordPipe opListKeywordArg ;

opKeywordPopExpr                : OpKeywordPop opUnaryKeywordArg
                                | OpKeywordPop ;

opKeywordPosExpr                : OpKeywordPos opUnaryKeywordArg
                                | OpKeywordPos ;

opKeywordPrintExpr              : OpKeywordPrint blockNonEmpty opListKeywordArg
                                | OpKeywordPrint BuiltinFilehandle opListKeywordArgNonBrace
                                | OpKeywordPrint BuiltinFilehandle
                                | OpKeywordPrint opListKeywordArgNonBrace
                                | OpKeywordPrint blockNonEmpty
                                | OpKeywordPrint ;

opKeywordPrintfExpr             : OpKeywordPrintf blockNonEmpty opListKeywordArg
                                | OpKeywordPrintf BuiltinFilehandle opListKeywordArgNonBrace
                                | OpKeywordPrintf BuiltinFilehandle
                                | OpKeywordPrintf opListKeywordArgNonBrace
                                | OpKeywordPrintf blockNonEmpty ;

opKeywordPrototypeExpr          : OpKeywordPrototype opUnaryKeywordArg
                                | OpKeywordPrototype ;

opKeywordPushExpr               : OpKeywordPush opListKeywordArg ;

opKeywordQuotemetaExpr          : OpKeywordQuotemeta opUnaryKeywordArg
                                | OpKeywordQuotemeta ;

opKeywordRandExpr               : OpKeywordRand opUnaryKeywordArg
                                | OpKeywordRand ;

opKeywordReadExpr               : OpKeywordRead opListKeywordArg ;

opKeywordReaddirExpr            : OpKeywordReaddir opUnaryKeywordArg ;

opKeywordReadlineExpr           : OpKeywordReadline opUnaryKeywordArg
                                | OpKeywordReadline ;

opKeywordReadlinkExpr           : OpKeywordReadlink opUnaryKeywordArg
                                | OpKeywordReadlink ;

opKeywordReadpipeExpr           : OpKeywordReadpipe opUnaryKeywordArg
                                | OpKeywordReadpipe ;

opKeywordRecvExpr               : OpKeywordRecv opListKeywordArg ;

opKeywordRedoExpr               : OpKeywordRedo opAssignKeywordArg
                                | OpKeywordRedo label
                                | OpKeywordRedo ;

opKeywordRefExpr                : OpKeywordRef opUnaryKeywordArg
                                | OpKeywordRef ;

opKeywordRenameExpr             : OpKeywordRename opListKeywordArg ;

opKeywordResetExpr              : OpKeywordReset opUnaryKeywordArg
                                | OpKeywordReset ;

opKeywordReturnExpr             : OpKeywordReturn opListKeywordArg
                                | OpKeywordReturn ;

opKeywordReverseExpr            : OpKeywordReverse opListKeywordArg ;

opKeywordRewinddirExpr          : OpKeywordRewinddir opUnaryKeywordArg
                                | OpKeywordRewinddir ;

opKeywordRindexExpr             : OpKeywordRindex opListKeywordArg
                                | OpKeywordRindex ;

opKeywordRmdirExpr              : OpKeywordRmdir opUnaryKeywordArg
                                | OpKeywordRmdir ;

opKeywordSayExpr                : OpKeywordSay blockNonEmpty opListKeywordArg
                                | OpKeywordSay BuiltinFilehandle opListKeywordArgNonBrace
                                | OpKeywordSay BuiltinFilehandle
                                | OpKeywordSay opListKeywordArgNonBrace
                                | OpKeywordSay blockNonEmpty
                                | OpKeywordSay ;

opKeywordScalarExpr             : OpKeywordScalar opUnaryKeywordArg ;

opKeywordSeekExpr               : OpKeywordSeek opListKeywordArg ;

opKeywordSeekdirExpr            : OpKeywordSeekdir opListKeywordArg ;

opKeywordSelectExpr             : OpKeywordSelect opListKeywordArg ;

opKeywordSemctlExpr             : OpKeywordSemctl opListKeywordArg ;

opKeywordSemgetExpr             : OpKeywordSemget opListKeywordArg ;

opKeywordSemopExpr              : OpKeywordSemop opListKeywordArg ;

opKeywordSendExpr               : OpKeywordSend opListKeywordArg ;

opKeywordSetpgrpExpr            : OpKeywordSetpgrp opListKeywordArg ;

opKeywordSetpriorityExpr        : OpKeywordSetpriority opListKeywordArg ;

opKeywordSetsockoptExpr         : OpKeywordSetsockopt opListKeywordArg ;

opKeywordShiftExpr              : OpKeywordShift opUnaryKeywordArg
                                | OpKeywordShift ;

opKeywordShmctlExpr             : OpKeywordShmctl opListKeywordArg ;

opKeywordShmgetExpr             : OpKeywordShmget opListKeywordArg ;

opKeywordShmreadExpr            : OpKeywordShmread opListKeywordArg ;

opKeywordShmwriteExpr           : OpKeywordShmwrite opListKeywordArg ;

opKeywordShutdownExpr           : OpKeywordShutdown opListKeywordArg ;

opKeywordSinExpr                : OpKeywordSin opUnaryKeywordArg
                                | OpKeywordSin ;

opKeywordSleepExpr              : OpKeywordSleep opUnaryKeywordArg
                                | OpKeywordSleep ;

opKeywordSocketExpr             : OpKeywordSocket opListKeywordArg ;

opKeywordSocketpairExpr         : OpKeywordSocketpair opListKeywordArg ;

opKeywordSortExpr               : OpKeywordSort blockNonEmpty opListKeywordArg
                                | OpKeywordSort varScalar opListKeywordArg
                                | OpKeywordSort opListKeywordArgNonBrace ;

opKeywordSpliceExpr             : OpKeywordSplice opListKeywordArg ;

opKeywordSplitExpr              : OpKeywordSplit opListKeywordArg ;

opKeywordSprintfExpr            : OpKeywordSprintf opListKeywordArg ;

opKeywordSqrtExpr               : OpKeywordSqrt opUnaryKeywordArg
                                | OpKeywordSqrt ;

opKeywordSrandExpr              : OpKeywordSrand opUnaryKeywordArg
                                | OpKeywordSrand ;

opKeywordStatExpr               : OpKeywordStat opUnaryKeywordArg
                                | OpKeywordStat ;

opKeywordStudyExpr              : OpKeywordStudy opUnaryKeywordArg
                                | OpKeywordStudy ;

opKeywordSubExpr                : OpKeywordSub subDefinition ;

opKeywordSubstrExpr             : OpKeywordSubstr opListKeywordArg ;

opKeywordSymlinkExpr            : OpKeywordSymlink opListKeywordArg ;

opKeywordSyscallExpr            : OpKeywordSyscall opListKeywordArg ;

opKeywordSysopenExpr            : OpKeywordSysopen opListKeywordArg ;

opKeywordSysreadExpr            : OpKeywordSysread opListKeywordArg ;

opKeywordSysseekExpr            : OpKeywordSysseek opListKeywordArg ;

opKeywordSyswriteExpr           : OpKeywordSyswrite opListKeywordArg ;

opKeywordSystemExpr             : OpKeywordSystem blockNonEmpty opListKeywordArg
                                | OpKeywordSystem opListKeywordArgNonBrace ;

opKeywordTellExpr               : OpKeywordTell opUnaryKeywordArg
                                | OpKeywordTell ;

opKeywordTelldirExpr            : OpKeywordTelldir opUnaryKeywordArg ;

opKeywordTieExpr                : OpKeywordTie opListKeywordArg ;

opKeywordTiedExpr               : OpKeywordTied opUnaryKeywordArg ;

opKeywordTimeExpr               : OpKeywordTime LParen RParen
                                | OpKeywordTime ;

opKeywordTimesExpr              : OpKeywordTimes LParen RParen
                                | OpKeywordTimes ;

opKeywordTruncateExpr           : OpKeywordTruncate opListKeywordArg ;

opKeywordUcExpr                 : OpKeywordUc opUnaryKeywordArg
                                | OpKeywordUc ;

opKeywordUcfirstExpr            : OpKeywordUcfirst opUnaryKeywordArg
                                | OpKeywordUcfirst ;

opKeywordUmaskExpr              : OpKeywordUmask opUnaryKeywordArg
                                | OpKeywordUmask ;

opKeywordUndefExpr              : OpKeywordUndef opUnaryKeywordArg
                                | OpKeywordUndef ;

opKeywordUnlinkExpr             : OpKeywordUnlink opUnaryKeywordArg
                                | OpKeywordUnlink ;

opKeywordUnpackExpr             : OpKeywordUnpack opListKeywordArg ;

opKeywordUnshiftExpr            : OpKeywordUnshift opListKeywordArg ;

opKeywordUntieExpr              : OpKeywordUntie opUnaryKeywordArg ;

opKeywordUtimeExpr              : OpKeywordUtime opUnaryKeywordArg ;

opKeywordValuesExpr             : OpKeywordValues opUnaryKeywordArg ;

opKeywordVecExpr                : OpKeywordVec opListKeywordArg ;

opKeywordWaitExpr               : OpKeywordWait LParen RParen
                                | OpKeywordWait ;

opKeywordWaitpidExpr            : OpKeywordWaitpid opListKeywordArg ;

opKeywordWantarrayExpr          : OpKeywordWantarray LParen RParen
                                | OpKeywordWantarray ;

opKeywordWarnExpr               : OpKeywordWarn opListKeywordArg
                                | OpKeywordWarn ;

opKeywordWriteExpr              : OpKeywordWrite opListKeywordArg
                                | OpKeywordWrite ;

opFile :
      OpFileReadableEffective
    | OpFileWritableEffective
    | OpFileRExecutableEffective
    | OpFileOwnedEffective
    | OpFileReadableReal
    | OpFileWritableReal
    | OpFileRExecutableReal
    | OpFileOwnedReal
    | OpFileExists
    | OpFileEmpty
    | OpFileNonEmpty
    | OpFilePlain
    | OpFileDirectory
    | OpFileSymbolic
    | OpFileNamedPipe
    | OpFileSocket
    | OpFileBlock
    | OpFileCharacter
    | OpFileOpenedTty
    | OpFileSetuid
    | OpFileSetgid
    | OpFileSticky
    | OpFileAsciiUtf8
    | OpFileBinary
    | OpFileStartTime
    | OpFileAccessTime
    | OpFileChangeTime ;

opFileExpr : opFile opFileArg ;
opFileArg : opUnaryKeywordArg
          | BuiltinFilehandle ;

qLikeValue : QLikeValueExpr | QLikeValueExprWithMods ;

QLikeValueExpr
    : QLikeFunction '(' NonRParenOrEscapedParens_Any               ')'
    | QLikeFunction '{' NonRBraceOrEscapedBraces_Any               '}'
    | QLikeFunction '<' NonRAngleOrEscapedAngles_Any               '>'
    | QLikeFunction '[' NonRBracketOrEscapedBrackets_Any           ']'
    | QLikeFunction '/' NonForwardSlashOrEscapedForwardSlashes_Any '/'
    | QLikeFunction '!' NonExclamPointOrEscapedExclamPoints_Any    '!'
    | QLikeFunction '|' NonPipeOrEscapedPipes_Any                  '|' ;

QLikeFunction : OpKeywordQ
              | OpKeywordQq
              | OpKeywordQx
              | OpKeywordQw ;
/*
# Here we begin with "qr//" and "m//" which can have parameters
# Then we continue with "s///", "tr///", and "y///" which have two args, not one
# "//" follow at the end
*/
QLikeValueExprWithMods
    : QLikeFunctionWithMods '(' NonRParenOrEscapedParens_Any               ')' RegexModifiers
    | QLikeFunctionWithMods '{' NonRBraceOrEscapedBraces_Any               '}' RegexModifiers
    | QLikeFunctionWithMods '<' NonRAngleOrEscapedAngles_Any               '>' RegexModifiers
    | QLikeFunctionWithMods '[' NonRBracketOrEscapedBrackets_Any           ']' RegexModifiers
    | QLikeFunctionWithMods '/' NonForwardSlashOrEscapedForwardSlashes_Any '/' RegexModifiers
    | QLikeFunctionWithMods '!' NonExclamPointOrEscapedExclamPoints_Any    '!' RegexModifiers
    | QLikeFunctionWithMods '|' NonPipeOrEscapedPipes_Any                  '|' RegexModifiers
    | QLikeSubstWithMods    '(' NonRParenOrEscapedParens_Any               ')(' NonRParenOrEscapedParens_Any               ')' RegexModifiers
    | QLikeSubstWithMods    '{' NonRBraceOrEscapedBraces_Any               '}{' NonRBraceOrEscapedBraces_Any               '}' RegexModifiers
    | QLikeSubstWithMods    '<' NonRAngleOrEscapedAngles_Any               '><' NonRAngleOrEscapedAngles_Any               '>' RegexModifiers
    | QLikeSubstWithMods    '[' NonRBracketOrEscapedBrackets_Any           '][' NonRBracketOrEscapedBrackets_Any           ']' RegexModifiers
    | QLikeSubstWithMods    '/' NonForwardSlashOrEscapedForwardSlashes_Any '/'  NonForwardSlashOrEscapedForwardSlashes_Any '/' RegexModifiers
    | QLikeSubstWithMods    '!' NonExclamPointOrEscapedExclamPoints_Any    '!'  NonExclamPointOrEscapedExclamPoints_Any    '!' RegexModifiers
    | QLikeSubstWithMods    '|' NonPipeOrEscapedPipes_Any                  '|'  NonPipeOrEscapedPipes_Any                  '|' RegexModifiers
    | '/' NonForwardSlashOrEscapedForwardSlashes_Any '/' RegexModifiers
    | '`' NonBacktickOrEscapedBackticks_Any '`' ;

QLikeFunctionWithMods : OpKeywordQr
                      | OpKeywordM ;
                      
QLikeSubstWithMods : OpKeywordS
                   | OpKeywordTr
                   | OpKeywordY ;

fragment RegexModifiers : [a-z]*;

/*
// Everything except: # q* / s / m / y / t*
//                         |       |   | |         |
// a b c d e f g h i j k l m n o p q r s t u v w x y z
// a -                   l   n - p   r     u -   x   z
// (Cannot begin with digit, so digits are out)
*/
NonQLikeLetters : [a-ln-pru-xzA-Z_]+ ;

/*
// Everything except: q / w / r / x (qq, qw, qr, qx)
//                                 | |         | |
// a b c d e f g h i j k l m n o p q r s t u v w x y z
// a -                           p     s -   v     y-z
// (digits also allowed at this point
*/
NonQRWXLetters : [a-ps-vy-zA-Z0-9]+ ;

/*
// Everything except: r (tr)
//                                    |
//  a b c d e f g h i j k l m n o p q r s t u v w x y z
//  a -                             q   s -           z
// (digits also allowed at this point)
*/
NonRLetter : [a-qs-zA-Z0-9_] ;

// Everything else allowed (including digits)
AllSubLetters : [a-zA-Z0-9_]+ ;

IdentComp : [a-zA-Z0-9]+ ;
PackageSep : '::';

versionExpr : VersionNumber ;
VersionNumber : VersionNumberSegments
              | 'v' VersionNumberSegments ;

VersionNumberSegments : VersionNumberSegment '.' VersionNumberSegment '.' VersionNumberSegment
                      | VersionNumberSegment '.' VersionNumberSegments
                      | VersionNumberSegment ;

VersionNumberSegment : [0-9] [0-9] [0-9]
                     | [0-9] [0-9]
                     | [0-9] ;

litNumber : litNumberDec
          | litNumberOct
          | litNumberHex
          | litNumberBin ;

litNumberDec : NumberDec ;
litNumberOct : NumberOct ;
litNumberHex : NumberHex ;
litNumberBin : NumberBin ;

NumberDec : NumberDecInt
          | NumberDecInt ExpDec
          | NumberDecInt '.' DigitsDec
          | NumberDecInt '.' DigitsDec ExpDec
          | '.' DigitDec DigitsDec
          | '.' DigitDec DigitsDec ExpDec ;

NumberDecInt : [1-9] DigitsDec | '0' ;

NumberOct : '0' Underbars DigitOct DigitsOct
          | '0' Underbars DigitOct DigitsBin ExpHex
          | '0' DigitsOct '.' DigitBin DigitsBin ExpHex ;

NumberHex : '0' [xX] Underbars DigitHex DigitsHex
          | '0' [xX] Underbars DigitHex DigitsHex ExpHex
          | '0' [xX] DigitsHex '.' DigitHex DigitsHex ExpHex ;

NumberBin : '0' [bB] Underbars DigitBin DigitsBin
          | '0' [bB] Underbars DigitBin DigitsBin ExpHex
          | '0' [bB] DigitsBin '.' DigitBin DigitsBin ExpHex ;

ExpDec : [eE] [+-] ExpDecExp
       | [eE] [_] [+-] ExpDecExp
       | [eE] ExpDecExp ;

ExpDecExp : Underbars DigitDec DigitsDec ;
Underbars : [_]+ ;

ExpHex : [pP] [+-] DigitDec DigitsDec
        | [pP] DigitDec DigitsDec ;

DigitDec : [0-9] ;
fragment DigitsDec : [0-9_]* ;

DigitOct : [0-7] ;
fragment DigitsOct : [0-7]* ;
DigitHex : [0-9a-fA-F] ;
fragment DigitsHex : [0-9a-fA-F]* ;

DigitBin : [01] ;
fragment DigitsBin : [01]* ;

Digits : [0-9]+ ;
SingleQuote : ['] ;
DoubleQuote : ["] ;

NonDoubleOrEscapedQuote_Many : NonDoubleOrEscapedQuote+ ;
NonDoubleOrEscapedQuote : EscapedDoubleQuote | NonDoubleQuote ;
EscapedDoubleQuote : Escape ["] ;
NonDoubleQuote : [^"] ;

NonSingleOrEscapedQuote_Many : NonSingleOrEscapedQuote+ ;
NonSingleOrEscapedQuote : EscapedSingleQuote | NonSingleQuote ;
EscapedSingleQuote : Escape ['] ;
NonSingleQuote : [^'] ;

Colon : ':';
Semicolon : ';';
Escape : '\\';

SigilScalar : '$';
SigilArray : '@';
SigilHash : '%';
SigilCode : '&';
SigilGlob : '*';
SigilArrayTop : '$#';

LParen : '(';
RParen : ')';
LBracket : '[';
RBracket : ']';
LBrace : '{';
RBrace : '}';

fragment NonRParenOrEscapedParens_Any : NonRParenOrEscapedParens* ;
NonRParenOrEscapedParens : EscapedParens | NonRParen ;
EscapedParens : EscapedLParen | EscapedRParen ;
EscapedLParen : Escape [(] ;
EscapedRParen : Escape [)] ;
NonRParen : [^)] ;

fragment NonRBracketOrEscapedBrackets_Any : NonRBracketOrEscapedBrackets* ;
NonRBracketOrEscapedBrackets : EscapedBrackets | NonRBracket ;
EscapedBrackets : EscapedLBracket | EscapedRBracket ;
EscapedLBracket : Escape [[] ;
EscapedRBracket : Escape [\]] ;
NonRBracket : [^\]] ;

fragment NonRBraceOrEscapedBraces_Any : NonRBraceOrEscapedBraces* ;
NonRBraceOrEscapedBraces : EscapedBraces | NonRBrace ;
EscapedBraces : EscapedLBrace | EscapedRBrace ;
EscapedLBrace : Escape [{] ;
EscapedRBrace : Escape [}] ;
NonRBrace : [^}] ;

fragment NonRAngleOrEscapedAngles_Any : NonRAngleOrEscapedAngles* ;
NonRAngleOrEscapedAngles : EscapedAngles | NonRAngle ;
EscapedAngles : EscapedLAngle | EscapedRAngle ;
EscapedLAngle : Escape [<] ;
EscapedRAngle : Escape [>] ;
NonRAngle : [^>] ;

fragment NonForwardSlashOrEscapedForwardSlashes_Any : NonForwardSlashOrEscapedForwardSlashes* ;
NonForwardSlashOrEscapedForwardSlashes : EscapedForwardSlash | NonForwardSlash ;
EscapedForwardSlash : Escape [/];
NonForwardSlash : [^/] ;

fragment NonExclamPointOrEscapedExclamPoints_Any : NonExclamPointOrEscapedExclamPoints* ;
NonExclamPointOrEscapedExclamPoints : EscapedExclamPoint | NonExclamPoint ;
EscapedExclamPoint : Escape [!];
NonExclamPoint : [^!] ;

fragment NonPipeOrEscapedPipes_Any : NonPipeOrEscapedPipes* ;
NonPipeOrEscapedPipes : EscapedPipe | NonPipe ;
EscapedPipe : Escape [|];
NonPipe : [^|] ;

fragment NonBacktickOrEscapedBackticks_Any : NonBacktickOrEscapedBackticks* ;
NonBacktickOrEscapedBackticks : EscapedBacktick | NonBacktick ;
EscapedBacktick : Escape [`];
NonBacktick : [^`] ;

Ellipsis : '...';

UnderscorePackage : '__PACKAGE__';
UnderscoreFile: '__FILE__';
UnderscoreLine : '__LINE__';
UnderscoreSub : '__SUB__';
UnderscoreData : '__DATA__';
UnderscoreEnd : '__END__';

PhaseName : 'BEGIN' | 'CHECK' | 'INIT' | 'UNITCHECK' | 'END';

SubAttrArgs : '(' NonRParenOrEscapedParens_Any ')' ;

OpArrow : '->';
OpInc : '++' | '--';
OpPower : '**';
OpUnary : '!' | '~' | '\\' | '+' | '-';
OpRegex : '=~' | '!~';
OpMulti : '*' | '/' | '%' | 'x';
OpAdd : '+' | '-' | '.';
OpShift : '<<' | '>>';
OpInequal : '<' | '>' | '<=' | '>=' | 'lt' | 'gt' | 'le' | 'ge';
OpEqual : '==' | '!=' | '<=>' | 'eq' | 'ne' | 'cmp';
OpBinAnd : '&';
OpBinOr : '|' | '^';
OpLogAnd : '&&';
OpLogOr : '||' | '//';
OpRange : '..' | '...';
OpTriThen : '?';
OpTriElse : ':';
OpAssign : '=' | '*=' | '/=' | 'x=' | '+=' | '-=' | '.=' | '<<=' | '>>=' | '&=' | '|=' | '^=' | '&&=' | '||=' | '//=';
OpComma : ',' | '=>';
OpNameNot : 'not';
OpNameAnd : 'and';
OpNameOr : 'or' | 'xor';

OpKeywordAbs : 'abs';
OpKeywordAccept : 'accept';
OpKeywordAlarm : 'alarm';
OpKeywordAtan2 : 'atan2';
OpKeywordBind : 'bind';
OpKeywordBinmode : 'binmode';
OpKeywordBless : 'bless';
OpKeywordBreak : 'break';
OpKeywordCaller : 'caller';
OpKeywordChdir : 'chdir';
OpKeywordChmod : 'chmod';
OpKeywordChomp : 'chomp';
OpKeywordChop : 'chop';
OpKeywordChown : 'chown';
OpKeywordChr : 'chr';
OpKeywordChroot : 'chroot';
OpKeywordClose : 'close';
OpKeywordClosedir : 'closedir';
OpKeywordConnect : 'connect';
OpKeywordContinue : 'continue';
OpKeywordCos : 'cos';
OpKeywordCrypt : 'crypt';
OpKeywordDbmclose : 'dbmclose';
OpKeywordDbmopen : 'dbmopen';
OpKeywordDefined : 'defined';
OpKeywordDelete : 'delete';
OpKeywordDie : 'die';
OpKeywordDo : 'do';
OpKeywordDump : 'dump';
OpKeywordEach : 'each';
OpKeywordEof : 'eof';
OpKeywordEval : 'eval';
OpKeywordEvalbytes : 'evalbytes';
OpKeywordExec : 'exec';
OpKeywordExists : 'exists';
OpKeywordExit : 'exit';
OpKeywordExp : 'exp';
OpKeywordFc : 'fc';
OpKeywordFor : 'for';
OpKeywordForeach : 'foreach';
OpKeywordFcntl : 'fcntl';
OpKeywordFileno : 'fileno';
OpKeywordFlock : 'flock';
OpKeywordFork : 'fork';
OpKeywordGetc : 'getc';
OpKeywordGetlogin : 'getlogin';
OpKeywordGetpeername : 'getpeername';
OpKeywordGetpgrp : 'getpgrp';
OpKeywordGetppid : 'getppid';
OpKeywordGetpriority : 'getpriority';
OpKeywordGetpwnam : 'getpwnam';
OpKeywordGetgrnam : 'getgrnam';
OpKeywordGethostbyname : 'gethostbyname';
OpKeywordGetnetbyname : 'getnetbyname';
OpKeywordGetprotobyname : 'getprotobyname';
OpKeywordGetpwuid : 'getpwuid';
OpKeywordGetgrgid : 'getgrgid';
OpKeywordGetservbyname : 'getservbyname';
OpKeywordGethostbyaddr : 'gethostbyaddr';
OpKeywordGetnetbyaddr : 'getnetbyaddr';
OpKeywordGetprotobynumber : 'getprotobynumber';
OpKeywordGetservbyport : 'getservbyport';
OpKeywordGetpwent : 'getpwent';
OpKeywordGetgrent : 'getgrent';
OpKeywordGethostent : 'gethostent';
OpKeywordGetnetent : 'getnetent';
OpKeywordGetprotoent : 'getprotoent';
OpKeywordGetservent : 'getservent';
OpKeywordSetpwent : 'setpwent';
OpKeywordSetgrent : 'setgrent';
OpKeywordSethostent : 'sethostent';
OpKeywordSetnetent : 'setnetent';
OpKeywordSetprotoent : 'setprotoent';
OpKeywordSetservent : 'setservent';
OpKeywordEndpwent : 'endpwent';
OpKeywordEndgrent : 'endgrent';
OpKeywordEndhostent : 'endhostent';
OpKeywordEndnetent : 'endnetent';
OpKeywordEndprotoent : 'endprotoent';
OpKeywordEndservent : 'endservent';
OpKeywordGetsockname : 'getsockname';
OpKeywordGetsockopt : 'getsockopt';
OpKeywordGlob : 'glob';
OpKeywordGmtime : 'gmtime';
OpKeywordGoto : 'goto';
OpKeywordGrep : 'grep';
OpKeywordHex : 'hex';
OpKeywordIndex : 'index';
OpKeywordInt : 'int';
OpKeywordIoctl : 'ioctl';
OpKeywordJoin : 'join';
OpKeywordKeys : 'keys';
OpKeywordKill : 'kill';
OpKeywordLast : 'last';
OpKeywordLc : 'lc';
OpKeywordLcfirst : 'lcfirst';
OpKeywordLength : 'length';
OpKeywordLink : 'link';
OpKeywordListen : 'listen';
OpKeywordLocal : 'local';
OpKeywordLocaltime : 'localtime';
OpKeywordLock : 'lock';
OpKeywordLog : 'log';
OpKeywordLstat : 'lstat';
OpKeywordM : 'm';
OpKeywordMap : 'map';
OpKeywordMkdir : 'mkdir';
OpKeywordMsgctl : 'msgctl';
OpKeywordMsgget : 'msgget';
OpKeywordMsgrcv : 'msgrcv';
OpKeywordMsgsnd : 'msgsnd';
OpKeywordMy : 'my';
OpKeywordNext : 'next';
OpKeywordNo : 'no';
OpKeywordOct : 'oct';
OpKeywordOpen : 'open';
OpKeywordOpendir : 'opendir';
OpKeywordOrd : 'ord';
OpKeywordOur : 'our';
OpKeywordPack : 'pack';
OpKeywordPackage : 'package';
OpKeywordPipe : 'pipe';
OpKeywordPop : 'pop';
OpKeywordPos : 'pos';
OpKeywordPrint : 'print';
OpKeywordPrintf : 'printf';
OpKeywordPrototype : 'prototype';
OpKeywordPush : 'push';
OpKeywordQ : 'q';
OpKeywordQq : 'qq';
OpKeywordQx : 'qx';
OpKeywordQw : 'qw';
OpKeywordQr : 'qr';
OpKeywordQuotemeta : 'quotemeta';
OpKeywordRand : 'rand';
OpKeywordRead : 'read';
OpKeywordReaddir : 'readdir';
OpKeywordReadline : 'readline';
OpKeywordReadlink : 'readlink';
OpKeywordReadpipe : 'readpipe';
OpKeywordRecv : 'recv';
OpKeywordRedo : 'redo';
OpKeywordRef : 'ref';
OpKeywordRename : 'rename';
OpKeywordRequire : 'require';
OpKeywordReset : 'reset';
OpKeywordReturn : 'return';
OpKeywordReverse : 'reverse';
OpKeywordRewinddir : 'rewinddir';
OpKeywordRindex : 'rindex';
OpKeywordRmdir : 'rmdir';
OpKeywordS : 's';
OpKeywordSay : 'say';
OpKeywordScalar : 'scalar';
OpKeywordSeek : 'seek';
OpKeywordSeekdir : 'seekdir';
OpKeywordSelect : 'select';
OpKeywordSemctl : 'semctl';
OpKeywordSemget : 'semget';
OpKeywordSemop : 'semop';
OpKeywordSend : 'send';
OpKeywordSetpgrp : 'setpgrp';
OpKeywordSetpriority : 'setpriority';
OpKeywordSetsockopt : 'setsockopt';
OpKeywordShift : 'shift';
OpKeywordShmctl : 'shmctl';
OpKeywordShmget : 'shmget';
OpKeywordShmread : 'shmread';
OpKeywordShmwrite : 'shmwrite';
OpKeywordShutdown : 'shutdown';
OpKeywordSin : 'sin';
OpKeywordSleep : 'sleep';
OpKeywordSocket : 'socket';
OpKeywordSocketpair : 'socketpair';
OpKeywordSort : 'sort';
OpKeywordSplice : 'splice';
OpKeywordSplit : 'split';
OpKeywordSprintf : 'sprintf';
OpKeywordSqrt : 'sqrt';
OpKeywordSrand : 'srand';
OpKeywordStat : 'stat';
OpKeywordState : 'state';
OpKeywordStudy : 'study';
OpKeywordSub : 'sub';
OpKeywordSubstr : 'substr';
OpKeywordSymlink : 'symlink';
OpKeywordSyscall : 'syscall';
OpKeywordSysopen : 'sysopen';
OpKeywordSysread : 'sysread';
OpKeywordSysseek : 'sysseek';
OpKeywordSystem : 'system';
OpKeywordSyswrite : 'syswrite';
OpKeywordTr : 'tr';
OpKeywordTell : 'tell';
OpKeywordTelldir : 'telldir';
OpKeywordTie : 'tie';
OpKeywordTied : 'tied';
OpKeywordTime : 'time';
OpKeywordTimes : 'times';
OpKeywordTruncate : 'truncate';
OpKeywordUc : 'uc';
OpKeywordUcfirst : 'ucfirst';
OpKeywordUmask : 'umask';
OpKeywordUndef : 'undef';
OpKeywordUnlink : 'unlink';
OpKeywordUnpack : 'unpack';
OpKeywordUnshift : 'unshift';
OpKeywordUntie : 'untie';
OpKeywordUse : 'use';
OpKeywordUtime : 'utime';
OpKeywordValues : 'values';
OpKeywordVec : 'vec';
OpKeywordWait : 'wait';
OpKeywordWaitpid : 'waitpid';
OpKeywordWantarray : 'wantarray';
OpKeywordWarn : 'warn';
OpKeywordWrite : 'write';
OpKeywordY : 'y';

OpFileReadableEffective : '-r';
OpFileWritableEffective : '-w';
OpFileRExecutableEffective : '-x';
OpFileOwnedEffective : '-o';
OpFileReadableReal : '-R';
OpFileWritableReal : '-W';
OpFileRExecutableReal : '-X';
OpFileOwnedReal : '-O';
OpFileExists : '-e';
OpFileEmpty : '-z';
OpFileNonEmpty : '-s';
OpFilePlain : '-f';
OpFileDirectory : '-d';
OpFileSymbolic : '-l';
OpFileNamedPipe : '-p';
OpFileSocket : '-S';
OpFileBlock : '-b';
OpFileCharacter : '-c';
OpFileOpenedTty : '-t';
OpFileSetuid : '-u';
OpFileSetgid : '-g';
OpFileSticky : '-k';
OpFileAsciiUtf8 : '-T';
OpFileBinary : '-B';
OpFileStartTime : '-M';
OpFileAccessTime : '-A';
OpFileChangeTime : '-C';

ConditionIf : 'if';
ConditionElsif : 'elsif';
ConditionElse : 'else';
ConditionUnless : 'unless';
ConditionWhile : 'while';
ConditionUntil : 'until';
ConditionFor : 'for';
ConditionForeach : 'foreach';

BuiltinFilehandle : 'STDIN' | 'STDOUT' | 'STDERR' | 'ARGV' | 'ARGVOUT' | 'DATA';

WS
   : [ \t\n\r] + -> skip
   ;