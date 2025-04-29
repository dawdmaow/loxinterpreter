import
  std/[
    parseutils, strutils, sugar, tables, strformat, math, sequtils, times, hashes, json,
    os,
  ]

# proc emscriptenForceExit() =
#   {.emit: "emscripten_force_exit(1);".}

# TODO: Doesnt work because calling procs (rather than the entire program) doesnt work well with nim's exception handling.
# TODO: Consider communicating through a buffer reserver for message passing between client and server?
# onUnhandledException = proc(msg: string) =
#   # echo "HELLO???"
#   # stderr.write msg
#   # quit 1
#   echo "A"
#   emscriptenForceExit()
#   echo "B"

# TODO: The formatter clearly doesn't want me putting lots of code into case branches. Let's split them up inro procs.
# TODO: resolver errors should be raised, and there should be only one error to show to the user.
# TODO: static methods
# TODO: detect return statement outside of proc (according ot guide!)
# TODO: "init is declared but never used" bogus error because of it being an implicit constructor.
# TODO: Uninitialized var read -> errror
# TODO: fun instead of proc causes cryptic errors

template header*(s: string) =
  echo "### " & s & " ###"

const debug = true

type
  Break* = ref object of CatchableError
  Continue* = ref object of CatchableError
  Return* = ref object of CatchableError
    value*: Value

  TokenizerError* = ref object of CatchableError
    token*: Token

  ParseError* = ref object of CatchableError
    index*: int

  ResolverError* = ref object of CatchableError

  EvalError* = ref object of CatchableError

  LiteralKind* = enum
    litInt = "integer"
    litFloat = "float"
    litString = "string"
    litBool = "boolean"
    litChar = "character"
    litNil = "nil"

  Literal* = ref object
    case kind*: LiteralKind
    of litInt:
      intVal*: int
    of litFloat:
      floatVal*: float
    of litString:
      stringVal*: string
    of litBool:
      boolVal*: bool
    of litChar:
      charVal*: char
    of litNil:
      discard

  ResolverClass* = enum
    resClassNone
    resClass
    resSubclass

  ResolverFunction* = enum
    resFuncNone
    resFunc
    resMethod
    resInitializer

  Resolver* = ref object
    class*: ResolverClass
    function*: ResolverFunction
    warnings*: seq[string]
    vm*: VM
    scopes*: seq[Scope] # TODO: This is only used during the resolve phase.

  Keyword* = enum
    kwSuper = "super"
    kwInvalid = "<invalid keyword>"
      # TODO: remove, along with all the other crappy "invalid" enums.
    kwThis = "this"
    kwIf = "if"
    kwElse = "else"
    kwWhile = "while"
    kwNil = "nil"
    kwVar = "var"
    kwFor = "for"
    kwClass = "class"
    kwProc = "proc"
    kwPrint = "print"
    kwReturn = "return"
    kwContinue = "continue"
    kwBreak = "break"

  TokenKind* = enum
    tokDot = "."
    tokInvalid = "<invalid token>"
    tokLiteral = "literal"
    tokKeyword = "keyword"
    tokIdentifier = "identifier"
    tokMinus = "-"
    tokBinary = "binary operator"
    tokUnary = "unary operator"
    tokParenLeft = "("
    tokParenRight = ")"
    tokBracketLeft = "["
    tokBracketRight = "]"
    tokBraceLeft = "{"
    tokBraceRight = "}"
    tokComma = ","
    tokSemicolon = ";"
    tokEOF = "eof" # TODO: consider removing this
    tokAssign = "=" # tokNewline = "\\n"

  BinaryOp* = enum
    binOpInvalid = "invalid"
    binOpPlus = "+"
    binOpMinus = "-"
    binOpMultiply = "*"
    # binOpIntegerDivide = "div"
    binOpDivide = "/"
    binOpModulo = "mod"
    binOpPower = "**"
    binOpAnd = "and"
    binOpOr = "or"
    binOpEqual = "=="
    binOpNotEqual = "!="
    binOpLessThan = "<"
    binOpLessThanOrEqual = "<="
    binOpGreaterThan = ">"
    binOpGreaterThanOrEqual = ">="
    binOpConcat = "&"

  UnaryOp* = enum
    unOpInvalid = "invalid"
    unOpMinus = "-"
    unOpNot = "not"

  Token* = object
    index*: int
    case kind*: TokenKind
    of tokLiteral:
      literal*: Literal
    of tokKeyword:
      keyword*: Keyword
    of tokIdentifier:
      identifier*: string
    of tokBinary:
      binaryOperator*: BinaryOp
    of tokUnary:
      unaryOperator*: UnaryOp
    of tokParenLeft, tokParenRight, tokBracketLeft, tokBracketRight, tokComma,
        tokSemicolon, tokBraceLeft, tokBraceRight, tokAssign, tokEOF, tokInvalid,
        tokMinus, tokDot:
      discard

  AstKind* = enum
    astSuper
    astThis
    astDot
    astDotAsgn
    astLambda
    astClass
    astReturn
    astProcDef
    astCall
    astBlock
    astInvalid
    astStatementList
    astIdentifier
    astBinaryOperator
    astUnaryOperator
    astLiteral
    astIf
    # astFor = "for"
    astWhile
    astParens
    astPrint
    astVarDef
    astAsgn
    astBreak
    astContinue

  Ast* = ref object
    index* = -1 # TODO: Just store line and column here, thx.
    case kind*: AstKind
    of astThis:
      discard
    of astSuper:
      superMethodName*: Ast # TODO: Do we need to store idents instead of strings?
    of astDotAsgn:
      dotAsgnCallee*: Ast
      dotAsgnMember*: Ast
      dotAsgnValue*: Ast
    of astDot:
      dotCallee*: Ast
      dotMember*: Ast
    of astClass:
      className*: Ast
      classParentName*: Ast
      classMethods*: seq[Ast] # Should reuse astProcDef here.
    of astReturn:
      returnValue*: Ast
    of astLambda:
      lambdaParams*: seq[string]
      lambdaBody*: Ast
    of astProcDef:
      procName*: string
      procParams*: seq[string]
      procBody*: Ast
    of astCall:
      callParen*: Token
      callCallee*: Ast
      callArgs*: seq[Ast]
    of astBreak:
      discard
    of astContinue:
      discard
    of astBlock:
      blockStmts*: seq[Ast]
    of astAsgn:
      asgnLeft*: Ast
      asgnRight*: Ast
    of astPrint:
      printArg*: Ast
    of astStatementList:
      statements*: seq[Ast]
    of astIdentifier:
      identifier*: string
    of astBinaryOperator:
      binaryOperator*: BinaryOp
      binaryLeft*: Ast
      binaryRight*: Ast
    of astUnaryOperator:
      unaryOperator*: UnaryOp
      unaryOperand*: Ast
    of astLiteral:
      literal*: Literal
    # of astFor:
    #   forInit*: Ast
    #   forCondition*: Ast
    #   forUpdate*: Ast
    #   forBody*: Ast
    of astIf:
      ifCondition*: Ast
      ifElse*: Ast
      ifBody*: Ast
    of astWhile:
      whileCondition*: Ast
      whileBody*: Ast
    of astInvalid:
      discard
    of astParens:
      parensExpr*: Ast
    of astVarDef:
      varDefName*: string # TODO: Make this into AST (ident)
      varDefValue*: Ast

  Parser* = ref object
    errors*: seq[string] # TODO: This should be a var proc argument thx.
    tokens*: seq[Token]
    i*: int # errors*: seq[ParseError]

  ResolvedSym* = enum
    symInvalid
    symDeclared
    symDefined

  ValueKind* = enum
    # valInt = "integer"
    valFloat = "float"
    valString = "string"
    valBool = "boolean" # valNil = "nil"
    valProc = "proc"
    valClass = "class"
    valObj = "object"

  ProcKind* = enum
    procNative
    procForeign
    procMethod

  Value* = ref object
    case kind*: ValueKind
    of valObj:
      objClass*: Value
      objFields*: Table[string, Value]
    of valClass:
      className*: string
      classMethods*: Table[string, Value]
      classParent*: Value
    of valProc:
      procIsInitializer*: bool
      procClosure*: Env
      procName*: string
      procParams*: seq[string]
      case procKind*: ProcKind
      of procMethod:
        methodBody*: Ast
        methodObj*: Value
      of procNative:
        procBody*: Ast
      of procForeign:
        foreignProc*: proc(args: openarray[Value]): Value
    # of valInt:
    #   int*: int
    of valFloat:
      float*: float
    of valString:
      string*: string
    of valBool:
      bool*: bool
    # of valNil:
    #   discard

  EnvKind* = enum
    envProc
    envBlock

  ValueIndices* = object
    envIdx*: int
    valueIdx*: int

  Env* = ref object
    prev*: Env
    kind*: EnvKind
    values*: seq[Value] # Use a sequence instead of a table for indexed access
    names*: Table[string, int] # Map variable names to their indices

  Scope* = ref object
    symbols*: Table[string, ResolvedSym]
    used*: Table[string, bool] # Track if each variable has been used
    indices*: Table[string, int] # Track the index of each variable in this scope
    nextIndex*: int # Next available index in this scope

  VM* = ref object
    currentEnv*, globalEnv*: Env
    locals*: Table[Ast, ValueIndices]

proc `$`*(lit: Literal): string =
  case lit.kind
  of litInt:
    result = $lit.intVal
  of litFloat:
    result = $lit.floatVal
  of litString:
    result = lit.stringVal.repr
  of litBool:
    result = $lit.boolVal
  of litChar:
    result = '\'' & lit.charVal & '\''
  of litNil:
    result = "nil"

proc `$`*(token: Token): string =
  case token.kind
  of tokDot:
    result = "dot"
  of tokMinus:
    result = "minus"
  of tokLiteral:
    result = "literal(" & $token.literal.kind & ", " & $token.literal & ")"
  of tokKeyword:
    result = "keyword(" & $token.keyword & ")"
  of tokIdentifier:
    result = "ident(" & token.identifier & ")"
  of tokBinary:
    result = "binary(" & $token.binaryOperator & ")"
  of tokUnary:
    result = "unary(" & $token.unaryOperator & ")"
  of tokParenLeft, tokParenRight, tokBracketLeft, tokBracketRight, tokBraceLeft,
      tokBraceRight, tokComma, tokSemicolon, tokEOF, tokAssign, tokInvalid:
    result = "token('" & $token.kind & "')"

proc `$`*(tokens: seq[Token]): string =
  result = "["
  for i, token in tokens:
    result.add $token
    if i != tokens.high:
      result.add ", "
  result.add "]"

proc eof*(p: Parser): bool =
  assert p.i in p.tokens.low .. p.tokens.high, $p.tokens
  result = p.tokens[p.i].kind == tokEOF

proc prev*(parser: Parser): Token =
  if (parser.i - 1) in parser.tokens.low .. parser.tokens.high:
    parser.tokens[parser.i - 1]
  else:
    Token(kind: tokInvalid)

proc current*(parser: Parser): Token =
  if not parser.eof:
    parser.tokens[parser.i]
  else:
    Token(kind: tokInvalid)

proc next*(parser: Parser): Token =
  if (parser.i + 1) in parser.tokens.low .. parser.tokens.high:
    parser.tokens[parser.i + 1]
  else:
    Token(kind: tokInvalid)

# proc setLine*(p: Parser, ast: Ast) =
#   if not p.eof:
#     ast.line = 1
#     var lastNewlineIndex = -1
#     for i in 0 ..< p.tokens.high:
#       if p.tokens[i].kind == tokNewline:
#         lastNewlineIndex = i
#         ast.line += 1

template consumeToken*(p: Parser, k: TokenKind, msg: string) =
  if p.current.kind != k:
    p.parseError p.current.index, msg
  inc p.i

template consumeIdent*(p: Parser, msg: string): Ast =
  block:
    if p.current.kind != tokIdentifier:
      p.parseError p.current.index, msg
    let result =
      Ast(kind: astIdentifier, index: p.current.index, identifier: p.current.identifier)
    inc p.i
    result

# # TODO: Get a rid of this. I should just store the token (which already has line information) and simplify the code.
# proc newAst*(p: Parser, kind: AstKind): Ast =
#   result = Ast(kind: kind)
#   p.setLine result

proc toLisp*(ast: Ast): string =
  proc visit(ast: Ast, result: var string) =
    if ast.isNil:
      result.add "(nil)"
      return
    case ast.kind
    of astSuper:
      result.add "(super "
      result.add ast.superMethodName.identifier
      result.add ")"
    of astThis:
      result.add "(this)"
    of astDotAsgn:
      result.add "(dot-asgn "
      result.add ast.dotAsgnCallee.toLisp
      result.add " "
      result.add ast.dotAsgnMember.identifier
      result.add " "
      result.add ast.dotAsgnValue.toLisp
      result.add ")"
    of astDot:
      result.add "(dot "
      result.add ast.dotCallee.toLisp
      result.add " "
      result.add ast.dotMember.identifier
      result.add ")"
    of astClass:
      result.add "(class "
      result.add ast.className.identifier
      result.add " "
      result.add ast.classParentName.toLisp
      result.add " "
      result.add "(methods"
      if ast.classMethods.len > 0:
        result.add " "
      for i, procDef in ast.classMethods:
        result.add procDef.toLisp
        if i != ast.classMethods.high:
          result.add " "
      result.add ")"
      result.add ")"
    of astLambda:
      result.add "(lambda"
      if ast.lambdaParams.len > 0:
        for i, param in ast.lambdaParams:
          result.add param
          if i != ast.lambdaParams.high:
            result.add " "
      result.add " "
      visit ast.lambdaBody, result
      result.add ")"
    of astReturn:
      result.add "(return"
      if ast.returnValue != nil:
        result.add " "
        visit ast.returnValue, result
      result.add ")"
    of astProcDef:
      result.add "(proc "
      result.add ast.procName
      result.add " "
      result.add "("
      for i, param in ast.procParams:
        result.add param
        if i != ast.procParams.high:
          result.add " "
      result.add ") "
      visit ast.procBody, result
      result.add ")"
    of astCall:
      result.add "(call "
      result.add ast.callCallee.toLisp
      if ast.callArgs.len > 0:
        result.add " "
        for i, arg in ast.callArgs:
          visit arg, result
          if i != ast.callArgs.high:
            result.add " "
      result.add ")"
    of astBreak:
      result.add "(break)"
    of astContinue:
      result.add "(continue)"
    of astBlock:
      result.add "(block "
      for i, statement in ast.blockStmts:
        visit statement, result
        if i != ast.blockStmts.high:
          result.add " "
      result.add ")"
    # of astFor:
    #   result.add "(for "
    #   visit ast.forInit, result
    #   result.add " "
    #   visit ast.forCondition, result
    #   result.add " "
    #   visit ast.forUpdate, result
    #   result.add " "
    #   visit ast.forBody, result
    #   result.add ")"
    of astAsgn:
      result.add "(asgn "
      visit ast.asgnLeft, result
      result.add " "
      visit ast.asgnRight, result
      result.add ")"
    of astVarDef:
      result.add "(var "
      result.add ast.varDefName
      result.add " "
      visit ast.varDefValue, result
      result.add ")"
    of astPrint:
      result.add "(print "
      visit ast.printArg, result
      result.add ")"
    of astStatementList:
      result.add "(stmts "
      for i, statement in ast.statements:
        visit statement, result
        if i != ast.statements.high:
          result.add " "
      result.add ")"
    of astIdentifier:
      result.add "(ident " & ast.identifier & ")"
    of astBinaryOperator:
      result.add "(" & $ast.binaryOperator & " "
      visit ast.binaryLeft, result
      result.add " "
      visit ast.binaryRight, result
      result.add ")"
    of astUnaryOperator:
      result.add "(" & $ast.unaryOperator & " "
      visit ast.unaryOperand, result
      result.add ")"
    of astLiteral:
      result.add $ast.literal
    of astIf:
      result.add "(if "
      visit ast.ifCondition, result
      result.add " "
      visit ast.ifBody, result
      if ast.ifElse != nil:
        result.add " (else "
        visit ast.ifElse, result
        result.add ")"
      result.add ")"
    of astWhile:
      result.add "(while "
      visit ast.whileCondition, result
      result.add " "
      visit ast.whileBody, result
      result.add ")"
    of astParens:
      result.add "(expr "
      visit ast.parensExpr, result
      result.add ")"
    of astInvalid:
      assert false

  visit ast, result

proc tokenize*(code: string): seq[Token] =
  template indexValid(): bool =
    i in code.low .. code.high

  template next(): char =
    if not i + 1 in code.low .. code.high:
      raise
        TokenizerError(token: Token(kind: tokInvalid, index: i), msg: "Unexpected EOF")
    code[i + 1]

  proc toNumberToken(s: string): Token =
    if '.' in s:
      result = Token(
        kind: tokLiteral, literal: Literal(kind: litFloat, floatVal: parseFloat(s))
      )
    else:
      result =
        Token(kind: tokLiteral, literal: Literal(kind: litInt, intVal: parseInt(s)))

  let keywords = collect:
    for it in Keyword:
      {$it: it}

  let binaryOperators = collect:
    for it in BinaryOp:
      {$it: it}

  let unaryOperators = collect:
    for it in UnaryOp:
      {$it: it}

  var identifierLikeChars: set[char]
  for it in binaryOperators.keys:
    for character in it:
      identifierLikeChars.incl character
  for it in unaryOperators.keys:
    for character in it:
      identifierLikeChars.incl character

  var i = 0
  var line = 0 # TODO: Save this with the tokens.

  while i <= code.high:
    let tokenIndex = i
    case code[i]
    of Whitespace - Newlines:
      inc i
    of '.':
      if next.isDigit:
        inc i
        var str = ""
        i += code.parseWhile(str, Digits, i)
        result.add toNumberToken(str)
      else:
        result.add Token(index: tokenIndex, kind: tokDot)
        inc i
        # raise TokenizerError(
        #   token: Token(kind: tokInvalid, index: tokenIndex),
        #   msg: "Invalid token: " & $code[i],
        # )
    of Digits:
      var str = ""
      var dotParsed = false
      while indexValid and (code[i].isDigit or code[i] == '.' and not dotParsed):
        if code[i] == '.':
          if dotParsed:
            break
          dotParsed = true
        str.add code[i]
        inc i
      result.add toNumberToken(str)
    of Letters:
      var str = ""
      i += code.parseWhile(str, Letters + Digits + {'_'}, i)
      # while indexValid and code[i] in Letters + Digits + {'_'}:
      #   str.add code[i]
      #   inc i
      if str in keywords:
        result.add Token(index: tokenIndex, kind: tokKeyword, keyword: keywords[str])
      elif str == "true":
        result.add Token(
          index: tokenIndex,
          kind: tokLiteral,
          literal: Literal(kind: litBool, boolVal: true),
        )
      elif str == "false":
        result.add Token(
          index: tokenIndex,
          kind: tokLiteral,
          literal: Literal(kind: litBool, boolVal: false),
        )
      elif str in binaryOperators:
        result.add Token(
          index: tokenIndex, kind: tokBinary, binaryOperator: binaryOperators[str]
        )
      elif str in unaryOperators:
        result.add Token(
          index: tokenIndex, kind: tokUnary, unaryOperator: unaryOperators[str]
        )
      else:
        result.add Token(index: tokenIndex, kind: tokIdentifier, identifier: str)
    of Newlines:
      inc line
      inc i
    of '-':
      inc i
      result.add Token(index: tokenIndex, kind: tokMinus)
    of '"':
      inc i
      var str = ""
      while indexValid and code[i] != '"':
        str.add code[i]
        inc i
      if not indexValid:
        raise TokenizerError(
          token: Token(kind: tokInvalid, index: tokenIndex),
          msg: "Unterminated string literal",
        )
      inc i
      result.add Token(
        index: tokenIndex,
        kind: tokLiteral,
        literal: Literal(kind: litString, stringVal: str),
      )
    of '(':
      result.add Token(index: tokenIndex, kind: tokParenLeft)
      inc i
    of ')':
      result.add Token(index: tokenIndex, kind: tokParenRight)
      inc i
    of '[':
      result.add Token(index: tokenIndex, kind: tokBracketLeft)
      inc i
    of ']':
      result.add Token(index: tokenIndex, kind: tokBracketRight)
      inc i
    of '{':
      result.add Token(index: tokenIndex, kind: tokBraceLeft)
      inc i
    of '}':
      result.add Token(index: tokenIndex, kind: tokBraceRight)
      inc i
    of ',':
      result.add Token(index: tokenIndex, kind: tokComma)
      inc i
    of ';':
      result.add Token(index: tokenIndex, kind: tokSemicolon)
      inc i
    of '!':
      if code[i + 1] == '=':
        result.add Token(
          index: tokenIndex, kind: tokBinary, binaryOperator: binOpNotEqual
        )
        inc i
      else:
        raise
          TokenizerError(token: Token(kind: tokInvalid, index: i), msg: "Invalid token")
    of '=':
      inc i
      assert i <= code.high
      if code[i] == '=':
        inc i
        result.add Token(index: tokenIndex, kind: tokBinary, binaryOperator: binOpEqual)
      else:
        result.add Token(index: tokenIndex, kind: tokAssign)
    else:
      if code[i] in identifierLikeChars:
        var str = ""
        i += code.parseWhile(str, identifierLikeChars, i)
        if str.startsWith "//":
          i += code.skipUntil(Newlines, i)
        elif str in binaryOperators:
          result.add Token(
            index: tokenIndex, kind: tokBinary, binaryOperator: binaryOperators[str]
          )
        elif str in unaryOperators:
          result.add Token(
            index: tokenIndex, kind: tokUnary, unaryOperator: unaryOperators[str]
          )
        else:
          raise TokenizerError(
            token: Token(kind: tokInvalid, index: tokenIndex),
            msg: "Invalid token: " & code[i].repr,
          )
      else:
        raise TokenizerError(
          token: Token(kind: tokInvalid, index: tokenIndex),
          msg: "Invalid token: " & code[i].repr,
        )
  assert i == code.high + 1
  result.add Token(index: i, kind: tokEOF)

const statementKeywords = {
  kwClass, kwProc, kwVar, kwFor, kwFor, kwIf, kwWhile, kwPrint, kwReturn, kwBreak,
  kwContinue,
}

proc synchronize*(p: Parser) =
  if not p.eof:
    inc p.i
    while not p.eof and p.prev.kind != tokSemicolon and
        not (p.current.kind == tokKeyword and p.current.keyword in statementKeywords):
      inc p.i

proc `$`*(pe: ParseError): string =
  result = "Parse error at index " & $pe.index & ": " & pe.msg

proc expressionRule*(p: Parser): Ast

# template debug*(s) =
#   block:
#     let inst = instantiationInfo()
#     styledEcho fgBlue, "[", inst.filename, ":", $inst.line, "] "
#   dump s

proc parseError*(p: Parser, index: int, msg: string) =
  raise ParseError(msg: msg, index: index)

proc lit*(x: int): Literal =
  Literal(kind: litInt, intVal: x)

proc lit*(x: float): Literal =
  Literal(kind: litFloat, floatVal: x)

proc lit*(x: string): Literal =
  Literal(kind: litString, stringVal: x)

proc lit*(x: bool): Literal =
  Literal(kind: litBool, boolVal: x)

const maxCallArgs = 255

proc blockRule*(p: Parser): Ast

proc lambdaRule*(p: Parser): Ast =
  assert p.current.kind == tokKeyword and p.current.keyword == kwProc
  inc p.i
  result = Ast(kind: astLambda, index: p.current.index)
  p.consumeToken tokParenLeft, "Expected '(' after proc keyword"
  while p.current.kind != tokParenRight:
    result.lambdaParams.add p.consumeIdent("Expected a parameter").identifier
    if result.lambdaParams.len > maxCallArgs:
      p.errors.add $ParseError(
        index: p.current.index, msg: "Procedure has too many parameters"
      )
    if p.current.kind == tokComma:
      inc p.i
    else:
      break
  p.consumeToken tokParenRight, "Expected ')' after function parameters"
  if p.current.kind != tokBraceLeft:
    p.parseError p.current.index, "Expected '{' after procedure parameters"
  result.lambdaBody = p.blockRule()

proc procDefRule*(p: Parser): Ast =
  assert p.current.kind == tokKeyword and p.current.keyword == kwProc
  inc p.i
  result = Ast(kind: astProcDef, index: p.current.index)
  result.procName = p.consumeIdent("Expected procedure name").identifier
  p.consumeToken tokParenLeft, "Expected '(' after procedure name"
  while p.current.kind != tokParenRight:
    result.procParams.add p.consumeIdent("Expected a parameter").identifier
    if result.procParams.len > maxCallArgs:
      p.errors.add $ParseError(
        index: p.current.index, msg: "Procedure has too many parameters"
      )
    if p.current.kind == tokComma:
      inc p.i
    else:
      break
  p.consumeToken tokParenRight, "Expected ')' after function parameters"
  if p.current.kind != tokBraceLeft:
    p.parseError p.current.index, "Expected '{' after procedure parameters"
  result.procBody = p.blockRule()
  # p.consumeToken tokBraceRight, "Expected '}' after procedure body" # TODO: Done by blockRule(), which is weird.

proc primaryRule*(p: Parser): Ast =
  # dumpCurrentRule "PRIMARY RULE"
  if not p.eof:
    if p.current.kind == tokLiteral:
      result = Ast(kind: astLiteral, index: p.current.index, literal: p.current.literal)
      inc p.i
    elif p.current.kind == tokIdentifier:
      result = Ast(
        kind: astIdentifier, index: p.current.index, identifier: p.current.identifier
      )
      inc p.i
    elif p.current.kind == tokParenLeft:
      inc p.i
      result = p.expressionRule()
      if p.current.kind != tokParenRight:
        p.parseError p.current.index, "Expected ')' after expression"
      inc p.i
    elif p.current.kind == tokBinary:
      # This is an error: binary operator has no left-hand side.
      # For the sake of reporting errors from right-hand side, we are going to parse it and then discard it.
      p.errors.add $ParseError(
        index: p.current.index, msg: "Binary operator has no left-hand side"
      )
      inc p.i
      discard p.expressionRule()
    elif p.current.kind == tokKeyword and p.current.keyword == kwProc:
      result = p.lambdaRule()
    elif p.current.kind == tokKeyword and p.current.keyword == kwThis:
      result = Ast(kind: astThis, index: p.current.index)
      inc p.i
    elif p.current.kind == tokKeyword and p.current.keyword == kwSuper:
      inc p.i
      p.consumeToken(tokDot, "Expected '.' after super keyword")
      let meth = p.consumeIdent("Expected a method name")
      result = Ast(kind: astSuper, index: p.current.index, superMethodName: meth)
  else:
    p.parseError p.current.index, "Expected expression"

proc callRule*(p: Parser): Ast =
  result = p.primaryRule()
  while not p.eof:
    if p.current.kind == tokParenLeft:
      result = Ast(kind: astCall, index: p.current.index, callCallee: result)
      inc p.i
      while p.current.kind != tokParenRight:
        let arg = p.expressionRule()
        result.callArgs.add arg
        if p.current.kind == tokComma:
          inc p.i
        else:
          break
      # if p.current.kind != tokParenRight:
      #   p.parseError p.current.index, "Expected ')' after function call"
      p.consumeToken tokParenRight, "Expected ')' after function call"
      if result.callArgs.len > maxCallArgs:
        p.errors.add $ParseError(
          index: p.current.index, msg: "Function call has too many arguments"
        )
    elif p.current.kind == tokDot:
      inc p.i
      let member = p.consumeIdent("Expected a method name")
      result =
        Ast(kind: astDot, index: p.current.index, dotCallee: result, dotMember: member)
    else:
      break

proc unaryRule*(p: Parser): Ast =
  # dumpCurrentRule "UNARY RULE"
  if not p.eof:
    if p.current.kind == tokMinus:
      let op = unOpMinus
      inc p.i
      let right = p.unaryRule()
      result = Ast(
        kind: astUnaryOperator,
        index: p.current.index,
        unaryOperator: op,
        unaryOperand: right,
      )
    else:
      result = p.callRule()

proc factorRule*(p: Parser): Ast =
  # dumpCurrentRule "FACTOR RULE"
  result = p.unaryRule()
  while not p.eof and p.current.kind == tokBinary and
      p.current.binaryOperator in {binOpPower, binOpDivide, binOpModulo, binOpMultiply}:
    let op = p.current.binaryOperator
    inc p.i
    let right = p.factorRule()
    result = Ast(
      kind: astBinaryOperator,
      index: p.current.index,
      binaryOperator: op,
      binaryLeft: result,
      binaryRight: right,
    )

proc termRule*(p: Parser): Ast =
  # dumpCurrentRule "TERM RULE"
  # TODO: The minus thing is a joke, make all operators inlined into the tokenkind type.
  result = p.factorRule()
  while not p.eof and (
    p.current.kind == tokMinus or (
      p.current.kind == tokBinary and
      p.current.binaryOperator in {binOpPlus, binOpConcat}
    )
  )
  :
    let op = if p.current.kind == tokMinus: binOpMinus else: p.current.binaryOperator
    inc p.i
    let right = p.factorRule()
    result = Ast(
      kind: astBinaryOperator,
      index: p.current.index,
      binaryOperator: op,
      binaryLeft: result,
      binaryRight: right,
    )

proc comparisonRule*(p: Parser): Ast =
  # dumpCurrentRule "COMPARISON RULE"
  result = p.termRule()
  while not p.eof and p.current.kind == tokBinary and
      p.current.binaryOperator in
      {binOpGreaterThan, binOpGreaterThanOrEqual, binOpLessThan, binOpLessThanOrEqual}
  :
    let op = p.current.binaryOperator
    inc p.i
    let right = p.termRule()
    result = Ast(
      kind: astBinaryOperator,
      index: p.current.index,
      binaryOperator: op,
      binaryLeft: result,
      binaryRight: right,
    )

proc equalityRule*(p: Parser): Ast =
  # dumpCurrentRule "EQUALITY RULE"
  result = p.comparisonRule()
  while not p.eof and p.current.kind == tokBinary and
      p.current.binaryOperator in {binOpEqual, binOpNotEqual}:
    let op = p.current.binaryOperator
    inc p.i
    let right = p.comparisonRule()
    result = Ast(
      kind: astBinaryOperator,
      index: p.current.index,
      binaryOperator: op,
      binaryLeft: result,
      binaryRight: right,
    )

proc andRule*(p: Parser): Ast =
  result = p.equalityRule()
  if result.isNil:
    return
  while not p.eof and p.current.kind == tokBinary and
      p.current.binaryOperator == binOpAnd:
    inc p.i
    let right = p.equalityRule()
    result = Ast(
      kind: astBinaryOperator,
      index: p.current.index,
      binaryOperator: binOpAnd,
      binaryLeft: result,
      binaryRight: right,
    )

proc orRule*(p: Parser): Ast =
  result = p.andRule()
  if result.isNil:
    return
  while not p.eof and p.current.kind == tokBinary and p.current.binaryOperator == binOpOr:
    inc p.i
    let right = p.andRule()
    result = Ast(
      kind: astBinaryOperator,
      index: p.current.index,
      binaryOperator: binOpOr,
      binaryLeft: result,
      binaryRight: right,
    )

proc assignmentRule*(p: Parser): Ast =
  # TODO: This is weird - in the guide, the check is "current == EQUAL, result is IDENT
  # TODO: Assignment should be a statement, not an expression.
  result = p.orRule()
  # dump result.toLisp
  # dump p.current
  # dump p.next
  if p.current.kind == tokAssign:
    case result.kind
    of astIdentifier, astThis:
      let lhs = result
      inc p.i
      let rhs = p.assignmentRule()
      result = Ast(kind: astAsgn, index: p.current.index, asgnLeft: lhs, asgnRight: rhs)
    of astDot:
      result = Ast(
        kind: astDotAsgn,
        index: p.current.index,
        dotAsgnCallee: result.dotCallee,
        dotAsgnMember: result.dotMember,
      )
      inc p.i
      let rhs = p.assignmentRule()
      result.dotAsgnValue = rhs
    else:
      p.parseError p.current.index,
        "Expected identifier or dot on left-hand side of assignment"
  # elif p.current.kind == tokDot:
  #   result = Ast(
  #     kind: astDotAsgn,
  #     index: p.current.index,
  #     dotAsgnCallee: result.dotCallee,
  #     dotAsgnMember: result.dotMember,
  #   )
  #   inc p.i
  #   let rhs = p.assignmentRule()
  #   result.dotAsgnValue = rhs

proc expressionRule*(p: Parser): Ast =
  # dumpCurrentRule "EXPRESSION RULE"
  result = p.assignmentRule()

proc expressionStatementRule*(p: Parser): Ast =
  # dumpCurrentRule "EXPRESSION STATEMENT RULE"
  # TODO: Remove this crappy rule.
  let value = p.expressionRule()
  p.consumeToken tokSemicolon, "Expected ';' after expression statement"
  result = value

proc declarationRule*(p: Parser): Ast

proc varDefRule*(p: Parser): Ast =
  if p.current.kind != tokKeyword or p.current.keyword != kwVar:
    p.parseError p.current.index, "Expected 'var' keyword"
  inc p.i
  result = Ast(
    kind: astVarDef,
    index: p.current.index,
    varDefName:
      p.consumeIdent("Expected identifier after declaration keyword").identifier,
  )
  if p.current.kind == tokAssign:
    inc p.i
    let value = p.expressionRule()
      # I don't know why this stops at a semicolon (hence there is no inc after this line )
    result.varDefValue = value
  p.consumeToken tokSemicolon, "Expected ';' after declaration"

proc blockRule*(p: Parser): Ast =
  assert p.current.kind == tokBraceLeft
  inc p.i
  result = Ast(kind: astBLock)
  while p.current.kind != tokBraceRight and not p.eof:
    let x = p.declarationRule()
    result.blockStmts.add x
  p.consumeToken tokBraceRight, "Expected '}' after declaration"

proc statementRule*(p: Parser): Ast =
  # dumpCurrentRule "STATEMENT RULE"
  if p.current.kind == tokKeyword and p.current.keyword in statementKeywords:
    case p.current.keyword
    of kwReturn:
      inc p.i
      result = Ast(kind: astReturn, index: p.current.index)
      if p.current.kind != tokSemicolon:
        result.returnValue = p.expressionRule()
      p.consumeToken tokSemicolon, "Expected ';' after return statement"
    of kwBreak:
      inc p.i
      result = Ast(kind: astBreak, index: p.current.index)
      p.consumeToken tokSemicolon, "Expected ';' after break statement"
    of kwContinue:
      inc p.i
      result = Ast(kind: astContinue, index: p.current.index)
      p.consumeToken tokSemicolon, "Expected ';' after continue statement"
    of kwPrint:
      inc p.i
      let value = p.expressionRule()
      if p.current.kind != tokSemicolon:
        p.parseError p.current.index, "Expected ';' after print statement"
      inc p.i
      result = Ast(kind: astPrint, index: p.current.index, printArg: value)
    of kwIf:
      inc p.i
      p.consumeToken tokParenLeft, "Expected '(' after if keyword"
      let condition = p.expressionRule()
      p.consumeToken tokParenRight, "Expected ')' after if condition"
      let body = p.statementRule()
      result =
        Ast(kind: astIf, index: p.current.index, ifCondition: condition, ifBody: body)
      if p.current.kind == tokKeyword and p.current.keyword == kwElse:
        inc p.i
        let elseBody = p.statementRule()
        result.ifElse = elseBody
    of kwWhile:
      inc p.i
      p.consumeToken tokParenLeft, "Expected '(' after while keyword"
      let condition = p.expressionRule()
      p.consumeToken tokParenRight, "Expected ')' after while condition"
      let body = p.statementRule()
      result = Ast(
        kind: astWhile,
        index: p.current.index,
        whileCondition: condition,
        whileBody: body,
      )
    of kwFor:
      inc p.i
      p.consumeToken tokParenLeft, "Expected '(' after for keyword"

      var initializer: Ast
      if p.current.kind == tokSemicolon:
        initializer = nil
      elif p.current.kind == tokKeyword and p.current.keyword == kwVar:
        initializer = p.varDefRule()
      else:
        initializer = p.expressionStatementRule()

      var condition: Ast
      if p.current.kind == tokSemicolon:
        condition = Ast(
          kind: astLiteral,
          index: p.current.index,
          literal: Literal(kind: litBool, boolVal: true),
        )
      else:
        condition = p.expressionRule()
        p.consumeToken tokSemicolon, "Expected ';' after for condition"

      var update: Ast
      if p.current.kind == tokSemicolon:
        update = nil
      else:
        update = p.expressionRule()

      p.consumeToken tokParenRight, "Expected ')' after for"

      result = p.statementRule()

      if not update.isNil:
        result =
          Ast(kind: astBlock, index: p.current.index, blockStmts: @[result, update])

      result = Ast(
        kind: astWhile,
        index: p.current.index,
        whileCondition: condition,
        whileBody: result,
      )

      if not initializer.isNil:
        result = Ast(
          kind: astBlock, index: p.current.index, blockStmts: @[initializer, result]
        )
    else:
      assert false, $p.current
  elif p.current.kind == tokBraceLeft:
    result = p.blockRule()
    # inc p.i
    # result = Ast(kind: astBLock)
    # while p.current.kind != tokBraceRight and not p.eof:
    #   let x = p.declarationRule()
    #   result.blockStmts.add x
    # p.consumeToken tokBraceRight, "Expected '}' after declaration"
  else:
    result = p.expressionStatementRule()

const declarationKeywords = {kwClass, kwProc, kwVar}

proc classDefRule*(p: Parser): Ast =
  assert p.current.kind == tokKeyword and p.current.keyword == kwClass
  inc p.i
  let className = p.consumeIdent("Expected class name")
  result = Ast(kind: astClass, index: p.current.index, className: className)
  if p.current.kind == tokBinary and p.current.binaryOperator == binOpLessThan:
    inc p.i
    result.classParentName = p.consumeIdent("Expected parent class name")
  p.consumeToken tokBraceLeft, "Expected '{' after class name"
  while not p.eof and p.current.kind != tokBraceRight:
    let methodName = p.consumeIdent("Expected method name")
    p.consumeToken tokParenLeft, "Expected '(' after method name"
    let procDef =
      Ast(kind: astProcDef, index: p.current.index, procName: methodName.identifier)
    while not p.eof and p.current.kind != tokParenRight:
      let paramName = p.consumeIdent("Expected parameter name")
      if p.current.kind != tokParenRight:
        p.consumeToken tokComma, "Expected ',' after parameter name"
      procDef.procParams.add paramName.identifier
    p.consumeToken tokParenRight, "Expected ')' after method parameters"
    let body = p.blockRule()
    # p.consumeToken tokBraceRight, "Expected '}' after method definition"
    procDef.procBody = body
    result.classMethods.add procDef
  inc p.i

proc declarationRule*(p: Parser): Ast =
  try:
    if p.current.kind == tokKeyword and p.current.keyword in declarationKeywords:
      case p.current.keyword
      of kwProc:
        result = p.procDefRule()
      of kwVar:
        result = p.varDefRule()
      of kwClass:
        result = p.classDefRule()
      else:
        assert false
    else:
      result = p.statementRule()
  except ParseError as e:
    p.errors.add $e
    p.synchronize()
    result = nil

proc declare*(r: Resolver, name: string) =
  if r.scopes != @[]:
    r.scopes[^1].symbols[name] = symDeclared
    r.scopes[^1].used[name] = false # Initialize as unused
    r.scopes[^1].indices[name] = r.scopes[^1].nextIndex
    inc r.scopes[^1].nextIndex

proc define*(r: Resolver, name: string) =
  if r.scopes != @[]:
    r.scopes[^1].symbols[name] = symDefined

proc markUsed*(r: Resolver, name: string) =
  if r.scopes != @[]:
    r.scopes[^1].used[name] = true

proc checkUnusedVariables*(r: Resolver) =
  discard
  # if r.scopes != @[]:
  #   for name, used in r.scopes[^1].used:
  #     if not used and r.scopes[^1].symbols[name] == symDefined:
  #       r.warnings.add &"Variable '{name}' is declared but never used"

proc resolveLocal*(r: Resolver, ast: Ast, name: string) =
  for i in countdown(r.scopes.high, r.scopes.low):
    if name in r.scopes[i].symbols:
      let envIdx = r.scopes.high - i
      let valueIdx = r.scopes[i].indices[name]
      r.vm.locals[ast] = ValueIndices(envIdx: envIdx, valueIdx: valueIdx)
      r.scopes[i].used[name] = true # Mark variable as used
      return

proc name*(ast: Ast): string =
  case ast.kind
  of astIdentifier:
    result = ast.identifier
  of astThis:
    result = "this"
  else:
    assert false, $ast.kind

proc error*(r: Resolver, msg: string) =
  raise ResolverError(msg: msg)

proc resolve*(r: Resolver, ast: Ast) =
  template withScope(body) =
    r.scopes.add Scope() # Initialize nextIndex to 0
    body
    r.checkUnusedVariables() # Check for unused variables before leaving scope
    r.scopes.del r.scopes.high

  template withFunction(f: ResolverFunction, body: untyped) =
    let prev = r.function
    r.function = f
    body
    r.function = prev

  proc resolveProc(r: Resolver, ast: Ast, f: ResolverFunction) =
    # TODO: We shouldn't have a separate type for lambdas.
    case ast.kind
    of astProcDef:
      r.declare ast.procName
      r.define ast.procName
      withScope:
        withFunction f:
          for param in ast.procParams:
            r.declare param
            r.define param
          r.resolve ast.procBody
    of astLambda:
      withScope:
        withFunction f:
          for param in ast.lambdaParams:
            r.declare param
            r.define param
          r.resolve ast.lambdaBody
    else:
      assert false, $ast.kind

  case ast.kind
  of astSuper:
    if r.class == resClassNone:
      r.error "Cannot use 'super' outside of a subclass"
    elif r.class == resClass:
      r.error "Cannot use 'super' in a class that has no parent"
    else:
      r.resolveLocal(ast, "super")
  of astThis:
    if r.class == resClassNone:
      r.error "Cannot use 'this' outside of a class"
    else:
      r.resolveLocal ast, "this"
  of astDotAsgn:
    r.resolve ast.dotAsgnCallee
    r.resolve ast.dotAsgnMember
    r.resolve ast.dotAsgnValue
  of astDot:
    r.resolve ast.dotCallee
    r.resolve ast.dotMember
  of astClass:
    let enclosingClass = r.class
    r.class = resClass
    r.resolve ast.className

    if not ast.classParentName.isNil:
      if ast.classParentName.identifier == ast.className.identifier:
        r.error "A class cannot extend itself"
      r.resolve ast.classParentName
      r.class = resSubclass

    if not ast.classParentName.isNil:
      r.scopes.add Scope()
      r.declare "super"
      r.define "super"
      r.scopes[^1].used["super"] = true

    withScope:
      r.declare "this"
      r.define "this"
      r.scopes[^1].used["this"] = true

      for procDef in ast.classMethods:
        if procDef.procName == "init":
          r.resolveProc procDef, resInitializer
        else:
          r.resolveProc procDef, resMethod

    if not ast.classParentName.isNil:
      r.scopes.del r.scopes.high

    r.class = enclosingClass
  of astCall:
    r.resolve ast.callCallee
    for arg in ast.callArgs:
      r.resolve arg
  of astStatementList:
    for it in ast.statements:
      r.resolve(it)
  of astVarDef:
    r.declare ast.varDefName
    if not ast.varDefValue.isNil:
      r.resolve ast.varDefValue
    r.define ast.varDefName
  of astProcDef:
    r.resolveProc ast, resFunc
    # r.declare ast.procName
    # r.define ast.procName
    # withScope:
    #   withFunction resFunc:
    #     for param in ast.procParams:
    #       r.declare param
    #       r.define param
    #     r.resolve ast.procBody
  of astAsgn:
    r.resolve ast.asgnRight
    r.resolveLocal ast.asgnLeft, ast.asgnLeft.identifier
  of astIdentifier:
    if r.scopes != @[]:
      if ast.identifier in r.scopes[^1].symbols:
        if r.scopes[^1].symbols[ast.identifier] == symDeclared:
          r.error &"Cannot read variable in it's own initializer: {ast.identifier}"
    r.resolveLocal ast, ast.identifier
  of astIf:
    r.resolve ast.ifCondition
    r.resolve ast.ifBody
    if not ast.ifElse.isNil:
      r.resolve ast.ifElse
  of astWhile:
    r.resolve ast.whileCondition
    r.resolve ast.whileBody
  of astReturn:
    if r.function in {resFuncNone, resInitializer}:
      r.error "Cannot return a value from this context"
    if not ast.returnValue.isNil:
      r.resolve ast.returnValue
  of astBreak:
    discard
  of astContinue:
    discard
  of astPrint:
    r.resolve ast.printArg
  of astLambda:
    r.resolveProc ast, resFunc
    # withScope:
    #   withFunction resFunc:
    #     for param in ast.lambdaParams:
    #       r.declare param
    #       r.define param
    #     r.resolve ast.lambdaBody
  of astBlock:
    withScope:
      for it in ast.blockStmts:
        r.resolve it
  of astInvalid:
    discard
  of astBinaryOperator:
    r.resolve ast.binaryLeft
    r.resolve ast.binaryRight
  of astUnaryOperator:
    r.resolve ast.unaryOperand
  of astParens:
    r.resolve ast.parensExpr
  of astLiteral:
    discard

proc parse*(p: Parser): Ast =
  result = Ast(kind: astStatementList)
  while not p.eof:
    result.statements.add p.declarationRule()
  assert result.statements != @[], "Expected at least one statement"

proc evalError*(vm: VM, ast: Ast, msg: string) =
  raise EvalError(msg: &"({ast.index}) {msg}")

# proc int*(ast: Ast): int =
#   if ast.kind != astLiteral or ast.literal.kind != litInt:
#     ast.evalError "Expected integer literal"
#   result = ast.literal.intVal

# proc float*(ast: Ast): float =
#   if ast.kind != astLiteral or ast.literal.kind != litFloat:
#     ast.evalError "Expected float literal"
#   result = ast.literal.floatVal

# proc string*(ast: Ast): string =
#   if ast.kind != astLiteral or ast.literal.kind != litString:
#     ast.evalError "Expected string literal"
#   result = ast.literal.stringVal

# proc bool*(ast: Ast): bool =
#   if ast.kind != astLiteral or ast.literal.kind != litBool:
#     ast.evalError "Expected boolean literal"
#   result = ast.literal.boolVal

# proc char*(ast: Ast): char =
#   if ast.kind != astLiteral or ast.literal.kind != litChar:
#     ast.evalError "Expected character literal"
#   result = ast.literal.charVal

# proc int*(val: Value): int =
#   if val.kind != valInt:
#     raiseAssert "Expected integer value"
#   result = val.intVal

# proc float*(val: Value): float =
#   if val.kind != valFloat:
#     raiseAssert "Expected float value"
#   result = val.floatVal

# proc string*(val: Value): string =
#   if val.kind != valString:
#     raiseAssert "Expected string value"
#   result = val.stringVal

# proc bool*(val: Value): bool =
#   if val.kind != valBool:
#     raiseAssert "Expected boolean value"
#   result = val.boolVal

proc equal*(a, b: Value): bool =
  if a.isNil or b.isNil:
    result = true
  elif a.isNil:
    result = false
  elif a.kind != b.kind:
    result = false
  else:
    case a.kind
    of valFloat:
      result = a.float == b.float
    of valString:
      result = a.string == b.string
    of valBool:
      result = a.bool == b.bool
    of valClass, valObj:
      result = a == b # Comparison by reference.
    of valProc:
      if a.procKind != b.procKind:
        result = false
      if a.procParams != b.procParams:
        result = false
      case a.procKind
      of procMethod:
        result = a.methodBody == b.methodBody
      of procNative:
        result = a.procBody == b.procBody
      of procForeign:
        result = a.foreignProc == b.foreignProc

proc `$`*(v: Value): string =
  if v.isNil:
    result = "nil"
  else:
    case v.kind
    of valObj:
      result = v.objClass.className & " {"
      if v.objFields.len > 0:
        result.add " "
        result.add v.objFields.pairs.toSeq.mapIt(it[0] & "=" & $it[1]).join(", ")
        result.add " "
      result.add "}"
    of valClass:
      result = "<class " & v.className & ">"
    of valFloat:
      result = $v.float
      if result.endsWith(".0"):
        result.setLen result.len - 2
    of valString:
      result = v.string.repr
    of valBool:
      result = $v.bool
    of valProc:
      case v.procKind
      of procMethod:
        result = "<method " & v.procName & ">"
      of procNative:
        result = "<proc " & v.procName & ">"
      of procForeign:
        result = "<foreign proc " & v.procName & ">"

proc ancestor*(env: Env, distance: int): Env =
  result = env
  for _ in 0 ..< distance:
    result = result.prev
  assert not result.isNil

proc getAtDistance*(env: Env, indices: ValueIndices): Value =
  var current = env
  for _ in 0 ..< indices.envIdx:
    current = current.prev
  result = current.values[indices.valueIdx]

proc setAtDistance*(env: Env, indices: ValueIndices, value: Value) =
  var current = env
  for _ in 0 ..< indices.envIdx:
    current = current.prev
  current.values[indices.valueIdx] = value

proc getGlobal*(vm: VM, ident: Ast): Value =
  if ident.identifier notin vm.globalEnv.names:
    vm.evalError ident, "Undefined variable: " & ident.identifier
  vm.globalEnv.values[vm.globalEnv.names[ident.identifier]]

proc getLocal*(vm: VM, ast: Ast): Value =
  if ast in vm.locals:
    result = vm.currentEnv.getAtDistance(vm.locals[ast])
  else:
    result = vm.getGlobal(ast)

proc setGlobal*(vm: VM, ident: Ast, value: Value) =
  if ident.identifier notin vm.globalEnv.names:
    vm.evalError ident, "Undefined variable: " & ident.identifier
  vm.globalEnv.values[vm.globalEnv.names[ident.identifier]] = value

proc asgnLocal*(vm: VM, ident: Ast, value: Value) =
  if ident.kind != astIdentifier:
    vm.evalError ident, "Expected identifier"

  if ident in vm.locals:
    vm.currentEnv.setAtDistance(vm.locals[ident], value)
  else:
    vm.setGlobal ident, value

proc declareLocal*(vm: VM, ident: Ast, val: Value) =
  assert not ident.isNil

  if ident.kind != astIdentifier:
    vm.evalError ident, "Expected identifier"

  if vm.currentEnv != nil:
    if ident.identifier in vm.currentEnv.names:
      vm.evalError ident, "Variable already declared in this scope: " & ident.identifier
    vm.currentEnv.names[ident.identifier] = vm.currentEnv.values.len
    vm.currentEnv.values.add val
  else:
    if ident.identifier in vm.globalEnv.names:
      vm.evalError ident, "Variable already declared in this scope: " & ident.identifier
    vm.globalEnv.names[ident.identifier] = vm.globalEnv.values.len
    vm.globalEnv.values.add val

template withNewEnv*(vm: VM, k: EnvKind, body: untyped) =
  block:
    let newEnv = Env(kind: k, prev: vm.currentEnv)
    vm.currentEnv = newEnv
    # if k == scopeProc:
    #   dump vm.currentScope.locals.keys.toSeq
    #   dump vm.currentScope.prevScope.locals.keys.toSeq
    # vm.scopes.add newScope
    try:
      body
    finally:
      # assert vm.scopes[^1] == newScope
      # vm.scopes.del vm.scopes.high
      # dump $vm.currentScope.locals
      # dump $newScope.locals
      assert vm.currentEnv == newEnv
      vm.currentEnv = vm.currentEnv.prev

const breakableScopes = {envBlock}

proc bindObj*(prc: Value, obj: Value) =
  let env = Env(kind: envProc, prev: prc.procClosure)
  env.values.add obj
  env.names["this"] = env.values.high
  prc.procClosure = env

proc eval*(vm: VM, ast: Ast): Value

proc callProc*(vm: VM, callee: Value, args: openarray[Value]): Value =
  assert callee.kind == valProc
  if args.len != callee.procParams.len:
    assert false, "Expected " & $callee.procParams.len & " arguments, got " & $args.len
  case callee.procKind
  of procNative, procMethod:
    try:
      let prev = vm.currentEnv
      let newEnv = Env(kind: envProc, prev: callee.procClosure)
      vm.currentEnv = newEnv
      try:
        for i, arg in args:
          let name = Ast(kind: astIdentifier, identifier: callee.procParams[i])
          vm.declareLocal name, arg
        case callee.procKind
        of procMethod:
          result = vm.eval(callee.methodBody)
        of procNative:
          result = vm.eval(callee.procBody)
        else:
          assert false
        if callee.procIsInitializer:
          result = callee.procClosure.getAtDistance(
            ValueIndices(envIdx: 0, valueIdx: callee.procClosure.names["this"])
          )
      finally:
        assert vm.currentEnv == newEnv
        vm.currentEnv = prev
    except Return as e:
      if callee.procIsInitializer:
        assert false
      result = e.value
  of procForeign:
    result = callee.foreignProc(args)

proc findMethod*(classVal: Value, name: string): Value =
  assert classVal.kind == valClass
  if name in classVal.classMethods:
    result = classVal.classMethods[name]
  else:
    if not classVal.classParent.isNil:
      result = findMethod(classVal.classParent, name)
    else:
      result = nil

proc eval*(vm: VM, ast: Ast): Value =
  assert not ast.isNil
  case ast.kind
  of astSuper:
    let distance = vm.locals[ast]
    let parent = vm.currentEnv.getAtDistance(distance)
    assert parent.kind == valClass, $parent.kind

    var objDistance = distance
    objDistance.envIdx -= 1
    let obj = vm.currentEnv.getAtDistance(objDistance)
    assert obj.kind == valObj, $obj.kind

    let meth = parent.findMethod(ast.superMethodName.identifier)
    if meth.isNil:
      vm.evalError ast, "Undefined method: " & ast.superMethodName.identifier

    meth.bindObj obj
    result = meth
  of astThis:
    result = vm.getLocal ast
  of astDotAsgn:
    let callee = vm.eval(ast.dotAsgnCallee)
    case callee.kind
    of valObj:
      callee.objFields[ast.dotAsgnMember.identifier] = vm.eval(ast.dotAsgnValue)
    else:
      vm.evalError ast, "This does not have properties: " & $callee
  of astDot:
    let callee = vm.eval(ast.dotCallee)
    if callee == nil:
      vm.evalError ast, "The dot operator is not allowed on nil values"
    case callee.kind
    of valObj:
      if ast.dotMember.identifier in callee.objFields:
        result = callee.objFields[ast.dotMember.identifier]
      elif (
        let meth = findMethod(callee.objClass, ast.dotMember.identifier)
        not meth.isNil
      ):
        meth.bindObj callee
        result = meth
      else:
        vm.evalError ast,
          "This object does not have a property: " & ast.dotMember.identifier
    else:
      vm.evalError ast, "This value does not have properties: " & $callee
  of astClass:
    let val = Value(kind: valClass, className: ast.className.identifier)
    let initialEnv = vm.currentEnv

    if not ast.classParentName.isNil:
      val.classParent = vm.eval(ast.classParentName)
      if val.classParent.kind != valClass:
        vm.evalError ast, "Parent class is not a class"

    if not val.classParent.isNil:
      vm.currentEnv = Env(kind: envBlock, prev: vm.currentEnv)
      vm.currentEnv.values.add val.classParent
      vm.currentEnv.names["super"] = vm.currentEnv.values.high

    for procDef in ast.classMethods:
      let meth = Value(
        kind: valProc,
        procName: procDef.procName,
        procKind: procMethod,
        methodBody: procDef.procBody,
        procParams: procDef.procParams,
        procClosure: vm.currentEnv,
      )
      if procDef.procName == "init":
        meth.procIsInitializer = true
      val.classMethods[procDef.procName] = meth

    if not val.classParent.isNil:
      vm.currentEnv = vm.currentEnv.prev
      assert vm.currentEnv == initialEnv

    vm.declareLocal ast.className, val
  of astLambda:
    result = Value(
      kind: valProc,
      procName: "<lambda>",
      procKind: procNative,
      procBody: ast.lambdaBody,
      procParams: ast.lambdaParams,
      procClosure: vm.currentEnv,
    )
  of astReturn:
    raise Return(value: vm.eval(ast.returnValue))
  of astProcDef:
    let name = Ast(kind: astIdentifier, identifier: ast.procName)
    let val = Value(
      kind: valProc,
      procName: ast.procName,
      procKind: procNative,
      procBody: ast.procBody,
      procParams: ast.procParams,
      procClosure: vm.currentEnv,
    )
    vm.declareLocal name, val
  of astCall:
    let callee = vm.eval(ast.callCallee)
    let args = ast.callArgs.mapIt(vm.eval(it))
    case callee.kind
    of valProc:
      result = vm.callProc(callee, args)
    of valClass:
      if (let init = callee.classMethods.getOrDefault("init"); not init.isNil):
        let obj = Value(kind: valObj, objClass: callee)
        init.bindObj obj
        result = vm.callProc(init, args)
      else:
        if args.len != 0:
          vm.evalError ast, "Expected 0 arguments, got " & $args.len
        result = Value(kind: valObj, objClass: callee)
    else:
      vm.evalError ast, "Can't call this value: " & $callee
  of astBreak:
    raise Break()
  of astContinue:
    raise Continue()
  of astBlock:
    vm.withNewEnv envBlock:
      for it in ast.blockStmts:
        result = vm.eval it
  of astAsgn:
    let rhs = vm.eval(ast.asgnRight)
    vm.asgnLocal ast.asgnLeft, rhs
  of astVarDef:
    # dump ast.toLisp
    let value =
      if ast.varDefValue.isNil:
        nil
      else:
        vm.eval(ast.varDefValue)
    let name = Ast(kind: astIdentifier, identifier: ast.varDefName)
    # echo "VAR DEF:"
    # dump name.toLisp
    # dump value[]
    vm.declareLocal name, value
  of astPrint:
    let value = vm.eval(ast.printArg)
    echo value
  of astLiteral:
    case ast.literal.kind
    of litInt:
      # result = Value(kind: valInt, int: ast.literal.intVal)
      result = Value(kind: valFloat, float: ast.literal.intVal.float)
    of litFloat:
      result = Value(kind: valFloat, float: ast.literal.floatVal)
    of litString:
      result = Value(kind: valString, string: ast.literal.stringVal)
    of litBool:
      result = Value(kind: valBool, bool: ast.literal.boolVal)
    of litNil:
      result = nil
    of litChar:
      assert false
  of astInvalid:
    assert false
  of astIdentifier:
    result = vm.getLocal ast
  of astUnaryOperator:
    let right = vm.eval(ast.unaryOperand)
    case ast.unaryOperator
    of unOpInvalid:
      assert false
    of unOpMinus:
      result = Value(kind: valFloat, float: -right.float)
    of unOpNot:
      result = Value(kind: valBool, bool: not right.bool)
  of astBinaryOperator:
    let left = vm.eval(ast.binaryLeft)
    let right = vm.eval(ast.binaryRight)
    case ast.binaryOperator
    of binOpPlus:
      result = Value(kind: valFloat, float: left.float + right.float)
    of binOpMinus:
      result = Value(kind: valFloat, float: left.float - right.float)
    of binOpMultiply:
      result = Value(kind: valFloat, float: left.float * right.float)
    of binOpDivide:
      result = Value(kind: valFloat, float: left.float / right.float)
    of binOpModulo:
      result = Value(kind: valFloat, float: left.float mod right.float)
    of binOpPower:
      result = Value(kind: valFloat, float: left.float.pow(right.float))
    of binOpAnd:
      result = Value(kind: valBool, bool: left.bool and right.bool)
    of binOpOr:
      result = Value(kind: valBool, bool: left.bool or right.bool)
    of binOpEqual:
      result = Value(kind: valBool, bool: left.float == right.float)
    of binOpNotEqual:
      result = Value(kind: valBool, bool: left.float != right.float)
    of binOpLessThan:
      result = Value(kind: valBool, bool: left.float < right.float)
    of binOpInvalid:
      assert false
    of binOpGreaterThan:
      result = Value(kind: valBool, bool: left.float > right.float)
    of binOpGreaterThanOrEqual:
      result = Value(kind: valBool, bool: left.float >= right.float)
    of binOpLessThanOrEqual:
      result = Value(kind: valBool, bool: left.float <= right.float)
    of binOpConcat:
      result = Value(kind: valString, string: left.string & right.string)
  of astParens:
    result = vm.eval(ast.parensExpr)
  of astStatementList:
    for it in ast.statements:
      result = vm.eval it
  of astIf:
    if vm.eval(ast.ifCondition).bool:
      result = vm.eval(ast.ifBody)
    else:
      if not ast.ifElse.isNil:
        result = vm.eval(ast.ifElse)
  of astWhile:
    while vm.eval(ast.whileCondition).bool:
      try:
        result = vm.eval(ast.whileBody)
      except Break:
        break
      except Continue:
        continue

proc newVm*(): VM =
  result = VM()
  result.globalEnv = Env()
  result.currentEnv = result.globalEnv

  # Add clock function to global environment
  let clockIndex = result.globalEnv.values.len
  result.globalEnv.values.add Value(
    kind: valProc,
    procKind: procForeign,
    procName: "clock",
    procParams: @[],
    foreignProc: proc(args: openarray[Value]): Value =
      result = Value(kind: valFloat, float: getTime().toUnixFloat()),
  )
  result.globalEnv.names["clock"] = clockIndex

type Output* = ref object
  value*: Value
  # tokenizerError*: string
  # parserErrors*: seq[string]
  # resolverErrors*: seq[string]
  errors*: seq[string]
  warnings*: seq[string]

when defined js:
  import jsffi

  proc runCodeFromJs*(cs: cstring): JsObject {.exportc.} =
    result = newJsObject()
    result["tokenizerError"] = nil
    result["parserErrors"] = nil
    result["parserCriticalError"] = nil
    result["resolverError"] = nil
    result["resolverWarnings"] = nil
    result["evalError"] = nil
    result["evalResult"] = nil

    let tokens =
      try:
        let s = $cs
        # dump s
        echo "Tokenizing..."
        tokenize(s)
      except TokenizerError as e:
        echo "Tokenizer error: " & e.msg
        result["tokenizerError"] = (msg: e.msg.toJs).toJs
        return result

    let p = Parser(tokens: tokens)

    let ast =
      try:
        echo "Parsing..."
        p.parse()
      except ParseError as e:
        echo "Parser error: " & e.msg
        result["parserErrors"] = p.errors.toJs
        result["parserCriticalError"] = (msg: e.msg.toJs).toJs
        return result

    result["parserErrors"] = p.errors.toJs

    let vm = newVm()

    try:
      echo "Evaluating..."
      let evalResult = vm.eval ast
      echo "Eval result: " & $evalResult
      result["evalResult"] = evalResult.toJs
      return result
    except EvalError as e:
      echo "Eval error: " & e.msg
      result["evalError"] = (msg: e.msg.toJs).toJs
      return result

proc runCode*(vm: VM, code: string): Output =
  when debug:
    header "CODE"
    echo code
  let tokens = tokenize(code)
  when debug:
    header "TOKENS"
    echo tokens
  result = Output()
  when debug:
    header "TOKENIZER"
  let p = Parser(tokens: tokens)
  when debug:
    header "PARSER"
  let ast = p.parse()
  when debug:
    header "LISP"
    echo ast.toLisp
  when debug:
    header "PARSER ERRORS"
    echo p.errors
  if p.errors == @[]:
    when debug:
      header "RESOLVER"
    let resolver = Resolver(vm: vm)
    when debug:
      try:
        resolver.resolve ast
      except ResolverError as e:
        header "RESOLVER ERROR"
        echo e.msg
        return
    else:
      resolver.resolve ast
    when debug:
      header "RESOLVER WARNINGS"
      echo resolver.warnings
    when debug:
      header "EVAL"
      try:
        result.value = vm.eval ast
      except EvalError as e:
        header "EVAL ERROR"
        echo e.msg
        return
    else:
      result.value = vm.eval ast
    when debug:
      header "RESULT"
      echo result.value
  else:
    result.errors = p.errors

# proc main(): cint {.cdecl, exportc.} =
#   echo "Hello world"

# TODO: Code below is just for emscripten.

when defined emscripten:
  proc `%`(o: char): JsonNode =
    result = JsonNode(kind: JString, str: $o)

  {.
    emit:
      """
    #include <emscripten/emscripten.h>
  """
  .}

  # proc foo*() {.exportc, codegenDecl: "EMSCRIPTEN_KEEPALIVE $# $#$#".} =
  #   echo "FOO"
  #   assert false

  # proc runCodeWasm*(
  #     cs: cstring
  # ): cstring {.exportc, codegenDecl: "EMSCRIPTEN_KEEPALIVE $# $#$#".} =
  #   assert false
  #   let json = JsonNode(kind: JObject)
  #   json["tokenizerError"] = nil
  #   json["parserErrors"] = nil
  #   json["parserCriticalError"] = nil
  #   json["resolverError"] = nil
  #   json["resolverWarnings"] = nil
  #   json["evalError"] = nil
  #   json["evalResult"] = nil

  #   let tokens =
  #     try:
  #       let s = $cs
  #       dump s
  #       echo "Tokenizing..."
  #       tokenize(s)
  #     except TokenizerError as e:
  #       echo "Tokenizer error: " & e.msg
  #       json["tokenizerError"] = %*{"msg": e.msg}
  #       return cstring $json

  #   let p = Parser(tokens: tokens)

  #   let ast =
  #     try:
  #       echo "Parsing..."
  #       p.parse()
  #     except ParseError as e:
  #       echo "Parser error: " & e.msg
  #       json["parserErrors"] = %*p.errors
  #       json["parserCriticalError"] = %*{"msg": e.msg}
  #       return cstring $json

  #   json["parserErrors"] = %*p.errors

  #   let vm = newVm()

  #   try:
  #     echo "Evaluating..."
  #     let evalResult = vm.eval ast
  #     echo "Eval result: " & $evalResult
  #     # json["evalResult"] = %*evalResult
  #     return cstring $json
  #   except EvalError as e:
  #     echo "Eval error: " & e.msg
  #     json["evalError"] = %*{"msg": e.msg}
  #     return cstring $json

  proc runCodeWasm*(s: string): string =
    let json = JsonNode(kind: JObject)
    json["tokenizerError"] = nil
    json["parserErrors"] = nil
    json["parserCriticalError"] = nil
    json["resolverError"] = nil
    json["resolverWarnings"] = nil
    json["evalError"] = nil
    json["evalResult"] = nil

    let tokens =
      try:
        let s = s
        # dump s
        echo "Tokenizing..."
        tokenize(s)
      except TokenizerError as e:
        echo "Tokenizer error: " & e.msg
        json["tokenizerError"] = %*{"msg": e.msg}
        return $json

    let p = Parser(tokens: tokens)

    let ast =
      try:
        echo "Parsing..."
        p.parse()
      except ParseError as e:
        echo "Parser error: " & e.msg
        json["parserErrors"] = %*p.errors
        json["parserCriticalError"] = %*{"msg": e.msg}
        return $json

    json["parserErrors"] = %*p.errors

    let vm = newVm()

    let resolver = Resolver(vm: vm)
    try:
      echo "Resolving..."
      resolver.resolve ast
    except ResolverError as e:
      echo "Resolver error: " & e.msg
      json["resolverError"] = %*{"msg": e.msg}
      json["resolverWarnings"] = %*resolver.warnings
      return $json

    json["resolverWarnings"] = %*resolver.warnings

    try:
      echo "Evaluating..."
      discard vm.eval ast
      # echo "Eval result: " & $evalResult
      # json["evalResult"] = %*evalResult
      return $json
    except EvalError as e:
      echo "Eval error: " & e.msg
      json["evalError"] = %*{"msg": e.msg}
      return $json

  when isMainModule:
    # echo "The lox file was loaded"
    echo runCodeWasm(paramStr(1))
