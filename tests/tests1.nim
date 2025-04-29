import ../src/nimlang
import std/[sugar, sequtils, tables]

# template section(code) =
#   block:
#     code
#     echo "--------------------------------"

# template section(s: string, body) =
#   echo "--------------------------------"
#   echo s
#   echo "--------------------------------"
#   body

# type Flag = enum
#   skipRunning

template testCode(s) =
  block:
    let vm = newVm()
    discard vm.runCode(s)
    # let code {.inject.} = s
    # let tokens {.inject.} = tokenize(code)
    # echo "Code:"
    # echo code.repr
    # echo "Tokens:"
    # echo tokens
    # let parser = Parser(tokens: tokens)
    # var ast: Ast
    # echo "Parser:"
    # # if parserErrorExpected:
    # try:
    #   ast = parser.parse()
    # except ParseError as e:
    #   echo e.msg
    # # else:
    # #   ast = parser.parse()
    # echo "AST:"
    # echo ast.toLisp
    # echo "Errors:"
    # echo parser.errors.mapIt($it)
    # if not ast.isNil and parser.errors == @[]:
    #   try:
    #     let vm = newVm()
    #     echo "Resolver:"
    #     vm.resolve ast
    #     echo "Output:"
    #     let result = vm.eval(ast)
    #     echo "Result: "
    #     echo result
    #   except Exception as e:
    #     echo "Stack trace:"
    #     echo e.getStackTrace()
    #     echo "Error: "
    #     echo e.msg
    # else:
    #   echo "(Code was not run)"
    echo "--------------------------------"

testCode "123;"
testCode "123 + 456;"
testCode "1 + 2 / 3;"
testCode "1 / 2 + 3;"
testCode "1 + 2 / 3 * 4;"

testCode "* 1;"
testCode "* /;"

testCode "\"foobar\";"
testCode "\"foo\" & \"bar\";"

testCode "print \"666\";"
testCode "print \"666\"; 777;"
testCode "print 1 + 2;"

testCode "var x = 1;"
testCode "var x = 1; x;"
testCode "var x = 1; x + 2;"

testCode "var x = 1; x = 123; print x;"

# TODO: catch parser errors
# testCode, "a = b + c;"
# testCode, "a + b = c;"

testCode:
  """
  {
    var x = 1;
    print x;
  }
  """

testCode:
  """
  var x = 2;
  var y = 0;
  {
    var x = 1;
    y = 1337;
    print x;
  }
  print x;
  print y;
  """

testCode:
  """
  var a = true;
  var b = false;
  if (a) {
    print "a is true";
  } else {
    print "a is false";
  }
  if (b) {
    print "b is true";
  } else {
    print "b is false";
  }
  """

testCode:
  """
  print true and true;
  print true and false;
  print false and true;
  print false and false;

  print true or true;
  print true or false;
  print false or true;
  print false or false;
  """

testCode:
  """
  var i = 1;
  while (i <= 5) {
    print i;
    i = i + 1;
  }
  """

testCode:
  """
  for (var i = 1; i <= 5; i = i + 1) {
    print i;
  }
  """

testCode:
  """
  var i = 1;
  while (true) {
    if (i > 5) {
      break;
    }
    print i;
    i = i + 1;
  }
  """

testCode:
  """
  print -1;
  """

testCode:
  """
  var i = -5;
  while (i <= 5) {
    if (i < 0) {
      i = i + 1;
      continue;
    }
    print i;
    i = i + 1;
  }
  """

testCode:
  """
  proc test() {
  }
  """

testCode:
  """
  proc sayHi(first, last) {
  }
  """

testCode:
  """
  proc sayHi(first, last) {
    print "Hi, " & first & " " & last & "!";
  }
  """

# TODO: catch the resolver error
# testCode {}:
#   """
#   foo();
#   """

testCode:
  """
  proc sayHi(first, last) {
    print "Hi, " & first & " " & last & "!";
  }

  sayHi("Dear", "Reader");
  """

testCode:
  """
  proc foo() {
    return 123;
  }

  print foo();
  """

testCode:
  """print 1 - 2;"""

# TODO: catch the resolver error
# testCode {}:
#   """
#   foo(n - 1);
#   """

testCode:
  """
    proc fib(n) {
      if (n <= 1) return n;
      return fib(n - 2) + fib(n - 1);
    }

    for (var i = 0; i < 20; i = i + 1) {
      print fib(i);
    }
  """

testCode:
  """
    proc foo() {
      var i = 123;
      proc bar() {
        print i;
      }
      bar();
    }

    foo();
  """

testCode:
  """
    proc makeCounter() {
      var i = 0;
      proc count() {
        i = i + 1;
        print i;
      }

      return count;
    }

    var counter = makeCounter();
    counter();
    counter();
  """

testCode:
  """
    proc callTwice(f) {
      f();
      f();
    }

    proc foo() {
      print "foo";
    }

    callTwice(foo);
  """

testCode:
  """
    proc callTwice(f) {
      f();
      f();
    }

    callTwice(proc() {
      print "foo";
    });
  """

testCode:
  """
    proc callWith123(f) {
      f(123);
    }

    callWith123(proc(x) {
      print x;
    });
  """

# Checking if resolved finds unused variables.
testCode:
  """
    var x = 1;
    var y = 2;
    var z = 3;
    {
      var xx = 1;
      var yy = 2;
      {
        var zz = xx;
        var aa = 123;
        {
          var bb = 456;
          var cc = aa;
        }
      }
    }
    var result = y;
  """

testCode:
  """
  class Breakfast {
    cook() {
      print "Eggs a-fryin'!";
    }

    serve(who) {
      print "Enjoy your breakfast, " + who + ".";
    }
  }

  print Breakfast;
  """

testCode:
  """
  class Bacon {
    eat() {
      print "Eating bacon!";
    }
  }

  Bacon().eat();
  Bacon().eat();
  Bacon().eat();
  """

testCode:
  """
  class Foo {
    init() {
      print this;
    }
  }

  var foo = Foo();
"""

testCode:
  """
  class Foo {
    init(name) {
      this.name = name;
    }

    sayHi() {
      print "Hi, " & this.name & "!";
    }
  }

  var foo = Foo("John");
  foo.sayHi();
"""

testCode:
  """
  class Foo {
    init() {
      return ":D";
    }
  }
"""

testCode:
  """
  class BostonCream < Doughnut {
  }
  """

testCode:
  """
  class Oops < Oops {}
  """

testCode:
  """
  class Doughnut {
    cook() {
      print "Fry until golden brown.";
    }
  }

  class BostonCream < Doughnut {}

  BostonCream().cook();
  """

testCode:
  """
    class Doughnut {
      cook() {
        print "Fry until golden brown.";
      }
    }

    class BostonCream < Doughnut {
      cook() {
        super.cook();
        print "Pipe full of custard and coat with chocolate.";
      }
    }

    BostonCream().cook();
  """

testCode:
  """
    super.test;
  """

testCode:
  """
    class Foo {
      init() {
        super.test;
      }
    }
  """

testCode:
  """
   // Lox language sample
  class Calculator {
    init() {
      this.result = 0;
    }

    add(value) {
      this.result = this.result + value;
      return this;
    }

    multiply(value) {
      this.result = this.result * value;
      return this;
    }

    getResult() {
      return this.result;
    }
  }

  // Create calculator and perform operations
  var calc = Calculator();
  calc.add(5).multiply(2).add(10);
  print calc.getResult(); // Prints: 20
  """
