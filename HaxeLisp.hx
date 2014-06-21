enum LObj {
  Nil;
  Num(num : Int);
  Sym(name : String);
  Error(msg : String);
  Cons(cell : Cell);
  Subr(fn : LObj -> LObj);
  Expr(args : LObj, body : LObj, env : LObj);
}

class Cell {
  public function new (a, d) {
    this.car = a;
    this.cdr = d;
  }

  public static function invalid() { return invalid_; }
  public function isValid() { return this != invalid_; }

  public var car : LObj;
  public var cdr : LObj;
  private static var invalid_ = new Cell(Nil, Nil);
}

class Util {
  public static function makeSym(str) {
    if (str == "nil") {
      return kNil;
    } else if (!symMap.exists(str)) {
      symMap.set(str, Sym(str));
    }
    return symMap.get(str);
  }

  public static function makeExpr(args, env) {
    return Expr(safeCar(args), safeCdr(args), env);
  }

  public static function safeCar(obj) {
    return switch (obj) {
      case Cons(cell): cell.car;
      default: kNil;
    }
  }

  public static function safeCdr(obj) {
    return switch (obj) {
      case Cons(cell): cell.cdr;
      default: kNil;
    }
  }

  public static function makeCons(a, d) {
    return Cons(new Cell(a, d));
  }

  public static function getCell(obj) {
    return switch (obj) {
      case Cons(cell): return cell;
      default: Cell.invalid();
    }
  }

  public static function isError(obj) {
    return switch (obj) {
      case Error(_): true;
      default: false;
    }
  }

  public static function isNum(obj) {
    return switch (obj) {
      case Num(_): true;
      default: false;
    }
  }

  public static function getNum(obj) {
    return switch (obj) {
      case Num(num): num;
      default: -1;
    }
  }

  public static function nreverse(lst) {
    var ret = kNil;
    var cell;
    while ((cell = Util.getCell(lst)).isValid()) {
      var tmp = cell.cdr;
      cell.cdr = ret;
      ret = lst;
      lst = tmp;
    }
    return ret;
  }

  public static function pairlis(lst1, lst2) {
    var ret = kNil;
    var cell1, cell2;
    while ((cell1 = getCell(lst1)).isValid() &&
           (cell2 = getCell(lst2)).isValid()) {
      ret = makeCons(makeCons(cell1.car, cell2.car), ret);
      lst1 = cell1.cdr;
      lst2 = cell2.cdr;
    }
    return nreverse(ret);
  }

  public static var kNil = Nil;
  private static var symMap : Map<String, LObj> = new Map();
}

class ParseState {
  public function new(o, n) {
    this.obj = o;
    this.next = n;
  }
  public var obj : LObj;
  public var next : String;
}

class Reader {
  private static function isSpace(c) {
    return c == "\t" || c == "\r" || c == "\n" || c == " ";
  }

  private static function isDelimiter(c) {
    return c == kLPar || c == kRPar || c == kQuote || isSpace(c);
  }

  private static function skipSpaces(str: String) {
    var i = 0;
    while (i < str.length) {
      if (!isSpace(str.charAt(i))) {
        break;
      }
      i++;
    }
    return str.substring(i);
  }

  private static function makeNumOrSym(str : String) {
    var n = Std.parseInt(str);
    if (n == null) {
      return Util.makeSym(str);
    }
    return Num(n);
  }

  private static function parseError(str) {
    return new ParseState(Error(str), "");
  }

  private static function readAtom(str : String) {
    var next : String = "";
    var i = 0;
    while (i < str.length) {
      if (isDelimiter(str.charAt(i))) {
        next = str.substring(i);
        str = str.substring(0, i);
        break;
      }
      i++;
    }
    return new ParseState(makeNumOrSym(str), next);
  }

  public static function read(str : String) {
    str = skipSpaces(str);
    if (str.length == 0) {
      return parseError("empty input");
    } else if (str.charAt(0) == kRPar) {
      return parseError("invalid syntax: " + str);
    } else if (str.charAt(0) == kLPar) {
      return readList(str.substring(1));
    } else if (str.charAt(0) == kQuote) {
      var tmp = read(str.substring(1));
      return new ParseState(Util.makeCons(Util.makeSym("quote"),
                                         Util.makeCons(tmp.obj, Util.kNil)),
                           tmp.next);
    }
    return readAtom(str);
  }

  public static function readList(str : String) {
    var ret = Util.kNil;
    while (true) {
      str = skipSpaces(str);
      if (str.length == 0) {
        return parseError("unfinished parenthesis");
      } else if (str.charAt(0) == kRPar) {
        break;
      }
      var tmp = read(str);
      if (Util.isError(tmp.obj)) {
        return tmp;
      }
      ret = Util.makeCons(tmp.obj, ret);
      str = tmp.next;
    }
    return new ParseState(Util.nreverse(ret), str.substring(1));
  }

  private static var kLPar = "(";
  private static var kRPar = ")";
  private static var kQuote = "'";
}

class Printer {
  public static function print(obj) {
    return switch (obj) {
      case Nil: "nil";
      case Num(num): Std.string(num);
      case Sym(name): name;
      case Error(msg): "<error: " + msg + ">";
      case Cons(cell): printList(obj);
      case Subr(fn): "<subr>";
      case Expr(args, body, env): "<expr>";
      default: "<unknown>";
    }
  }

  private static function printList(obj) {
    var ret = "";
    var first = true;
    var cell;
    while ((cell = Util.getCell(obj)).isValid()) {
      if (first) {
        first = false;
      } else {
        ret += " ";
      }
      ret += print(cell.car);
      obj = cell.cdr;
    }
    if (obj == Util.kNil) {
      return "(" + ret + ")";
    }
    return "(" + ret + " . " + print(obj) + ")";
  }
}

class Evaluator {
  private static function findVar(sym, env) {
    var cell;
    while ((cell = Util.getCell(env)).isValid()) {
      var alist = cell.car;
      var cell2;
      while ((cell2 = Util.getCell(alist)).isValid()) {
        if (Util.safeCar(cell2.car) == sym) {
          return cell2.car;
        }
        alist = cell2.cdr;
      }
      env = cell.cdr;
    }
    return Util.kNil;
  }

  public static function addToEnv(sym, val, env) {
    var cell = Util.getCell(env);
    cell.car = Util.makeCons(Util.makeCons(sym, val), cell.car);
  }

  public static function eval(obj, env) {
    switch (obj) {
      case Nil: return obj;
      case Num(_): return obj;
      case Error(_): return obj;
      case Sym(name): {
        var bind = findVar(obj, env);
        if (bind == Util.kNil) {
          return Error(name + " has no value");
        }
        return Util.getCell(bind).cdr;
      }
      default: true;
    }

    var op = Util.safeCar(obj);
    var args = Util.safeCdr(obj);
    if (op == Util.makeSym("quote")) {
      return Util.safeCar(args);
    } else if (op == Util.makeSym("if")) {
      var c = eval(Util.safeCar(args), env);
      if (Util.isError(c)) { return c; }
      if (c == Util.kNil) {
        return eval(Util.safeCar(Util.safeCdr(Util.safeCdr(args))), env);
      }
      return eval(Util.safeCar(Util.safeCdr(args)), env);
    } else if (op == Util.makeSym("lambda")) {
      return Util.makeExpr(args, env);
    } else if (op == Util.makeSym("defun")) {
      var expr = Util.makeExpr(Util.safeCdr(args), env);
      var sym = Util.safeCar(args);
      addToEnv(sym, expr, gEnv);
      return sym;
    } else if (op == Util.makeSym("setq")) {
      var val = eval(Util.safeCar(Util.safeCdr(args)), env);
      if (Util.isError(val)) { return val; }
      var sym = Util.safeCar(args);
      var bind = findVar(sym, env);
      if (bind == Util.kNil) {
        addToEnv(sym, val, gEnv);
      } else {
        Util.getCell(bind).cdr = val;
      }
      return val;
    }
    return apply(eval(op, env), evlis(args, env), env);
  }

  private static function evlis(lst, env) {
    var ret = Util.kNil;
    var cell;
    while ((cell = Util.getCell(lst)).isValid()) {
      var elm = eval(cell.car, env);
      if (Util.isError(elm)) { return elm; }
      ret = Util.makeCons(elm, ret);
      lst = cell.cdr;
    }
    return Util.nreverse(ret);
  }

  private static function progn(body, env) {
    var ret = Util.kNil;
    var cell;
    while ((cell = Util.getCell(body)).isValid()) {
      ret = eval(cell.car, env);
      body = cell.cdr;
    }
    return ret;
  }

  private static function apply(fn, args, env) {
    if (Util.isError(fn)) { return fn; }
    if (Util.isError(args)) { return args; }
    return switch (fn) {
      case Subr(fn): fn(args);
      case Expr(a, b, e): progn(b, Util.makeCons(Util.pairlis(a, args), e));
      default: Error(Printer.print(fn) + " is not function");
    }
  }

  private static function subrCar(args) {
    return Util.safeCar(Util.safeCar(args));
  }

  private static function subrCdr(args) {
    return Util.safeCdr(Util.safeCar(args));
  }

  private static function subrCons(args) {
    return Util.makeCons(Util.safeCar(args), Util.safeCar(Util.safeCdr(args)));
  }

  private static function subrEq(args) {
    var x = Util.safeCar(args);
    var y = Util.safeCar(Util.safeCdr(args));
    if (Util.isNum(x) && Util.isNum(y)) {
      if (Util.getNum(x) == Util.getNum(y)) {
        return Util.makeSym("t");
      }
      return Util.kNil;
    } else if (x == y) {
      return Util.makeSym("t");
    }
    return Util.kNil;
  }

  private static function subrAtom(args) {
    return switch (Util.safeCar(args)) {
      case Cons(_): Util.kNil;
      default: Util.makeSym("t");
    };
  }

  private static function subrNumberp(args) {
    return switch (Util.safeCar(args)) {
      case Num(_): Util.makeSym("t");
      default: Util.kNil;
    };
  }

  private static function subrSymbolp(args) {
    return switch (Util.safeCar(args)) {
      case Sym(_): Util.makeSym("t");
      default: Util.kNil;
    };
  }

  private static function subrAddOrMul(initVal, fn) {
    return function(args) {
      var ret = initVal;
      var cell;
      while ((cell = Util.getCell(args)).isValid()) {
        if (!Util.isNum(cell.car)) {
          return Error("wrong type");
        }
        ret = fn(ret, Util.getNum(cell.car));
        args = cell.cdr;
      }
      return Num(ret);
    };
  }
  private static var subrAdd =
      subrAddOrMul(0, function(x, y) { return x + y; });
  private static var subrMul =
      subrAddOrMul(1, function(x, y) { return x * y; });

  private static function subrSubOrDivOrMod(fn) {
    return function(args) {
      var x = Util.safeCar(args);
      var y = Util.safeCar(Util.safeCdr(args));
      if (!Util.isNum(x) || !Util.isNum(y)) {
        return Error("wrong type");
      }
      return Num(fn(Util.getNum(x), Util.getNum(y)));
    };
  }
  private static var subrSub =
      subrSubOrDivOrMod(function(x, y) { return x - y; });
  private static var subrDiv =
      subrSubOrDivOrMod(function(x, y) { return Std.int(x / y); });
  private static var subrMod =
      subrSubOrDivOrMod(function(x, y) { return x % y; });

  private static function makeGlobalEnv() {
    var env = Util.makeCons(Util.kNil, Util.kNil);
    addToEnv(Util.makeSym("car"), Subr(subrCar), env);
    addToEnv(Util.makeSym("cdr"), Subr(subrCdr), env);
    addToEnv(Util.makeSym("cons"), Subr(subrCons), env);
    addToEnv(Util.makeSym("eq"), Subr(subrEq), env);
    addToEnv(Util.makeSym("atom"), Subr(subrAtom), env);
    addToEnv(Util.makeSym("numberp"), Subr(subrNumberp), env);
    addToEnv(Util.makeSym("symbolp"), Subr(subrSymbolp), env);
    addToEnv(Util.makeSym("+"), Subr(subrAdd), env);
    addToEnv(Util.makeSym("*"), Subr(subrMul), env);
    addToEnv(Util.makeSym("-"), Subr(subrSub), env);
    addToEnv(Util.makeSym("/"), Subr(subrDiv), env);
    addToEnv(Util.makeSym("mod"), Subr(subrMod), env);
    addToEnv(Util.makeSym("t"), Util.makeSym("t"), env);
    return env;
  }

  public static function globalEnv() { return gEnv; }

  private static var gEnv = makeGlobalEnv();
}

class HaxeLisp {
  static function main() {
    var stdin = Sys.stdin();
    var gEnv = Evaluator.globalEnv();
    Sys.print("> ");
    try while (true) {
      var line = stdin.readLine();
      Sys.println(Printer.print(Evaluator.eval(Reader.read(line).obj, gEnv)));
      Sys.print("> ");
    } catch(_ : haxe.io.Eof) true;
  }
}