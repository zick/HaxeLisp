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
      default: return Error("noimpl");
    }
  }

  private static function makeGlobalEnv() {
    var env = Util.makeCons(Util.kNil, Util.kNil);
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