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
  public var car : LObj;
  public var cdr : LObj;
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

  public static function nreverse(lst) {
    var ret = kNil;
    while (true) {
      var cell;
      switch (lst) {
        case Cons(c): cell = c;
        default: break;  // break from while
      }
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
      switch (tmp.obj) {
        case Error(_): return tmp;  // return from readList
        default: true;
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
    while (true) {
      switch (obj) {
        case Cons(_): true;
        default: break;  // break from while
      }
      if (first) {
        first = false;
      } else {
        ret += " ";
      }
      ret += print(Util.safeCar(obj));
      obj = Util.safeCdr(obj);
    }
    if (obj == Util.kNil) {
      return "(" + ret + ")";
    }
    return "(" + ret + " . " + print(obj) + ")";
  }
}

class HaxeLisp {
  static function main() {
    var stdin = Sys.stdin();
    Sys.print("> ");
    try while (true) {
      var line = stdin.readLine();
      Sys.println(Printer.print(Reader.read(line).obj));
      Sys.print("> ");
    } catch(_ : haxe.io.Eof) true;
  }
}