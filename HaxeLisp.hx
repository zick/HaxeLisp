enum LObj {
  Nil;
  Num(num : Int);
  Sym(name : String);
  Error(msg : String);
  Cons(car : LObj, cdr : LObj);
  Subr(fn : LObj -> LObj);
  Expr(args : LObj, body : LObj, env : LObj);
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
      case Cons(car, cdr): car;
      default: kNil;
    }
  }

  public static function safeCdr(obj) {
    return switch (obj) {
      case Cons(car, cdr): cdr;
      default: kNil;
    }
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
      return parseError("noimpl");
    } else if (str.charAt(0) == kQuote) {
      return parseError("noimpl");
    }
    return readAtom(str);
  }

  private static var kLPar = "(";
  private static var kRPar = ")";
  private static var kQuote = "'";
}

class HaxeLisp {
  static function main() {
    var stdin = Sys.stdin();
    Sys.print("> ");
    try while (true) {
      var line = stdin.readLine();
      Sys.println(Reader.read(line));
      Sys.print("> ");
    } catch(_ : haxe.io.Eof) true;
  }
}