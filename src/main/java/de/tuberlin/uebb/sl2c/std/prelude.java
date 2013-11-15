package de.tuberlin.uebb.sl2c.std;

import de.tuberlin.uebb.sl2.runtime.Closure;

public final class prelude {
    
    public final static Object True() { return true; }
    public final static Object False() { return false; }

    private static final class Add implements Closure {
	public Object apply(final Object arg) {
	    return new Add1(arg);
	}
    }

    private static final class Add1 implements Closure {
	private final int arg0;

	public Add1(final Object a) {
	    this.arg0 = (Integer)a;
	}

	public Object apply(final Object arg) {
	    return arg0 + (Integer)arg; 
	}
    }
  

    final static Object add = new Add();
    public final static Object $p() { return add; }

    private static final class Sub implements Closure {
	public Object apply(final Object arg) {
	    return new Sub1(arg);
	}
    }

    private static final class Sub1 implements Closure {
	private final int arg0;

	public Sub1(final Object a) {
	    this.arg0 = (Integer)a;
	}

	public Object apply(final Object arg) {
	    return arg0 - (Integer)arg; 
	}
    }
    
    final static Object sub = new Sub();
    public final static Object $m() { return sub; }

    private static final class Mult implements Closure {
	public Object apply(final Object arg) {
	    return new Mult1(arg);
	}
    }

    private static final class Mult1 implements Closure {
	private final int arg0;

	public Mult1(final Object a) {
	    this.arg0 = (Integer)a;
	}

	public Object apply(final Object arg) {
	    return arg0 * (Integer)arg; 
	}
    }
  
    final static Object mult = new Mult();
    public final static Object $t() { return mult; }

    private static final class Div implements Closure {
	public Object apply(final Object arg) {
	    return new Div1(arg);
	}
    }

    private static final class Div1 implements Closure {
	private final int arg0;

	public Div1(final Object a) {
	    this.arg0 = (Integer)a;
	}

	public Object apply(final Object arg) {
	    return arg0 / (Integer)arg; 
	}
    }
  
    final static Object div = new Div();
    public final static Object $d() { return div; }


    private static final class Less implements Closure {
	public Object apply(final Object arg) {
	    return new Less1(arg);
	}
    }

    private static final class Less1 implements Closure {
	private final int arg0;

	public Less1(final Object a) {
	    this.arg0 = (Integer)a;
	}

	public Object apply(final Object arg) {
	    return arg0 < (Integer)arg; 
	}
    }
  
    final static Object less = new Less();
    public final static Object $l() { return less; } 


    private static final class Concat implements Closure {
	public Object apply(final Object arg) {
	    return new Concat1(arg);
	}
    }

    private static final class Concat1 implements Closure {
	private final String arg0;

	public Concat1(final Object a) {
	    this.arg0 = (String)a;
	}

	public Object apply(final Object arg) {
	    return arg0 + (String)arg; 
	}
    }
  
    final static Object conc = new Concat();
    public final static Object $p$p() { return conc; }


    private static final class IntEq implements Closure {
	public Object apply(final Object a) {
	    return new IntEq1(a);
	}
    }

    private static final class IntEq1 implements Closure {
	private final int arg0;
	
	public IntEq1(final Object a) {
	    this.arg0 = (int)a;
	}

	public Object apply(final Object arg) {
	    return arg.equals(arg0);
	}       
    }

    final static Object intEq = new IntEq();
    public static Object $e$e() { return intEq; }

    private static final class IntNeg implements Closure {
	public Object apply(final Object arg) {
	    return -1 * ((Integer)arg);
	}
    }

    final static Object intNeg = new IntNeg();
    public static Object neg() { return intNeg; }
    
}
